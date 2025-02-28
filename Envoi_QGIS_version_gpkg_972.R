############################################################
##### Programme R pour création des plans de collectes #####
############################################################

# proxy INsee, si on ne le lance pas il ne va pas trouver les packages (l'Insee bloque les packages sinon)

Sys.setenv(no_proxy = "")
Sys.setenv(https_proxy ="http://proxy-rie.http.insee.fr:8080")
Sys.setenv(http_proxy ="http://proxy-rie.http.insee.fr:8080")

# on défini les packages à lancer et on les lance tous avec lapply

packages <- c("readr", "purrr", "readxl", "pbapply","dplyr","sf","igraph","ggplot2","fpc", "gridExtra", "geomtextpath","ggrepel","raster","tidyterra","arrow","grid","gridExtra","stringr")
#lapply(packages,install.packages)
sapply(packages, function(x) library(x, character.only = TRUE))
sf_use_s2(FALSE)

rm(list=ls())
source("fonctions.R")

chemin <- "C:/Users/H0G0U0/Documents/RP/Travail avec Clement/Zoom_RP_Martinique/"

##liste_GC <- c("97207","97209","97210","97213","97220","97221","97222","97223","97224","97228","97229","97230")
liste_PC_GR2 <- c("97201","97212","97225","97226","97227")

dir <-"input/ril/"
dir_ril <- paste0(chemin,dir)

# on récupère les RIL millésimés
ril <- lapply(list.files(dir_ril), function(x){
  #x <- list.files(dir_ril)[1]
  print(x)
#  detecteur_pc <- sapply(liste_PC_GR2,function(pc) grepl(pc,x))|> any()
#  table <- if(!detecteur_pc) read.csv2(paste0(dir_ril,x)) else read.csv(paste0(dir_ril,x))
#  names(table) <- iconv(names(table), from = "UTF-8", to = "ASCII//TRANSLIT")
  table <-  read.csv2(paste0(dir_ril,x))
  table %>% select(
    depcom,numero,actualite,repetition,type_voie,libelle,complement,categorie,
    x,y,code_epsg,ilot,id_rp,
    echantillon,grp_rotation
    )
})|>bind_rows()


epsg <- ril$code_epsg %>% unique()%>% as.numeric()

# 3857 -> transformation des points dans le système Pseudo-mercator qui convient à la plupart des logiciels
ril <- ril %>% 
  st_as_sf(coords = c("x", "y"), crs = epsg) %>% # epsg quelle est l'origine géographique de mon repère ?
  st_transform(3857) # pseudo mercator

ril <- bind_cols(
  ril,
  st_coordinates(ril)%>% `colnames<-`(c("x","y"))
)

# on renomme des variables pour être cohérent avec le fichier de JF Baron
ril <- ril %>%
    rename(etat = actualite, ea_type = categorie,libelle_voie =libelle)%>%
    mutate(code_ilot = str_pad(ilot,4,"left",0))

# on enleve les périmées, on garde unqiuement les EA habitables (on a viré les communautés ?)
ril <- ril%>%
  filter(nchar(id_rp)==16)%>% 
  filter(ea_type == "HABIT")%>% 
  filter(etat != "1" | is.na(etat))%>% 
  select(id_rp, grp_rotation, code_ilot, depcom, x, y, numero, echantillon, type_voie, libelle_voie, complement) %>% 
  mutate(adr_rang = sapply(strsplit(id_rp," "),"[[",5))

st_write(ril,"../QGIS/ril_complet.gpkg")

# on conserve le groupe de rotation de l'année
#1 ilot
gr <- 2 ;
ril_gr <- ril %>% 
  filter(grp_rotation == {{gr}})

ril_gr$code_ident_ilot <- with(ril_gr, paste0(depcom, code_ilot))
liste_ril_ident_ilot <- split(ril_gr, ril_gr$code_ident_ilot)

### Possible de réaliser l'opération par groupe d'ilot et de changer les seuils et tout reconcatener
l <- pblapply(liste_ril_ident_ilot, function(ril_ilot){
  # ril_ilot <- liste_ril_ident_ilot[[1]]
  res_classif <- classif_kmeans_seq(ril_ilot, seuil_distance = 150, n_class_max = 7)
})

ril_avec_groupe <- Reduce(rbind, l)

code_commune <- read.csv2(paste0(chemin,"input/Communes_EAR2025.csv"))%>% rename(commune = "Nom_commune")%>% select(depcom,commune)

ril_avec_groupe <- ril_avec_groupe %>%
  mutate(depcom = as.numeric(depcom)) %>%
  left_join(code_commune, by = c("depcom" = "depcom"))

# Grouper par ident_ilot et groupe, créer les polygones
plans_groupes <- ril_avec_groupe %>%
  group_by(code_ident_ilot, groupe) %>%
  summarise(
    # on récupère l'enveloppe convexe de l'union des points précédentes
    # buffer de 5 mètres pour prendre une marge de sécurité
    geom_union = st_union(geometry)%>% st_convex_hull() %>% st_buffer(5),
    depcom = as.character(first(depcom)), 
    code_ilot = first(code_ilot),
    commune = first(commune),
    .groups = "drop"
  )

## groupe 0 correspond au non-zoom, ilot entier
## TO DO pour les PC : partir des contours d'ilot pour construire plan_ilot 
plan_ilot <- ril_avec_groupe %>%
  group_by(code_ident_ilot) %>%
  summarise(
    geom_union = st_union(geometry) %>% st_convex_hull() %>% st_buffer(5),
    depcom = as.character(first(depcom)), 
    code_ilot = first(code_ilot),
    commune = first(commune),
    .groups = "drop"
  )%>%
  mutate(groupe = 0)

plans_groupes <- bind_rows(plans_groupes,plan_ilot)
plans_groupes <- plans_groupes %>% 
  arrange(code_ident_ilot,groupe) %>% 
  mutate(groupe = groupe +1)

plans_groupes <- plans_groupes %>%
  group_by(code_ident_ilot) %>%
  mutate(max_groupe = (max(groupe))) %>%
  ungroup()

#st_write(ril_avec_groupe, "../QGIS/ril_avec_groupe.gpkg")
st_write(plans_groupes, "../QGIS/ilots_groupes.gpkg")

### duplication nécessaire pour l'écritures des listes d'adresses pour le groupe non zoomé
ril_groupe_0 <- ril_avec_groupe %>% 
  mutate(groupe=0)

ril_avec_groupe <- bind_rows(ril_avec_groupe,ril_groupe_0) %>% 
  mutate(groupe = groupe +1)

tableaux_groupes <- ril_avec_groupe %>%
  group_by(code_ident_ilot, groupe) %>%
  group_split() %>%
  set_names(map(., ~paste(unique(.x$code_ident_ilot), unique(.x$groupe), sep = "_"))) %>%
  map(~ st_drop_geometry(.x)) %>%
  map(~ .x %>% filter(echantillon == 'O')) %>%
  map(~ .x %>% select(code_ident_ilot, groupe, adr_rang, numero, type_voie, libelle_voie, complement)) %>%
  map(~ .x %>% arrange(adr_rang)) 

tab_avec_groupe <- Reduce(rbind, tableaux_groupes)


# générer pdf liste adresses avce les bons noms pour la fusion à venir dans le code bat
pblapply(seq_along(tableaux_groupes),function(indice){
  table <- tableaux_groupes[[indice]]
  if(nrow(table) ==0) return(NULL);
  nom_ilot <- sub("^[0-9]+", "", table$code_ident_ilot %>% unique())
  zoom <- table$groupe %>% unique()
  dir_output <- "../output/"
  nom_fichier <- with(table,paste0(dir_output,"Zoom_RP_",code_ident_ilot,"_",groupe,"_liste_adresse"))
  generer_pdf_tableau(table[-c(1,2)], nom_fichier, nom_ilot, zoom)
})



## ecriture du bat qui fusionne zoom et liste adresses
chaine_call <- "call .\\sejda-console-3.2.60\\bin\\sejda-console.bat merge -f .\\output\\"
prefixe <- "Zoom_RP_"
vec_ilot_groupe <- with(tab_avec_groupe,paste0(code_ident_ilot,"_",groupe)) %>%unique()
vec_ilot_groupe <- with(tab_avec_groupe,code_ident_ilot) %>%unique()

sejda_vecteur <- sapply(vec_ilot_groupe,function(ilot_groupe){
  #ilot_groupe <- vec_ilot_groupe[1]
  paste0(chaine_call,prefixe,ilot_groupe,"*.pdf"," -o .\\pdf_merged\\",paste0(prefixe,ilot_groupe),".pdf")
})

write_lines(sejda_vecteur,"../script_plans_merged.bat")


## ecriture du bat qui fusionne un pdf commune par commune
chaine_call_cmn <- "call .\\sejda-console-3.2.60\\bin\\sejda-console.bat merge -f .\\pdf_merged\\"
prefixe <- "Zoom_RP_"
code_commune <- with(tab_avec_groupe, sub("[^0-9].*", "", code_ident_ilot)) %>% unique()

sejda_vecteur_cmn <- sapply(code_commune, function(cmn){
  paste0(chaine_call_cmn, prefixe, cmn,"*.pdf"," -o .\\pdf_merged_communes\\",paste0(prefixe, cmn),".pdf")
})

write_lines(sejda_vecteur_cmn,"script_plans_merged_cmn.bat")
