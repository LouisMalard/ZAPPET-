

# produire les plans avec les points colorés en couleurs
# et les classes 
# sélectionner variables utiles
# group by ident_ilot et groupe et st_union, st_bbox et st_ buffer de 20 m

# seuil_distance, distance max que l'on voudrait sur le plan en moyenne
#' Classification séquentielle des points par K-means
#' 
#' @description
#' Cette fonction réalise une classification séquentielle des points d'un îlot en groupes,
#' en utilisant l'algorithme K-means. Elle traite les points par lots successifs en
#' commençant par les plus grands groupes.
#'
#' @param ril_ilot data.frame/sf Un objet sf contenant les points à classifier avec au minimum
#'                 les colonnes id_rp, x, y et la géométrie
#' @param seuil_distance numeric Distance maximale souhaitée (en mètres) entre les points
#'                      d'un même groupe (défaut: 200)
#' @param n_class_max numeric Nombre maximum de classes autorisé (défaut: 5)
#'
#' @return Un data.frame/sf avec les mêmes colonnes que l'entrée plus une colonne 'groupe'
#'         indiquant l'affectation de chaque point
#'
#' @details
#' L'algorithme fonctionne de manière séquentielle :
#' 1. Calcule le nombre optimal de classes basé sur la distance maximale entre points
#' 2. Applique K-means sur les points restants
#' 3. Sélectionne le plus grand groupe et tous les points dans sa bbox
#' 4. Répète jusqu'à ce que tous les points soient classés
#'
#' @examples
#' \dontrun{
#' resultat <- classif_kmeans_seq(ril_ilot, seuil_distance = 150, n_class_max = 3)
#' }
#' 
classif_kmeans_seq <- function(ril_ilot, seuil_distance = 200, n_class_max = 7, nom_col_geom = "geometry"){
  # ril_ilot = liste_ril_ident_ilot[["97305AK08"]]; seuil_distance = 200; n_class_max = 5
    
    dist_max <- max(sf::st_distance(x = ril_ilot[[nom_col_geom]] %>% st_transform(3857)))
    n_class <- round(dist_max/seuil_distance) %>% as.numeric()
    n_class <- min(n_class, n_class_max) # maximum de classes = 10 ..

    if(n_class <= 1) return(ril_ilot%>% mutate(groupe =1))

    ril_restant <- ril_ilot # intiitialisation du ril restant on va attribuer les points à des classes séquentiellement
    n_class_en_cours <- n_class
    ril_final_groupe <- data.frame()
    compteur_groupe <- 1  
    
    #initialement, si nb_point < n_class 
    if (nrow(ril_restant) / n_class < 1) return(ril_ilot%>% mutate(groupe =1))
    
    #initialement, si nb_point < 2 * n_class
    if ((nrow(ril_restant) / n_class < 2) & (nrow(ril_restant) / n_class >= 1)) {
      n_class_en_cours <- 2
    }
    
    while(nrow(ril_restant)>0){
    # print(compteur_groupe)
    # si il n'en reste plus que 4 completement arbitraire
    
    if (nrow(ril_restant)<=5){ 
        ril_restant$groupe <- compteur_groupe
        ril_final_groupe <- rbind(
            ril_final_groupe,
            ril_restant
        )
        ril_restant <-data.frame()
        ril_final_groupe$groupe_kmeans <- NULL
        return(ril_final_groupe)
    }
      
    # Réduire le nombre de classes si nécessaire
    if (nrow(ril_restant) < n_class_en_cours) {
      n_class_en_cours <- nrow(ril_restant)
    }
      
    res_kmeans <- kmeans(
        st_coordinates(ril_restant),
        centers = n_class_en_cours
        )$cluster

    ril_restant$groupe_kmeans <- res_kmeans

    # on récupère le groupe le plus gros
    gros_groupe <- ril_restant$groupe_kmeans %>% 
        table() %>% 
        which.max() %>% 
        names()
    
    bb <- st_bbox(
        ril_restant %>% filter(groupe_kmeans == gros_groupe)
        )

        # on récupère tous les ponts dans la bbox pour le plan
    ril_select <- ril_restant %>% 
        filter (
        x >= bb["xmin"],
        x <= bb["xmax"],
        y <= bb["ymax"],
        y >= bb["ymin"]
        ) %>% 
        mutate(groupe = compteur_groupe)

    ril_final_groupe <- rbind(
        ril_final_groupe,
        ril_select
        )

    ril_restant <- ril_restant %>% 
        filter(!id_rp %in% ril_select$id_rp)

    n_class_en_cours <- n_class_en_cours - 1
    compteur_groupe <- compteur_groupe + 1 
    }
    
    ril_final_groupe$groupe_kmeans <- NULL
    ## Suppression des groupes dégénérés contenant aucun logement échantillonnés
    groupe_avec_un_seul_logement_non_ech <- ril_final_groupe %>%
      group_by(groupe)%>%
      summarise(comptage = sum(echantillon == "O"))%>%
      filter(comptage==0)%>%
      pull(groupe)%>%unique()
    
    if(length(groupe_avec_un_seul_logement_non_ech)!=0) {
     ril_final_groupe <- ril_final_groupe %>% filter(groupe != groupe_avec_un_seul_logement_non_ech)
    }
    
    return(ril_final_groupe)
}


representer_resultat_classif <- function(ilot_data){
    #ilot_data <- liste_ilots[[1]]

    p <- ggplot(data = ilot_data) +
        geom_sf(aes(color = as.factor(groupe)), size = 3) +
        theme_minimal() +
        ggtitle(paste("Ilot:", unique(ilot_data$code_ident_ilot))) +
        labs(x = NULL, y = NULL) +
        theme_classic() +
        ggtitle(paste("Ilot:", unique(ilot_data$code_ident_ilot),"- Répartition des logements en groupes"))+
        labs(x = NULL, y = NULL) +
        theme(axis.text = element_blank())+
        scale_color_discrete(name = "Groupe") 
        code_ident_ilot <- ilot_data$code_ident_ilot%>%unique()
        ggsave(
            filename = paste0("Export_Plans/Zoom_RP_", code_ident_ilot, "_0_regroupement.pdf"),
            plot = p,
            width = 8,
            height = 6
        )
}


generer_pdf_tableau  <- function(table, nom_fichier, ilot, groupe){
  colnames(table) <- c("rgA", "Numéro", "Type", "Libellé voie", "Complément adresse")
  table[is.na(table)] <- ""
  
  # Calculer le nombre de lignes et de colonnes
  n_rows <- nrow(table)
  n_cols <- ncol(table)  # Libellé, Montant Total, Moyenne
  
  # Créer des matrices pour hjust et x
  hjust_matrix <- matrix(rep(c(0.5, 0.5, 0.5, 0.5, 0.5), each = n_rows), nrow = n_rows, ncol = n_cols)
  x_matrix <- matrix(rep(c(0.5, 0.5, 0.5, 0.5, 0.5), each = n_rows), nrow = n_rows, ncol = n_cols)
  
  # Créer le tableau avec les matrices d'alignement
  table_grob <- table %>%
    tableGrob(
      rows = NULL,
      theme = ttheme_default(
        # Style du corps du tableau
        core = list(
          bg_params = list(
            fill = rep(c("#FFFFFF", "#e6e8ea"), length.out = n_rows)
          ),
          fg_params = list(
            fontface = "plain",
            hjust = as.vector(hjust_matrix),
            x = as.vector(x_matrix),
            fontsize = 3
          ),
          padding = unit(c(3, 0.5), "mm")
        ),
        # Style de l'en-tête
        colhead = list(
          bg_params = list(fill = "#FFFFFF"),
          fg_params = list(
            fontface = "bold",
            hjust = c(0.5, 0.5, 0.5, 0.5, 0.5),
            x = c(0.5, 0.5, 0.5, 0.5, 0.5),
            fontsize = 3.5
          ),
          padding = unit(c(3, 1), "mm")
        )
      )
    )
  
  
  # Ajuster la taille du tableau de manière plus précise
  #table_grob$widths <- unit(c(0.5,0.5,0.5,0.5,0.5), "inches")  # Largeurs ajustées
  initial_widths <- convertWidth(table_grob$widths, "inches", valueOnly = TRUE)
  table_grob$widths <- unit(initial_widths * 1.2, "inches")
  
  pdf_filename <- paste0(nom_fichier,".pdf")
  pdf(pdf_filename, width = 6, height = 4)
  grid.newpage()
  
  # Ajouter le texte en haut du PDF
  grid.text(
    paste0(ilot, " Plan ", groupe, " - Nombre total d'adresses à enquêter : ", n_rows),
    x = 0.5, y = 0.95, gp = gpar(fontsize = 5, fontface = "bold")
  )
  grid.draw(table_grob)
  dev.off()
}
