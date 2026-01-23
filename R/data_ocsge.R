#' Collecte les données OCS GE depuis un FlatGeobuf sur S3
#'
#' @param roi Un objet `sf` définissant la zone d'intérêt.
#' @param fgb_url L'URL publique ou signée du fichier .fgb sur Scaleway S3.
#'
#' @return Un objet `sf` coupé à la bounding box du ROI.
#' @export
get_ocsge_data_fgb <- function(roi, fgb_url) {

  # 1. Gestion des projections
  # On s'assure que le ROI est dans la même projection que la source (souvent Lambert-93 pour OCS GE)
  # Note: Idéalement, vérifiez la projection du FGB avant. Ici on force 2154 comme dans votre script original.
  roi <- sf::st_transform(roi, 2154)

  # 2. Préparation de la lecture "Cloud Native"
  # Le préfixe /vsicurl/ indique à GDAL (utilisé par sf) de lire via HTTP
  # Si l'URL commence déjà par http, sf le détecte souvent, mais /vsicurl/ est explicite.
  dsn <- paste0("/vsicurl/", fgb_url)

  # 3. Récupération de la Bounding Box du ROI pour le filtre
  # FlatGeobuf est très efficace pour filtrer par BBOX sans tout lire
  bbox <- sf::st_bbox(roi)

  # 4. Lecture filtrée
  tryCatch({
    # st_read avec filtre spatial.
    # GDAL va demander uniquement les chunks de données qui intersectent le bbox.
    res <- sf::st_read(
      dsn,
      wkt_filter = sf::st_as_text(sf::st_as_sfc(bbox)),
      quiet = TRUE
    )

    # 5. Intersection précise (optionnel mais recommandé)
    # Le filtre bbox récupère un rectangle, on coupe selon le polygone exact du ROI
    res <- sf::st_intersection(res, roi)

    return(res)

  }, error = function(e) {
    stop("Erreur lors de la lecture du FGB sur S3. Vérifiez l'URL et la connexion internet.\n", e)
  })
}


# -------------------------------------------------------------------
# Exported data objects
# -------------------------------------------------------------------

#' OCS GE nomenclature
#'
#' Named character vector mapping OCS GE class codes
#' to human-readable land cover labels.
#'
#' @format A named character vector
#' @export
nomenclature <- c(
  "CS1.1.1.1" = "zones baties",
  "CS1.1.1.2" = "zones imperméables non baties",
  "CS1.1.2.1" = "zones à matériaux minéraux",
  "CS1.1.2.2" = "zones à matériaux composites",
  "CS1.2.1"   = "sols nus",
  "CS1.2.2"   = "surfaces d'eau",
  "CS1.2.3"   = "névées et glaciers",
  "CS2.1.1.1" = "feuillus",
  "CS2.1.1.2" = "conifères",
  "CS2.1.1.3" = "peuplement mixte",
  "CS2.1.2"   = "formations arbustives et sous-arbrisseaux",
  "CS2.1.3"   = "autres formations ligneuses",
  "CS2.2.1"   = "formations herbacées",
  "CS2.2.3"   = "autres formations non ligneuses"
)

#' Codes OCS-GE
#'
#' Liste regroupant toutes les catégories OCS-GE
#'
#' @export
OCSGE_codes <- list(
  forest = c("CS2.1.1.1", "CS2.1.1.2", "CS2.1.1.3", "CS2.1.3"),
  grass = "CS2.2.1",
  water = "CS1.2.2",
  soil = c("CS1.2.1","CS2.1.1.1","CS2.1.1.2","CS2.1.1.3",
           "CS2.1.2","CS2.1.3","CS2.2.1","CS2.2.3"),
  sealed = c("CS1.1.1.1", "CS1.1.1.2", "CS1.1.2.1")
)


# ------------------
# COLORS
# ------------------
#' data.frame OCS-GE for visulisation
#'
#' @export
ref_ocsge <- data.frame(
  code_cs = c(
    "CS1.1.1.1",
    "CS1.1.1.2",
    "CS1.1.2.1",
    "CS1.1.2.2",
    "CS1.2.1",
    "CS1.2.2",
    "CS1.2.3",
    "CS2.1.1.1",
    "CS2.1.1.2",
    "CS2.1.1.3",
    "CS2.1.2",
    "CS2.1.3",
    "CS2.2.1",
    "CS2.2.2"
  ),
  code_cs_ = c(
    "CS 1.1.1.1",
    "CS 1.1.1.2",
    "CS 1.1.2.1",
    "CS 1.1.2.2",
    "CS 1.2.1",
    "CS 1.2.2",
    "CS 1.2.3",
    "CS 2.1.1.1",
    "CS 2.1.1.2",
    "CS 2.1.1.3",
    "CS 2.1.2",
    "CS 2.1.3",
    "CS 2.2.1",
    "CS 2.2.2"
  ),
  nomenclature = c(
    "Zones bâties",
    "Zones non bâties",
    "Matériaux minéraux",
    "Matériaux composites",
    "Sols nus",
    "Surfaces d'eau",
    "Névés et glaciers",
    "Feuillus",
    "Conifères",
    "Mixte",
    "Formations arbustives, sous-arbrisseaux",
    "Autres formations ligneuses",
    "Formations herbacées",
    "Autres formations non ligneuses"
  ),
  couleur = c(
    "#FF1493", # CS 1.1.1.1 - Rose vif
    "#FF9999", # CS 1.1.1.2 - Saumon
    "#FFFF99", # CS 1.1.2.1 - Jaune pâle
    "#8B4513", # CS 1.1.2.2 - Marron
    "#C0C0C0", # CS 1.2.1   - Gris
    "#00BFFF", # CS 1.2.2   - Bleu cyan
    "#C4FFFF", # CS 1.2.3   - Bleu très pâle/Menthe
    "#7FFF00", # CS 2.1.1.1 - Vert chartreuse
    "#006400", # CS 2.1.1.2 - Vert foncé
    "#6B8E23", # CS 2.1.1.3 - Vert olive
    "#90EE90", # CS 2.1.2   - Vert clair
    "#FF8C00", # CS 2.1.3   - Orange
    "#CCFF00", # CS 2.2.1   - Jaune-Vert
    "#F0FFF0"  # CS 2.2.2   - Vert très pâle
  ),
  stringsAsFactors = FALSE
)

# Aperçu
# print(ref_ocsge)




