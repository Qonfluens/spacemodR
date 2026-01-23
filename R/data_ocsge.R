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




