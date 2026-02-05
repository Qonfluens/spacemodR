#' Collect data from OCS GE repository using FlatGeobuf on S3
#'
#' @param roi An object `sf` defiing Region Of Interest.
#' @param fgb_url Public URL or a .fgb file.
#'
#' @return An object `sf` cropped to the bounding bbox form ROI.
#' @export
get_ocsge_data_fgb <- function(roi, fgb_url) {

  # 1. PROJECTION
  # On s'assure que le ROI est dans la même projection que la source (souvent Lambert-93 pour OCS GE)
  roi <- sf::st_transform(roi, 2154)

  # 2. CLEAN CLOUD READING
  # Le préfixe /vsicurl/ indique à GDAL (utilisé par sf) de lire via HTTP
  dsn <- paste0("/vsicurl/", fgb_url)

  # 3. BOUDING BOX OF ROI
  # FlatGeobuf est très efficace pour filtrer par BBOX sans tout lire
  bbox <- sf::st_bbox(roi)

  # 4. FILTER READING
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
    stop("Error when reading FGB on S3. Check URL and Internet connection.\n", e)
  })
}




