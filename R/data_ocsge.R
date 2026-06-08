#' Efficiently retrieve OCS GE data from a remote FlatGeobuf
#'
#' @description
#' This function retrieves OCS GE (Land Cover) data for a specific Region of Interest (ROI)
#' directly from a remote FlatGeobuf (.fgb) file hosted on a server (e.g., S3).
#'
#' It leverages GDAL's virtual file system (`/vsicurl/`) and the spatial indexing capabilities
#' of FlatGeobuf to download only the data chunks intersecting the bounding box of the ROI,
#' making it highly efficient for large datasets.
#'
#' @param roi An \code{\link[sf]{sf}} object defining the Region Of Interest.
#' It can be in any projection, but will be transformed to EPSG:2154 (Lambert-93) internally.
#' @param fgb_url Character string. The public URL to the remote `.fgb` file.
#'
#' @return An \code{\link[sf]{sf}} object containing the OCS GE polygons intersected by the ROI,
#' projected in Lambert-93 (EPSG:2154).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Transforms the \code{roi} to Lambert-93 (EPSG:2154).
#'   \item calculates the bounding box of the \code{roi}.
#'   \item Uses \code{sf::st_read} with a WKT filter to fetch only relevant features from the remote file.
#'   \item Applies a precise geometric intersection (`st_intersection`) to clip the data to the exact shape of the \code{roi}.
#' }
#'
#' @note
#' This function requires a working internet connection and GDAL support for the
#' FlatGeobuf driver and network capabilities (vsicurl).
#'
#' @examples
#' \dontrun{
#'   library(sf)
#'
#'   # 1. Define a Region of Interest (ROI)
#'   # Example: A small bounding box in France
#'   my_roi <- st_as_sf(data.frame(
#'     lon = c(2.3, 2.4, 2.4, 2.3, 2.3),
#'     lat = c(48.8, 48.8, 48.9, 48.9, 48.8)
#'   ), coords = c("lon", "lat"), crs = 4326)
#'
#'   # 2. URL to the remote FlatGeobuf file
#'   # (Replace with the actual URL of your OCS GE bucket)
#'   url_fgb <- "https://example.com/data/ocsge_grand_est.fgb"
#'
#'   # 3. Fetch data
#'   ocsge_data <- get_ocsge_data_fgb(roi = my_roi, fgb_url = url_fgb)
#'
#'   # 4. Check result
#'   print(ocsge_data)
#'   plot(st_geometry(ocsge_data))
#' }
#'
#' @importFrom sf st_transform st_bbox st_as_text st_as_sfc st_read st_intersection
#' @export
get_ocsge_data_fgb <- function(roi, fgb_url) {
  # 1. PROJECTION
  roi <- sf::st_transform(roi, 2154)
  # 2. CLEAN CLOUD READING
  # Le préfixe /vsicurl/ indique à GDAL (utilisé par sf) de lire via HTTP
  dsn <- paste0("/vsicurl/", fgb_url)
  # 3. BOUDING BOX OF ROI
  bbox <- sf::st_bbox(roi)
  # 4. FILTER READING
  tryCatch({
    res <- sf::st_read(
      dsn,
      wkt_filter = sf::st_as_text(sf::st_as_sfc(bbox)),
      quiet = TRUE
    )
    # 5. Intersection précise (optionnel mais recommandé)
    res <- sf::st_intersection(res, roi)
    return(res)
  }, error = function(e) {
    stop("Error when reading FGB on S3. Check URL and Internet connection.\n", e)
  })
}


#' Join OCSGE Spatial Data with Species Traits
#'
#' @description
#' This function merges a spatial `sf` object containing OCSGE (Occupation du Sol
#' à Grande Échelle) polygons with a dictionary of species-specific traits
#' (e.g., habitat suitability weights, resistance). It handles potential code
#' bridging via a reference dictionary.
#'
#' @param sf_obj A spatial \code{sf} object containing the landscape polygons.
#' @param species_pattern Character. A regex pattern or exact name to search for in the species dictionary.
#' @param code_col Character. The name of the column in \code{sf_obj} that contains the OCSGE codes. Default is \code{"code_cs"}.
#' @param species_dict A data.frame containing the species traits. Defaults to \code{spacemodR::ocsge_species_dict}.
#' @param ref_dict An optional reference data.frame to bridge codes between the spatial object and the species dictionary. Defaults to \code{spacemodR::ref_ocsge}.
#'
#' @return An \code{sf} object enriched with the species traits. Returns \code{NULL} if the species is not found.
#'
#' @importFrom dplyr left_join
#' @importFrom stats setNames
#'
#' @export
join_ocsge_species <- function(sf_obj,
                               species_pattern,
                               code_col = "code_cs",
                               species_dict = spacemodR::ocsge_species_dict,
                               ref_dict = spacemodR::ref_ocsge) {

  # 1. Inputs validation
  if (!inherits(sf_obj, "sf")) {
    stop("`sf_obj` must be a valid 'sf' spatial object.")
  }
  if (!code_col %in% names(sf_obj)) {
    stop(sprintf("Column '%s' not found in `sf_obj`.", code_col))
  }

  # 2. Match species in the dictionary
  matched <- species_dict[grepl(species_pattern, species_dict$nom_espece, ignore.case = TRUE), ]

  if (nrow(matched) == 0) {
    warning(sprintf("No data found in the dictionary for species pattern: '%s'", species_pattern))
    return(NULL)
  }

  # Check for multiple species matches and isolate the target
  unique_species <- unique(matched$nom_espece)
  sp_target <- unique_species[1]

  if (length(unique_species) > 1) {
    warning(sprintf("Multiple species matched: %s.\nUsing the first match: '%s'.",
                    paste(unique_species, collapse = ", "), sp_target))
  }

  df_sp <- matched[matched$nom_espece == sp_target, ]

  # 3. Join spatial data with dictionaries
  if (!is.null(ref_dict) && "code_cs_" %in% names(ref_dict)) {
    df_merged <- dplyr::left_join(ref_dict, df_sp, by = c("code_cs_" = "code_cs"))
  } else {
    df_merged <- df_sp
  }

  # Join the spatial object programmatically
  join_by <- stats::setNames(names(df_merged)[1], code_col)
  sf_merged <- dplyr::left_join(sf_obj, df_merged, by = join_by)

  # Check if the join actually retrieved data
  if (all(is.na(sf_merged$weight_global))) {
    warning("No matching OCSGE codes were found between the spatial object and the species dictionary. The resulting traits will be empty.")
  }

  # Return the enriched sf object and attach the target species name as an attribute for downstream use
  attr(sf_merged, "species_target") <- sp_target

  return(sf_merged)
}

#' Plot Habitat Suitability Maps from OCSGE Data
#'
#' @description
#' Generates a composite plot of three spatial maps (Global Weight, Foraging Weight,
#' and Resistance) from an `sf` object previously enriched with species traits.
#'
#' @param sf_merged An `sf` object enriched with species traits (e.g., output of \code{\link{join_ocsge_species}}).
#' @param title_species Character (optional). The name of the species to display in the title.
#' If NULL, it will try to retrieve the name from the object attributes.
#'
#' @return A \code{patchwork} object containing the combined \code{ggplot2} maps.
#'
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_distiller theme_void labs theme element_text
#' @importFrom patchwork plot_annotation
#'
#' @export
plot_species_habitat <- function(sf_merged, title_species = NULL) {

  if (!inherits(sf_merged, "sf")) {
    stop("`sf_merged` must be a valid 'sf' spatial object.")
  }

  # Required columns check
  req_cols <- c("weight_global", "weight_foraging", "resistance")
  if (!all(req_cols %in% names(sf_merged))) {
    stop("Missing required columns in `sf_merged`. Expected: 'weight_global', 'weight_foraging', 'resistance'. Did you run `join_ocsge_species` first?")
  }

  # Retrieve species name for the title
  if (is.null(title_species)) {
    title_species <- attr(sf_merged, "species_target")
    if (is.null(title_species)) title_species <- "Species"
  }

  # Generate Maps
  p1 <- ggplot2::ggplot(sf_merged) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$weight_global), color = NA) +
    ggplot2::scale_fill_distiller(palette = "YlGn", direction = 1, na.value = "transparent", name = "Score") +
    ggplot2::theme_void() +
    ggplot2::labs(subtitle = "Global Weight") +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))

  p2 <- ggplot2::ggplot(sf_merged) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$weight_foraging), color = NA) +
    ggplot2::scale_fill_distiller(palette = "Blues", direction = 1, na.value = "transparent", name = "Score") +
    ggplot2::theme_void() +
    ggplot2::labs(subtitle = "Foraging Weight") +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))

  p3 <- ggplot2::ggplot(sf_merged) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$resistance), color = NA) +
    ggplot2::scale_fill_distiller(palette = "Reds", direction = 1, na.value = "transparent", name = "Score") +
    ggplot2::theme_void() +
    ggplot2::labs(subtitle = "Resistance") +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))

  # Combine maps
  combined_plot <- p1 + p2 + p3 +
    patchwork::plot_annotation(
      title = paste("Habitat analysis for:", title_species),
      theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5))
    )

  return(combined_plot)
}



