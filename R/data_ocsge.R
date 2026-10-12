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
#' (e.g., habitat suitability, resistance). Polygons are matched on their
#' cover code (\code{CS}) and, when the object also carries the land use
#' (\code{US}), on the (cover, use) pair.
#'
#' @param sf_obj A spatial \code{sf} object containing the landscape polygons.
#' @param species_pattern Character. A regex pattern or exact name to search for in the species dictionary.
#' @param code_col Character. The name of the column in \code{sf_obj} that contains the OCSGE cover codes. Default is \code{"code_cs"}.
#' @param species_dict A data.frame containing the species traits. Defaults to \code{spacemodR::ocsge_species_dict}.
#' @param ref_dict An optional reference data.frame giving the label and color of each cover code, added to the result. Defaults to \code{spacemodR::ref_ocsge}.
#' @param us_col Character. The name of the column in \code{sf_obj} that contains the OCSGE use codes. Default is \code{"code_us"}. Ignored when the column is absent; set to \code{NULL} to score polygons on their cover only.
#'
#' @return An \code{sf} object enriched with the species traits. Returns \code{NULL} if the species is not found.
#'
#' @details
#' Cover codes are compared without spaces, so \code{"CS2.2.1"} and
#' \code{"CS 2.2.1"} are the same class.
#'
#' In \code{\link{ocsge_species_dict}}, rows with \code{code_us = NA} hold the
#' scores of a cover whatever its use, and rows with a \code{code_us} hold the
#' scores of a (cover, use) pair. A polygon takes the scores of its (cover,
#' use) pair when the dictionary has one, and the scores of its cover
#' otherwise. The logical column \code{use_matched} of the result tells which
#' of the two was used.
#'
#' @importFrom dplyr left_join
#' @importFrom stats setNames
#'
#' @export
join_ocsge_species <- function(sf_obj,
                               species_pattern,
                               code_col = "code_cs",
                               species_dict = spacemodR::ocsge_species_dict,
                               ref_dict = spacemodR::ref_ocsge,
                               us_col = "code_us") {

  # 1. Inputs validation
  if (!inherits(sf_obj, "sf")) {
    stop("`sf_obj` must be a valid 'sf' spatial object.")
  }
  if (!code_col %in% names(sf_obj)) {
    stop(sprintf("Column '%s' not found in `sf_obj`.", code_col))
  }
  if (!all(c("species_name", "code_cs") %in% names(species_dict))) {
    stop("`species_dict` must have the columns 'species_name' and 'code_cs'.")
  }

  # 2. Match species in the dictionary
  matched <- species_dict[grepl(species_pattern, species_dict$species_name, ignore.case = TRUE), ]

  if (nrow(matched) == 0) {
    warning(sprintf("No data found in the dictionary for species pattern: '%s'", species_pattern))
    return(NULL)
  }

  # Check for multiple species matches and isolate the target
  unique_species <- unique(matched$species_name)
  sp_target <- unique_species[1]

  if (length(unique_species) > 1) {
    warning(sprintf("Multiple species matched: %s.\nUsing the first match: '%s'.",
                    paste(unique_species, collapse = ", "), sp_target))
  }

  df_sp <- matched[matched$species_name == sp_target, ]

  # 3. Match each polygon to a dictionary row
  strip <- function(x) gsub(" ", "", as.character(x))
  poly_cs <- strip(sf_obj[[code_col]])
  dict_cs <- strip(df_sp$code_cs)
  dict_us <- if ("code_us" %in% names(df_sp)) strip(df_sp$code_us) else rep(NA_character_, nrow(df_sp))

  # cover only
  rows_cs <- which(is.na(dict_us))
  idx <- rows_cs[match(poly_cs, dict_cs[rows_cs])]

  # (cover, use) pair, where the object has the use and the dictionary has the pair
  use_matched <- rep(FALSE, nrow(sf_obj))
  rows_us <- which(!is.na(dict_us))
  if (!is.null(us_col) && us_col %in% names(sf_obj) && length(rows_us) > 0) {
    idx_us <- rows_us[match(paste(poly_cs, strip(sf_obj[[us_col]])),
                            paste(dict_cs[rows_us], dict_us[rows_us]))]
    use_matched <- !is.na(idx_us)
    idx[use_matched] <- idx_us[use_matched]
  }

  # 4. Add the traits, then the label and color of the cover
  sf_merged <- sf_obj
  trait_cols <- setdiff(names(df_sp), c("code_cs", "code_us"))
  for (col in trait_cols) {
    sf_merged[[col]] <- df_sp[[col]][idx]
  }
  sf_merged$use_matched <- use_matched

  if (!is.null(ref_dict) && "code_cs" %in% names(ref_dict)) {
    ref_idx <- match(poly_cs, strip(ref_dict$code_cs))
    for (col in setdiff(names(ref_dict), c("code_cs", "code_cs_", names(sf_merged)))) {
      sf_merged[[col]] <- ref_dict[[col]][ref_idx]
    }
  }

  # Check if the join actually retrieved data
  if (all(is.na(idx))) {
    warning("No matching OCSGE codes were found between the spatial object and the species dictionary. The resulting traits will be empty.")
  }

  # Return the enriched sf object and attach the target species name as an attribute for downstream use
  attr(sf_merged, "species_target") <- sp_target

  return(sf_merged)
}

#' Plot Habitat Suitability Maps from OCSGE Data
#'
#' @description
#' Generates a composite plot of three spatial maps (suitability, foraging
#' weight and resistance) from an `sf` object previously enriched with species traits.
#'
#' @param sf_merged An `sf` object enriched with species traits (e.g., output of \code{\link{join_ocsge_species}}).
#' @param title_species Character (optional). The name of the species to display in the title.
#' If NULL, it will try to retrieve the name from the object attributes.
#'
#' @return A \code{patchwork} object containing the combined \code{ggplot2} maps.
#'
#' @details
#' Suitability and foraging weight are drawn on their full 0-10 scale, and
#' resistance on its 1-100 logarithmic scale, so that maps of different
#' species can be compared.
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
  req_cols <- c("suitability", "weight_foraging", "resistance")
  if (!all(req_cols %in% names(sf_merged))) {
    stop("Missing required columns in `sf_merged`. Expected: 'suitability', 'weight_foraging', 'resistance'. Did you run `join_ocsge_species` first?")
  }

  # Retrieve species name for the title
  if (is.null(title_species)) {
    title_species <- attr(sf_merged, "species_target")
    if (is.null(title_species)) title_species <- "Species"
  }

  # Generate Maps
  p1 <- ggplot2::ggplot(sf_merged) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$suitability), color = NA) +
    ggplot2::scale_fill_distiller(palette = "YlGn", direction = 1, na.value = "transparent", name = "Score", limits = c(0, 10)) +
    ggplot2::theme_void() +
    ggplot2::labs(subtitle = "Suitability") +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))

  p2 <- ggplot2::ggplot(sf_merged) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$weight_foraging), color = NA) +
    ggplot2::scale_fill_distiller(palette = "Blues", direction = 1, na.value = "transparent", name = "Score", limits = c(0, 10)) +
    ggplot2::theme_void() +
    ggplot2::labs(subtitle = "Foraging Weight") +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))

  p3 <- ggplot2::ggplot(sf_merged) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$resistance), color = NA) +
    ggplot2::scale_fill_distiller(palette = "Reds", direction = 1, na.value = "transparent", name = "Score",
                                  trans = "log10", limits = c(1, 100)) +
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



