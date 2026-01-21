#' Download and extract OCS GE data for a Region Of Interest
#'
#' This function downloads (if needed) OCS GE v2 datasets from IGN,
#' extracts them, merges data from all intersecting departments,
#' and crops the result to the provided region of interest (ROI).
#'
#' @param roi An `sf` object defining the region of interest.
#'   Only the first feature is used.
#' @param local_cache Optional directory path where downloaded
#'   archives are cached. If empty, a temporary directory is used.
#'
#' @return An `sf` object in Lambert-93 projection (EPSG:2154)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' roi <- sf::st_read("roi.shp")
#' data <- get_ocsge_data(roi)
#' }
get_ocsge_data <- function(roi, local_cache = "") {

  # Ensure CRS is Lambert-93
  roi <- sf::st_transform(roi, 2154)

  # Identify intersecting departments
  dpts <- get_departements_for_roi(roi)

  if (length(dpts) == 0) {
    stop("no overlapping department found.")
  }

  # Check data availability
  for (d in dpts) {
    if (!d %in% names(ocsge_v2_links)) {
      stop(sprintf("data not available for department %s", d))
    }
  }

  # Download, extract and read data for each department
  dfs <- lapply(
    dpts,
    function(d) {
      get_ocsge_extract(
        dl_url = ocsge_v2_links[[d]],
        roi = roi,
        local_cache = local_cache
      )
    }
  )

  do.call(rbind, dfs)
}

# -------------------------------------------------------------------
# Internal helpers
# -------------------------------------------------------------------

#' Download, extract and read OCS GE data for a single department
#'
#' Internal helper. Downloads a .7z archive if necessary,
#' extracts it, reads the OCCUPATION_SOL shapefile
#' and crops it to the ROI bounding box.
#'
#' @param dl_url Download URL of the OCS GE archive
#' @param roi An `sf` object (EPSG:2154)
#' @param local_cache Optional cache directory
#'
#' @return An `sf` object
#'
#' @keywords internal
get_ocsge_extract <- function(dl_url, roi, local_cache = "") {

  filename <- stringr::str_match(dl_url, ".*/([^/]+)$")[, 2]
  # unique tempdir
  workdir <- tempfile()
  dir.create(workdir)

  arc_name <- if (nzchar(local_cache)) {
    file.path(local_cache, filename)
  } else {
    # file.path(workdir, "archive.7z")
    file.path(workdir, filename)
  }

  # Download archive if not cached
  if (!nzchar(local_cache) || !file.exists(arc_name)) {
    message(sprintf("downloading %s...", dl_url))
    req <- httr::GET(dl_url)
    writeBin(httr::content(req, "raw"), arc_name)
  }

  archive::archive_extract(arc_name, dir = workdir)

  # Locate OCCUPATION_SOL shapefile
  paths <- list.files(workdir, recursive = TRUE, full.names = TRUE)
  gpkg_paths <- paths[grepl("OCCUPATION_SOL\\.gpkg$", paths)]

  if (length(gpkg_paths) == 0) {
    stop("OCSGE: expected path name not found")
  } else if (length(gpkg_paths) > 1) {
    stop("OCSGE: several files have the expected name")
  }

  # Crop to ROI bounding box
  roi_2154 <- sf::st_transform(roi, 2154)
  roi_union <- sf::st_union(roi_2154)
  bbox_polygon <- sf::st_as_sfc(sf::st_bbox(roi_union))

  result <- sf::st_read(
    gpkg_paths[1],
    quiet = TRUE,
    wkt_filter = sf::st_as_text(bbox_polygon)
  )
  # Supprimer le répertoire temporaire après utilisation (optionnel)
  unlink(workdir, recursive = TRUE)
  return(result)
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



