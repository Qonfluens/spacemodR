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

#' OCS GE v2 download links by department
#'
#' Named character vector mapping French department
#' codes to official IGN OCS GE v2 download URLs.
#'
#' @format A named character vector
#' @export
#'
#'
#'
#'
ocsge_v2_links <- c(
  "01" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D001_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D001_2018-01-01.7z",
  "02" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D002_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D002_2018-01-01.7z",
  "04" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D004_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D004_2018-01-01.7z",
  "05" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D005_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D005_2018-01-01.7z",
  "06" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D006_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D006_2017-01-01.7z",
  "11" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D011_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D011_2018-01-01.7z",
  "17" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D017_2018-02-01/OCS-GE_2-0__GPKG_LAMB93_D017_2018-02-01.7z",
  "22" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D022_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D022_2018-01-01.7z",
  "24" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D024_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D024_2017-01-01.7z",
  "27" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D027_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D027_2019-01-01.7z",
  "29" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D029_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D029_2018-01-01.7z",
  "30" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D030_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D030_2018-01-01.7z",
  "32" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D032_2016-01-01/OCS-GE_2-0__GPKG_LAMB93_D032_2016-01-01.7z",
  "33" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D033_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D033_2018-01-01.7z",
  "34" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D034_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D034_2018-01-01.7z",
  "35" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D035_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D035_2017-01-01.7z",
  "37" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D037_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D037_2018-01-01.7z",
  "38" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D038_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D038_2018-01-01.7z",
  "40" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D040_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D040_2018-01-01.7z",
  "42" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D042_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D042_2019-01-01.7z",
  "46" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D046_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D046_2019-01-01.7z",
  "47" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D047_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D047_2017-01-01.7z",
  "48" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D048_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D048_2018-01-01.7z",
  "49" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D049_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D049_2020-01-01.7z",
  "59" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D059_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D059_2018-01-01.7z",
  "60" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D060_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D060_2018-01-01.7z",
  "62" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D062_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D062_2018-01-01.7z",
  "63" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D063_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D063_2019-01-01.7z",
  "64" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D064_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D064_2018-01-01.7z",
  "66" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D066_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D066_2018-01-01.7z",
  "67" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D067_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D067_2018-01-01.7z",
  "68" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D068_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D068_2018-01-01.7z",
  "69" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D069_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D069_2017-01-01.7z",
  "72" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D072_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D072_2019-01-01.7z",
  "73" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D073_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D073_2019-01-01.7z",
  "75" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D075_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D075_2018-01-01.7z",
  "76" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D076_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D076_2019-01-01.7z",
  "77" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D077_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D077_2017-01-01.7z",
  "78" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D078_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D078_2018-01-01.7z",
  "80" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D080_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D080_2017-01-01.7z",
  "83" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D083_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D083_2017-01-01.7z",
  "84" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D084_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D084_2018-01-01.7z",
  "85" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D085_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D085_2019-01-01.7z",
  "91" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D091_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D091_2018-01-01.7z",
  "92" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D092_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D092_2018-01-01.7z",
  "93" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D093_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D093_2018-01-01.7z",
  "94" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D094_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D094_2018-01-01.7z",
  "95" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D095_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D095_2021-01-01.7z",
  "2A" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D02A_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D02A_2019-01-01.7z",
  "2B" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D02B_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D02B_2019-01-01.7z"
)



# ocsge_v3_links <- c(
#   # Région Auvergne-Rhône-Alpes
#   D001 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D001_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D001_2021-01-01.7z",
#   D003 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D003_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D003_2022-01-01.7z",
#   D007 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D007_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D007_2023-01-01.7z",
#   D015 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D015_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D015_2022-01-01.7z",
#   D026 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D026_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D026_2023-01-01.7z",
#   D038 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D038_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D038_2021-01-01.7z",
#   D042 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D042_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D042_2022-01-01.7z",
#   D043 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D043_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D043_2022-01-01.7z",
#   D063 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D063_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D063_2022-01-01.7z",
#   D069 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D069_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D069_2020-01-01.7z",
#   D073 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D073_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D073_2022-01-01.7z",
#   D074 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D074_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D074_2023-01-01.7z",
#   # Région Bourgogne-Franche-Comté
#   D021 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D021_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D021_2023-01-01.7z",
#   D025 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D025_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D025_2023-01-01.7z",
#   D039 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D039_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D039_2023-01-01.7z",
#   D058 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D058_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D058_2023-01-01.7z",
#   D070 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D070_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D070_2023-01-01.7z",
#   D071 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D071_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D071_2023-01-01.7z",
#   D089 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D089_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D089_2023-01-01.7z",
#   D090 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D090_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D090_2023-01-01.7z",
#   # Région Bretagne
#   D022 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D022_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D022_2021-01-01.7z",
#   D029 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D029_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D029_2021-01-01.7z",
#   D035 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D035_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D035_2020-01-01.7z",
#   D056 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D056_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D056_2022-01-01.7z",
#   # Région Centre-Val de Loire
#
# )

ocsge_v3_links <- c(
  # --- Région Auvergne-Rhône-Alpes (Mixte 2021-2023) ---
  D001 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D001_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D001_2021-01-01.7z",
  D003 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D003_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D003_2022-01-01.7z",
  D007 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D007_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D007_2023-01-01.7z",
  D015 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D015_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D015_2022-01-01.7z",
  D026 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D026_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D026_2023-01-01.7z",
  D038 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D038_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D038_2021-01-01.7z",
  D042 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D042_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D042_2022-01-01.7z",
  D043 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D043_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D043_2022-01-01.7z",
  D063 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D063_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D063_2022-01-01.7z",
  D069 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D069_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D069_2020-01-01.7z",
  D073 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D073_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D073_2022-01-01.7z",
  D074 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D074_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D074_2023-01-01.7z",

  # --- Région Bourgogne-Franche-Comté (Maj 2023) ---
  D021 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D021_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D021_2023-01-01.7z",
  D025 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D025_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D025_2023-01-01.7z",
  D039 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D039_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D039_2023-01-01.7z",
  D058 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D058_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D058_2023-01-01.7z",
  D070 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D070_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D070_2023-01-01.7z",
  D071 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D071_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D071_2023-01-01.7z",
  D089 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D089_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D089_2023-01-01.7z",
  D090 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D090_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D090_2023-01-01.7z",

  # --- Région Bretagne (2020-2022) ---
  D022 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D022_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D022_2021-01-01.7z",
  D029 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D029_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D029_2021-01-01.7z",
  D035 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D035_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D035_2020-01-01.7z",
  D056 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D056_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D056_2022-01-01.7z",

  # --- Région Centre-Val de Loire (Maj 2023, sauf Indre-et-Loire 2021) ---
  D018 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D018_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D018_2023-01-01.7z",
  D028 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D028_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D028_2023-01-01.7z",
  D036 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D036_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D036_2023-01-01.7z",
  D037 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D037_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D037_2021-01-01.7z",
  D041 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D041_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D041_2023-01-01.7z",
  D045 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D045_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D045_2023-01-01.7z",

  # --- Région Hauts-de-France (Maj 2021) ---
  D002 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D002_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D002_2021-01-01.7z",
  D059 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D059_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D059_2021-01-01.7z",
  D060 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D060_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D060_2021-01-01.7z",
  D062 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D062_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D062_2021-01-01.7z",
  D080 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D080_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D080_2021-01-01.7z",

  # --- Région Île-de-France (Maj 2021) ---
  D075 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D075_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D075_2021-01-01.7z",
  D077 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D077_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D077_2021-01-01.7z",
  D078 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D078_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D078_2021-01-01.7z",
  D091 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D091_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D091_2021-01-01.7z",
  D092 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D092_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D092_2021-01-01.7z",
  D093 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D093_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D093_2021-01-01.7z",
  D094 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D094_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D094_2021-01-01.7z",
  D095 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D095_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D095_2021-01-01.7z",

  # --- Région Normandie (Maj 2022 pour 76, 2020 pour autres) ---
  D014 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D014_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D014_2020-01-01.7z",
  D027 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D027_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D027_2020-01-01.7z",
  D050 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D050_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D050_2020-01-01.7z",
  D061 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D061_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D061_2020-01-01.7z",
  D076 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D076_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D076_2022-01-01.7z",

  # --- Région Nouvelle-Aquitaine (Mixte 2020, 2021, 2023) ---
  D016 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D016_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D016_2020-01-01.7z",
  D017 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D017_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D017_2020-01-01.7z",
  D019 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D019_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D019_2020-01-01.7z",
  D023 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D023_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D023_2020-01-01.7z",
  D024 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D024_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D024_2020-01-01.7z",
  D033 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D033_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D033_2021-01-01.7z",
  D040 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D040_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D040_2021-01-01.7z",
  D047 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D047_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D047_2020-01-01.7z",
  D064 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D064_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D064_2020-01-01.7z",
  D079 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D079_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D079_2023-01-01.7z",
  D086 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D086_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D086_2023-01-01.7z",
  D087 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D087_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D087_2023-01-01.7z",

  # --- Région Occitanie (2019 de base, maj 2022/2021) ---
  D009 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D009_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D009_2019-01-01.7z",
  D011 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D011_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D011_2019-01-01.7z",
  D012 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D012_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D012_2019-01-01.7z",
  D030 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D030_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D030_2019-01-01.7z",
  D031 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D031_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D031_2022-01-01.7z",
  D032 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D032_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D032_2019-01-01.7z",
  D034 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D034_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D034_2019-01-01.7z",
  D046 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D046_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D046_2019-01-01.7z",
  D048 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D048_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D048_2021-01-01.7z",
  D065 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D065_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D065_2022-01-01.7z",
  D066 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D066_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D066_2019-01-01.7z",
  D081 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D081_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D081_2019-01-01.7z",
  D082 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D082_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D082_2019-01-01.7z",

  # --- Région Pays de la Loire (Maj 2022) ---
  D044 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D044_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D044_2022-01-01.7z",
  D049 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D049_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D049_2022-01-01.7z",
  D053 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D053_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D053_2022-01-01.7z",
  D072 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D072_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D072_2022-01-01.7z",
  D085 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D085_2022-01-01/OCS-GE_2-0__GPKG_LAMB93_D085_2022-01-01.7z",

  # --- Région Provence-Alpes-Côte d'Azur (2021) ---
  D004 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D004_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D004_2021-01-01.7z",
  D005 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D005_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D005_2021-01-01.7z",
  D006 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D006_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D006_2021-01-01.7z",
  D013 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D013_2023-01-01/OCS-GE_2-0__GPKG_LAMB93_D013_2023-01-01.7z",
  D083 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D083_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D083_2021-01-01.7z",
  D084 = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D084_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D084_2021-01-01.7z",

  # --- Région Corse (2021) ---
  D02A = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D02A_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D02A_2021-01-01.7z",
  D02B = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D02B_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D02B_2021-01-01.7z"
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



