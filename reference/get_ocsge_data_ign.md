# Retrieve OCS GE data from the IGN Géoplateforme

This function retrieves OCS GE (Land Cover) data for a specific Region
of Interest (ROI) from the download service of the IGN Géoplateforme
(<https://data.geopf.fr/telechargement/resource/OCSGE>), which needs no
API key.

IGN delivers the OCS GE as one compressed GeoPackage per department and
vintage (15 to 270 MB each). The departments intersecting the ROI are
downloaded in full, extracted and kept in `cache_dir`, so that later
calls on the same departments read the local file only.

## Usage

``` r
get_ocsge_data_ign(
  roi,
  year = NULL,
  cache_dir = tools::R_user_dir("spacemodR", "cache"),
  quiet = FALSE
)
```

## Arguments

- roi:

  An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) (or `sfc`)
  object defining the Region Of Interest. It can be in any projection.

- year:

  Integer. Vintage (*millésime*) to retrieve. Default `NULL` takes the
  latest vintage of each department. Vintages differ between
  departments; the error raised for a missing one lists those available.

- cache_dir:

  Character. Directory where the extracted GeoPackages are kept.
  Defaults to the user cache directory of the package. Each department
  takes several hundred MB once extracted; delete the files to free the
  space.

- quiet:

  Logical. If `FALSE` (default), reports the datasets used and the
  download progress.

## Value

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object
containing the OCS GE polygons intersected by the ROI, with the columns
of the IGN delivery (notably `code_cs`, `code_us` and `millesime`), in
the projection of the delivery: Lambert-93 (EPSG:2154) in mainland
France and Corsica, the local UTM projection in overseas departments.

## Details

The function performs the following steps:

1.  Finds the departments intersecting the `roi` with
    [`get_departements_for_roi`](https://qonfluens.github.io/spacemodR/reference/get_departements_for_roi.md).

2.  Lists the OCS GE deliveries of these departments (version 2,
    GeoPackage) and selects the vintage.

3.  Downloads the archives missing from `cache_dir`, checks their MD5
    sum and extracts the `OCCUPATION_SOL` layer.

4.  Reads the features in the bounding box of the `roi` and clips them
    to its exact shape.

Corrective patches published separately by IGN (`PATCHCORRECTIF`) and
the layers of differences between two vintages are not used.

## Note

This function requires a working internet connection and a way to
extract 7z archives: either the archive package, or the `7z` command
line tool (7-Zip, p7zip).

## See also

[`get_ocsge`](https://qonfluens.github.io/spacemodR/reference/get_ocsge.md),
[`get_ocsge_data_fgb`](https://qonfluens.github.io/spacemodR/reference/get_ocsge_data_fgb.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  data(roi_metaleurop)

  # latest vintage
  ocsge_data <- get_ocsge_data_ign(roi_metaleurop)

  # a given vintage
  ocsge_2021 <- get_ocsge_data_ign(roi_metaleurop, year = 2021)

  habitat_vole <- join_ocsge_species(ocsge_2021, "Microtus_arvalis")
} # }
```
