# Retrieve species occurrences from GBIF

This function retrieves the occurrence records of a species from the
Global Biodiversity Information Facility (GBIF), with the public
occurrence search API. No account and no API key are needed.

Only records with coordinates and without known geospatial issue are
returned.

## Usage

``` r
get_gbif_data(species, roi = NULL, limit = 1000, ..., api_url = NULL)
```

## Arguments

- species:

  Character. Scientific name of the species, for instance
  `"Apodemus sylvaticus"`. Underscores are read as spaces, so the names
  of
  [`ocsge_species_dict`](https://qonfluens.github.io/spacemodR/reference/ocsge_species_dict.md)
  and
  [`eunis_species_dict`](https://qonfluens.github.io/spacemodR/reference/eunis_species_dict.md)
  can be used directly.

- roi:

  An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) (or `sfc`)
  object defining the Region Of Interest, in any projection. The search
  is made on its bounding box, then only the records inside `roi` are
  kept. With `NULL` (default), the search is made on the whole world.

- limit:

  Integer. Maximum number of records to download. Default is 1000. The
  API does not return more than 100,000 records for one search (see
  Details).

- ...:

  Other filters of the GBIF occurrence search API, as named arguments.
  For instance `year = "2000,2024"`,
  `coordinateUncertaintyInMeters = "0,100"`,
  `basisOfRecord = "HUMAN_OBSERVATION"` or `country = "FR"`. A range is
  written `"min,max"`.

- api_url:

  Character. Base URL of the GBIF API. Exposed mainly for testing
  against a different endpoint.

## Value

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object of
points in EPSG:4326, one row for each record, with the columns:

- key:

  GBIF identifier of the record
  (`https://www.gbif.org/occurrence/<key>`).

- species:

  Scientific name of the species.

- coordinateUncertaintyInMeters:

  Uncertainty of the position, in meters. Often `NA`.

- eventDate, year:

  Date and year of the observation.

- basisOfRecord:

  Type of record, for instance `"HUMAN_OBSERVATION"` or
  `"PRESERVED_SPECIMEN"`.

- datasetKey:

  GBIF identifier of the dataset
  (`https://www.gbif.org/dataset/<datasetKey>`).

- license:

  License of the record.

The total number of records matching the search in GBIF is stored in the
attribute `"gbif_count"`.

## Details

The name is first matched against the GBIF taxonomy (`species/match`),
then the records are downloaded by pages of 300 (`occurrence/search`).

**Limitations:**

- The search API stops at 100,000 records. For a larger request, split
  the region of interest, or the years with `year`.

- The search API gives no DOI. For a publication, GBIF asks to cite a
  download: see <https://www.gbif.org/citation-guidelines> and the
  function `occ_download()` of the 'rgbif' package.

- GBIF records are opportunistic observations. They show where the
  species was seen, not where it is absent, and the position can be
  uncertain by several kilometers.

## Note

This function requires a working internet connection.

## See also

[`get_eunis_data`](https://qonfluens.github.io/spacemodR/reference/get_eunis_data.md)
and
[`get_ocsge`](https://qonfluens.github.io/spacemodR/reference/get_ocsge.md)
for the land cover of the same region of interest.

## Examples

``` r
if (FALSE) { # \dontrun{
  data("roi_metaleurop")
  occ <- get_gbif_data("Apodemus sylvaticus", roi = roi_metaleurop)
  plot(sf::st_geometry(occ))

  # recent records with a position known within 100 m
  occ <- get_gbif_data("Apodemus sylvaticus", roi = roi_metaleurop,
                       year = "2000,2024",
                       coordinateUncertaintyInMeters = "0,100")
} # }
```
