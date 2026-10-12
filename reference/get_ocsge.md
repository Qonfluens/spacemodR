# Retrieve OCS GE data for a region of interest

Generic entry point to get OCS GE (Occupation du Sol à Grande Echelle)
polygons, with their cover (`code_cs`) and use (`code_us`) codes, for a
Region of Interest (ROI). The `source` argument selects where the data
come from:

- `"ign"`:

  the official per-department deliveries of the IGN Géoplateforme, see
  [`get_ocsge_data_ign`](https://qonfluens.github.io/spacemodR/reference/get_ocsge_data_ign.md).
  Nothing to host, but each department is downloaded in full the first
  time.

- `"fgb"`:

  a remote FlatGeobuf file that you host, see
  [`get_ocsge_data_fgb`](https://qonfluens.github.io/spacemodR/reference/get_ocsge_data_fgb.md).
  Only the features of the ROI are downloaded.

## Usage

``` r
get_ocsge(roi, source = c("ign", "fgb"), ...)
```

## Arguments

- roi:

  An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) (or `sfc`)
  object defining the Region Of Interest.

- source:

  Character. `"ign"` (default) or `"fgb"`.

- ...:

  Arguments passed to the function of the selected source: `year`,
  `cache_dir` and `quiet` for `"ign"`, `fgb_url` for `"fgb"`.

## Value

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object
containing the OCS GE polygons clipped to the ROI.

## See also

[`join_ocsge_species`](https://qonfluens.github.io/spacemodR/reference/join_ocsge_species.md)
to score the polygons for a species, and
[`get_eunis_data`](https://qonfluens.github.io/spacemodR/reference/get_eunis_data.md)
for the EUNIS land cover.

## Examples

``` r
if (FALSE) { # \dontrun{
  data(roi_metaleurop)

  # from the IGN Géoplateforme (latest vintage of each department)
  ocsge_data <- get_ocsge(roi_metaleurop)
  ocsge_data <- get_ocsge(roi_metaleurop, source = "ign", year = 2021)

  # from a FlatGeobuf file
  ocsge_data <- get_ocsge(roi_metaleurop, source = "fgb",
                          fgb_url = "https://example.com/data/ocsge_france.fgb")
} # }
```
