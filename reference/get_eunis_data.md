# Efficiently retrieve EUNIS land cover data from the EEA Ecosystem Type Map

This function retrieves EUNIS (European Nature Information System) land
cover data for a Region of Interest (ROI), as an alternative to the
French OCS-GE nomenclature used by
[`get_ocsge_data_fgb`](https://qonfluens.github.io/spacemodR/reference/get_ocsge_data_fgb.md).
It queries the European Environment Agency (EEA) "Ecosystem Type Map
v3.1, terrestrial part" ArcGIS MapServer, a 100 m resolution,
EEA-39-wide classified raster available at EUNIS Level 1 (9 broad
classes) or Level 2 (46 detailed classes).

## Usage

``` r
get_eunis_data(
  roi,
  level = c("L1", "L2"),
  resolution = 100,
  service_url =
    "https://bio.discomap.eea.europa.eu/arcgis/rest/services/Ecosystem/EcosystemTypeMap_v3_1_Terrestrial/MapServer"
)
```

## Arguments

- roi:

  An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) (or `sfc`)
  object defining the Region Of Interest. It can be in any projection,
  but will be transformed to EPSG:3035 (ETRS89-LAEA Europe) internally,
  the native grid of the source service.

- level:

  Character. `"L1"` (9 broad classes, default) or `"L2"` (46 detailed
  classes). See Details for a caveat on `"L2"`.

- resolution:

  Numeric. Requested pixel size in meters. Default is 100, the native
  resolution of the source raster; values below 100 only upsample
  without adding real detail. Use a coarser value for large ROIs to stay
  under the service's image-size limit (see Details).

- service_url:

  Character. The ArcGIS MapServer base URL. Exposed mainly for testing
  against a different endpoint.

## Value

A categorical
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
in EPSG:3035, with cell values pointing to a factor level table
([`levels`](https://rspatial.github.io/terra/reference/factors.html))
giving the EUNIS `code` for that level (see
[`ref_eunis`](https://qonfluens.github.io/spacemodR/reference/ref_eunis.md)
for the matching labels and colors). Cells outside the mapped
terrestrial extent (e.g. sea) are `NA`.

## Details

Unlike the OCS-GE FlatGeobuf source, this EUNIS layer has no raw-value
raster endpoint (no ArcGIS ImageServer is published for it) – only a
symbolized MapServer `export`. This function requests that export image
at the native resolution with nearest-neighbor resampling (so pixel
colors are not blended at class boundaries), then decodes each pixel's
exact RGB color back into an EUNIS code using the fixed legend palette
shipped as
[`ref_eunis`](https://qonfluens.github.io/spacemodR/reference/ref_eunis.md).

**Limitations:**

- The service caps a single export at 4096 x 4096 pixels. At the default
  100 m resolution that is about 410 x 410 km; larger ROIs must use a
  coarser `resolution` or be split and fetched in tiles.

- At `level = "L2"`, two pairs of classes are rendered with the
  identical color by the source service and cannot be told apart from
  the image alone: `D5`/`F6` and `D6`/`F7`. Affected pixels are returned
  with a combined code (e.g. `"D5/F6"`) rather than silently assigned to
  one of the two. `level = "L1"` has no such collision.

- Areas outside the mapped terrestrial extent (e.g. open sea) are
  detected from the export's alpha channel and returned as `NA`, not
  guessed from color.

## Note

This function requires a working internet connection.

## See also

[`ref_eunis`](https://qonfluens.github.io/spacemodR/reference/ref_eunis.md)
for the EUNIS code/label/color lookup table, and
[`get_ocsge_data_fgb`](https://qonfluens.github.io/spacemodR/reference/get_ocsge_data_fgb.md)
for the equivalent OCS-GE fetcher.

## Examples

``` r
if (FALSE) { # \dontrun{
  library(sf)

  my_roi <- st_as_sf(data.frame(
    lon = c(2.3, 2.4, 2.4, 2.3, 2.3),
    lat = c(48.8, 48.8, 48.9, 48.9, 48.8)
  ), coords = c("lon", "lat"), crs = 4326) |> st_combine() |> st_cast("POLYGON")

  eunis_data <- get_eunis_data(roi = my_roi, level = "L1")
  terra::plot(eunis_data)
} # }
```
