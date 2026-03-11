# Efficiently retrieve OCS GE data from a remote FlatGeobuf

This function retrieves OCS GE (Land Cover) data for a specific Region
of Interest (ROI) directly from a remote FlatGeobuf (.fgb) file hosted
on a server (e.g., S3).

It leverages GDAL's virtual file system (\`/vsicurl/\`) and the spatial
indexing capabilities of FlatGeobuf to download only the data chunks
intersecting the bounding box of the ROI, making it highly efficient for
large datasets.

## Usage

``` r
get_ocsge_data_fgb(roi, fgb_url)
```

## Arguments

- roi:

  An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object
  defining the Region Of Interest. It can be in any projection, but will
  be transformed to EPSG:2154 (Lambert-93) internally.

- fgb_url:

  Character string. The public URL to the remote \`.fgb\` file.

## Value

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object
containing the OCS GE polygons intersected by the ROI, projected in
Lambert-93 (EPSG:2154).

## Details

The function performs the following steps:

1.  Transforms the `roi` to Lambert-93 (EPSG:2154).

2.  calculates the bounding box of the `roi`.

3.  Uses
    [`sf::st_read`](https://r-spatial.github.io/sf/reference/st_read.html)
    with a WKT filter to fetch only relevant features from the remote
    file.

4.  Applies a precise geometric intersection (\`st_intersection\`) to
    clip the data to the exact shape of the `roi`.

## Note

This function requires a working internet connection and GDAL support
for the FlatGeobuf driver and network capabilities (vsicurl).

## Examples

``` r
if (FALSE) { # \dontrun{
  library(sf)

  # 1. Define a Region of Interest (ROI)
  # Example: A small bounding box in France
  my_roi <- st_as_sf(data.frame(
    lon = c(2.3, 2.4, 2.4, 2.3, 2.3),
    lat = c(48.8, 48.8, 48.9, 48.9, 48.8)
  ), coords = c("lon", "lat"), crs = 4326)

  # 2. URL to the remote FlatGeobuf file
  # (Replace with the actual URL of your OCS GE bucket)
  url_fgb <- "https://example.com/data/ocsge_grand_est.fgb"

  # 3. Fetch data
  ocsge_data <- get_ocsge_data_fgb(roi = my_roi, fgb_url = url_fgb)

  # 4. Check result
  print(ocsge_data)
  plot(st_geometry(ocsge_data))
} # }
```
