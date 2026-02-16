# Collect data from OCS GE repository using FlatGeobuf on S3

Collect data from OCS GE repository using FlatGeobuf on S3

## Usage

``` r
get_ocsge_data_fgb(roi, fgb_url)
```

## Arguments

- roi:

  An object \`sf\` defiing Region Of Interest.

- fgb_url:

  Public URL or a .fgb file.

## Value

An object \`sf\` cropped to the bounding bbox form ROI.
