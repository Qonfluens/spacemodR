# Nomenclature and render colors of EUNIS land cover classes

Code / label / render-color lookup table for the EEA "Ecosystem Type Map
v3.1, terrestrial part" ArcGIS service, at both EUNIS Level 1 (9 broad
classes) and Level 2 (46 detailed classes). It is used internally by
[`get_eunis_data`](https://qonfluens.github.io/spacemodR/reference/get_eunis_data.md)
to decode the service's exported image back into EUNIS codes, since that
service publishes no raw-value raster endpoint.

## Usage

``` r
data(ref_eunis)
```

## Format

A data frame with the following columns:

- level:

  Character. `"L1"` or `"L2"`.

- code:

  Character. The EUNIS code (e.g. `"J1"` at L2, `"J"` at L1).

- nomenclature:

  Character. The class label.

- couleur:

  Character. The hex render color used by the source ArcGIS service.

- r, g, b:

  Integer (0-255). The render color, split into channels for exact
  matching.

## Source

European Environment Agency, "Ecosystem Type Map v3.1, terrestrial
part":
<https://bio.discomap.eea.europa.eu/arcgis/rest/services/Ecosystem/EcosystemTypeMap_v3_1_Terrestrial/MapServer>

## Details

At `level == "L2"`, two pairs of classes share an identical render color
and cannot be distinguished from the exported image alone: `D5`/`F6` and
`D6`/`F7`. `level == "L1"` has no such collision. See
[`get_eunis_data`](https://qonfluens.github.io/spacemodR/reference/get_eunis_data.md)
for how this is handled.

## Examples

``` r
data(ref_eunis)
```
