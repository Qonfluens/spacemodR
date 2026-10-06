# Soil metal concentrations sampled around the former Metaleurop smelter

Topsoil concentrations of cadmium (Cd), lead (Pb) and zinc (Zn) measured
at 595 sampling points around the former Metaleurop Nord lead and zinc
smelter (Noyelles-Godault, northern France). This is the data set used
to build the soil rasters shipped in \`inst/extdata\`
(\`ground_concentration\_\*.tif\`), see the kriging article.

Simple feature collection (POINT), projected CRS: RGF93 v1 / Lambert-93
(EPSG:2154).

## Usage

``` r
data(soil_metaleurop)
```

## Format

An \`sf\` object with 595 rows and 9 columns:

- id:

  Sample identifier.

- usage:

  Land-use class of the sampled plot, as coded in the original survey
  (\`"A"\`, \`"L"\`, \`"U"\`).

- dist:

  Distance (m) to the smelter chimney.

- angle:

  Direction of the sample seen from the smelter (degrees clockwise from
  the North).

- wind:

  Wind-rose frequency (in %) of the 20-degree sector containing
  \`angle\` (18 sectors centred on 0, 20, ..., 340 degrees, summing to
  100). The highest values lie north-east of the smelter, i.e. downwind
  of the dominant south-westerly winds.

- cd, pb, zn:

  Total concentrations in mg/kg of dry soil.

- geometry:

  Sampling location.

## Examples

``` r
data(soil_metaleurop)
summary(soil_metaleurop[, c("cd", "pb", "zn")])
#>        cd                 pb                zn                   geometry  
#>  Min.   :   0.100   Min.   :   16.0   Min.   :   44.0   POINT        :595  
#>  1st Qu.:   3.584   1st Qu.:  192.8   1st Qu.:  290.1   epsg:2154    :  0  
#>  Median :   5.200   Median :  287.3   Median :  425.0   +proj=lcc ...:  0  
#>  Mean   :  12.615   Mean   :  567.1   Mean   :  702.6                      
#>  3rd Qu.:   9.150   3rd Qu.:  519.5   3rd Qu.:  697.5                      
#>  Max.   :2402.062   Max.   :41958.8   Max.   :38762.9                      
```
