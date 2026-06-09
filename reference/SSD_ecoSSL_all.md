# Eco-SSL Toxicity Data for Multiple Taxonomic Groups

A comprehensive dataset combining toxicity values used for the
derivation of Ecological Soil Screening Levels (Eco-SSL). It includes
ecotoxicological data (NOAEL and LOAEL) for mammals, birds,
invertebrates, and plants exposed to various chemical compounds.

## Usage

``` r
SSD_ecoSSL_all
```

## Format

A tibble (data frame) with 9 variables:

- ERE:

  Character. Abbreviation for the Ecological Receptor Endpoint or effect
  category (e.g., "BIO" for Biochemical, "BEH" for Behavior).

- order_tox_value:

  Integer. The sequential order or index of the toxicity value within
  its specific group.

- tox_value_NOAEL:

  Numeric. The No Observed Adverse Effect Level (NOAEL), typically
  expressed in mg/kg/day or mg/kg soil.

- tox_value_LOAEL:

  Numeric. The Lowest Observed Adverse Effect Level (LOAEL), typically
  expressed in mg/kg/day or mg/kg soil.

- compound:

  Character. The chemical compound or trace element evaluated (e.g.,
  "Antimony", "Cadmium").

- species_group:

  Character. The broad taxonomic or functional group of the tested
  species (e.g., "Mammalian Wildlife").

- test_organism:

  Character. The common and/or scientific name of the specific organism
  tested (e.g., "Rat (Rattus norvegicus)").

- tox_value:

  Numeric. The primary toxicity value retained for the assessment
  (usually corresponds to the NOAEL or a derived threshold).

- ERE_full:

  Character. The full, unabbreviated name of the endpoint or effect
  category (e.g., "Biochemical", "Behavior").

## Source

Derived from the United States Environmental Protection Agency (US EPA)
Ecological Soil Screening Levels (Eco-SSL) database and documentation.

## Examples

``` r
data(SSD_ecoSSL_all)
head(SSD_ecoSSL_all)
#> # A tibble: 6 × 9
#> # Groups:   ERE [2]
#>   ERE   order_tox_value tox_value_NOAEL tox_value_LOAEL compound species_group  
#>   <chr>           <int>           <dbl>           <dbl> <chr>    <chr>          
#> 1 BIO                 1            0.06            0.64 Antimony Mammalian Wild…
#> 2 BIO                 2           NA               3.5  Antimony Mammalian Wild…
#> 3 BIO                 3           81             413    Antimony Mammalian Wild…
#> 4 BEH                 1            6.1            46    Antimony Mammalian Wild…
#> 5 BEH                 2            6.35           11.1  Antimony Mammalian Wild…
#> 6 BEH                 3           NA              23.4  Antimony Mammalian Wild…
#> # ℹ 3 more variables: test_organism <chr>, tox_value <dbl>, ERE_full <chr>
table(SSD_ecoSSL_all$species_group)
#> 
#>              Avian       Invertebrate Mammalian Wildlife              Plant 
#>               1615                234               2562                332 
```
