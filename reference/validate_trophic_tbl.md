# Validate a trophic table

Internal function used to ensure that a trophic_tbl object respects all
structural constraints.

## Usage

``` r
validate_trophic_tbl(df)
```

## Arguments

- df:

  A data.frame intended to be a trophic_tbl object.

## Value

A validated `trophic_tbl` object.

## Details

The function checks that:

- All links are unique

- No self-loops are present

- The graph is acyclic

It also computes and updates the trophic level attribute.
