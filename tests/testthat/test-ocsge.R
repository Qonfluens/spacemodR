test_that("test ocsge collector", {

  testthat::skip_on_cran()
  testthat::skip_if_offline()

  public_url_fgb = "https://spacemod-ocsge.s3.fr-par.scw.cloud/ocsge_france.fgb"
  # small polygon to test
  roi_test <- sf::st_as_sfc("POLYGON((2.4 46.5, 2.5 46.5, 2.5 46.6, 2.4 46.6, 2.4 46.5))")
  sf::st_crs(roi_test) <- 4326 # WGS84
  try({
    test_data <- spacemodR::get_ocsge_data_fgb(roi = roi_test, fgb_url = public_url_fgb)
    print(test_data)
    plot(sf::st_geometry(test_data))
  })

})

test_that("ocsge_species_dict covers every cover and the crosswalk pairs", {

  d <- spacemodR::ocsge_species_dict
  xw <- spacemodR::ref_ocsge_eunis

  expect_true(all(c("code_cs", "code_us", "species_name", "suitability", "weight_breeding",
                    "weight_foraging", "permeability", "resistance") %in% names(d)))
  expect_setequal(names(d)[-2], sub("code_eunis", "code_cs", names(spacemodR::eunis_species_dict)))

  one <- d[d$species_name == "Apodemus_sylvaticus", ]
  expect_setequal(one$code_cs[is.na(one$code_us)], spacemodR::ref_ocsge$code_cs)
  pairs <- unique(xw[!is.na(xw$code_us), c("code_cs", "code_us")])
  expect_setequal(paste(one$code_cs, one$code_us)[!is.na(one$code_us)], paste(pairs$code_cs, pairs$code_us))
  expect_equal(nrow(d), 935 * nrow(one))

  expect_true(all(d$suitability >= 0 & d$suitability <= 10))
  expect_equal(d$resistance, round(10^(2 * (1 - d$permeability / 10)), 1))
})

test_that("join_ocsge_species matches on cover, then on cover and use", {

  square <- function(x) sf::st_polygon(list(cbind(x + c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))))
  landscape <- sf::st_sf(
    code_cs = c("CS2.2.1", "CS2.2.1", "CS 2.2.1", "CS2.1.1.1", "CS9.9"),
    code_us = c("US1.1", "US5", "US1.4", "US1.2", "US1.1"),
    geometry = sf::st_sfc(lapply(0:4, square), crs = 2154)
  )
  d <- spacemodR::ocsge_species_dict
  vole <- d[d$species_name == "Myodes_glareolus", ]
  score <- function(cs, us) {
    vole$suitability[vole$code_cs == cs & (if (is.na(us)) is.na(vole$code_us) else vole$code_us %in% us)]
  }

  # cover and use: pairs known to the dictionary use their own row, the others the cover row
  res <- join_ocsge_species(landscape, "Myodes_glareolus")
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), nrow(landscape))
  expect_equal(attr(res, "species_target"), "Myodes_glareolus")
  expect_equal(res$use_matched, c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(res$suitability[1:4], c(score("CS2.2.1", "US1.1"), score("CS2.2.1", "US5"),
                                       score("CS2.2.1", NA), score("CS2.1.1.1", NA)))
  expect_true(is.na(res$suitability[5]))
  expect_equal(res$nomenclature[1], "Formations herbacées")

  # cover only: when asked, or when the object has no use column
  res_cs <- join_ocsge_species(landscape, "Myodes_glareolus", us_col = NULL)
  expect_false(any(res_cs$use_matched))
  expect_equal(res_cs$suitability[1:3], rep(score("CS2.2.1", NA), 3))
  res_nous <- join_ocsge_species(landscape[, "code_cs"], "Myodes_glareolus")
  expect_equal(res_nous$suitability, res_cs$suitability)

  # the shipped datasets
  expect_false(anyNA(join_ocsge_species(spacemodR::ocsge_metaleurop, "Myodes_glareolus")$suitability))
  expect_false(anyNA(join_ocsge_species(spacemodR::ocsge_us_metaleurop, "Myodes_glareolus")$suitability))

  expect_warning(expect_null(join_ocsge_species(landscape, "Not_a_species")), "No data found")
  expect_warning(join_ocsge_species(landscape, "Sorex"), "Multiple species matched")
  expect_s3_class(plot_species_habitat(res), "patchwork")
  expect_error(plot_species_habitat(landscape), "Missing required columns")
})
