test_that("test eunis collector", {

  testthat::skip_on_cran()
  testthat::skip_if_offline()

  roi_test <- sf::st_as_sf(data.frame(
    lon = c(2.30, 2.40, 2.40, 2.30, 2.30),
    lat = c(48.83, 48.83, 48.88, 48.88, 48.83)
  ), coords = c("lon", "lat"), crs = 4326)
  roi_test <- sf::st_sf(geometry = sf::st_cast(sf::st_combine(roi_test), "POLYGON"))

  try({
    test_data <- spacemodR::get_eunis_data(roi = roi_test, level = "L1")
    print(test_data)
    terra::plot(test_data)
  })

})
