context("ipdwInterp")

test_that("ipdwInterp works", {
  sf_ob                 <- data.frame(aa = rnorm(2))
  xy                    <- data.frame(x = c(4, 2), y = c(8, 4))
  sf_ob                 <- st_as_sf(cbind(sf_ob, xy), coords = c("x", "y"))
  m                     <- matrix(NA, 10, 5)
  costras               <- terra::rast(xmin = 0, xmax = ncol(m),
    ymin = 0, ymax = nrow(m))
  crs(costras)   <- NA
  costras[]             <- 1

  costras[]     <- runif(terra::ncell(costras), min = 1, max = 10)
  for (i in seq_len(nrow(costras))) {
    costras[i, ] <- costras[i, ] + i
  }
  for (i in seq_len(ncol(costras))) {
    costras[, i]  <- costras[, i] + i
  }

  rstack       <- pathdistGen(sf_ob, costras, 100, progressbar = FALSE)
  pointslayers <- rm_na_pointslayers(param_name = "rnorm.2.", sf_ob = sf_ob,
    rstack = rstack)
  rstack       <- pointslayers$rstack
  sf_ob        <- pointslayers$sf_ob
  res          <- ipdwInterp(sf_ob, rstack, "aa")

  expect_equal(dim(rstack)[3], nrow(sf_ob))
  expect_s4_class(res, "ipdwResult")
  expect_error(ipdwInterp(sf_ob, rstack),
    "Must pass a specific column name to the paramlist argument.")

  expect_error(ipdwInterp(sf_ob, rstack, "bb"),
    "Variable(s) 'bb' does not exist in sf_ob object.", fixed = TRUE)

  expect_s4_class(ipdwInterp(sf_ob, rstack, "aa", trim_rstack = TRUE), "ipdwResult")
})

