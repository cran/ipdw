## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.pos = "center", fig.align = "center")
# , fig.path = "images/"

## ----load_package, message=FALSE, warning=FALSE-------------------------------
library(ipdw)

## ----load_data, message = FALSE, results='hide'-------------------------------
library(sf)
pols <- st_read(system.file("extdata/kattegat_coast.gpkg", package = "ipdw"))
pnts <- st_read(system.file("extdata/kattegat_pnts.gpkg", package = "ipdw"))

## ----create_costraster--------------------------------------------------------
costras <- costrasterGen(pnts, pols, extent = "pnts",
  projstr = projection(pols))
# insert contiguous barrier
costras[160:170, 1:80] <- 10000

## ----avg_nearest_neigh, message = FALSE---------------------------------------
# find average nearest neighbor
library(spatstat)

W              <- owin(range(c(st_bbox(pnts)["xmin"], st_bbox(pnts)["xmax"])),
                       range(c(st_bbox(pnts)["ymin"], st_bbox(pnts)["ymax"])))
kat.pp         <- ppp(st_coordinates(pnts)[,1], st_coordinates(pnts)[,2], window = W)
mean.neighdist <- mean(nndist(kat.pp))

# grid building
gridsize       <- mean.neighdist * 2
grainscale.fac <- gridsize / res(costras)[1]
gridras        <- aggregate(costras, fact = grainscale.fac)
gridpol        <- rasterToPolygons(gridras)
gridpol$value  <- row.names(gridpol)

# spatial join
fulldataset.over <- sf::st_join(pnts, st_as_sf(gridpol))

# grid selection
set.seed(2)
gridlev <- unique(fulldataset.over$value)
for (i in seq_along(gridlev)) {
  activesub <- subset(fulldataset.over, fulldataset.over$value == gridlev[i])
  selectnum <- gdata::resample(seq_len(nrow(activesub)), 1)
  if (i == 1) {
    training <- activesub[selectnum, ]
  } else {
    training <- rbind(training, activesub[selectnum, ])
  }
}

## ----split_training_validation------------------------------------------------
validate             <- fulldataset.over[!(row.names(fulldataset.over) %in%
  row.names(training)), ]

## ----plot_cost_raster, fig.cap = "<strong>Figure 1: Cost raster representing the high cost of travel through land areas. Training and validation points are shown in black and red respectively.</strong>", fig.height = 6, fig.width = 5----

plot(costras)
plot(st_geometry(training), add = TRUE)
plot(st_geometry(validate), col = "red", add = TRUE)

## ----interpolate, cache = FALSE, message = FALSE, results = 'hide'------------
paramlist <- c("salinity")
final.ipdw <- ipdw(training, costras, range = mean.neighdist * 10, paramlist,
  overlapped = TRUE)

## ----plot_interpolation, fig.cap = "<strong>Figure 2: Interpolated salinity surface by IPDW.</strong>", fig.height = 6, fig.width = 5----
plot(final.ipdw, main = "Kattegat salinity (ppt)")

## ----create_idw, eval=FALSE---------------------------------------------------
#  idw.grid <- rasterToPoints(costras, fun = function(x) {
#    x < 10000
#  }, spatial = FALSE)
#  idw.grid <- st_as_sf(data.frame(idw.grid), coords = c("x", "y"), crs = st_crs(training))
#  kat.idw  <- gstat::idw(salinity ~ 1, training, idw.grid, maxdist = mean.neighdist * 10,
#    debug.level = 0)["var1.pred"]
#  final.idw <- rasterize(as_Spatial(kat.idw), final.ipdw)
#  final.idw <- raster::subset(final.idw, "var1.pred")

## ----plot_ipdw_vs_idw, fig.cap = "<strong>Figure 3: Comparison between IPDW and IDW outputs. Note the overestimation of salinity on the upstream (south) side of the contiguous barrier.</strong>", fig.width = 6, fig.height = 4, eval=FALSE----
#  par(mfrow = c(1, 3), mar = c(5.1, 4.1, 4.1, 5.1))
#  plot(final.ipdw, main = "IPDW")
#  plot(final.idw, main = "IDW")
#  plot(final.idw - final.ipdw,  main = "IDW versus IPDW")

## ----plot_ipdw_vs_idw_img, echo=FALSE-----------------------------------------
knitr::include_graphics("images/plot_ipdw_vs_idw-1.png")

## ----generate_validation, eval=FALSE------------------------------------------
#  measured.spdf              <- data.frame(validate$salinity)
#  
#  valid.ipdw <- errorGen(final.ipdw, validate["salinity"], measured.spdf)
#  valid.idw  <- errorGen(final.idw, validate["salinity"], measured.spdf)

## ----plot_validation, fig.cap = "<strong>Figure 4: Comparison between IPDW and IDW interpolation error.  A one-to-one line and best-fit line are shown in black and red respectively.</strong>", fig.width = 8, fig.height = 5, eval=FALSE----
#  par(mfrow = c(1, 2))
#  valid.ipdw <- errorGen(final.ipdw, validate["salinity"], measured.spdf,
#    plot = TRUE, title = "IPDW")
#  valid.idw <- errorGen(final.idw, validate["salinity"], measured.spdf,
#    plot = TRUE, title = "IDW")

## ----plot_validation_img, echo=FALSE------------------------------------------
knitr::include_graphics("images/plot_validation-1.png")

