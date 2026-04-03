
library(terra)

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/ER_SHP/ER 1")
er1 <- vect('ER1.shp') |> makeValid() |> aggregate()
setwd("C:/Users/brendan.turley/Documents/data/shapefiles/ER_SHP/ER 2")
er2 <- vect('ER2.shp') |> makeValid() |> aggregate()
setwd("C:/Users/brendan.turley/Documents/data/shapefiles/ER_SHP/ER 3")
er3 <- vect('ER3.shp') |> makeValid() |> aggregate()
setwd("C:/Users/brendan.turley/Documents/data/shapefiles/ER_SHP/ER 4")
er4 <- vect('ER4.shp') |> makeValid() |> aggregate()
setwd("C:/Users/brendan.turley/Documents/data/shapefiles/ER_SHP/ER 5")
er5 <- vect('ER5.shp') |> makeValid() |> aggregate()

par(mfrow=c(2,3))
plot(er1)
plot(er2)
plot(er3)
plot(er4)
plot(er5)

er <- rbind(er1,er2,er3,er4,er5)
plot(er,col=2:6)


setwd("C:/Users/brendan.turley/Documents/data/shapefiles/Habitat_Zone")
eez <- vect('gulf_eez.shp') |> makeValid()
est <- vect('estuarine.shp') |> makeValid()
nearshore <- vect('nearshore.shp') |> makeValid()
offshore <- vect('offshore.shp') |> makeValid()

eez_4326 <- project(eez, crs(est))

hz <- rbind(eez, est, nearshore, offshore)
plot(hz, col = 2:5)

crs(est)==crs(eez)
crs(est)==crs(eez_4326)
crs(est)==crs(offshore)
crs(est)==crs(nearshore)

er_4326 <- project(er, crs(est))
plot(er_4326)
plot(hz, add = T)

shapes <- intersect(er_4326, hz)
plot(shapes, col = 1:15)
