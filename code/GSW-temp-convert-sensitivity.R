
install.packages('gsw')
library(gsw)
library(fields)

z <- -seq(5,200,1)
lat <- 28 #seq(24,30,.2)

gsw_p_from_z(z, lat)

gsw_SA_from_SP(36, 75, 85, 28) # SP, p, longitude, latitude

gsw_CT_from_pt(36, pt) # SA, pt

gsw_t_from_CT(36, CT, p) # SA, CT, p

### what are the potential biases in bot temp if not corrected?
### range of depths
### range of temperature

test <- expand.grid(z = z, CT = seq(5,30,.5))
out <- gsw_t_from_CT(rep(35,nrow(test)), test$CT, gsw_p_from_z(test$z, lat))
out_m <- matrix(out, length(seq(5,30,.5)), length(z))
imagePlot(seq(5,30,.5), -z, out_m)


test2 <- expand.grid(SA = seq(0,40,1), CT = seq(5,30,.5))
out2 <- gsw_t_from_CT(test2$SA, test2$CT, gsw_p_from_z(-70, lat))
out2_m <- matrix(out2, length(seq(5,30,.5)), length(seq(0,40,1)))
imagePlot(seq(5,30,.5), seq(0,40,1), out2_m)


