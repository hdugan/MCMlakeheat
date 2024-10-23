library(mgcv)
install.packages("VLTimeCausality")
library(VLTimeCausality)

# Summarise by day 
heat.day = hypo.fill |> 
  mutate(tempV = tempUse * vol_layer_m3) |> 
  summarise(ice.approx = mean(ice.approx, na.rm = T), heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D), vol = sum(volUse, na.rm = T), LL = first(masl.approx), 
            tempV = sum(tempV, na.rm = T)/vol, tempUse = mean(tempUse, na.rm = T),
            kd.ice = first(kd.ice)) |> 
  # mutate(deltaVol = c(NA, diff(vol)), deltaLL = c(NA, diff(LL)), deltaIce = c(NA, -diff(ice.approx))) |> 
  # mutate(deltaiceLL = deltaLL - deltaIce) |> 
  mutate(heatTot_J_m2 = (heat_J - heatIce_J)/Area_2D) |> 
  ungroup() |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  mutate(dec.date = decimal_date(date_time), yday = yday(date_time)) |> 
  mutate(yday = if_else(yday > 200, yday, yday+365)) |> 
  filter(month(date_time) >= 10)

# geom_smooth(aes(x = date_time, y = tempUse, col = 'tempMean'), method = 'gam', formula = y ~ s(x, bs = "cs", k = 20)) +
#   geom_point(aes(x = date_time, y = tempUse, col = 'tempMean'), size = 0.5) +
#   geom_smooth(aes(x = date_time, y = (heat_J - heatIce_J)/1e16, col = 'heat'), method = 'gam', formula = y ~ s(x, bs = "cs", k = 20)) +
#   geom_point(aes(x = date_time, y = (heat_J - heatIce_J)/1e16, col = 'heat'), size = 0.5) +
#   geom_smooth(aes(x = date_time, y = -ice.approx, col = 'ice'), method = 'gam', formula = y ~ s(x, bs = "cs", k = 20)) +
#   geom_point(aes(x = date_time, y = -ice.approx, col = 'ice'), size = 0.5) +
#   geom_smooth(aes(x = date_time, y = vol/1e7, col = 'vol'), method = 'gam', formula = y ~ s(x, bs = "cs", k = 20)) +
#   geom_point(aes(x = date_time, y = vol/1e7, col = 'vol'), size = 0.5) +


## fit GAM using gamm() with a CAR(1)
# mod <- gamm(ice.approx ~ s(dec.date, k = 15), data = heat.day |> filter(location_name == 'Lake Fryxell'),
#             correlation = corCAR1(form = ~ dec.date), method = "REML")
# # The estimated value of ϕ for the CAR(1) can be extracted from the fitted model via the $lme
# # component. Here we just extract the correlation structure component.
# ## estimate of phi and confidence interval
# smallPhi <- intervals(mod$lme, which = "var-cov")$corStruct
# smallPhi
# ## summary object
# summary(mod$gam)
# ## plot CAR(1) process
# S <- seq(0, 50, length = 100)
# car1 <- setNames(as.data.frame(t(outer(smallPhi, S, FUN = `^`)[1, , ])),
#                  c("Lower","Correlation","Upper"))
# car1 <- transform(car1, S = S)
# ggplot(car1, aes(x = S, y = Correlation)) +
#   geom_ribbon(aes(ymax = Upper, ymin = Lower),
#               fill = "black", alpha = 0.2) +
#   geom_line() +
#   ylab(expression(italic(c) * (list(h, varphi)))) +
#   xlab(expression(h ~ (years)))



## fit GAM 
uselake = 'Lake Fryxell'
mod.ice <- gam(ice.approx ~ s(dec.date, k = 40, bs = 'ad'), 
               data = heat.day |> filter(location_name == uselake), method = "REML")
mod.temp <- gam(tempUse ~ s(dec.date, k = 40, bs = 'ad'), 
               data = heat.day |> filter(location_name == uselake), method = "REML")
mod.vol <- gam(vol ~ s(dec.date, k = 40, bs = 'ad'), 
                data = heat.day |> filter(location_name == uselake), method = "REML")
mod.LL <- gam(LL ~ s(dec.date, k = 40, bs = 'ad'), 
               data = heat.day |> filter(location_name == uselake), method = "REML")

summary(mod.ice)
summary(mod.temp)
summary(mod.vol)
summary(mod.LL)

N <- 300 # number of points at which to evaluate the smooth
## create new data to predict at start of every month
newYear = data.frame(date_time = seq.Date(from = as.Date('1994-10-01'), to = as.Date('2024-02-01'), by = 'month')) |> 
  mutate(dec.date = decimal_date(date_time))

## Predict from the fitted model; note we predict from the gam part
newYear <- newYear |> 
  bind_cols(predict(mod.ice, newYear, se.fit = TRUE)) |> rename(fit.ice = fit, se.fit.ice = se.fit) |> 
  bind_cols(predict(mod.temp, newYear, se.fit = TRUE)) |> rename(fit.temp = fit, se.fit.temp = se.fit) |> 
  bind_cols(predict(mod.vol, newYear, se.fit = TRUE)) |> rename(fit.vol = fit, se.fit.vol = se.fit) |> 
  bind_cols(predict(mod.LL, newYear, se.fit = TRUE)) |> rename(fit.LL = fit, se.fit.LL = se.fit)

## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(mod.ice))
crit.t.temp = qt(0.975, df = df.residual(mod.temp))
crit.t.vol = qt(0.975, df = df.residual(mod.vol))
crit.t.LL = qt(0.975, df = df.residual(mod.LL))

newYear <- newYear |> mutate(upper.ice = fit.ice + (crit.t * se.fit.ice),
                     lower.ice = fit.ice - (crit.t * se.fit.ice)) |> 
  mutate(upper.temp = fit.temp + (crit.t * se.fit.temp),
         lower.temp = fit.temp - (crit.t * se.fit.temp)) |> 
  mutate(upper.vol = fit.vol + (crit.t * se.fit.vol),
         lower.vol = fit.vol - (crit.t * se.fit.vol)) |> 
  mutate(upper.LL = fit.LL + (crit.t * se.fit.LL),
         lower.LL = fit.LL - (crit.t * se.fit.LL))


## Plot estimated trend
p1 = ggplot(newYear, aes(x = dec.date, y = fit.ice)) +
  geom_ribbon(aes(ymin = lower.ice, ymax = upper.ice, x = dec.date), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = heat.day |> filter(location_name == uselake), 
             mapping = aes(x = dec.date, y = ice.approx, color = month(date_time)),
             inherit.aes = FALSE) +
  geom_line() +
  labs(y = "ice (m)", x = "dec.date")

p2 = ggplot(newYear, aes(x = dec.date, y = fit.temp)) +
  geom_ribbon(aes(ymin = lower.temp, ymax = upper.temp, x = dec.date), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = heat.day |> filter(location_name == uselake), 
             mapping = aes(x = dec.date, y = tempUse, color = month(date_time)),
             inherit.aes = FALSE) +
  geom_line() +
  labs(y = "temp (°C)", x = "dec.date")

p3 = ggplot(newYear, aes(x = dec.date, y = fit.vol)) +
  geom_ribbon(aes(ymin = lower.vol, ymax = upper.vol, x = dec.date), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = heat.day |> filter(location_name == uselake), 
             mapping = aes(x = dec.date, y = vol, color = month(date_time)),
             inherit.aes = FALSE) +
  geom_line() +
  labs(y = "vol (m3)", x = "dec.date")

p4 = ggplot(newYear, aes(x = dec.date, y = fit.LL)) +
  geom_ribbon(aes(ymin = lower.LL, ymax = upper.LL, x = dec.date), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = heat.day |> filter(location_name == uselake), 
             mapping = aes(x = dec.date, y = LL, color = month(date_time)),
             inherit.aes = FALSE) +
  geom_line() +
  labs(y = "LL (m)", x = "dec.date")


p1/p2/p3/p4

