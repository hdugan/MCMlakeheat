
library(mgcv)
# install.packages("VLTimeCausality")
library(VLTimeCausality)
# Load necessary libraries
library(broom)
library(xtable)
library(corrr)
library(GGally)
library(olsrr) #collinearity 

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
  filter(month(date_time) >= 10) |> 
  mutate(tempUse = if_else(location_name == 'West Lake Bonney' & year(date_time) == 2005, NA, tempUse))

#!!!! 2005 data from WLB hypo looks too warm

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

lakecolor = data.frame(uselake = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'), 
           plotColor = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), 
           k = c(30,30,30,30))

output.plots = list()
output.fit1 = list()
output.fit2 = list()
output.fit3 = list()
output.fit4 = list()
output.fit5 = list()
output.fit5.5 = list()
output.predict = list()

# Big old loop ####
for (i in 1:4) {
  # Set up 
  uselake = lakecolor$uselake[i]
  plotColor = lakecolor$plotColor[i]
  usek = lakecolor$k[i]
  
  # There are a number of options to make a gam less wiggly:
  # Set the default s(..., k = 10) to a smaller value.
  # Set the default s(...,bs = 'tp') to ts.
  # Set gam(..., select = TRUE).
  # Set the default gam(..., gamma = 1) to a larger value. Try values between 1 and 2.
  # Set the default s(..., m = 2) to m = 1.
  # Set the default method = "GCV.Cp" to method = "REML" (section 1.1; Wood, 2011).
  # Force monotonically increasing/decreasing curves. See scam package and other options.
  # Change some of the smoothed predictors + s(X1) to linear terms + X1.
  # Use fewer predictors.
  
  ## Fit gam
  mod.ice <- gam(ice.approx ~ s(dec.date, k = usek, bs = 'tp'), 
                 data = heat.day |> filter(location_name == uselake))
  mod.temp <- gam(tempUse ~ s(dec.date, k = usek, bs = 'tp'), 
                 data = heat.day |> filter(location_name == uselake))
  mod.vol <- gam(vol ~ s(dec.date, k = usek, bs = 'tp'), 
                  data = heat.day |> filter(location_name == uselake))
  mod.LL <- gam(LL ~ s(dec.date, k = usek, bs = 'tp'), 
                 data = heat.day |> filter(location_name == uselake))
  mod.tempV <- gam(tempV ~ s(dec.date, k = usek, bs = 'tp'), 
                  data = heat.day |> filter(location_name == uselake))
  
  summary(mod.ice)
  summary(mod.temp)
  summary(mod.vol)
  summary(mod.LL)
  summary(mod.tempV)
  N <- 300 # number of points at which to evaluate the smooth
  
  ## create new data to predict at start of every month
  newYear = data.frame(date_time = seq.Date(from = as.Date('1993-10-01'), to = as.Date('2024-01-01'), by = 'month')) |> 
    mutate(dec.date = decimal_date(date_time))
  
  if (uselake == 'Lake Fryxell') {
    newYear = data.frame(date_time = seq.Date(from = as.Date('1995-10-01'), to = as.Date('2024-01-01'), by = 'month')) |> 
      mutate(dec.date = decimal_date(date_time))
  }
  if (uselake == 'Lake Hoare') {
    newYear = data.frame(date_time = seq.Date(from = as.Date('1995-10-01'), to = as.Date('2023-01-01'), by = 'month')) |> 
      mutate(dec.date = decimal_date(date_time))
  }
  
  ## Predict from the fitted model; note we predict from the gam part
  newYear <- newYear |> 
    bind_cols(predict(mod.ice, newYear, se.fit = TRUE)) |> rename(fit.ice = fit, se.fit.ice = se.fit) |> 
    bind_cols(predict(mod.temp, newYear, se.fit = TRUE)) |> rename(fit.temp = fit, se.fit.temp = se.fit) |> 
    bind_cols(predict(mod.vol, newYear, se.fit = TRUE)) |> rename(fit.vol = fit, se.fit.vol = se.fit) |> 
    bind_cols(predict(mod.LL, newYear, se.fit = TRUE)) |> rename(fit.LL = fit, se.fit.LL = se.fit) |> 
    bind_cols(predict(mod.tempV, newYear, se.fit = TRUE)) |> rename(fit.tempV = fit, se.fit.tempV = se.fit)
  
  ## Create the 95% confidence interval, get critical t-value 
  crit.t.ice <- qt(0.975, df = df.residual(mod.ice))
  crit.t.temp = qt(0.975, df = df.residual(mod.temp))
  crit.t.vol = qt(0.975, df = df.residual(mod.vol))
  crit.t.LL = qt(0.975, df = df.residual(mod.LL))
  crit.t.tempV = qt(0.975, df = df.residual(mod.tempV))
  
  # For a two-sided confidence interval, multiply the critical value by the sample's standard error of the mean
  newYear <- newYear |> mutate(upper.ice = fit.ice + (crit.t.ice * se.fit.ice),
                       lower.ice = fit.ice - (crit.t.ice * se.fit.ice)) |> 
    mutate(upper.temp = fit.temp + (crit.t.temp * se.fit.temp),
           lower.temp = fit.temp - (crit.t.temp * se.fit.temp)) |> 
    mutate(upper.vol = fit.vol + (crit.t.vol * se.fit.vol),
           lower.vol = fit.vol - (crit.t.vol * se.fit.vol)) |> 
    mutate(upper.LL = fit.LL + (crit.t.LL * se.fit.LL),
           lower.LL = fit.LL - (crit.t.LL * se.fit.LL)) |> 
    mutate(upper.tempV = fit.tempV + (crit.t.tempV * se.fit.tempV),
           lower.tempV = fit.tempV - (crit.t.tempV * se.fit.tempV))
  
  
  plotCustom = list(scale_color_manual(values = c('#3a6c85','#67bfeb', '#b1d4e6')),
                      theme_bw(base_size = 9),
                      theme(axis.title.x = element_blank(),
                            legend.position = 'none',
                            legend.title = element_blank()))
  
  
  ## Plot estimated trend
  p1 = ggplot(newYear, aes(x = dec.date, y = fit.temp)) +
    geom_ribbon(aes(ymin = lower.temp, ymax = upper.temp, x = dec.date), alpha = 0.2,
                inherit.aes = FALSE, fill = plotColor) +
    # geom_ribbon(aes(ymin = lower.tempV, ymax = upper.tempV, x = dec.date), alpha = 0.2,
    #             inherit.aes = FALSE, fill = "black") +
    geom_point(data = heat.day |> filter(location_name == uselake), 
               mapping = aes(x = dec.date, y = tempUse, color = factor(month(date_time))),
               inherit.aes = FALSE) +
    geom_line() +
    labs(y = "Mean Temp (°C)") +
    plotCustom
  
  p2 = ggplot(newYear, aes(x = dec.date, y = fit.ice)) +
    geom_ribbon(aes(ymin = lower.ice, ymax = upper.ice, x = dec.date), alpha = 0.2,
                inherit.aes = FALSE, fill = plotColor) +
    geom_point(data = heat.day |> filter(location_name == uselake), 
               mapping = aes(x = dec.date, y = ice.approx, color = factor(month(date_time))),
               inherit.aes = FALSE) +
    geom_line() +
    labs(y = "Ice Thickness (m)") +
    plotCustom
  
  p3 = ggplot(newYear, aes(x = dec.date, y = fit.vol)) +
    geom_ribbon(aes(ymin = lower.vol, ymax = upper.vol, x = dec.date), alpha = 0.2,
                inherit.aes = FALSE, fill = plotColor) +
    geom_point(data = heat.day |> filter(location_name == uselake), 
               mapping = aes(x = dec.date, y = vol, color = factor(month(date_time))),
               inherit.aes = FALSE) +
    scale_y_continuous(labels=function(x) x/1e6) +
    geom_line() +
    labs(y = bquote(Volume~(x10^6~m^3)))  +
    plotCustom
  
  p4 = ggplot(newYear, aes(x = dec.date, y = fit.LL)) +
    geom_ribbon(aes(ymin = lower.LL, ymax = upper.LL, x = dec.date), alpha = 0.2,
                inherit.aes = FALSE, fill = plotColor) +
    geom_point(data = heat.day |> filter(location_name == uselake), 
               mapping = aes(x = dec.date, y = LL, color = factor(month(date_time))),
               inherit.aes = FALSE) +
    geom_line() +
    labs(y = "Lake Level (m)")  +
    plotCustom
  
  # Join plots
  # p1/p2/p4/p3 +
  #   plot_layout(guides = 'collect')
  # 
  newYear2 = newYear |>
    filter(month(date_time) == 12) |> 
    select(date_time, fit.ice, fit.temp, fit.vol, fit.LL, fit.tempV) |> 
    mutate(ice.diff = c(0,diff(fit.ice))) |> 
    mutate(temp.diff = c(0,diff(fit.temp))) |> 
    mutate(tempV.diff = c(0,diff(fit.tempV))) |> 
    mutate(LL.diff = c(0,diff(fit.LL))) |> 
    mutate(vol.diff = c(0,diff(fit.vol)))

  output.predict[[i]] = newYear2

  # Standardize predictors
  newYear2.scale = newYear2 %>%
    mutate(across(where(is.numeric), scale))
    
  # Plots of Diffs 
  p5 = ggplot(newYear2) +
    geom_col(aes(x = date_time, y = temp.diff), fill = plotColor) +
    ylab('Temp Diff (°C)') +
    plotCustom
  p6 = ggplot(newYear2) +
    geom_col(aes(x = date_time, y = ice.diff), fill = plotColor) +
    ylab('Ice Diff (°C)') +
    plotCustom
  p7 = ggplot(newYear2) +
    geom_col(aes(x = date_time, y = LL.diff), fill = plotColor) +
    ylab('LL Diff (°C)') +
    plotCustom
  p8 = ggplot(newYear2) +
    geom_col(aes(x = date_time, y = vol.diff), fill = plotColor) +
    ylab('Vol Diff (°C)') +
    plotCustom
  
  # Join plots
  output.plots[[i]] = (p1 + labs(title = uselake))/p2/p4/p3/p5/p6/p7/p8 + 
    plot_annotation(subtitle = uselake) +
    plot_layout(guides = 'collect')
  # ggsave(paste0('figures/GAM_', uselake, '.png'), width = 3.5, height = 8.5, dpi = 500)
  
  # summary(lm(temp.diff ~ ice.diff + LL.diff + ice.diff:LL.diff, data = newYear2))
  # summary(lm(temp.diff ~ ice.diff + fit.ice + LL.diff + vol.diff, data = newYear2))
  # summary(lm(fit.temp ~ fit.ice , data = newYear2))
  
  ########################################### FIT 1 #####################################################
  # Fit a linear model
  output.fit1[[i]] <- lm(temp.diff ~ ice.diff + LL.diff + ice.diff:LL.diff, data = newYear2.scale)
  
  ####################################### FIT 2 #########################################################
  # Fit a linear model
  output.fit2[[i]] <- lm(temp.diff ~ fit.ice, data = newYear2.scale)
  
  ####################################### FIT 3 #########################################################
  # Fit a linear model
  output.fit3[[i]] <- lm(fit.temp ~ fit.ice, data = newYear2.scale)

  ####################################### FIT 4 #########################################################
  # Fit a linear model
  output.fit4[[i]] <- lm(fit.temp ~ fit.LL, data = newYear2.scale)

  output.fit5[[i]] <- lm(fit.temp ~ fit.LL + fit.ice, data = newYear2.scale)
  output.fit5.5[[i]] <- lm(fit.temp ~ fit.LL + fit.ice + fit.LL:fit.ice, data = newYear2.scale)
  
}

# Join two plots
output.plots[[1]] | output.plots[[2]] 
ggsave(paste0('figures/GAM_LF_LH.png'), width = 6.5, height = 8.5, dpi = 500)

output.plots[[3]] | output.plots[[4]] 
ggsave(paste0('figures/GAM_ELB_WLB.png'), width = 6.5, height = 8.5, dpi = 500)


# getCoeffs(1, usefit = output.fit1) 

#Synchrony between timeseries
sync1 = data.frame(date_time = output.predict[[3]]$date_time, 
                   LF_temp = c(NA, NA,output.predict[[1]]$fit.temp), 
              LH_temp = c(NA, NA, output.predict[[2]]$fit.temp,NA), 
              ELB_temp = output.predict[[3]]$fit.temp, 
              WLB_temp = output.predict[[4]]$fit.temp)

# Create a scatterplot matrix using ggpairs()
sync1 |> pivot_longer(cols = 2:5) |> 
  ggplot() +
  geom_path(aes(x = date_time, y = value, col = name)) +
  ylab('Temp (°C)') +
  scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a')) +
  theme_bw(base_size = 9)

# ggpairs(sync1, 
#         title = "Scatterplot Temp Correlations",
#         upper = list(continuous = wrap("cor", method="pearson", stars = FALSE)),
#         lower = list(continuous = wrap("smooth", alpha = 1, size = 0.8))) +
#   theme_minimal(base_size = 9)

options(digits=2)
correlation_table <- corrr::correlate(sync1, method = "pearson")
correlation_table

#Syncrony between timeseries
sync2 = data.frame(date_time = output.predict[[3]]$date_time, 
                   LF_tempdiff = c(NA, NA,output.predict[[1]]$temp.diff), 
                   LH_tempdiff = c(NA, NA, output.predict[[2]]$temp.diff,NA), 
                   ELB_tempdiff = output.predict[[3]]$temp.diff, 
                   WLB_tempdiff = output.predict[[4]]$temp.diff)

# Create a scatterplot matrix using ggpairs()
sync2 |> pivot_longer(cols = 2:5) |> 
  ggplot() +
  geom_path(aes(x = date_time, y = value, col = name)) +
  ylab('Temp Diff (°C)') +
  scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a')) +
  theme_bw(base_size = 9)

# # Create a scatterplot matrix using ggpairs()
# ggpairs(sync2, 
#         title = "Scatterplot TempDiff Correlations",
#         upper = list(continuous = wrap("cor", method="pearson", stars = FALSE)),
#         lower = list(continuous = wrap("smooth", alpha = 1, size = 0.8))) +
#         theme_minimal(base_size = 9)


options(digits=2)
correlation_table <- corrr::correlate(sync2, method = "pearson")
correlation_table

### Assess model fits 
getCoeffs <- function(i, usefit) {
  AIC = broom::glance(usefit[[i]])$AIC
  BIC = broom::glance(usefit[[i]])$BIC
  
  # Extract summary of the model
  summary_fit <- summary(usefit[[i]])
  
  # Extract adjusted R-squared
  adj_r_squared <- round(summary_fit$adj.r.squared,2)
  
  # Extract coefficients and p-values
  coeffs <- summary_fit$coefficients
  coeffs_df <- as.data.frame(coeffs) |> 
    select(Estimate, 4,) 
  
  coeffs_df = bind_cols(Lake = lakecolor$uselake[i], Parameter = rownames(coeffs_df), coeffs_df) |> 
    filter(Parameter != '(Intercept)')
  
  # Remove the row names
  rownames(coeffs_df) <- NULL
  # Add significance stars based on p-values
  coeffs_df$Signif <- cut(coeffs_df[, 4], 
                          breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                          labels = c("***", "**", "*", ".", " "))
  coeffs_df = coeffs_df |> left_join(data.frame(Parameter = coeffs_df$Parameter[1], AIC = AIC, BIC = BIC, 
                                                r2 = adj_r_squared))
  # VIFs 
  if(nrow(coeffs_df) >= 2) {
    out.coeffs = coeffs_df |> left_join(
      data.frame(Lake = lakecolor$uselake[i]) |> 
      bind_cols(ols_vif_tol(usefit[[i]])) |> 
        rename(Parameter = Variables))
  } else {
    out.coeffs = coeffs_df
  }
  
  return(out.coeffs)
}

### Variable Inflation Factors to assess colinearity 
# VIF = 1: There is no correlation between a given predictor variable and any other predictor variables in the model.
# VIF between 1 and 5: There is moderate correlation between a given predictor variable and other predictor variables in the model.
# VIF > 5: There is severe correlation between a given predictor variable and other predictor variables in the model.

##### Output coefficient of lm table #####
coeffs_fit1 = getCoeffs(1, usefit = output.fit1) |> bind_rows(getCoeffs(2, usefit = output.fit1)) |> 
  bind_rows(getCoeffs(3, usefit = output.fit1)) |> bind_rows(getCoeffs(4, usefit = output.fit1)) 

coeffs_fit2 = getCoeffs(1, usefit = output.fit2) |> bind_rows(getCoeffs(2, usefit = output.fit2)) |> 
  bind_rows(getCoeffs(3, usefit = output.fit2)) |> bind_rows(getCoeffs(4, usefit = output.fit2))

coeffs_fit3 = getCoeffs(1, usefit = output.fit3) |> bind_rows(getCoeffs(2, usefit = output.fit3)) |> 
  bind_rows(getCoeffs(3, usefit = output.fit3)) |> bind_rows(getCoeffs(4, usefit = output.fit3)) 

coeffs_fit4 = getCoeffs(1, usefit = output.fit4) |> bind_rows(getCoeffs(2, usefit = output.fit4)) |> 
  bind_rows(getCoeffs(3, usefit = output.fit4)) |> bind_rows(getCoeffs(4, usefit = output.fit4)) 

coeffs_fit5 = getCoeffs(1, usefit = output.fit5) |> bind_rows(getCoeffs(2, usefit = output.fit5)) |> 
  bind_rows(getCoeffs(3, usefit = output.fit5)) |> bind_rows(getCoeffs(4, usefit = output.fit5))

coeffs_fit5.5 = getCoeffs(1, usefit = output.fit5.5) |> bind_rows(getCoeffs(2, usefit = output.fit5.5)) |> 
  bind_rows(getCoeffs(3, usefit = output.fit5.5)) |> bind_rows(getCoeffs(4, usefit = output.fit5.5))

latexTable <- function(coefffit, usecols = 8) {
  # Create the LaTeX table
  latex_table <- xtable(coefffit,math.style.exponents = TRUE, digits = 3,
                        caption = paste0('Linear model fit 1'), 
                          align = c("l", "l", "l", rep("r", usecols)))
  # Convert to LaTeX
  print(latex_table, 
        include.rownames = FALSE, 
        sanitize.text.function = identity, 
        latex.environments = "center")
}

# latexTable(coeffs_fit5)
latexTable(coeffs_fit5.5)
latexTable(coeffs_fit4, usecols = 6)
latexTable(coeffs_fit3, usecols = 6)
latexTable(coeffs_fit1)
latexTable(coeffs_fit2, usecols = 6)

################################ Variable-lag Granger Causality ################################
for (i in 1:4) {
  print(i)
  print(VLTimeCausality::VLGrangerFunc(Y = output.predict[[i]]$fit.temp, X = output.predict[[i]]$fit.ice, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = output.predict[[i]]$fit.temp, X = output.predict[[i]]$fit.LL, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = output.predict[[i]]$fit.temp, X = output.predict[[i]]$fit.vol, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = output.predict[[i]]$fit.temp, X = output.predict[[i]]$ice.diff, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = output.predict[[i]]$fit.temp, X = output.predict[[i]]$LL.diff, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = output.predict[[i]]$fit.temp, X = output.predict[[i]]$vol.diff, gamma = 0.5)$XgCsY)
}

for (i in 1:4) {
  print(i)
  print(VLTimeCausality::VLGrangerFunc(Y = output.predict[[i]]$fit.temp, X = output.predict[[i]]$fit.ice, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = output.predict[[i]]$fit.temp, X = lead(output.predict[[i]]$fit.ice), gamma = 0.5)$XgCsY)
}

for (i in 1:4) {
  print(i)
  print(VLTimeCausality::VLGrangerFunc(Y = output.predict[[i]]$temp.diff, X = output.predict[[i]]$fit.ice, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = output.predict[[i]]$temp.diff, X = lead(output.predict[[i]]$fit.ice), gamma = 0.5)$XgCsY)
}

#### Autocorrelation in timeseries ####
for (i in 1:4) {
  uselake = lakecolor$uselake[i]
  nrows = length(output.predict[[i]]$temp.diff)
  print(acf(output.predict[[i]]$temp.diff, main = uselake, ci = 0.99))
  # Compute confidence interval 
  # Use 95 or 99% confidence? 
  ci = qnorm((1 + 0.99)/2)/sqrt(nrows)
  
  sig = acf(output.predict[[i]]$temp.diff, plot = F)$acf[2] > ci
  print(paste0(uselake, ' acf temp.diff: ', sig))
 
}

for (i in 1:4) {
  uselake = lakecolor$uselake[i]
  nrows = length(output.predict[[i]]$ice.diff)
  print(acf(output.predict[[i]]$ice.diff, main = uselake, ci = 0.99))
  # Compute confidence interval 
  # Use 95 or 99% confidence? 
  ci = qnorm((1 + 0.99)/2)/sqrt(nrows)
  
  sig = acf(output.predict[[i]]$ice.diff, plot = F)$acf[2] > ci
  print(paste0(uselake, ' acf ice.diff: ', sig))
  # print(pacf(output.predict[[i]]$ice.diff, main = lakecolor$uselake[i]))
}

