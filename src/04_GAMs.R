
library(mgcv)
# install.packages("VLTimeCausality")
library(VLTimeCausality)
# Load necessary libraries
library(broom)
library(xtable)
library(corrr)
library(GGally)
library(olsrr) #collinearity 
library(Hmisc) #correlation matrix w/ Sig

usecolors =  c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a')
usecolors = c("#BB9F2F", "#94B9AF", "#942911", "#593837")

lakecolor = data.frame(uselake = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'), 
                       lakeAbbr = c('LF','LH','ELB','WLB'),
           plotColor = usecolors, 
           k = c(30,30,30,30))

output.plots = list(); output.fit1 = list(); output.fit2 = list(); output.fit3 = list(); output.fit3.5 = list()
output.fit4 = list(); output.fit5 = list(); output.fit5.5 = list(); output.fit6 = list()
output.predict = list(); output.predict.dec = list(); interp.out = list()

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
  
  ## Fit gam to mean temp data 
  mod.temp <- gam(tempUse ~ s(dec.date, k = usek, bs = 'tp', m = 2), 
                 data = heat.day |> filter(location_name == uselake))
  # mod.vol <- gam(vol ~ s(dec.date, k = usek, bs = 'tp'),
  #                 data = heat.day |> filter(location_name == uselake))
  
  # Fit GAM to ice
  ice.dec = ice |> mutate(dec.date = decimal_date(date_time)) |> 
    filter(yday(date_time) >= 244 & yday(date_time) <= 350) |> # Between Sep 1 and Dec 15 for all lakes 
    mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) 
  
  mod.ice = gam(z_water_m ~ s(dec.date, k = usek, bs = 'tp', m = 2), 
               data = ice.dec |> filter(location_name == uselake))
  
  # Fit GAM to lake level data 
  ll.dec = ll |> mutate(dec.date = decimal_date(date_time)) |>
    filter(yday(date_time) >= 244 & yday(date_time) <= 350) |> # Between Sep 1 and Dec 15 for all lakes 
    mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) 
  
  mod.LL = gam(masl ~ s(dec.date, k = 25, bs = 'tp', m = 2), 
      data = ll.dec |> filter(location_name == uselake))
  
  summary(mod.ice)
  summary(mod.temp)
  summary(mod.LL)
  
  ## create new data to predict at start of every month
  newYear = data.frame(date_time = seq.Date(from = as.Date('1995-10-01'), to = as.Date('2024-01-01'), by = 'month')) |> 
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
    bind_cols(predict(mod.LL, newYear, se.fit = TRUE)) |> rename(fit.LL = fit, se.fit.LL = se.fit) 
  
  ## Create the 95% confidence interval, get critical t-value 
  crit.t.ice <- qt(0.975, df = df.residual(mod.ice))
  crit.t.temp = qt(0.975, df = df.residual(mod.temp))
  crit.t.LL = qt(0.975, df = df.residual(mod.LL))
  
  # For a two-sided confidence interval, multiply the critical value by the sample's standard error of the mean
  newYear <- newYear |> mutate(upper.ice = fit.ice + (crit.t.ice * se.fit.ice),
                       lower.ice = fit.ice - (crit.t.ice * se.fit.ice)) |> 
    mutate(upper.temp = fit.temp + (crit.t.temp * se.fit.temp),
           lower.temp = fit.temp - (crit.t.temp * se.fit.temp)) |> 
    mutate(upper.LL = fit.LL + (crit.t.LL * se.fit.LL),
           lower.LL = fit.LL - (crit.t.LL * se.fit.LL)) 
  
  # Create difference columns
  newYear2 = newYear |>
    filter(month(date_time) == 12) |> 
    mutate(across(-date_time, ~ ifelse(year(date_time) == 2020, NA, .))) |> 
    mutate(fit.WL = fit.LL + fit.ice) |> 
    select(date_time, dec.date, fit.ice, fit.temp, fit.LL, fit.WL)
    
  output.predict[[i]] = newYear |> mutate(location_name = uselake)
  output.predict.dec[[i]] = newYear2 |> mutate(location_name = uselake) 

 # Take mean of sampled data for temp, ice, and lake level 
  fit.interp = 
    data.frame(year = 1995:2024, dec.date = 1995:2024 + 0.9) |>
    left_join(
    heat.day |> filter(location_name == uselake) |> 
    group_by(year = year(date_time)) |> 
    summarise(temp = mean(tempUse, na.rm = TRUE))
    ) |> 
    left_join(
    ice.dec |> filter(location_name == uselake) |> 
    group_by(year = year(date_time)) |> 
    summarise(iceZ = mean(z_water_m, na.rm = TRUE)) 
    ) |> 
    left_join(
      ll.dec |> filter(location_name == uselake) |> 
        group_by(year = year(date_time)) |> 
        summarise(LL = mean(masl, na.rm = TRUE))
    ) |> 
    
    arrange(year) |> 
    mutate(ice.diff = c(NA,diff(iceZ))) |> 
    mutate(temp.diff = c(NA,diff(temp))) |> 
    mutate(LL.diff = c(NA,diff(LL)))
  
  interp.out[[i]] = fit.interp |> mutate(location_name = uselake) 
  ########################################### FIT 1 #####################################################
  output.fit1[[i]] <- lm(temp.diff ~ iceZ + ice.diff + LL.diff, data = fit.interp)
  output.fit2[[i]] <- lm(temp.diff ~ LL.diff, data = fit.interp)
  output.fit3[[i]] <- lm(temp.diff ~ ice.diff, data = fit.interp)
  output.fit3.5[[i]] <- lm(temp.diff ~ iceZ, data = fit.interp)

  ####################################### FIT 4 #########################################################
  # Fit a linear model
  output.fit4[[i]] <- lm(temp ~ LL, data = fit.interp)
  output.fit5[[i]] <- lm(temp ~ iceZ, data = fit.interp)
  output.fit5.5[[i]] <- lm(temp ~ LL + iceZ, data = fit.interp)
  output.fit6[[i]] <- lm(temp ~ LL + iceZ + ice.diff + LL, data = fit.interp)
  
}


################ GAM PLOTS #################
full.predict = bind_rows(output.predict) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) 
full.predict.dec = bind_rows(output.predict.dec) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) 
full.interp = bind_rows(interp.out) |>
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) 
  

plotCustom = list(  scale_colour_grey(end = 0.5),
                    scale_fill_manual(values = usecolors),
                    theme_bw(base_size = 9),
                    facet_wrap(~location_name, scales = 'free_y', nrow = 1),
                    scale_x_continuous(breaks = c(2000,2010,2020)), 
                    theme(axis.title.x = element_blank(),
                          legend.position = 'none',
                          legend.title = element_blank(),
                          strip.background = element_rect(fill = "white", color = NA),  # Set background to white and remove border
                          strip.text = element_text(color = "black", face = "bold", size = 10))
                    )

p1 = ggplot(full.predict, aes(x = dec.date, y = fit.temp)) +
  geom_ribbon(aes(ymin = lower.temp, ymax = upper.temp, x = dec.date, fill = location_name), alpha = 0.5,
              inherit.aes = FALSE) +
  geom_point(data = heat.day, 
             mapping = aes(x = dec.date, y = tempUse, color = factor(month(date_time))),
             inherit.aes = FALSE, size = 0.4) +
  geom_point(data = full.interp, 
             mapping = aes(x = dec.date, y = temp, fill = location_name), 
             shape = 24, size = 1, stroke = 0.2) +
  geom_line(size = 0.3) +
  labs(y = "Mean Temp (°C)") +
  plotCustom; p1

p2 = ggplot(full.predict, aes(x = dec.date, y = fit.ice)) +
  geom_ribbon(aes(ymin = lower.ice, ymax = upper.ice, x = dec.date, fill = location_name), alpha = 0.5,
              inherit.aes = FALSE) +
  geom_point(data = ice.dec , 
             mapping = aes(x = dec.date, y = z_water_m, color = factor(month(date_time))),
             inherit.aes = FALSE, size = 0.4) +
  geom_point(data = full.interp, 
             mapping = aes(x = dec.date, y = iceZ, fill = location_name), 
             shape = 24, size = 1, stroke = 0.2) +
  geom_line(size = 0.3) +
  labs(y = "Ice Thickness (m)") +
  plotCustom + 
  theme(strip.background = element_blank(), strip.text.x = element_blank())

p3 = ggplot(full.predict, aes(x = dec.date, y = fit.LL)) +
  geom_ribbon(aes(ymin = lower.LL, ymax = upper.LL, x = dec.date, , fill = location_name), alpha = 0.5,
              inherit.aes = FALSE) +
  geom_point(data = ll.dec, 
             mapping = aes(x = dec.date, y = masl, color = factor(month(date_time))),
             inherit.aes = FALSE, size = 0.4) +
  geom_point(data = full.interp, 
             mapping = aes(x = dec.date, y = LL, fill = location_name), 
             shape = 24, size = 1, stroke = 0.2) +
  geom_line(size = 0.3) +
  geom_ribbon(aes(ymin = lower.LL + lower.ice, ymax = upper.LL + upper.ice, x = dec.date, fill = location_name), alpha = 0.5,
              inherit.aes = FALSE) +
  geom_line(aes(y = fit.LL + fit.ice)) +
  labs(y = "Lake & Water Level (m)")  +
  plotCustom +
  theme(strip.background = element_blank(), strip.text.x = element_blank())

# Plots of Diffs 
p5 = ggplot(full.interp) +
  geom_col(aes(x = dec.date, y = temp.diff, fill = location_name)) +
  ylab('Temp Diff (°C)') +
  ylim(-0.32,0.36) +
  plotCustom + 
  theme(strip.background = element_blank(), strip.text.x = element_blank())

p6 = ggplot(full.interp) +
  geom_col(aes(x = dec.date, y = ice.diff, fill = location_name)) +
  ylab('Ice Diff (°C)') +
  ylim(-0.72,1.5) +
  plotCustom + 
  theme(strip.background = element_blank(), strip.text.x = element_blank())

p7 = ggplot(full.interp) +
  geom_col(aes(x = dec.date, y = LL.diff, fill = location_name)) +
  ylab('LL Diff (°C)') +
  ylim(-0.25,0.82) +
  plotCustom + 
  theme(strip.background = element_blank(), strip.text.x = element_blank())

p1/p2/p3/p5/p6/p7 + 
  plot_layout(heights = c(1.5,1.5,1.5,1,1,1)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))
ggsave(paste0('figures/Fig4_GAMS.png'), width = 6.5, height = 7, dpi = 500)


################ Assess model fits #################
getCoeffs <- function(i, usefit) {
  AIC = broom::glance(usefit[[i]])$AIC
  BIC = broom::glance(usefit[[i]])$BIC
  
  # Extract dependent variable 
  depVar = names(model.frame(usefit[[i]]))[1]
  
  # Extract summary of the model
  summary_fit <- summary(usefit[[i]])
  
  # Extract adjusted R-squared
  adj_r_squared <- round(summary_fit$adj.r.squared,2)
  
  # Extract coefficients and p-values
  coeffs <- summary_fit$coefficients
  coeffs_df <- as.data.frame(coeffs) |> 
    select(Estimate, 4,) 
  
  coeffs_df = bind_cols(Lake = lakecolor$lakeAbbr[i], 
                        y = depVar, 
                        Parameter = rownames(coeffs_df), coeffs_df) |> 
    filter(Parameter != '(Intercept)')
  
  # Remove the row names
  rownames(coeffs_df) <- NULL
  # Add significance stars based on p-values
  coeffs_df$Signif <- cut(coeffs_df[, 5], 
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
coeffs_fit1 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output.fit1))
coeffs_fit2 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output.fit2))
coeffs_fit3 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output.fit3))
coeffs_fit3.5 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output.fit3.5))

coeffs_fit4 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output.fit4))
coeffs_fit5 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output.fit5))
coeffs_fit5.5 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output.fit5.5))
coeffs_fit6 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output.fit6))

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
latexTable(coeffs_fit1 |> bind_rows(coeffs_fit2) |> bind_rows(coeffs_fit3) |> bind_rows(coeffs_fit3.5) |>
  mutate(Lake = factor(Lake, levels = c('LF','LH','ELB','WLB'))) |> 
  arrange(Lake), usecols = 9)

latexTable(coeffs_fit5.5 |> bind_rows(coeffs_fit5) |> bind_rows(coeffs_fit4) |>
             mutate(Lake = factor(Lake, levels = c('LF','LH','ELB','WLB'))) |> 
             arrange(Lake), usecols = 9)

# latexTable(coeffs_fit1, usecols = 9)
# latexTable(coeffs_fit2, usecols = 7)
# latexTable(coeffs_fit3, usecols = 7)
# latexTable(coeffs_fit3.5, usecols = 7)
# 
# latexTable(coeffs_fit5.5, usecols = 9)
# latexTable(coeffs_fit4, usecols = 7)
# latexTable(coeffs_fit5, usecols = 7)

