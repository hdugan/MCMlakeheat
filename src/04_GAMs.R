
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
           plotColor = usecolors)

# Create empty output lists for linear model results
output_names <- c("temp.diff1", "temp.diff2", "temp.diff3", "temp.diff4", "temp.diff5", "temp.diff6", 
                  "temp1", "temp2", "temp3", "temp4", "temp5", "temp6", 
                  "flux1", "flux2", "flux3", "flux4", "flux5")
output <- setNames(vector("list", length(output_names)), paste0("reg.", output_names))

annual.list = list()

# Big old loop ####
for (i in 1:4) {
  # Set up 
  uselake = lakecolor$uselake[i]
  
  annual.list[[i]] = heat_flux |> filter(location_name == uselake) |> 
    mutate(ice.diff = ice.approx - lag(ice.approx), LL.diff = LL - lag(LL), year = year(chosen_date)) |> 
    select(year = year, temp = tempUse, flux = flux_W_m2, iceZ = ice.approx, LL,
           temp.diff = delta_temp, ice.diff, LL.diff)
  
  ########################################### Regression models for temp diff #####################################################
  output$reg.temp.diff1[[i]] <- lm(temp.diff ~ LL.diff, data = annual.list[[i]])
  output$reg.temp.diff2[[i]] <- lm(temp.diff ~ iceZ, data = annual.list[[i]])
  output$reg.temp.diff3[[i]] <- lm(temp.diff ~ ice.diff, data = annual.list[[i]])
  output$reg.temp.diff4[[i]] <- lm(temp.diff ~ iceZ + ice.diff + LL.diff, data = annual.list[[i]])

  ####################################### Regression models for temperature #########################################################
  output$reg.temp1[[i]] <- lm(temp ~ LL, data = annual.list[[i]])
  output$reg.temp2[[i]] <- lm(temp ~ iceZ, data = annual.list[[i]])
  output$reg.temp3[[i]] <- lm(temp ~ LL + iceZ, data = annual.list[[i]])
  
  ####################################### Regression models for heat flux #########################################################
  output$reg.flux1[[i]] <- lm(flux ~ LL.diff, data = annual.list[[i]])
  output$reg.flux2[[i]] <- lm(flux ~ iceZ, data = annual.list[[i]])
  output$reg.flux3[[i]] <- lm(flux ~ ice.diff, data = annual.list[[i]])
  output$reg.flux4[[i]] <- lm(flux ~ iceZ + ice.diff, data = annual.list[[i]])
}


################ GAM PLOTS #################
annual.df = bind_rows(annual.list) |> mutate(dec.date = year + 0.9) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) 

p1 = ggplot(heat.day) +
  geom_smooth(aes(x = dec.date, y = tempUse, fill = location_name, col = location_name), 
              method = "gam", formula = y ~ s(x, k = 15), linewidth = 0.2) +
  geom_point(data = heat.day, 
             mapping = aes(x = dec.date, y = tempUse, color = factor(month(date_time))),
             inherit.aes = FALSE, size = 0.4) +
  geom_point(data = annual.df, 
             mapping = aes(x = dec.date, y = temp, fill = location_name), 
             shape = 24, size = 1, stroke = 0.2) +
  labs(y = "Mean Temp (°C)") +
  plotCustom; p1

p2 = ggplot(ice.dec) +
  geom_smooth(aes(x = dec.date, y = z_water_m, fill = location_name, col = location_name), 
              method = "gam", formula = y ~ s(x, k = 15), linewidth = 0.2) +
  geom_point(data = ice.dec , 
             mapping = aes(x = dec.date, y = z_water_m, color = factor(month(date_time))),
             inherit.aes = FALSE, size = 0.4) +
  geom_point(data = annual.df, 
             mapping = aes(x = dec.date, y = iceZ, fill = location_name), 
             shape = 24, size = 1, stroke = 0.2) +
  labs(y = "Ice Thickness (m)") +
  plotCustom + 
  theme(strip.background = element_blank(), strip.text.x = element_blank())

p3 = ggplot(ll.dec) +
  geom_smooth(aes(x = dec.date, y = masl, fill = location_name, col = location_name), 
              method = "gam", formula = y ~ s(x, k = 15), linewidth = 0.2) +
  geom_point(data = ll.dec, 
             mapping = aes(x = dec.date, y = masl, color = factor(month(date_time))),
             inherit.aes = FALSE, size = 0.4) +
  geom_point(data = annual.df, 
             mapping = aes(x = dec.date, y = LL, fill = location_name), 
             shape = 24, size = 1, stroke = 0.2) +
  labs(y = "Lake Level (m)")  +
  plotCustom +
  theme(strip.background = element_blank(), strip.text.x = element_blank())

# Plots of Diffs 
p5 = ggplot(annual.df) +
  geom_col(aes(x = dec.date, y = flux, fill = location_name)) +
  # ylab('Heat Flux\n(W/m2)') +
  ylab('Heat Flux\n(W m^<sup>-2</sup>)') +
  ylim(-1.2,0.75) +
  plotCustom + 
  theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.title.y = element_markdown())

p6 = ggplot(annual.df) +
  geom_col(aes(x = dec.date, y = ice.diff, fill = location_name)) +
  ylab('∆ Ice (m)') +
  ylim(-1.5,0.75) +
  plotCustom + 
  theme(strip.background = element_blank(), strip.text.x = element_blank())

p7 = ggplot(annual.df) +
  geom_col(aes(x = dec.date, y = LL.diff, fill = location_name)) +
  ylab('∆ LL (m)') +
  ylim(-0.25,0.83) +
  plotCustom + 
  theme(strip.background = element_blank(), strip.text.x = element_blank())

p1/p2/p3/p5/p6/p7 + 
  plot_layout(heights = c(1.5,1.5,1.5,1.5,1,1)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))
ggsave(paste0('figures/Fig5_GAMS.png'), width = 6.5, height = 7, dpi = 500)

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
      data.frame(Lake = lakecolor$lakeAbbr[i]) |> 
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
# coeffs_fit1 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output.fit1))
# coeffs_fit2 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output.fit2))
# coeffs_fit3 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output.fit3))
# coeffs_fit3.5 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output.fit3.5))

coeffs_temp.diff1 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output$reg.temp.diff1))
coeffs_temp.diff2 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output$reg.temp.diff2))
coeffs_temp.diff3 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output$reg.temp.diff3))
coeffs_temp.diff4 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output$reg.temp.diff4))

coeffs_temp1 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output$reg.temp1))
coeffs_temp2 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output$reg.temp2))
coeffs_temp3 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output$reg.temp3))

coeffs_flux1 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output$reg.flux1))
coeffs_flux2 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output$reg.flux2))
coeffs_flux3 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output$reg.flux3))
coeffs_flux4 = bind_rows(lapply(X = 1:4, getCoeffs, usefit = output$reg.flux4))


latexTable <- function(coefffit, usecols = 8) {
  # Create the LaTeX table
  latex_table <- xtable(coefffit,math.style.exponents = TRUE, digits = 3,
                        caption = paste0('Linear model fit 1'), 
                        align = c("l", "l", "l", rep("r", usecols)))
  # Convert to LaTeX
  print(latex_table, 
        hline.after = c(-1, 0, 1:nrow(coefffit)),
        include.rownames = FALSE, 
        sanitize.text.function = identity, 
        latex.environments = "center")
}


# Supplemental Table for Manuscript 
latexTable(coeffs_flux1 |> bind_rows(coeffs_flux2) |> bind_rows(coeffs_flux3) |>
             bind_rows(coeffs_flux4) |> 
             mutate(Lake = factor(Lake, levels = c('LF','LH','ELB','WLB'))) |> 
             arrange(Lake), usecols = 9)

# Supplemental Table for Manuscript 
latexTable(coeffs_temp1 |> bind_rows(coeffs_temp2) |> bind_rows(coeffs_temp3) |>
             mutate(Lake = factor(Lake, levels = c('LF','LH','ELB','WLB'))) |> 
             arrange(Lake), usecols = 9)

# Table for Main Manuscript 
#### r values for model correlation using "pearson" method ####
cor4 = round(cor(annual.list[[1]]$temp, annual.list[[1]]$LL, use = 'pairwise.complete.obs'),3)
cor5 = round(cor(annual.list[[1]]$temp, annual.list[[1]]$iceZ, use = 'pairwise.complete.obs'),3)
cor6 = round(cor(annual.list[[2]]$temp, annual.list[[2]]$iceZ, use = 'pairwise.complete.obs'),3)
cor7 = round(cor(annual.list[[3]]$temp, annual.list[[3]]$LL, use = 'pairwise.complete.obs'),3)
cor8 = round(cor(annual.list[[4]]$temp, annual.list[[4]]$LL, use = 'pairwise.complete.obs'),3)
round(cor(annual.list[[4]]$temp, annual.list[[4]]$iceZ, use = 'pairwise.complete.obs'),3)

latexTable(coeffs_fit5.5 |> filter(Lake == 'LF') |> 
             select(-Tolerance, -VIF) |> 
             mutate(r = c(cor4, cor5)) |> 
             bind_rows(coeffs_fit5 |> 
                         filter(Lake == 'LH') |> 
                         mutate(r = cor6)) |> 
             bind_rows(coeffs_fit4 |> 
                         filter(Lake %in% c('ELB','WLB')) |> 
                         mutate(r = c(cor7, cor8))) |>
             mutate(Lake = factor(Lake, levels = c('LF','LH','ELB','WLB'))) |> 
             arrange(Lake), usecols = 8)

# Manuscript Table for Flux 
corf1 = round(cor(annual.list[[1]]$flux, annual.list[[1]]$ice.diff, use = 'pairwise.complete.obs'),3)
corf2 = round(cor(annual.list[[2]]$flux, annual.list[[2]]$ice.diff, use = 'pairwise.complete.obs'),3)
corf3 = round(cor(annual.list[[3]]$flux, annual.list[[3]]$iceZ, use = 'pairwise.complete.obs'),3)
corf4 = round(cor(annual.list[[3]]$flux, annual.list[[3]]$ice.diff, use = 'pairwise.complete.obs'),3)
corf5 = round(cor(annual.list[[4]]$flux, annual.list[[4]]$iceZ, use = 'pairwise.complete.obs'),3)
# corf6 = round(cor(annual.list[[4]]$flux, annual.list[[4]]$ice.diff, use = 'pairwise.complete.obs'),3)


latexTable(coeffs_flux3 |> 
                         filter(Lake == 'LF') |> 
                         mutate(r = corf1) |> 
             bind_rows(coeffs_flux3 |> 
                         filter(Lake == 'LH') |> 
                         mutate(r = corf2)) |> 
             bind_rows(coeffs_flux4 |> 
                         select(-Tolerance, -VIF) |>
                         filter(Lake %in% c('ELB')) |> 
                         mutate(r = c(corf3, corf4))) |>
             bind_rows(coeffs_flux2 |> 
                         filter(Lake == 'WLB') |> 
                         mutate(r = corf5)) |> 
             mutate(Lake = factor(Lake, levels = c('LF','LH','ELB','WLB'))) |> 
             arrange(Lake), usecols = 8)

