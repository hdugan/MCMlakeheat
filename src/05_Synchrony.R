library(synchrony)

getSynchrony <- function(sync1) {
  col1 = c(1,1,2)
  col2 = c(2,3,3)
  
  # Pair wise synchrony for temperature 
  sync1.out = data.frame(pair = c('LF-ELB','LF-WLB','ELB-WLB'), 
                         meanCorr = NA, meanCorr.p = NA,
                         Concur = NA, Concur.p = NA,
                         Phase = NA, Phase.p = NA)
  
  for (i in 1:3) {
    ts1 = sync1[,col1[i]]
    ts2 = sync1[,col2[i]]
    
    ts1.na = !is.na(ts1)
    ts2.na = !is.na(ts2)
    na.inx = ts1.na & ts2.na
    
    ts1 = ts1[na.inx]
    ts2 = ts2[na.inx]
    
    #Correlation
    sync1.out$meanCorr[i] = rcorr(ts1, ts2)$r[1,2]
    sync1.out$meanCorr.p[i] = round(rcorr(ts1, ts2)$P[1,2],2)
    
    # (implemented in function peaks), which simply measures the proportion of concurrent peaks (local maxima) and
    # troughs (local minima) between pairs of time series (Buonaccorsi et al. 2001)
    # This metric varies between 0 when the time series never peak and trough together, and 1 when the time
    # series always peak and trough simultaneously
    concurence = peaks(ts1, ts2, nrands = 99)
    sync1.out$Concur[i] = concurence$obs
    sync1.out$Concur.p[i] = concurence$pval
    
    # phase.sync, measures phase synchrony between quasiperiodic times series
    # The strength of phase synchrony can be quantified by a Q index that falls between 0 (no phase synchrony) 
    # and 1 (full phase synchrony) 
    # Function to safely handle phase sync in cases where it fails 
    safe_phase.sync <- function(ts1, ts2) {
      tryCatch({
        phase.sync(ts1, ts2, mins = FALSE, nrands = 99)
      }, error = function(e) {
        if (grepl("need at least two non-NA values to interpolate", e$message)) {
          # return(NA) # Return NA for specific error
          return(data.frame(Q.obs = NA, pval = NA))
        } else {
          stop(e) # Rethrow other errors
        }
      })
    }
    phase = safe_phase.sync(ts1,ts2)
    # phase = phase.sync(ts1, ts2, mins = TRUE, nrands = 99)
    sync1.out$Phase[i] = phase$Q.obs
    sync1.out$Phase.p[i] = phase$pval
  }
  return(sync1.out)
}

############ Synchrony for Heat Flux ###########
sync0 = annual.df |> select(year, location_name, flux) |> 
  pivot_wider(names_from = location_name, values_from = flux) |> 
  arrange(year) |> 
  filter(year >= 1996) |> 
  select(-year) |> 
  rename(LF_temp = 1, LH_temp = 2, ELB_temp = 3, WLB_temp = 4) |> 
  select(-LH_temp)

options(digits=2)
correlation_table <- corrr::correlate(sync0, method = "pearson")
cor_2 <- rcorr(as.matrix(sync0[,-1]), type = 'pearson')

## Compute the concordance (and its statistical significance) between multiple variable
kendall.w(data = sync0, nrands = 999)
# kendall.w(data = sync1[-3], nrands = 999)
sync0.out = getSynchrony(sync0)
latexTable(sync0.out, usecols = 5)

############ Synchrony for Temperature ###########
sync1 = annual.df |> select(year, location_name, temp) |> 
  pivot_wider(names_from = location_name, values_from = temp) |> 
  arrange(year) |> 
  filter(year >= 1996) |> 
  select(-year) |> 
  rename(LF_temp = 1, LH_temp = 2, ELB_temp = 3, WLB_temp = 4) |> 
  select(-LH_temp)

options(digits=2)
correlation_table <- corrr::correlate(sync1, method = "pearson")
cor_2 <- rcorr(as.matrix(sync1[,-1]), type = 'pearson')

## Compute the concordance (and its statistical significance) between multiple variable
kendall.w(data = sync1, nrands = 999)
# kendall.w(data = sync1[-3], nrands = 999)
sync1.out = getSynchrony(sync1)
latexTable(sync1.out, usecols = 5)

############ Synchrony for Ice Thickness ###########
sync2 = annual.df |> select(year, location_name, iceZ) |> 
  pivot_wider(names_from = location_name, values_from = iceZ) |> 
  arrange(year) |> 
  filter(year >= 1996) |> 
  select(-year) |> 
  rename(LF_temp = 1, LH_temp = 2, ELB_temp = 3, WLB_temp = 4) |> 
  select(-LH_temp)

options(digits=2)
correlation_table <- corrr::correlate(sync2, method = "pearson")
cor_2 <- rcorr(as.matrix(sync2), type = 'pearson')

## Compute the concordance (and its statistical significance) between multiple variable
kendall.w(data = sync2, nrands = 999)
# kendall.w(data = sync1[-3], nrands = 999)
sync2.out = getSynchrony(sync2)
latexTable(sync2.out, usecols = 5)
