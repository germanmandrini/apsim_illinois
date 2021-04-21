#--------------------------
# Corn and N price
#--------------------------
# Pc = 0.14#3.5/25.4 #$/bu to $/kg https://markets.businessinsider.com/commodities/corn-price
# Pn = 5*Pc


Pc = 0.158 #equivalent to 4.00 $\bu
# Pn = 0.88 #equivalent to 0.40 $/lbs N
Pn = 5*Pc #round the ratio

# https://farmdocdaily.illinois.edu/2018/09/fertilizer-prices-higher-for-2019-crop.html
# https://www.ams.usda.gov/mnreports/gx_gr210.txt

print(paste('Pn/Pc', Pn/Pc))


Yld_response_threshold <- 200 

low_var <- c("rain_1", "rain_2", 
             "tmean_1", "tmean_2", 
             "rad_1", "rad_2", "surfaceom_wt_v5", 'Y_corn_lt_avg', "day_sow", "day_v5", "lai_v5")#'Y_corn_lt_min', 'Y_corn_lt_max', 

high_var <- c("whc",  "oc_20cm_v5", "sw_dep_v5", "n_0_60cm_v5",  
              "sand_40cm", "clay_40cm") #"root_wt_v5",, "n_deep_v5", "esw_pct_v5", 

future_var <- c("rain_3", "rain_4", "rain_5", 
                "tmean_3", "tmean_4", "tmean_5", 
                "rad_3", "rad_4", "rad_5")

NRT_paper <- c('ex_post',"mrtn_peak", "mrtn_high","eonr_mean", "rf_full")
policies_paper <-  c('ratio', 'leach', 'bal', 'red')
