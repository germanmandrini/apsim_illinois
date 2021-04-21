rm(list=ls())
# 
# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'#Dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
codes_folder <-'C:/Users/germanm2/Documents'#CPSC
# setwd('~')#Server
# codes_folder <-'~' #Server

source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
library(randomForest)
library(reticulate)

# use_condaenv('GEOANN', conda = '/opt/anaconda3/condabin/conda')
# source_python("./n_policy_git/Codes/3c_cnn_functions_sep10.py")

# yc_yearly_dt3 <- readRDS("./n_policy_box/Data/files_rds/yc_yearly_dt3.rds")
# grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()

yc_field_dt <- readRDS("./n_policy_box/Data/files_rds/yc_field_dt.rds")
thresholds_dt <- readRDS( "./n_policy_box/Data/files_rds/thresholds_dt.rds") #this is tricky. Are the mean values after running this script and using the ratio_5 policy for all the z. I do it this way to use always the same


z_n = 10
for(z_n in 1:30){
  print(z_n)
  results_list <- list()
  
  # =========================================================================================================================================================
  # CREATE THE N RATIO TAX MODEL
  
  #Load datasets again
  training_set_dt <- yc_field_dt[station == 1 & z != z_n]
  evaluation_set_dt <- yc_field_dt[station != 1 & z == z_n]
  prediction_set_aggregated_dt <- evaluation_set_dt[N_fert == 180][,-c('N_fert')] #one line per field, not yield curve
  
  ratio_seq <- sort(unique(c(seq(5,20,2), seq(12.5,13.5,0.2))))
  level_n <- 12.9#Pn/Pc
  for(level_n in ratio_seq){
    
    Pn_tmp = level_n * Pc
    print(Pn_tmp/Pc)
    
    training_set_dt[, P := Y_corn * Pc - N_fert * Pn_tmp]  #update profits
    
    # =========================================================================================================================================================
    # CREATE THE RF-HIGH
    training_eonr_dt  <- training_set_dt[, .SD[ P == max(P)], by = .(id_10, id_field, z)] %>%
      .[, .SD[ N_fert == min( N_fert)], by = .(id_10, id_field, z)] %>%
      .[,c('N_fert', low_var, high_var, 'year'), with = FALSE]
    
    setnames(training_eonr_dt, 'N_fert', 'eonr')
    
    # RF Model 2------------------------
    # mtry <- tuneRF(training_eonr_dt2[,c(pred_vars), with = FALSE],training_eonr_dt2$eonr, mtryStart = 6, ntreeTry=1000,
    #                 stepFactor=1.1,improve=0.01, trace=TRUE, plot=TRUE) # ,mtryStart = 5
    # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
    
    best.m = 6
    
    dynamic <- randomForest(formula = as.formula(paste('eonr ~ ', paste(c(low_var, high_var), collapse = ' + '))), 
                           data = training_eonr_dt[,c('eonr', low_var, high_var, 'year'), with = FALSE],
                           strata = year, #I think it will bootstrap by year
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
    
    varImpPlot(dynamic, type=2)
    plot(dynamic)
    
    if(level_n == Pn/Pc & z_n == 10){
      pdf("./n_policy_box/Data/figures/VarImportancePlot_ratio.pdf")
      varImpPlot(dynamic, type=2, main = '')
      dev.off() 
    }
    
   
    #===================================================================================================================
    # EVALUATION
    
    #Prepare the data
    evaluation_set_dt[, P := Y_corn * Pc - N_fert * Pn_tmp]  #update profits
    evaluation_set_dt[, G := N_fert * (Pn_tmp - Pn)] #gov collection
    
    #===================================================================================================================
    # 2) dynamic
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(dynamic, prediction_set_aggregated_dt)/10,0)*10]
    
    evaluation_set_tmp <- merge(evaluation_set_dt,
                             prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "region_eq", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRT := 'dynamic'][,policy := paste0('ratio_', level_n)]
    
    if(level_n == Pn/Pc){
      setnames(prediction_set_aggregated_dt, 'eonr_pred', 'eonr_ratio5')
      saveRDS(prediction_set_aggregated_dt, paste0("./n_policy_box/Data/files_rds/field_perfomances_tmp_rodrigo/ratio5_recommendations_",z_n, ".rds"))
    }
    
  }#end of ratio loop
  
  
 
  # =========================================================================================================================================================
  # CREATE THE LEACHING FEE MODEL
  source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
  
  #Load datasets again
  training_set_dt <- yc_field_dt[station == 1 & z != z_n]
  evaluation_set_dt <- yc_field_dt[station != 1 & z == z_n]
  prediction_set_aggregated_dt <- evaluation_set_dt[N_fert == 180][,-c('N_fert')] #one line per field, not yield curve
  
  #-------------------------------------
  #Update threholds
  # leach_threshold <- yc_field_dt[station == 1 & N_fert == 100] %>%
  #   .[, .(L_thr = quantile(L, probs = 0.5)), region] %>%
  #   .[order(region)] # not change with z_n
  thresholds_dt[,L_thr := round(L * 0.6, 0)]
  
  training_set_dt <- merge(training_set_dt, thresholds_dt[,.(region, L_thr)], by = 'region')
  
  training_set_dt[,L_extra := L - L_thr]
  training_set_dt[L_extra <= 0, L_extra := 0]
  training_set_dt$L_extra %>% summary()
  
  evaluation_set_dt <- merge(evaluation_set_dt, thresholds_dt[,.(region, L_thr)], by = 'region')
  
  evaluation_set_dt[,L_extra := L - L_thr]
  evaluation_set_dt[L_extra <= 0, L_extra := 0]
  #-------------------------------------
  
  leach_seq <-  sort(unique(c(seq(0,40,2), seq(13,14,0.2))))
  
  set.seed(123)
  
  level_n = 13.2
  for(level_n in leach_seq){
    print(level_n)
    
    # training_set_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
    
    training_set_dt[, P := Y_corn * Pc - N_fert * Pn - L_extra * level_n] #update profits
    
    training_set_dt[, P1 := Y_corn * Pc - N_fert * Pn]
    training_set_dt[, P2 := Y_corn * Pc - N_fert * Pn - L_extra * level_n] #update profits
    
    plot_dt <- training_set_dt[, .(P1 = mean(P1), 
                             P2 = mean(P2), 
                             L_extra = round(mean(L_extra),0)), by = .(region, N_fert)][order(region, N_fert)]
    
    ggplot() + 
      geom_line(data = plot_dt, aes(x = N_fert, y = P1, color = region))+
      geom_point(data = plot_dt[,.SD[P1 == max(P1)], by = region], aes(x = N_fert, y = P1))+
      geom_line(data = plot_dt, aes(x = N_fert, y = P2, color = region), linetype = 'dashed')+
      geom_point(data = plot_dt[,.SD[P2 == max(P2)], by = region], aes(x = N_fert, y = P2))
    
    
    # =========================================================================================================================================================
    # CREATE THE RF-HIGH
    training_eonr_dt  <- training_set_dt[, .SD[ P == max(P)], by = .(id_10, id_field, z)] %>%
      .[, .SD[ N_fert == min( N_fert)], by = .(id_10, id_field, z)] %>%
      .[,c('N_fert', low_var, high_var, 'year'), with = FALSE]
    
    setnames(training_eonr_dt, 'N_fert', 'eonr')
    
    # RF Model 2------------------------
    # mtry <- tuneRF(training_eonr_dt2[,c(pred_vars), with = FALSE],training_eonr_dt2$eonr, mtryStart = 6, ntreeTry=1000,
    #                 stepFactor=1.1,improve=0.01, trace=TRUE, plot=TRUE) # ,mtryStart = 5
    # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
    
    best.m = 6
    
    dynamic <- randomForest(formula = as.formula(paste('eonr ~ ', paste(c(low_var, high_var), collapse = ' + '))), 
                           data = training_eonr_dt[,c('eonr', low_var, high_var, 'year'), with = FALSE],
                           strata = year, #I think it will bootstrap by year
                           importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
    
    varImpPlot(dynamic, type=2)
    plot(dynamic)
    
    if(level_n == 5 & z_n == 10){
      pdf("./n_policy_box/Data/figures/VarImportancePlot_leach.pdf")
      varImpPlot(dynamic, type=2, main = '')
      dev.off() 
    }
    
    #===================================================================================================================
    # EVALUATION
    
    #Prepare the data
    evaluation_set_dt[, P := Y_corn * Pc - N_fert * Pn - L_extra * level_n] #update profits
    evaluation_set_dt[, G := L_extra * level_n] #gov collectionn
    #===================================================================================================================
    # # 1) STATIC MRTN
    # 
    # # the NRT is trained with z1-10 and testing is evaluated with z11-25
    # evaluation_set_tmp <- merge(evaluation_set_dt, static_mrtn_dt, 
    #                          by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    #   .[N_fert == eonr_pred] %>%
    #   .[,c("region", "region_eq", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    # 
    # 
    # results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRT := 'static'][,policy := paste0('leach_', level_n)]
    #===================================================================================================================
    # 2) dynamic
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[,eonr_pred := round(predict(dynamic, prediction_set_aggregated_dt)/10,0)*10]
    
    evaluation_set_tmp <- merge(evaluation_set_dt,
                             prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "region_eq", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRT := 'dynamic'][,policy := paste0('leach_', level_n)]
    
    
    #===================================================================================================================
  } #end of leaching loop
    
    # =========================================================================================================================================================
    # CREATE THE BALANCE FEE MODEL
    source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
    
    #Load datasets again
    training_set_dt <- yc_field_dt[station == 1 & z != z_n]
    evaluation_set_dt <- yc_field_dt[station != 1 & z == z_n]
    prediction_set_aggregated_dt <- evaluation_set_dt[N_fert == 180][,-c('N_fert')] #one line per field, not yield curve
    
    #Update threholds
  
    training_set_dt[,N_balance := N_fert - Y_corn * 11.5/1000]
    
    # bal_threshold <- yc_field_dt[station == 1 & N_fert == 100] %>%
    #   .[,N_balance := N_fert - Y_corn * 11.5/1000] %>%
    #   .[, .(N_balance_thr = quantile(N_balance, probs = 0.5)), region] %>%
    #   .[order(region)] # not change with z_n
    
    thresholds_dt[, N_balance_thr := round(N_balance-60,  0)] 
    
    training_set_dt <- merge(training_set_dt[,-'N_balance_thr'], thresholds_dt[,.(region, N_balance_thr)], by = 'region')
    
    training_set_dt[,N_extra := N_balance - N_balance_thr]
    training_set_dt[N_extra <= 0, N_extra := 0]
    
    #evaluation set
    evaluation_set_dt[,N_balance := N_fert - Y_corn * 11.5/1000]
    evaluation_set_dt <- merge(evaluation_set_dt[,-'N_balance_thr'], thresholds_dt[,.(region, N_balance_thr)], by = 'region')
  
    evaluation_set_dt[,N_extra := N_balance - N_balance_thr]
    evaluation_set_dt[N_extra <= 0, N_extra := 0]
    
    
    bal_seq <- sort(unique(c(seq(0,1,0.25), seq(1,5,1), seq(1.5,2,0.1))))
    set.seed(123)
    level_n = 1.8
    for(level_n in bal_seq){
      # 
      print(level_n)
      
      # training_set_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
      training_set_dt[, P := Y_corn * Pc - N_fert * Pn - N_extra * level_n]#update profits
      training_set_dt[, P1 := Y_corn * Pc - N_fert * Pn]
      training_set_dt[, P2 := Y_corn * Pc - N_fert * Pn - N_extra * level_n]
      
      plot_dt <- training_set_dt[, .(P1 = mean(P1), 
                               P2 = mean(P2), 
                               N_extra = round(mean(N_extra),0)), by = .(region, N_fert)][order(region, N_fert)]
      ggplot() + 
        geom_line(data = plot_dt, aes(x = N_fert, y = P1, color = region))+
        geom_point(data = plot_dt[,.SD[P1 == max(P1)], by = region], aes(x = N_fert, y = P1))+
        geom_line(data = plot_dt, aes(x = N_fert, y = P2, color = region), linetype = 'dashed')+
        geom_point(data = plot_dt[,.SD[P2 == max(P2)], by = region], aes(x = N_fert, y = P2))
      
      
      # =========================================================================================================================================================
      # CREATE THE RF-HIGH
      training_eonr_dt  <- training_set_dt[, .SD[ P == max(P)], by = .(id_10, id_field, z)] %>%
        .[, .SD[ N_fert == min( N_fert)], by = .(id_10, id_field, z)] %>%
        .[,c('N_fert', low_var, high_var, 'year'), with = FALSE]
      
      setnames(training_eonr_dt, 'N_fert', 'eonr')
      
      # RF Model 2------------------------
      # mtry <- tuneRF(training_eonr_dt2[,c(pred_vars), with = FALSE],training_eonr_dt2$eonr, mtryStart = 6, ntreeTry=1000,
      #                 stepFactor=1.1,improve=0.01, trace=TRUE, plot=TRUE) # ,mtryStart = 5
      # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
      
      best.m = 6
      
      dynamic <- randomForest(formula = as.formula(paste('eonr ~ ', paste(c(low_var, high_var), collapse = ' + '))), 
                             data = training_eonr_dt[,c('eonr', low_var, high_var, 'year'), with = FALSE],
                             strata = year, #I think it will bootstrap by year
                             importance = TRUE , mtry = best.m, ntree=2000, nodesize = 30)
      
      varImpPlot(dynamic, type=2)
      plot(dynamic)
      
      if(level_n == 2 & z_n == 10){
        pdf("./n_policy_box/Data/figures/VarImportancePlot_bal.pdf")
        varImpPlot(dynamic, type=2, main = '')
        dev.off() 
      }
      
       #===================================================================================================================
      # EVALUATION
      
      #Prepare the data
      evaluation_set_dt[, P := Y_corn * Pc - N_fert * Pn - N_extra * level_n]#update profits
      evaluation_set_dt[, G := N_extra * level_n] #gov collection
      
      #===================================================================================================================
     
      # 2) dynamic
      # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
      
      prediction_set_aggregated_dt[,eonr_pred := round(predict(dynamic, prediction_set_aggregated_dt)/10,0)*10]
      
      evaluation_set_tmp <- merge(evaluation_set_dt,
                                  prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
        .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
        .[N_fert == eonr_pred] %>%
        .[,c("region", "region_eq", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
      
      evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
      
      results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRT := 'dynamic'][,policy := paste0('bal_', level_n)]
      
      
      #===================================================================================================================
    } #end of balance loop
  

  # =========================================================================================================================================================
  # VOLUNTARY REDUCTION (REDUCE RATIO 5 RECOMENDATIONS)
  
  source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))
  
  #Load datasets again
  training_set_dt <- yc_field_dt[station == 1 & z != z_n]
  evaluation_set_dt <- yc_field_dt[station != 1 & z == z_n]
  prediction_set_aggregated_dt <- readRDS(paste0("./n_policy_box/Data/files_rds/field_perfomances_tmp_rodrigo/ratio5_recommendations_",
                                                 z_n, ".rds")) #we lower the ratio5 dynamic recommendation
  
  red_seq <- sort(unique(c(seq(0,30,2), seq(18.5,19.5,0.1))))
  level_n = 10
  for(level_n in red_seq){
    print(level_n) 
    #===================================================================================================================
    # EVALUATION
    
    #Prepare the data
    evaluation_set_dt[, P := Y_corn * Pc - N_fert * Pn]  #update profits
    evaluation_set_dt[, G := 0] #gov collection
    # #===================================================================================================================
    # # 1) STATIC MRTN
    # static_mrtn_dt <- copy(ratio5_recommendations_list$static_mrtn)
    # static_mrtn_dt[, eonr_pred := round(eonr_pred*((100-level_n)/100)/10)*10]
    # 
    # # the NRT is trained with z1-10 and testing is evaluated with z11-25
    # evaluation_set_tmp <- merge(evaluation_set_dt, static_mrtn_dt, 
    #                             by = c('region')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
    #   .[N_fert == eonr_pred] %>%
    #   .[,c("region", "region_eq", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    # 
    # results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRT := 'static'][,policy := paste0('red_', level_n)]
    #===================================================================================================================
    # 2) dynamic
    # GET THE RECOMMENDATION FOR THE Z11-30 FOR EACH id_field
    
    prediction_set_aggregated_dt[, eonr_pred := round(eonr_ratio5*((100-level_n)/100)/10)*10]
    
    evaluation_set_tmp <- merge(evaluation_set_dt,
                                prediction_set_aggregated_dt[,.(id_10, id_field, z, eonr_pred)], by = c('id_10', 'id_field','z')) %>% #here we joing back the predictions with the data with all the N rates and then filter the N rate = to the predicted to measure testing
      .[,eonr_pred := ifelse(eonr_pred <0, 0, ifelse(eonr_pred > 320, 320, eonr_pred))] %>%
      .[N_fert == eonr_pred] %>%
      .[,c("region", "region_eq", "id_10", 'id_field', "z", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert",'P', "G")]
    
    evaluation_set_tmp[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z)]$area_ha %>% table()
    
    results_list[[length(results_list)+1]] <- evaluation_set_tmp[,NRT := 'dynamic'][,policy := paste0('red_', level_n)]
    
    
    #===================================================================================================================
    
  }#end of reduction loop
  perfomances_z_tmp <- rbindlist(results_list)
  
  saveRDS(perfomances_z_tmp, paste0("./n_policy_box/Data/files_rds/field_perfomances_tmp_rodrigo/field_performances_", z_n,'.rds'))

}#end of z_n

# load all the results
perfomances_list <- list()
files_path <- list.files("./n_policy_box/Data/files_rds/field_performances_tmp_ntree1000", full.names = TRUE, pattern = 'field_performances_[1-9]')

for(file_n in files_path){
  # file_n = files_path[1]
  perfomances_list[[length(perfomances_list)+1]] <- readRDS(file_n)

}
field_perfomances_dt <- rbindlist(perfomances_list)


field_perfomances_dt[,.N, z][order(z)]
field_perfomances_dt[,.N, .(id_10, id_field)] %>% .[,.N, .(id_10)] %>% .[,N] %>% table() #number of fields by cell
field_perfomances_dt[,.N, .(id_10, id_field, policy, NRT, z)] %>% .[,.N, .(policy)] #field x year by NRT
4031*15

saveRDS(field_perfomances_dt, "./n_policy_box/Data/files_rds/field_perfomances_dt.rds")
#--------------------------------------------------------------------------------

field_perfomances_dt <- readRDS("./n_policy_box/Data/files_rds/field_perfomances_dt.rds")

thresholds_dt <- field_perfomances_dt[policy == 'ratio_5' & NRT == 'dynamic']

thresholds_dt[,N_balance := N_fert - Y_corn * 11.5/1000]


thresholds_dt <- thresholds_dt[,
                           .(N_balance = mean(N_balance),
                             Y_corn = mean(Y_corn),
                             N_fert = mean(N_fert),
                             L = mean(L)), by = region]
# thresholds_dt[,L_thr := round(L * 0.6, 0)]
# thresholds_dt[, N_balance_thr := round(N_balance-60,  0)] 
# saveRDS(thresholds_dt, "./n_policy_box/Data/files_rds/thresholds_dt.rds")

thresholds_dt2 <- readRDS("./n_policy_box/Data/files_rds/thresholds_dt.rds")


