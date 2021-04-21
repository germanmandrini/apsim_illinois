# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
# grid10_soils_sf1 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf1.rds")


#-------------------------------------------------------------------------------------------------
#CONVERT TO DT
grid10_soils_dt <- data.table(grid10_soils_sf2)
nrow(grid10_soils_dt)

#Check areas
area <- grid10_soils_dt[,.(area_ha = sum(area_ha)), by = .(id_tile, id_10, id_field)]
summary(area$area_ha)

#Summarize by mukey in each field. We don't care the polygons, mukeys are the ones adding simulations
grid10_soils_dt <- grid10_soils_dt[, .(area_ha = sum(area_ha)) , by = .(id_tile, id_10, id_field, mukey, state_name,
                                                     county_name, corn_avg_ha, corn5_tile, corn5_cell, region)]

grid10_soils_dt[, field_area := sum(area_ha), by = .(id_10, id_field)]
summary(grid10_soils_dt$field_area)
grid10_soils_dt[, prop_area := area_ha/field_area]

#Remove mukeys < 5%
grid10_soils_dt2 <- grid10_soils_dt[prop_area > 0.05]

#Keep the 3 most important mukeys
grid10_soils_dt2 <- grid10_soils_dt2[order(id_tile, id_10, id_field, -prop_area)]
grid10_soils_dt2[, mukey_rank := seq_along(prop_area), by = .(id_tile, id_10, id_field)]
grid10_soils_dt3 <- grid10_soils_dt2[mukey_rank <4]

#Recalculate areas
grid10_soils_dt3[, field_area := sum(area_ha), by = .(id_10, id_field)]
grid10_soils_dt3[, prop_area := area_ha/field_area]
grid10_soils_dt3[,area_ha := 40 * prop_area]
table(grid10_soils_dt3[,.N, by = .(id_tile, id_10, id_field)]$N)
saveRDS(grid10_soils_dt3, "./vr_value_v2/Data/Grid/grid10_soils_dt3.rds")

