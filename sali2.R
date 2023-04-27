############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
# load libraries
library(dplyr); library(tidyr); library(lubridate); library(reshape); library(plyr); library(mpspline2); library(DescTools)

attributes <- c('cat_ca', 'cat_cec', 'cat_k', 'cat_mg', 'cat_na', 'clay', 'clay_act', 'col_p', 'cs', 'ec', 'esp', 'fs',
               'phcl', 'phw', 'sar', 'silt', 'tn', 'tp', 'wb_oc')

attribute          <- 'clay' # attribute to process
SALI_file_location <- 'U:/QSRS/code/sali/SALI/'
save_location      <- paste0(SALI_file_location, attribute, '/'); dir.create(save_location, showWarnings=F)
step1              <- paste0(SALI_file_location, 'step1.R')
step2              <- paste0(SALI_file_location, 'step2.R')
step3              <- paste0(SALI_file_location, 'step3.R')
step4              <- paste0(SALI_file_location, 'step4.R')

###########################################
library(arcgisbinding); arc.check_product()
proj_dir <- 'C:/Users/leos/Documents/ArcGIS/Projects/MyProject' # link to project directory
sali     <- file.path(proj_dir, 'SALI_LEOS.sde') # link to SALI connection name
sali_tables <- c('SALI.SIT_SAMPLES','SALI.REG_PROJECTS','SALI.SIT_LOCATIONS','SALI.SIT_HORIZONS','SALI.SIT_LAB_RESULTS',
                 'SALI.SIT_SEGREGATIONS','SALI.SIT_LAB_METHODS','SALI.SIT_SOIL_CLASSIFICATIONS','SALI.SIT_ASCS',
                 'SALI.SIT_OBSERVATIONS','SALI.SIT_DISTURBANCES','SALI.SIT_FIELD_TESTS')
for (i in sali_tables){
  print(paste0('Downloading ', i))
  repeat {
    sali_data <- file.path(sali, i) %>% arc.open %>% arc.select %>% as.data.frame
    if(exists('sali_data')){
      break
    }
    Sys.sleep(5)
  }
  write.csv(sali_data, paste0(SALI_file_location, i, '.csv'), row.names=F)
  remove(sali_data)
}

############################################################################################################################
############################################################################################################################
# step 1
source(step1)

## copied from https://github.com/bitproxima/Modelling-mapping-with-Cubist/blob/master/Step1_SiteData.sql#L292
# standard filters
extract <- all_extract %>% filter(!is.na(VALUE),
                              VALUE > 0,
                              !(PROJECT_CODE == 'CQC'),
                              !(PROJECT_CODE == 'EIM' & SITE_ID == 6051),
                              !(PROJECT_CODE == 'QCS' & SITE_ID %in% c(20, 21, 85, 86)),
                              !(PROJECT_CODE == 'FSE' & SITE_ID == 126 & SAMPLE_NO == 3),
                              !(PROJECT_CODE == 'ABC' & SITE_ID == 315))

# specific filters for each attribute
if (attribute == 'clay' | attribute == 'cs' | attribute == 'fs'){
  extract <- extract %>% filter(VALUE <= 100,
                                !(PROJECT_CODE == 'BAN' & SITE_ID == 95),
                                !(PROJECT_CODE == 'BAMAR' & SITE_ID == 952 & SAMPLE_NO == 5))
} else if (attribute == 'esp'){
  extract <- extract %>% filter(VALUE <= 100,
                                !(PROJECT_CODE == 'ABC' & SITE_ID == 505  & SAMPLE_NO == 33),
                                !(PROJECT_CODE == 'ABC' & SITE_ID == 505  & SAMPLE_NO == 36),
                                !(PROJECT_CODE == 'ABC' & SITE_ID == 500  & SAMPLE_NO == 31),
                                !(PROJECT_CODE == 'WDH' & SITE_ID == 9098 & SAMPLE_NO == 13))
} else if (attribute == 'ec'){
  extract <- extract %>% filter(!(PROJECT_CODE == 'MON'   & SITE_ID == 6094 & SAMPLE_NO == 4),
                                !(PROJECT_CODE == 'MON'   & SITE_ID == 6089 & SAMPLE_NO == 3),
                                !(PROJECT_CODE == 'AGOPS' & SITE_ID == 162  & SAMPLE_NO == 16),
                                !(PROJECT_CODE == 'EIL'   & SITE_ID == 1000))
} else if (attribute == 'silt'){
  extract <- extract %>% filter(VALUE <= 100,
                                !(PROJECT_CODE == 'BAN'   & SITE_ID == 95),
                                !(PROJECT_CODE == 'MCL'   & SITE_ID == 9052 & SAMPLE_NO == 31),
                                !(PROJECT_CODE == 'BAMAR' & SITE_ID == 952  & SAMPLE_NO == 5),
                                !(PROJECT_CODE == 'MON'),
                                !(PROJECT_CODE == 'CCL'   & SITE_ID == 317  & SAMPLE_NO == 2))
} else if (attribute == 'cat_ca'){
  extract <- extract %>% filter(!(PROJECT_CODE == 'BAMAR' & SITE_ID == 952  & SAMPLE_NO == 5),
                                !(PROJECT_CODE == 'EIR'   & SITE_ID == 9021 & SAMPLE_NO == 36),
                                !(PROJECT_CODE == 'SALTC' & SITE_ID == 400  & SAMPLE_NO == 3),
                                !(PROJECT_CODE == 'SALTC' & SITE_ID == 400  & SAMPLE_NO == 4),
                                !(PROJECT_CODE == 'BDSM'  & SITE_ID == 345  & SAMPLE_NO == 1),
                                !(PROJECT_CODE == '3MC'   & SITE_ID == 9014))
} else if (attribute == 'cat_mg'){
  extract <- extract %>% filter(!(PROJECT_CODE == 'BDSM' & SITE_ID == 345 & SAMPLE_NO == 1))
} else if (attribute == 'cat_na'){
  extract <- extract %>% filter(!(PROJECT_CODE == 'CQA' & SITE_ID == 1001),
                                !(PROJECT_CODE == 'ABC' & SITE_ID == 500  & SAMPLE_NO == 31),
                                !(PROJECT_CODE == 'ABC' & SITE_ID == 505  & SAMPLE_NO == 33),
                                !(PROJECT_CODE == 'ABC' & SITE_ID == 505  & SAMPLE_NO == 36),
                                !(PROJECT_CODE == 'WDH' & SITE_ID == 9098 & SAMPLE_NO == 13))
} else if (attribute == 'col_p'){
  land_disturb <- c('5', '6', '7', '8')
  extract <- extract %>% filter(VALUE <= 150,
                                UD == 0,
                                between(LD, 9, 11), # LD == 10 - not working???
                                !grepl(paste(land_disturb, collapse='|'), DISTURB_TYPE),
                                !is.na(LATITUDE))
  extract <- extract %>% dplyr::select(LATITUDE, LONGITUDE, ID, VALUE)
  extract <- extract %>% dplyr::rename(X0.10.cm = VALUE)
  write.csv(extract, 'U:/QSRS/code/sali/col_p/col_p_harmonized.csv', row.names=F)
}

write.csv(extract, paste0(save_location, 'lab_data.csv'), row.names=F)
############################################################################################################################
# step 2
source(step2)

# step 3
source(step3)

# step 4
source(step4)
