# load and read in sali tables
sali_files   <- lapply(list.files(path=SALI_file_location, pattern='csv', full.names=T), read.csv)
sali_files   <- setNames(sali_files, gsub('.csv', '', list.files(path=SALI_file_location, pattern='csv')))

# select appropriate column names
projects     <- sali_files$SALI.REG_PROJECTS %>% select(PROJECT_CODE, OBJECTID)
results      <- sali_files$SALI.SIT_LAB_RESULTS %>% select(PROJECT_CODE, SITE_ID, OBS_NO, HORIZON_NO, SAMPLE_NO, LAB_METH_CODE, NUMERIC_VALUE, QC_CODE)
locations    <- sali_files$SALI.SIT_LOCATIONS %>% select(PROJECT_CODE, SITE_ID, OBS_NO, DATUM, LONGITUDE, LATITUDE, ZONE, EASTING, NORTHING)
samples      <- sali_files$SALI.SIT_SAMPLES %>% select(PROJECT_CODE, SITE_ID, OBS_NO, HORIZON_NO, SAMPLE_NO, SAMPLE_DATE, UPPER_DEPTH, LOWER_DEPTH)
horizons     <- sali_files$SALI.SIT_HORIZONS %>% select(PROJECT_CODE, SITE_ID, OBS_NO, HORIZON_NO, DESIGN_MASTER, TEXTURE_CODE)
disturbances <- sali_files$SALI.SIT_DISTURBANCES %>% select(PROJECT_CODE, SITE_ID, OBS_NO, DISTURB_NO, DISTURB_TYPE)
segregations <- sali_files$SALI.SIT_SEGREGATIONS %>% select(PROJECT_CODE, SITE_ID, OBS_NO, HORIZON_NO, SEG_NO, NATURE)

# other edits to SALI tables
disturbances <- disturbances %>% pivot_wider(names_from = c(DISTURB_NO), values_from = c(DISTURB_TYPE), names_prefix = 'DISTURB_NO') %>% unnest %>% as.data.frame
disturbances <- disturbances %>% unite('DISTURB_TYPE', DISTURB_NO1:DISTURB_NO2)
segregations <- segregations %>% pivot_wider(names_from = c(SEG_NO), values_from = c(NATURE), names_prefix = 'SEG_NO') %>% unnest %>% as.data.frame
segregations <- segregations %>% unite('NATURE', SEG_NO3:SEG_NO12)
locations    <- locations    %>% dplyr::filter(DATUM == 3)

# filter methods and QC_CODES
all_SALI <- results %>% filter(LAB_METH_CODE %like% c('15%','18F%','2Z2_%','2Z1_%','2Z1%','7%','9%','6B%','3A1','4A1','4B1','5A2','6A1'),
                               NUMERIC_VALUE != 0,
                               QC_CODE != 'Q',
                               QC_CODE != 'P')
all_SALI <- all_SALI %>% select(-QC_CODE)

# pivot table
SALI_p <- all_SALI %>% pivot_wider(names_from = c(LAB_METH_CODE), values_from = c(NUMERIC_VALUE)) %>% unnest %>% as.data.frame

# need to add lab method column name otherwise coalesce throws an error
lab_methods <- c('2Z2_Clay','2Z2_Silt','2Z2_CS','2Z2_FS', # clay, silt, coarse sand, fine sand
                 '6A1','15G1_H','3A1','4A1','4B1','5A2', # organic C (W&B), exchange acidity, EC, pH(H2O), pH(CaCl), chloride
                 '7A1','7A2','7A3','7A4','7A5','7A6','7A7', # total N
                 '9B1','9B2', # Colwell P
                 '9A3a','9A1', # total P
                 '6B1','6B3','6B2a','6B2b','6B4','6B4b_MQ01','6B4b_MQ02', # total C
                 '15A2_Ca','15A1_Ca','15F1_Ca','15C1_Ca','18F1_Ca','15A1_Ca_MQ02','15C1_Ca_MQ01','15D3_Ca', # calcium
                 '15A2_K','15A1_K','15F1_K','15C1_K','18F1_K','15D3_K', # potassium
                 '15A2_Na','15A3_Na','15A1_Na','15F1_Na','15C1_Na','18F1_Na','15D3_Na', # sodium
                 '15A2_Mg','15A1_Mg','15F1_Mg','15C1_Mg','18F1_Mg','15A1_Mg_MQ02','15C1_Mg_MQ01','15D3_Mg', # magnesium
                 '15B2_CEC','15D2_CEC','15J2_MQ02','15C2_CEC_MQ01','15C1_CEC','15C1_CEC_MC91','15B1_CEC','15D1_CEC','15J1')
for (i in lab_methods){
  check <- i %in% names(SALI_p)
  if (!check) {SALI_p[i] <- NA}
}

# add methods
results <- SALI_p %>% mutate(
  clay     = `2Z2_Clay` , clay_method     = '2Z2_Clay' ,
  silt     = `2Z2_Silt` , silt_method     = '2Z2_Silt' ,
  cs       = `2Z2_CS`   , cs_method       = '2Z2_CS'   ,
  fs       = `2Z2_FS`   , fs_method       = '2Z2_FS'   ,
  wb_oc    = `6A1`      , wb_oc_method    = '6A1'      ,
  cat_acid = `15G1_H`   , cat_acid_method = '15G1_H'   ,
  ec       = `3A1`      , ec_method       = '3A1'      ,
  phw      = `4A1`      , phw_method      = '4A1'      ,
  phcl     = `4B1`      , phcl_method     = '4B1'      ,
  chloride = `5A2`      , chloride_method = '5A2'      ,
  
  col_p        = coalesce(`9B2`,`9B1`),
  col_p_method = case_when(col_p == `9B2` ~ '9B2',
                           col_p == `9B1` ~ '9B1'),
  
  tp        = coalesce(`9A3a`,`9A1`),
  tp_method = case_when(tp == `9A3a` ~ '9A3a',
                        tp == `9A1` ~ '9A1'),
  
  tn        = coalesce(`7A1`,`7A2`,`7A3`,`7A4`,`7A5`,`7A6`,`7A7`),
  tn_method = case_when(tn == `7A1` ~ '7A1',
                        tn == `7A2` ~ '7A2',
                        tn == `7A3` ~ '7A3',
                        tn == `7A4` ~ '7A4',
                        tn == `7A5` ~ '7A5',
                        tn == `7A6` ~ '7A6',
                        tn == `7A7` ~ '7A7'),
  
  tc = coalesce(`6B1`, `6B3`, `6B2a`, `6B2b`, `6B4`, `6B4b_MQ01`, `6B4b_MQ02`),
  tc_method = case_when(tc == `6B1` ~ '6B1',
                        tc == `6B3` ~ '6B3',
                        tc == `6B2a` ~ '6B2a',
                        tc == `6B2b` ~ '6B2b',
                        tc == `6B4` ~ '6B4',
                        tc == `6B4b_MQ01` ~ '6B4b_MQ01',
                        tc == `6B4b_MQ02` ~ '6B4b_MQ02'),
  
  cn = tc/coalesce(`7A1`, `7A2`, `7A3`, `7A4`, `7A5`, `7A6`),
  cn_method = case_when(tc == `6B1` ~ '6B1',
                        tc == `6B3` ~ '6B3',
                        tc == `6B2a` ~ '6B2a',
                        tc == `6B2b` ~ '6B2b',
                        tc == `6B4` ~ '6B4',
                        tc == `6B4b_MQ01` ~ '6B4b_MQ01',
                        tc == `6B4b_MQ02` ~ '6B4b_MQ02'),
  
  cat_ca = case_when(phw < 7.3 & ec > 0.3 ~ coalesce(`15A2_Ca`, `15A1_Ca`, `15F1_Ca`, `15C1_Ca`, `18F1_Ca`/200, `15A1_Ca_MQ02`, `15C1_Ca_MQ01`),
                     phw >= 7.3           ~ coalesce(`15C1_Ca`, `15F1_Ca`, `15A1_Ca`, `15C1_Ca_MQ01`),
                     TRUE                 ~ coalesce(`15A1_Ca`, `15A2_Ca`, `15F1_Ca`, `15C1_Ca`, `15D3_Ca`, `18F1_Ca`/200, `15A1_Ca_MQ02`, `15C1_Ca_MQ01`)),
  cat_ca_method = case_when(cat_ca == `15A2_Ca` ~ '15A2_Ca',
                            cat_ca == `15A1_Ca` ~ '15A1_Ca',
                            cat_ca == `15F1_Ca` ~ '15F1_Ca',
                            cat_ca == `15C1_Ca` ~ '15C1_Ca',
                            cat_ca == `18F1_Ca`/200 ~ '18F1_Ca',
                            cat_ca == `15A1_Ca_MQ02` ~ '15A1_Ca_MQ02',
                            cat_ca == `15C1_Ca_MQ01` ~ '15C1_Ca_MQ01',
                            cat_ca == `15D3_Ca` ~ '15D3_Ca'),
  
  cat_k = case_when(phw < 7.3 & ec > 0.3 ~ coalesce(`15A2_K`, `15A1_K`, `15F1_K`, `15C1_K`, `18F1_K`/390),
                    phw >= 7.3           ~ coalesce(`15C1_K`, `15F1_K`, `15A1_K`, `18F1_K`/390),
                    TRUE                 ~ coalesce(`15A1_K`, `15A2_K`, `15F1_K`, `15C1_K`, `15D3_K`, `18F1_K`/390)),
  cat_k_method = case_when(cat_k == `15A2_K` ~ '15A2_K',
                           cat_k == `15A1_K` ~ '15A1_K',
                           cat_k == `15F1_K` ~ '15F1_K',
                           cat_k == `15C1_K` ~ '15C1_K',
                           cat_k == `18F1_K`/390 ~ '18F1_K',
                           cat_k == `15D3_K` ~ '15D3_K'),
  
  cat_na = case_when(phw < 7.3 & ec > 0.3 ~ coalesce(`15A2_Na`, `15A3_Na`, (`15A1_Na`-(chloride/354.5)), `15A1_Na`, `15F1_Na`, `15C1_Na`, `18F1_Na`/230),
                     phw >= 7.3           ~ coalesce(`15C1_Na`, `15A1_Na`, `15F1_Na`, `18F1_Na`/230),
                     TRUE                 ~ coalesce(`15A1_Na`, `15A2_Na`, `15F1_Na`, `15C1_Na`, `15D3_Na`, `18F1_Na`/230)),
  cat_na_method = case_when(cat_na == `15A2_Na` ~ '15A2_Na',
                            cat_na == `15A3_Na` ~ '15A3_Na',
                            cat_na == (`15A1_Na`-(chloride/354.5)) ~ '15A1_Na',
                            cat_na == `15A1_Na` ~ '15A1_Na',
                            cat_na == `15F1_Na` ~ '15F1_Na',
                            cat_na == `15C1_Na` ~ '15C1_Na',
                            cat_na == `18F1_Na`/230 ~ '18F1_Na',
                            cat_na == `15D3_Na` ~ '15D3_Na'),
  
  cat_mg = case_when(phw < 7.3 & ec > 0.3 ~ coalesce(`15A2_Mg`, `15A1_Mg`, `15F1_Mg`, `15C1_Mg`, `18F1_Mg`/120, `15A1_Mg_MQ02`, `15C1_Mg_MQ01`),
                     phw >= 7.3           ~ coalesce(`15C1_Mg`, `15F1_Mg`, `15A1_Mg`, `18F1_Mg`/120, `15C1_Mg_MQ01`),
                     TRUE                 ~ coalesce(`15A1_Mg`, `15A2_Mg`, `15F1_Mg`, `15C1_Mg`, `15D3_Mg`, `18F1_Mg`/120, `15A1_Mg_MQ02`, `15C1_Mg_MQ01`)),
  cat_mg_method = case_when(cat_mg == `15A2_Mg` ~ '15A2_Mg',
                            cat_mg == `15A1_Mg` ~ '15A1_Mg',
                            cat_mg == `15F1_Mg` ~ '15F1_Mg',
                            cat_mg == `15C1_Mg` ~ '15C1_Mg',
                            cat_mg == `18F1_Mg`/120 ~ '18F1_Mg',
                            cat_mg == `15A1_Mg_MQ02` ~ '15A1_Mg_MQ02',
                            cat_mg == `15C1_Mg_MQ01` ~ '15C1_Mg_MQ01',
                            cat_mg == `15D3_Mg` ~ '15D3_Mg'),
  
  cat_cec = case_when(phw < 7.3 & ec > 0.3 ~ coalesce(`15B2_CEC`,`15D2_CEC`,(cat_ca+cat_mg+cat_k+cat_na+ifelse(is.na(cat_acid), 0, cat_acid)),`15J2_MQ02`, `15C2_CEC_MQ01`),
                      phw >= 7.3           ~ coalesce(`15C1_CEC`,(cat_ca+cat_mg+cat_k+cat_na),`15C1_CEC_MC91`, `15C2_CEC_MQ01`),
                      TRUE                 ~ coalesce(`15B1_CEC`, `15D1_CEC`, `15J1`,(cat_ca+cat_mg+cat_k+cat_na+ifelse(is.na(cat_acid), 0, cat_acid)),`15C1_CEC`,`15J2_MQ02`,`15C2_CEC_MQ01`,`15C1_CEC_MC91`)),
  cat_cec_method = case_when(cat_cec == `15B2_CEC` ~ '15B2_CEC',
                             cat_cec == `15D2_CEC` ~ '15D2_CEC',
                             cat_cec == (cat_ca+cat_mg+cat_k+cat_na+ifelse(is.na(cat_acid), 0, cat_acid)) & phw < 7.3 & ec > 0.3 ~ 'cat_ca',
                             cat_cec == `15J2_MQ02` ~ '15J2_MQ02',
                             cat_cec == `15C2_CEC_MQ01` ~ '15C2_CEC_MQ01',
                             cat_cec == `15C1_CEC` ~ '15C1_CEC',
                             cat_cec == (cat_ca+cat_mg+cat_k+cat_na) & phw >= 7.3 ~ 'cat_na',
                             cat_cec == `15C1_CEC_MC91` ~ '15C1_CEC_MC91',
                             cat_cec == `15B1_CEC` ~ '15B1_CEC',
                             cat_cec == `15D1_CEC` ~ '15D1_CEC',
                             cat_cec == (cat_ca+cat_mg+cat_k+cat_na+ifelse(is.na(cat_acid), 0, cat_acid)) ~ 'cat_na',
                             cat_cec == `15J1` ~ '15J1'),
  
  ca_mg = case_when(cat_cec > 3 ~ cat_ca/cat_mg),
  ca_mg_method = case_when(cat_cec > 3 ~ 'cat_ca',
                           !is.na(cat_ca) & !is.na(cat_mg) ~ 'CEC too low'),
  
  esp = case_when(cat_cec > 3 ~ cat_na/cat_cec*100,
                  !is.na(cat_na) & !is.na(cat_cec) ~ 0.1),
  esp_method = case_when(cat_cec > 3 ~ 'cat_cec',
                         !is.na(cat_na) & !is.na(cat_cec) ~ 'CEC too low'),
  
  sar = case_when(cat_cec > 3 ~ cat_na/(sqrt(0.5*(cat_ca+cat_mg)))),
  sar_method = case_when(cat_cec > 3 ~ 'cat_na',
                         !is.na(cat_na) & !is.na(cat_cec) ~ 'CEC too low'),
  
  clay_act = case_when(cat_cec > 3 & clay > 30 ~ cat_cec/clay,
                       !is.na(cat_cec) & !is.na(clay) ~ 0.1),
  clay_act_method = case_when(cat_cec > 3 & clay > 30 ~ 'clay',
                              !is.na(cat_cec) & !is.na(clay) ~ 'not clay'),
  
  approx_cn = wb_oc/coalesce(`7A1`, `7A2`, `7A3`, `7A4`, `7A5`, `7A6`),
  approx_cn_method = case_when(approx_cn == wb_oc/`7A1` ~ '7A1',
                               approx_cn == wb_oc/`7A2` ~ '7A2',
                               approx_cn == wb_oc/`7A3` ~ '7A3',
                               approx_cn == wb_oc/`7A4` ~ '7A4',
                               approx_cn == wb_oc/`7A5` ~ '7A5',
                               approx_cn == wb_oc/`7A6` ~ '7A6')
)

# merge results with projects, locations and samples
results                   <- join(results, projects)
results                   <- join(results, disturbances)
results                   <- join(results, segregations)
results_locations         <- join(results, locations)
results_locations_samples <- merge(results_locations, samples, by=c('PROJECT_CODE','SITE_ID','OBS_NO','SAMPLE_NO'), suffixes=c('.rl', '.s')) # not joining by HORIZON_NO because of SITED app issue
results_locations_samples <- results_locations_samples %>% dplyr::rename(HORIZON_NO = HORIZON_NO.s)
all_data                  <- join(results_locations_samples, horizons)

# extract columns needed
extract <- all_data[order(all_data$PROJECT_CODE, all_data$SITE_ID, all_data$HORIZON_NO, all_data$SAMPLE_NO),]
extract <- extract %>% mutate(ID = paste0(OBJECTID, SITE_ID),
                              UD = UPPER_DEPTH*100,
                              LD = LOWER_DEPTH*100,
                              YEAR = year(SAMPLE_DATE))
extract <- extract %>% select(-HORIZON_NO.rl, -UPPER_DEPTH, -LOWER_DEPTH)
extract <- extract %>% 
  relocate("OBJECTID", "DATUM", "LONGITUDE", "LATITUDE", "ZONE", "EASTING", "NORTHING", "HORIZON_NO", "SAMPLE_DATE", "ID", "UD", "LD", "YEAR")

write.csv(extract, paste0('X:/leos/QSRS/soil_attributes/', 'all_SALI_data.csv'), row.names=F)

# extract <- extract %>% filter(!is.na(DESIGN_MASTER))
extract <- extract %>% select(ID, UD, LD, paste0(attribute), paste0(attribute, '_method'), OBJECTID, PROJECT_CODE, SITE_ID, OBS_NO, HORIZON_NO, SAMPLE_NO,
                              YEAR, LATITUDE, LONGITUDE, ZONE, EASTING, NORTHING, DISTURB_TYPE, NATURE)
names(extract)[which(names(extract) == paste0(attribute))] <- "VALUE"
extract <- extract %>% filter(!is.na(VALUE))
all_extract <- distinct(extract, ID, UD, LD, .keep_all=T)

extract %>%
  group_by(PROJECT_CODE,SITE_ID,OBS_NO,SAMPLE_NO) %>%
  filter(n() > 1) %>%
  ungroup %>% View
