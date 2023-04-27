# load and read in sali tables
sali_files      <- list.files(path=SALI_file_location, pattern='csv', full.names=T)
observations    <- read.csv(sali_files[grep(pattern='OBSERVATIONS'   , sali_files)])
locations       <- read.csv(sali_files[grep(pattern='LOCATIONS'      , sali_files)])
classifications <- read.csv(sali_files[grep(pattern='CLASSIFICATIONS', sali_files)])
horizons        <- read.csv(sali_files[grep(pattern='HORIZONS'       , sali_files)])

# select appropriate column names
observations    <- observations    %>% select(PROJECT_CODE, SITE_ID, OBS_NO)
horizons        <- horizons        %>% select(PROJECT_CODE, SITE_ID, OBS_NO, UPPER_DEPTH, LOWER_DEPTH, DESIGN_MASTER, DESIGN_NUM_PREFIX)
classifications <- classifications %>% select(PROJECT_CODE, SITE_ID, OBS_NO, ASC_ORD, PPF, EXHIBIT_GSG_CODE)

# merge tables
merge1 <- join(observations, horizons)
merge2 <- join(merge1, classifications)

# A horizon
Anew <- merge2 %>% filter(DESIGN_MASTER %like% 'A',
                          is.na(DESIGN_NUM_PREFIX),
                          (ASC_ORD %in% c('CH','KU','SO') | PPF %like% 'D%' | EXHIBIT_GSG_CODE %in% c('SDS','BP','LP','SH','YP','GP','GBP','SB','SC','RP','SK','SZ')))
Anew <- Anew %>% group_by(PROJECT_CODE, SITE_ID) %>% dplyr::summarise(UD = max(LOWER_DEPTH-0.01)*100, LD = max(LOWER_DEPTH)*100) %>% as.data.frame
Anew <- Anew[order(Anew$PROJECT_CODE, Anew$SITE_ID),]
write.csv(Anew, paste0(save_location, 'A_horizon_inserts.csv'), row.names=F)

# B horizon
Bnew <- merge2 %>% filter(DESIGN_MASTER %like% 'B',
                          is.na(DESIGN_NUM_PREFIX),
                          (ASC_ORD %in% c('CH','KU','SO') | grepl('^D', PPF) | EXHIBIT_GSG_CODE %in% c('SDS','BP','LP','SH','YP','GP','GBP','SB','SC','RP','SK','SZ')))
Bnew <- Bnew %>% group_by(PROJECT_CODE, SITE_ID) %>% dplyr::summarise(UD = min(UPPER_DEPTH)*100, LD = min(UPPER_DEPTH+0.01)*100) %>% as.data.frame
Bnew <- Bnew[order(Bnew$PROJECT_CODE, Bnew$SITE_ID),]
write.csv(Bnew, paste0(save_location, 'B_horizon_inserts.csv'), row.names=F)