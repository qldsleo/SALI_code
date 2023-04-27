# load and read in lab data
lab.data <- read.csv(paste0(save_location, 'lab_data.csv'))
labdata1 <- rename(lab.data, c(UD = 'SUD', LD = 'SLD'))

##Remove sites with only one analysed depth
count1    <- count(labdata1, vars='ID')
morethan1 <- subset(count1, freq > 1)
labdata   <- merge(labdata1, morethan1, by=c('ID'))

## A inserts
Inserts       <- read.csv(paste0(save_location, 'A_horizon_inserts.csv')) #Import Anew_depths.csv data generated in SQLDev for each duplex soil 
Allvalue      <- merge(Inserts, labdata, by=c('PROJECT_CODE', 'SITE_ID')) #Add lab data to new_depths (one to one)
Allvalue$diff <- Allvalue$UD - Allvalue$SUD #Add a 'diff' field to Allvalue df which is the 'difference between sample depth and A/B horizon depth'
Value         <- subset(Allvalue, diff >= 0 & UD >= 10, select=c(ID, UD, LD, VALUE, diff)) #Limit records to lab samples in A horizon using 'diff' field and only where A horizon >= 10cm depth
Nearsamp      <- aggregate(x=Value$diff, by=list(ID=Value$ID), min) #Limit records to sample nearest to A/B change
Nearsamp1     <- rename(Nearsamp, c(x='diff')) #Rename column 'x' to 'diff' in 'Nearsamp' df to conform to 'Value' df
Aresult       <- join(Value, Nearsamp1, by=c('ID', 'diff'), type='right', match='first') #Combine df with nearest sample 'nearsamp' with df with lab data 'Value'
Aresult       <- subset(Aresult, select=c(ID, UD, LD, VALUE)) #Remove 'diff' in prep for Spline program

## B inserts
Inserts       <- read.csv(paste0(save_location, 'B_horizon_inserts.csv')) #Import Anew_depths.csv data generated in SQLDev for each duplex soil 
Allvalue      <- merge(Inserts, labdata, by=c('PROJECT_CODE', 'SITE_ID')) #Add lab data to new_depths (one to one)
Allvalue$diff <- Allvalue$LD - Allvalue$SLD #Add a 'diff' field to Allvalue df which is the 'difference between sample depth and A/B horizon depth'
Value         <- subset(Allvalue, diff <= 0 & UD >= 10, select=c(ID, UD, LD, VALUE, diff)) #Limit records to lab samples in B horizon using 'diff' field and only where B horizon Upper Depth >= 10cm
Nearsamp      <- aggregate(x=Value$diff, by=list(ID=Value$ID), max) #Limit records to sample nearest to A/B change
Nearsamp1     <- rename(Nearsamp, c(x='diff')) #Rename column 'x' to 'diff' in 'Nearsamp' df to conform to 'Value' df
Bresult       <- join(Value, Nearsamp1, by=c('ID', 'diff'), type='right', match='first') #Combine df with nearest sample 'nearsamp' with df with lab data 'Value'
Bresult       <- subset(Bresult, select=c(ID, UD, LD, VALUE)) #Remove 'diff' in prep for Spline program

## Real sample depths
Realdata    <- na.omit(subset(labdata, select=c(ID, SUD, SLD, VALUE))) ##select only fields required for spline and omit nulls 
DupsRemoved <- unique(Realdata[,1:2]) ## delete duplicates or overlaps
Realdata    <- join(Realdata, DupsRemoved, by=c('ID', 'SUD'), type='right', match='first')
Realdata    <- rename(Realdata, c(SUD='UD', SLD='LD')) #rename depth columns to conform with Aresult and Bresult df

##merge Real samples with A & B inserts
Result1 <- rbind(Realdata, Aresult, Bresult) #Append data from each result above into the one df
Result2 <- aggregate(x=Result1$VALUE, by=list(ID=Result1$ID, UD=Result1$UD, LD=Result1$LD), mean) #For identical sample depths with more than one measured value, take the average of these values
Result2 <- rename(Result2, c(x='VALUE'))
Result  <- Result2[order(Result2$ID, Result2$UD, Result2$LD),] #Order df records on ID, upper depth
write.csv(Result, paste0(save_location, attribute, '_with_inserts.csv'), row.names=F) #Export result to project directory