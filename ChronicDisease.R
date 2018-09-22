# Does the required library exist, if not install it...
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if("Hmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("Hmisc")}
if("zoo" %in% rownames(installed.packages()) == FALSE) {install.packages("zoo")}
if("readxl" %in% rownames(installed.packages()) == FALSE) {install.packages("readxl")}
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
if("quantmod" %in% rownames(installed.packages()) == FALSE) {install.packages("quantmod")}
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}


# Attached Libraries
library(dplyr)
library(Hmisc, quietly=TRUE)
library(zoo)
library(readxl)
library(stringr)
library(quantmod)
library(lubridate)

# Set my working directory
setwd("C:/Data/ChronicDisease")

'*************************************************************************************************

                            CDC US Chronic Disease Indicators  
                                     Data Preparation
                          
                          Google cdc chronic disease indicators data

https://data.cdc.gov/Chronic-Disease-Indicators/U-S-Chronic-Disease-Indicators-CDI-/g4ie-h725
https://www.kaggle.com/cdc/chronic-disease/home

*************************************************************************************************'

# read csv file 
CDCFullData <- read.csv("U.S._Chronic_Disease_Indicators__CDI_.csv", stringsAsFactors = F)

# Take a look at the first and last 15 rows..
head(CDCFullData , n = 15)
tail(CDCFullData, n = 15) # Last 15 rows

# what does the object structure look like?
str(CDCFullData)

# I like this funtion.
contents(CDCFullData)

#  All values contain NAs
unique(CDCFullData$Response)
unique(CDCFullData$StratificationCategory2)

CDCAlcoholData <- CDCFullData %>%
       select(  -Response
              , -StratificationCategory2
              , -Stratification2
              , -StratificationCategory3
              , -Stratification3
              , -ResponseID
              , -StratificationCategoryID2
              , -StratificationID2
              , -StratificationCategoryID3
              , -StratificationID3
              ) %>%
        filter(!(LocationAbbr %in% c("US","VI","GU","PR"))  
               & Topic == "Alcohol" 
               & StratificationCategory1 == 'Overall') %>%
        select(YearStart, LocationAbbr, LocationDesc, DataSource, Topic, Question, DataValueUnit, DataValue, DataValueAlt)
  
contents(CDCAlcoholData)
head(CDCAlcoholData)

######## review missing DataValueAlt

df <- CDCAlcoholData %>%
      filter(is.na(DataValueAlt))

head(df)
rm(df)
#######

unique(CDCAlcoholData$Topic) # Make sure topic Alchohol is the only one selected
unique(CDCAlcoholData$Question) # What are the types of quesitons asked?
unique(CDCAlcoholData$YearStart) # How many year of data? 2010 to 2016
unique(CDCAlcoholData$StratificationCategory1) # What is in here?

# Write it out to csv
write.csv(CDCAlcoholData, file = "CDCAlcoholData.csv")

'*************************************************************************************************

                                    Census Tax Revenue
                https://www.census.gov/programs-surveys/stc/data/datasets.All.html

*************************************************************************************************'

# Let's pull in some data
# read csv file into training_raw object
StateTaxData <- read_excel("STC_Historical_DB (2017).xls")
head(StateTaxData , n = 15)
contents(StateTaxData)

# Expand StateTaxData Look for "Alcoholic" values. Use these to filter

StateTaxAlcohol <- StateTaxData %>%
                   select(Year, State, Name, T10, T20) %>% # Select only what is needed
                   rename("Alcoholic Beverage Tax (T10)" = T10) %>% # Rename Column
                   rename("Alcoholic Beverage Lic (T20)" = T20) %>%
                   slice(3:n()) # blank line along with US Total

head(StateTaxAlcohol)
str(StateTaxAlcohol)

# Write it out to csv
write.csv(StateTaxAlcohol, file = "StateTaxAlcohol.csv")

'*************************************************************************************************

                            Occupational Employment Statistics (OES) 
                                https://www.bls.gov/oes/#data

*************************************************************************************************'

# Occupational Employment Statistics BLS
# Manually unzip files. Then run this. Remember to explain why to the class. It is an event.

# Get files. Use regexp ^ to start at the beginning and capture anything that is enclosed in ()
OESFiles <- list.files(pattern="^(state_M20)") 


# create an empty list to store dataframes in below loop
dfList = list()

# Loop through each file name and assign resulting dataframe to a list. Then bind together in single df
for (i in 1:length(OESFiles)) 
  {
      FileName <- OESFiles[i]
      #print(FileName) # For Testing
      #FileName <- "state_M2010_dl.xlsx" # For Testing
    
      
      df1 <- read_xlsx(FileName) %>% # read xlsx
             # Rehabilitation Counselors & Mental Health and Substance Abuse Social Workers
             filter(OCC_CODE %in% c('21-1015','21-1023')) %>%
             mutate(YEAR = as.numeric(str_extract(FileName, '[0-9]+'))) %>% # Create year attribute from FileName
             select(YEAR, AREA, ST, STATE, OCC_TITLE, TOT_EMP) # Select only what is needed
        
        # Add df to list
        dfList[[i]] <- df1
  } 

OESData <- bind_rows(dfList) # Bind dataframes together

# Clean up : Remove dataframes to free up memory
rm(df1)
rm(dfList)

# Nice little check to see if file exists...can come in handy.
# file.exists("state_M2010_dl.xlsx") 

# Write it out to csv
write.csv(OESData, file = "OESData.csv")

'*************************************************************************************************

                          FRED Homeownership Rate Data Preparation   

                                https://fred.stlouisfed.org/

*************************************************************************************************'

# Load State lookup data. Need this to get State Abbr from row number
StateLookupData <- read_excel("StateLookup.xlsx")

# create an empty list to store dataframes in below loop
dfList = list()

for (i in 1:nrow(StateLookupData)) 
{
  # Get StateCode Value based on index (RowNo)
  StateCode <- StateLookupData %>%
               filter(RowNo == i) %>%
               select(STAbbr)
  
  # Concate StateCode and HOWN to form a string to be use as the Symbol in the getSymbols function
  HOWNSymbol <- paste0(StateCode,"HOWN")

  # Use getSymbols from quantmod to obtain Homeownership Rate data from 
  # Federal Reserve Economic Data | FRED | St. Louis Fed 
  df <- getSymbols(HOWNSymbol, src = 'FRED', auto.assign = F) 
  
  # Take the xts object and convert date to value. Coredate is from the zoo library
  df <- data.frame(date=time(df), coredata(df)) 
  
  # rename HOWNSymbol column using base r to "Value"
  names(df)[2] <- "value"
  
  # Get year from date and assign back to dataframe - use year function from lubridate library
  df$year <- year(as.Date(df$date, origin = '1900-1-1'))
  # Assign measure type for reporting purposes later on.
  df$measure <- "Homeownership Rate"
  df$statecode <- StateCode$STAbbr
  
  # Put it all together in a single dateframe
   df <- df %>%
         select(year, statecode, measure, value)
  
  # Add df to list
  dfList[[i]] <- df
  
  # Loop back and repeat 
  
}

# bind dataframes together into single dataframe
HOWNRateData <- bind_rows(dfList) 

# Clean up
rm(df)
rm(dfList)

# Write it out to csv
write.csv(HOWNRateData, file = "HOWNRateData.csv")


'*************************************************************************************************

                              Census All Ages in Poverty

                https://www.census.gov/data-tools/demo/saipe/saipe.html

*************************************************************************************************'


# read csv file into training_raw object
CensusPovertyData <- read.csv("SAIPESNC_03SEP18_12_18_39_99.csv", stringsAsFactors = F)

contents(CensusPovertyData)
head(CensusPovertyData)

CensusPoverty <- CensusPovertyData %>%
                 filter(Year <= 2010 & State != 0) %>%
                 select(Year, State, State...County.Name, All.Ages.in.Poverty.Percent) %>%
                 rename(StateName = State...County.Name) %>%
                 rename(value = All.Ages.in.Poverty.Percent) %>%
                 mutate(measure = "All Ages in Poverty (%)")
  
# Write it out to csv
write.csv(CensusPoverty, file = "CensusPoverty.csv")
