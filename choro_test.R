library(foreign)
library(choroplethr)
library(plyr)
library(choroplethrMaps)

#Set working directory
  setwd("/Users/connormurphy/OneDrive/Documents/Econ/R Projects/Health/Aging/App2")

#Load the data
  health_aging <- read.csv("Healthy_Aging_Data.csv", header = TRUE)

#Keep relevant columns
  health_aging <- health_aging[ , c("YearStart","LocationAbbr","LocationDesc","Topic","Data_Value")]
  
#Drop non-states
  #Includes D.C.
  health_aging <- health_aging[ ! health_aging$LocationAbbr %in% c("GU","MDW","NRE","PR","SOU","US","VI","WEST"), ]
  #List: GU MDW NRE PR SOU US VI WEST

#Attach dataframe
  attach(health_aging)

#Aggregate Data
  df_fmd <- aggregate(Data_Value~LocationDesc, data = subset(health_aging, Topic=="Frequent mental distress" & YearStart==2011), mean, na.rm = TRUE)

#Prep for choropleth function
  df_fmd$LocationDesc <- tolower(df_fmd$LocationDesc)
  df_fmd <- rename(df_fmd, c("LocationDesc"="region", "Data_Value"="value"))
  
  state_choropleth(df_fmd, title = "FMD", legend = "Legend", num_colors = 1, reference_map = FALSE)
