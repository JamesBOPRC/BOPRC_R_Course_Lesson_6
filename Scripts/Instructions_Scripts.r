
library(tidyverse)
library(outliers)
library(plyr)
library(ggpubr)
# Simple example using MCI data

MCI_Data <- read.csv("MCI_Waiari.csv")

str(MCI_Data)

MCI_Data <- MCI_Data %>%
  mutate(Year = substring(Period,1,4)) %>%
  mutate(myDate = as.Date(paste0(Year,"-01-01"),tz="etc/GMT+12")) %>%
  select(Aquarius._SIteID,myDate,MCI) %>%
  mutate(analyte = "MCI")


#Add on time increment and extra date information - NOTE if this dataset was is based on Water Years firstMonth = 7
MCI_Data <- GetMoreDateInfo(MCI_Data)


#Process censored values
MCI_Data <- RemoveAlphaDetect(MCI_Data,ColToUse="MCI")



#this function will allow you to use annual trend analysis if it's MCI data only.
MCI_Inspect <- InspectTrendData(MCI_Data,
                                EndYear = max(MCI_Data$Year),
                                ReturnALLincr=TRUE,
                                do.plot = TRUE,
                                mymain="Example 1",
                                UseMidObs=TRUE,
                                Year="Year",
                                propYearTol = 0.8,#this is saying that 80% of time increments need data
                                propIncrTol = 0.8)#this is saying that 80% of time increments need data

#look at the plots.
library(ggpubr)
ggarrange(MCI_Inspect[[3]][[1]],ggarrange(MCI_Inspect[[3]][[2]],MCI_Inspect[[3]][[3]],MCI_Inspect[[3]][[4]],nrow=1,align="h"),nrow=2)

MCI_Data <- MCI_Inspect[[1]]

#not seasonal because annual

MCI_Trend_Analysis_Output<-NonSeasonalTrendAnalysis(MCI_Data,mymain="Ex 1 Raw Trend",do.plot=T)

#The assignConfCat function has a variety of options to classify trend output.  The most common, for our purposes is below.
MCI_Trend_Analysis_Output$Direction <- AssignConfCat(MCI_Trend_Analysis_Output,CatType="Improve",Reverse=c("VC","MCI"))

# Congrats, you have now completed your first trend analysis.


#Now let's try something a bit more difficult.

library(aquarius2018) ### AQ FLAT JAMS WHEN APPENDING FLOW

#
# Rangitaiki_DF <- AQMultiExtractFlat(sitelist = c("FC231176","JL350292"),param = c("TN","NNN","TP","DRP"),AppendFlow = T)
# Rangitaiki_DF$Unique <- paste0(Rangitaiki_DF$Site,round_date(Rangitaiki_DF$Time,unit = "hour"))
#
# library(tidyverse)
#
#
# Flow_FC231176 <- getdata("Discharge.Field Visits@FC231176")
# Flow_JL350292 <- getdata("Discharge.Field Visits@JL350292")
#
# Flow_DF <- rbind(Flow_FC231176,Flow_JL350292)
# Flow_DF$Unique <- paste0(Flow_DF$Site,round_date(Flow_DF$Time,unit = "hour"))
#
# FA_Dataset <- merge(Rangitaiki_DF,Flow_DF,by='Unique',all.x = T)
#
# FA_Dataset <-FA_Dataset %>%
#   select(Site.x,Time.x,LocationName.x,Value.x,"Discharge (m^3/s)","Value.y",Qualifiers.x,Parameter.x) %>%
#   filter(Qualifiers.x %in% c("Labstar - Legacy Data", "Routine","Routine","NIWA Data")) %>%
#   rename("Site" = Site.x,"Time" = Time.x, "LocationName" = LocationName.x,"Value"= Value.x,
#          "Discharge"= "Discharge (m^3/s)","Disc_Discrete"="Value.y","analyte"=Parameter.x) %>%
#   mutate(Discharge = ifelse(is.na(Discharge),Disc_Discrete,Discharge)) %>%
#   select(Site, Time,LocationName,analyte,Value, Discharge)
#
#  write.csv(x = FA_Dataset,file= "DF_for_Trends.csv",row.names = F)


# Suggested procedure for trend analysis.


#add in censoring
WQ_Data <-WQ_Data %>%
  mutate(Value = ifelse(analyte == "TN (g/m^3)" & Value < 0.01,"<0.01",Value)) %>%
  mutate(Value = ifelse(analyte == "NNN (g/m^3)" & Value < 0.001,"<0.001",Value)) %>%
  mutate(Value = ifelse(analyte == "TP (g/m^3)" & Value < 0.001,"<0.001",Value)) %>%
  mutate(Value = ifelse(analyte == "DRP (g/m^3)" & Value < 0.001,"<0.001",Value))

#view data
WQ_Data %>%
  mutate(Value = as.numeric(gsub("<", "", Value))) %>%
  ggplot(aes(x=Time, y=Value))+
  geom_point(aes(colour=analyte))+
  theme_bw()+
  facet_wrap(~LocationName+analyte,scales="free")

#Remove outliers
#based on 'scores' function

names(dataset) <- c("LocationName", "Site", "Time",  "Parameter", "Value")

#c("z", "t", "chisq", "mad")
Value_Output <- Outliers_Z_Score(dataset=WQ_Data %>% select("LocationName", "Site", "Time",  "analyte", "Value"),
                                 probability = 0.99,output = "outliers",type="z")

#create a unique column
Value_Output <-Value_Output %>%
  mutate(Unique = paste0(Site,Time,Parameter))

#flag columns in WQ_Data
WQ_Data <- WQ_Data %>%
  mutate(Unique = paste0(Site,Time,analyte)) %>%
  mutate(Outlier = ifelse(Unique %in% Value_Output$Unique, TRUE, FALSE)) %>%
  select(Site, LocationName,Time,analyte,Value,Discharge,Outlier)

WQ_Data%>%
  ggplot(aes(x=Time, y=Value))+
  geom_point(aes(colour=Outlier))+
  theme_bw()+
  facet_wrap(~LocationName+analyte,scales="free")

#now we have a dataset for running trends.
Trend_DF <- WQ_Data %>%
  filter(Outlier == FALSE) %>%
  select(Site, LocationName,Time,analyte,Value,Discharge)

#Lets simplify to one site and one parameter and run season analysis

SH5_DRP <- Trend_DF %>%
  filter(LocationName == "Rangitaiki at SH5") %>%
  filter(analyte == "DRP (g/m^3)")

#we need to convert Time to mydate

SH5_DRP <- SH5_DRP %>%
  mutate(myDate = as.Date(Time, format = "%Y-%m-%d",tz="etc/GMT+12")) %>%
  select(Site, myDate, Value)

#Add on time increment and extra date information - NOTE if this dataset was is based on Water Years firstMonth = 7
SH5_DRP <- GetMoreDateInfo(SH5_DRP)

#Process censored values
SH5_DRP <- RemoveAlphaDetect(SH5_DRP,ColToUse="Value")

summary(SH5_DRP)

#inspect data

Inspect_Output_SH5_DRP <- InspectTrendData(SH5_DRP, EndYear = max(SH5_DRP$Year),
                                           ReturnALLincr=TRUE,do.plot = TRUE,mymain="Example 1",UseMidObs=TRUE)

#four three elements - 1 dataset automatically amended with most appropriate interval
#2 - shows the analysis to determine the time interval increase. selected as the lowest frequency increment that that meets the minimum data requirements specified
#3 - shows plots - 4 sub plots in each one.

#UseMidObs
#Defines how multiple observations in a month are dealt with. Default is TRUE, which takes the observation that is closest to the middle of the season. Set to FALSE, to take the median across the season.

#look at the plots.
library(ggpubr)
ggarrange(Inspect_Output_SH5_DRP[[3]][[1]],ggarrange(Inspect_Output_SH5_DRP[[3]][[2]],Inspect_Output_SH5_DRP[[3]][[3]],Inspect_Output_SH5_DRP[[3]][[4]],nrow=1,align="h"),nrow=2)
#see that there is probably some seasonality.  Censored values are not really an issue.
#We have duplicates per month!!! Not ideal...  But not going to affect TA because functions to deal with it.

#can see that we do not have enough data to run trend analysis.. over this period.  We need to adjust out dataset
Inspect_Output_SH5_DRP[[2]]

Inspect_Output_SH5_DRP <- InspectTrendData(SH5_DRP, EndYear = max(SH5_DRP$Year),
                                           ReturnALLincr=TRUE,do.plot = TRUE,mymain="Example 1",UseMidObs=TRUE,
                                           TrendPeriod = 10)

Inspect_Output_SH5_DRP[[2]]

ggarrange(Inspect_Output_SH5_DRP[[3]][[1]],ggarrange(Inspect_Output_SH5_DRP[[3]][[2]],Inspect_Output_SH5_DRP[[3]][[3]],Inspect_Output_SH5_DRP[[3]][[4]],nrow=1,align="h"),nrow=2)

SH5_DRP <- Inspect_Output_SH5_DRP[[1]]

#check seasonality
Season_Output<-GetSeason(SH5_DRP,ValuesToUse = "RawValue",mymain="Example 1",do.plot = TRUE)

Season_Output[[2]] # Data is definitely seasonal.  Monthly.
Season_Output[[3]] #Data is not seasonal

SH5_DRP <- Season_Output[[1]]

Trend_Analysis_Output_SH5_DRP<-NonSeasonalTrendAnalysis(SH5_DRP,mymain="Ex 1 Raw Trend",do.plot=T)

Trend_Analysis_Output_SH5_DRP[[1]]
Trend_Analysis_Output_SH5_DRP[[2]]

Trend_Analysis_Output_SH5_DRP <- Trend_Analysis_Output_SH5_DRP[[1]]
Trend_Analysis_Output_SH5_DRP$analyte <- "DRP"

Trend_Analysis_Output_SH5_DRP$Direction <- AssignConfCat(Trend_Analysis_Output_SH5_DRP,CatType="Improve",Reverse=c("VC"))



undebug(SeasonalTrendAnalysis)
undebug(SeasonalSenSlope)
Trend_Analysis_Output_NNN_Te_Teko <-SeasonalTrendAnalysis(NNN_Te_Teko, ValuesToUse="Flow_Adjusted", RawValues=FALSE, ValuesToUseforMedian="RawValue", mymain="Example 1a: Flow Adjusted Trend", do.plot=T)

SeasonalSenSlope



debug(GetInterObservationSlopes)
