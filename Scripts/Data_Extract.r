

library(aquarius2018)
library(tidyverse)

Rangitaiki_SH5_Flow <- getdata("Discharge.HourMean@FC231176",start="2000-01-01")

Rangitaiki_SH5_Flow$Unique <- paste0(Rangitaiki_SH5_Flow$Site,round_date(Rangitaiki_SH5_Flow$Time,unit = "hour"))

WQ_Data <- read.csv("DF_for_Trends.csv")

WQ_Data <- WQ_Data %>%
  mutate(Time = parse_date_time(Time, orders = c("%Y-%m-%d %H:%M:%S","%Y-%m-%d"),tz="etc/GMT+12"))
WQ_Data$Unique <- paste0(WQ_Data$Site,round_date(WQ_Data$Time,unit = "hour"))


WQ_Data <- merge(WQ_Data,Rangitaiki_SH5_Flow,by="Unique",all.x = T)


WQ_Data <- WQ_Data %>%
  select(Site.x,Time.x,LocationName.x,analyte, Value.x,Discharge,Value.y) %>%
  mutate(Discharge = ifelse(is.na(Discharge),Value.y,Discharge)) %>%
  select(Site.x,Time.x,LocationName.x,analyte, Value.x,Discharge) %>%
  rename('Site' = 'Site.x', 'Time'='Time.x', 'LocationName' = 'LocationName.x', 'Value' = 'Value.x')

write.csv(WQ_Data, file = "DF_for_Trends.csv",row.names = F)
