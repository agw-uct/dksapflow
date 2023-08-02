# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 
# Process plant pulse sapflow logger
# Read in csv files and extract hr values and plot
# Date : 02/09/2023
# Author : Adam West
# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

library(lubridate)
library(tidyverse)
library(ggpubr)

#### Set paths  ----------------------------------------------------------------- ####
mainDir <- dirname(rstudioapi::getActiveDocumentContext()$path)
mainDir
sapflowData_path          <- paste0(mainDir,"/data/raw data/")
output_path               <- paste0(mainDir,'/outputs/')


####List directories####
directory <- list.dirs(sapflowData_path,full.names = T)[-1]
metadata_path <- list.files(paste0(sapflowData_path,"/"), pattern = "\\.csv$", full.names = F)

#### get species names ####
species_names <- read.csv(paste0(sapflowData_path,"/",metadata_path),header=TRUE)  %>%  # get data from .csv file
  rename(lora_id = lora.id..last.three.) %>%
  select(lora_id, species)


#### merge .csv files for each plant ####

j<-1
i<-1

for (j in 1:length(directory)) {        #loop through directories
  file_names <- list.files(directory[j], pattern = "\\.csv$", full.names = F)   # find csv files
  
  for (i in 1:length(file_names)) {         # loop through files
    raw <- read.csv(paste0(directory[j],"/",file_names[i]),header=FALSE)     # get data from .csv file
    
    # wrangle data to extract heat ratio data and battery voltage from raw data
      raw1 <- raw %>%    
      mutate(flag = ifelse(str_detect(raw[,1],"heatratio average"),TRUE,FALSE)) %>%    #set flag to TRUE for heat ratio data per measurement cycle
      mutate(hr = ifelse(flag == TRUE,str_trim(str_split(raw[,1],":",simplify = TRUE)[,2]),0)) %>%   #get heat ratio when flag is TRUE
      mutate(d = ifelse(flag == TRUE,lag(raw[,1],n=2L),0)) %>%    # get Date when flag is TRUE
      mutate(t = ifelse(flag == TRUE,lag(raw[,2],n=2L),0)) %>%    # get Time when flag is TRUE
      mutate(battv = as.numeric(ifelse(flag == TRUE,lag(raw[,3],n=2L),0))) %>%   # get Batt volt when flag is TRUE
      filter(flag == TRUE) %>%      # get rid of unnecessary lines
      mutate(hr = as.numeric(hr)) %>%   
      mutate(d2 = ifelse(str_length(d)==7,str_c(str_sub(d, 1, 4),"0", str_sub(d, -3)),
                         ifelse(str_length(d)==6,str_c(str_sub(d, 1, 4),"0", str_sub(d, 5,5),"0",str_sub(d, -1)),
                                d))) %>%     #insert zeros into date stamp as necessary
      separate(t,into = c("h", "m", "s"), sep=":") %>%   # seperate time into hours, mins, seconds
      mutate(h2 = ifelse(str_length(h)==1,str_c("0", h),h)) %>%    # insert zero in front of single digit hours
      mutate(datetime = ymd_hms(paste0(d2," ", h2, ":", m, ":", s))) %>%  # create date and time stamp
      select(d2, h2, m, s, datetime, hr, battv) %>%    # keep necessary columns
      rename(date=d2, hour=h2,min=m,sec=s)  %>%    #rename
      mutate(h3 = as.numeric(hour)) %>%              # make numeric hour
      mutate(sun = ifelse(h3 > 7 & h3 <19,1.25,0.8))   # make daylight proxy
    
    if (i==1) {dc <- raw1}
    else dc <- rbind(dc, raw1)
    
  }


#### ggplot figures ALL DATA ####
hr_plot<- ggplot(NULL)+
  geom_ribbon(data=dc,aes(x=datetime, ymin=min(sun),ymax=sun),col=NA, fill="yellow", alpha=0.3)+
  geom_line(data=dc,aes(x=datetime, y=hr)) +
  geom_point(data=dc,aes(x=datetime, y=hr, col=battv))  +
  ylim(0,2.5)

#hr_plot <- hr_plot + ggtitle(basename(directory[j]))

hr_plot <- hr_plot + ggtitle(species_names$species[species_names$lora_id == basename(directory[j])])

battv_plot<- ggplot(NULL)+
  geom_point(data=dc,aes(x=datetime, y=battv, col=battv)) 

plots <-ggarrange(hr_plot,battv_plot,ncol=1,common.legend=TRUE,legend="none",align="hv")
plots

ggsave(paste0(output_path,basename(directory[j]),".pdf"),width=20,height=10,units="cm",dpi=600)



#### ggplot subset data   #####
dc_sub <- dc %>%
  filter(datetime > ymd_hms("20230701 00:00:00")& datetime < ymd_hms("20230801 12:00:00"))

#ggplot figure
hr_plot_s<- ggplot(NULL)+
  geom_ribbon(data=dc_sub,aes(x=datetime, ymin=min(sun),ymax=sun),col=NA, fill="yellow", alpha=0.3)+
  geom_line(data=dc_sub,aes(x=datetime, y=hr)) +
  geom_point(data=dc_sub,aes(x=datetime, y=hr, col=battv))  +
  ylim(0,2.5)

hr_plot_s <- hr_plot_s + ggtitle(species_names$species[species_names$lora_id == basename(directory[j])])

battv_plot_s <- ggplot(NULL)+
  geom_point(data=dc_sub,aes(x=datetime, y=battv, col=battv)) 
plots_s <-ggarrange(hr_plot_s,battv_plot_s,ncol=1,common.legend=TRUE,legend="none",align="hv")
plots_s

ggsave(paste0(output_path,"subset_",basename(directory[j]),".pdf"),width=20,height=10,units="cm",dpi=600)


}



