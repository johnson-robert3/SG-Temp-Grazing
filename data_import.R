#~~~
# Script to import data sets archived in the Environmental Data Initiative repository.
#
# Code provided by the Environmental Data Initiative
#~~~

### Run the following code to import all data sets used in tables, figures, and analyses in the manuscript. 


# Package ID: edi.1206.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Seagrass growth rates and physical characteristics and measures of water temperature and salinity during a simulated green turtle grazing experiment in The Bahamas, 1999 â 2000..
# Data set creator: Dr.  Robert Johnson - University of Wisconsin-Madison 
# Data set creator: Dr.  Kathleen Hanes - Washington University in St. Louis 
# Data set creator: Dr. Alan Bolten - University of Florida 
# Data set creator: Dr. Karen Bjorndal - University of Florida 
# Contact: Dr. Robert Johnson -  University of Wisconsin-Madison  - robert.a.johnson@wisc.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1206/1/a17f55a21b34590d9aa59a8bb4c653d7" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "plot",     
                    "treatment",     
                    "date",     
                    "interval",     
                    "exp_week",     
                    "ag_biomass"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$plot)=="factor") dt1$plot <-as.numeric(levels(dt1$plot))[as.integer(dt1$plot) ]               
if (class(dt1$plot)=="character") dt1$plot <-as.numeric(dt1$plot)
if (class(dt1$treatment)!="factor") dt1$treatment<- as.factor(dt1$treatment)                                   
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date) 
if (class(dt1$interval)=="factor") dt1$interval <-as.numeric(levels(dt1$interval))[as.integer(dt1$interval) ]               
if (class(dt1$interval)=="character") dt1$interval <-as.numeric(dt1$interval)
if (class(dt1$exp_week)=="factor") dt1$exp_week <-as.numeric(levels(dt1$exp_week))[as.integer(dt1$exp_week) ]               
if (class(dt1$exp_week)=="character") dt1$exp_week <-as.numeric(dt1$exp_week)
if (class(dt1$ag_biomass)=="factor") dt1$ag_biomass <-as.numeric(levels(dt1$ag_biomass))[as.integer(dt1$ag_biomass) ]               
if (class(dt1$ag_biomass)=="character") dt1$ag_biomass <-as.numeric(dt1$ag_biomass)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(plot)
summary(treatment)
summary(date)
summary(interval)
summary(exp_week)
summary(ag_biomass) 
                # Get more details on character variables
                 
summary(as.factor(dt1$treatment))
detach(dt1)               
         

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/1206/1/cc7aa1d4857450ce8b22d54d037eb4bc" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")

                   
 dt2 <-read.csv(infile2,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "plot",     
                    "treatment",     
                    "date",     
                    "interval",     
                    "exp_week",     
                    "gr_length"    ), check.names=TRUE)
               
unlink(infile2)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt2$plot)=="factor") dt2$plot <-as.numeric(levels(dt2$plot))[as.integer(dt2$plot) ]               
if (class(dt2$plot)=="character") dt2$plot <-as.numeric(dt2$plot)
if (class(dt2$treatment)!="factor") dt2$treatment<- as.factor(dt2$treatment)                                   
# attempting to convert dt2$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2date<-as.Date(dt2$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2date) == length(tmp2date[!is.na(tmp2date)])){dt2$date <- tmp2date } else {print("Date conversion failed for dt2$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2date) 
if (class(dt2$interval)=="factor") dt2$interval <-as.numeric(levels(dt2$interval))[as.integer(dt2$interval) ]               
if (class(dt2$interval)=="character") dt2$interval <-as.numeric(dt2$interval)
if (class(dt2$exp_week)=="factor") dt2$exp_week <-as.numeric(levels(dt2$exp_week))[as.integer(dt2$exp_week) ]               
if (class(dt2$exp_week)=="character") dt2$exp_week <-as.numeric(dt2$exp_week)
if (class(dt2$gr_length)=="factor") dt2$gr_length <-as.numeric(levels(dt2$gr_length))[as.integer(dt2$gr_length) ]               
if (class(dt2$gr_length)=="character") dt2$gr_length <-as.numeric(dt2$gr_length)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(plot)
summary(treatment)
summary(date)
summary(interval)
summary(exp_week)
summary(gr_length) 
                # Get more details on character variables
                 
summary(as.factor(dt2$treatment))
detach(dt2)               
         

inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/1206/1/53cd073fddb2350c7c30377fa06e9691" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")

                   
 dt3 <-read.csv(infile3,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "plot",     
                    "treatment",     
                    "date",     
                    "interval",     
                    "exp_week",     
                    "gr_mass"    ), check.names=TRUE)
               
unlink(infile3)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt3$plot)=="factor") dt3$plot <-as.numeric(levels(dt3$plot))[as.integer(dt3$plot) ]               
if (class(dt3$plot)=="character") dt3$plot <-as.numeric(dt3$plot)
if (class(dt3$treatment)!="factor") dt3$treatment<- as.factor(dt3$treatment)                                   
# attempting to convert dt3$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp3date<-as.Date(dt3$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp3date) == length(tmp3date[!is.na(tmp3date)])){dt3$date <- tmp3date } else {print("Date conversion failed for dt3$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp3date) 
if (class(dt3$interval)=="factor") dt3$interval <-as.numeric(levels(dt3$interval))[as.integer(dt3$interval) ]               
if (class(dt3$interval)=="character") dt3$interval <-as.numeric(dt3$interval)
if (class(dt3$exp_week)=="factor") dt3$exp_week <-as.numeric(levels(dt3$exp_week))[as.integer(dt3$exp_week) ]               
if (class(dt3$exp_week)=="character") dt3$exp_week <-as.numeric(dt3$exp_week)
if (class(dt3$gr_mass)=="factor") dt3$gr_mass <-as.numeric(levels(dt3$gr_mass))[as.integer(dt3$gr_mass) ]               
if (class(dt3$gr_mass)=="character") dt3$gr_mass <-as.numeric(dt3$gr_mass)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(plot)
summary(treatment)
summary(date)
summary(interval)
summary(exp_week)
summary(gr_mass) 
                # Get more details on character variables
                 
summary(as.factor(dt3$treatment))
detach(dt3)               
         

inUrl4  <- "https://pasta.lternet.edu/package/data/eml/edi/1206/1/4185a9c02f6d65b151b71f6286a8a8af" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")

                   
 dt4 <-read.csv(infile4,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "plot",     
                    "treatment",     
                    "date",     
                    "interval",     
                    "exp_week",     
                    "blade_length",     
                    "blade_width",     
                    "shoot_density",     
                    "bps",     
                    "lai"    ), check.names=TRUE)
               
unlink(infile4)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt4$plot)=="factor") dt4$plot <-as.numeric(levels(dt4$plot))[as.integer(dt4$plot) ]               
if (class(dt4$plot)=="character") dt4$plot <-as.numeric(dt4$plot)
if (class(dt4$treatment)!="factor") dt4$treatment<- as.factor(dt4$treatment)                                   
# attempting to convert dt4$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp4date<-as.Date(dt4$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp4date) == length(tmp4date[!is.na(tmp4date)])){dt4$date <- tmp4date } else {print("Date conversion failed for dt4$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp4date) 
if (class(dt4$interval)=="factor") dt4$interval <-as.numeric(levels(dt4$interval))[as.integer(dt4$interval) ]               
if (class(dt4$interval)=="character") dt4$interval <-as.numeric(dt4$interval)
if (class(dt4$exp_week)=="factor") dt4$exp_week <-as.numeric(levels(dt4$exp_week))[as.integer(dt4$exp_week) ]               
if (class(dt4$exp_week)=="character") dt4$exp_week <-as.numeric(dt4$exp_week)
if (class(dt4$blade_length)=="factor") dt4$blade_length <-as.numeric(levels(dt4$blade_length))[as.integer(dt4$blade_length) ]               
if (class(dt4$blade_length)=="character") dt4$blade_length <-as.numeric(dt4$blade_length)
if (class(dt4$blade_width)=="factor") dt4$blade_width <-as.numeric(levels(dt4$blade_width))[as.integer(dt4$blade_width) ]               
if (class(dt4$blade_width)=="character") dt4$blade_width <-as.numeric(dt4$blade_width)
if (class(dt4$shoot_density)=="factor") dt4$shoot_density <-as.numeric(levels(dt4$shoot_density))[as.integer(dt4$shoot_density) ]               
if (class(dt4$shoot_density)=="character") dt4$shoot_density <-as.numeric(dt4$shoot_density)
if (class(dt4$bps)=="factor") dt4$bps <-as.numeric(levels(dt4$bps))[as.integer(dt4$bps) ]               
if (class(dt4$bps)=="character") dt4$bps <-as.numeric(dt4$bps)
if (class(dt4$lai)=="factor") dt4$lai <-as.numeric(levels(dt4$lai))[as.integer(dt4$lai) ]               
if (class(dt4$lai)=="character") dt4$lai <-as.numeric(dt4$lai)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt4)                            
attach(dt4)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(plot)
summary(treatment)
summary(date)
summary(interval)
summary(exp_week)
summary(blade_length)
summary(blade_width)
summary(shoot_density)
summary(bps)
summary(lai) 
                # Get more details on character variables
                 
summary(as.factor(dt4$treatment))
detach(dt4)               
         

inUrl5  <- "https://pasta.lternet.edu/package/data/eml/edi/1206/1/6f60da82429f4b24a92425ba0d164f9f" 
infile5 <- tempfile()
try(download.file(inUrl5,infile5,method="curl"))
if (is.na(file.size(infile5))) download.file(inUrl5,infile5,method="auto")

                   
 dt5 <-read.csv(infile5,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "treatment",     
                    "date",     
                    "exp_week",     
                    "min_temp",     
                    "max_temp",     
                    "salinity"    ), check.names=TRUE)
               
unlink(infile5)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt5$treatment)!="factor") dt5$treatment<- as.factor(dt5$treatment)                                   
# attempting to convert dt5$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp5date<-as.Date(dt5$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp5date) == length(tmp5date[!is.na(tmp5date)])){dt5$date <- tmp5date } else {print("Date conversion failed for dt5$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp5date) 
if (class(dt5$exp_week)=="factor") dt5$exp_week <-as.numeric(levels(dt5$exp_week))[as.integer(dt5$exp_week) ]               
if (class(dt5$exp_week)=="character") dt5$exp_week <-as.numeric(dt5$exp_week)
if (class(dt5$min_temp)=="factor") dt5$min_temp <-as.numeric(levels(dt5$min_temp))[as.integer(dt5$min_temp) ]               
if (class(dt5$min_temp)=="character") dt5$min_temp <-as.numeric(dt5$min_temp)
if (class(dt5$max_temp)=="factor") dt5$max_temp <-as.numeric(levels(dt5$max_temp))[as.integer(dt5$max_temp) ]               
if (class(dt5$max_temp)=="character") dt5$max_temp <-as.numeric(dt5$max_temp)
if (class(dt5$salinity)=="factor") dt5$salinity <-as.numeric(levels(dt5$salinity))[as.integer(dt5$salinity) ]               
if (class(dt5$salinity)=="character") dt5$salinity <-as.numeric(dt5$salinity)
                
# Convert Missing Values to NA for non-dates
                
dt5$salinity <- ifelse((trimws(as.character(dt5$salinity))==trimws("NA")),NA,dt5$salinity)               
suppressWarnings(dt5$salinity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt5$salinity))==as.character(as.numeric("NA"))),NA,dt5$salinity))


# Here is the structure of the input data frame:
str(dt5)                            
attach(dt5)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(treatment)
summary(date)
summary(exp_week)
summary(min_temp)
summary(max_temp)
summary(salinity) 
                # Get more details on character variables
                 
summary(as.factor(dt5$treatment))
detach(dt5)               
        




