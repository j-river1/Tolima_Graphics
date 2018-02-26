#libraries 

library(ggplot2)


#Read file and put format

list_files <- lapply(list.files(here::here('Data'),full.names = T), function (x)
                      {
                        #Read file
                        file <-  read.table(x, header=T)
                        #Format Date
                        
                        file$Dates <- as.Date(as.character(file$Dates), format = "%Y-%m-%d")
                        file$Value <- as.numeric(file$Value)
                        
                        return(file)
                        })


#Graph_station plots variable each station 
#Arguments  - name_station  = name of station e.g. (21255160, MixIbague, Piedra) 
#                           
#           - variables ESOL = Solar Irradiation
#                       RAIN = Precipitation
#                       RHUM = Relative Humidity
#                       TMAX = Maximun Temperature
#                       TMIN = Minimun Temperature
#           - period         = period of graphic
#Return    - graph of station 

Graph_station <- function (name_station, variable, period=NULL)
{
  #Read file
  file <- read.table(list.files(here::here('Data'),full.names = T, pattern = paste0(name_station, "_", variable)), header=T)
  
  #Format
  file$Dates <- as.Date(as.character(file$Dates), format = "%Y-%m-%d")
  file$Value <- as.numeric(file$Value)
  
  #Plot
  
  
  return (file)
  
  
}


