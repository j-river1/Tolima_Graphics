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
#Arguments  - name_station  = name of station 
#           - variables ESOL = Solar Irradiation
#                       RAIN = Precipitation
#                       RHUM = Relative Humidity
#                       TMAX = Maximun Temperature
#                       TMIN = Minimun Temperature
#Return    - graph of station 

Graph_station


