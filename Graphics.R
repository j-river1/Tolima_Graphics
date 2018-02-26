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
  
  if(variable == "ESOL")
  {
    y <- "calorias_cm2_diarios"
    title <- "Radiación Solar"
  }
  
  if(variable == "RAIN")
  {
    y <- "Mililitros"
    title <- "Precipitación"
  }
  
  if(variable == "RHUM")
  {
    y <- "Valor"
    title <- "Humedad Relativa"
  }
  
  if(variable == "TMAX")
  {
    y <- "Grados_Centigrados"
    title <- "Temperatura Mínima"
  }
  
  
  if(variable == "TMIN")
  {
    y <- "Grados_Centigrados"
    title <- "Temperatura Máxima"
  }
  
  x <- "Dias"
  
  
  #Read file
  file <- read.table(list.files(here::here('Data'),full.names = T, pattern = paste0(name_station, "_", variable)), header=T)
  
  #Format
  file$Dates <- as.Date(as.character(file$Dates), format = "%Y-%m-%d")
  file$Value <- as.numeric(file$Value)
  
  #Data
  colnames(file)[3] <- c("Data")
  levels(file$Data) <- c("Datos_Reales", "Datos_Estimados")
  
  
  #Plot
  ggplot(file, aes(x=Dates, y=Value, colour= Data)) + geom_point() + labs(y = y, x = x ) +
  ggtitle(paste0("Estación ", name_station, "\n", title))+ theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste0("./Graphics/",name_station, "_", variable, ".jpg"))
  return (file)
  
  
}


