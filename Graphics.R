#libraries 

library(ggplot2)
library(dplyr)
library(Hmisc)

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
#           - menu graphs   1 = Graphs only variable
#                           2 = Graphs temperature maximun and minimum
#Return    - graph of station 

Graph_station <- function (name_station, variable, period=NULL, menu)
{
  
  
  if(variable == "ESOL")
  {
    y <- "calorias_cm2_diarios"
    title <- "Radiación Solar Promedio"
  }
  
  if(variable == "RAIN")
  {
    y <- "Mililitros"
    title <- "Precipitación Acumulada"
  }
  
  if(variable == "RHUM")
  {
    y <- "Valor"
    title <- "Humedad Relativa Promedio"
  }
  
  if(variable == "TMAX")
  {
    y <- "Grados_Centigrados"
    title <- "Temperatura Máxima Promedio"
  }
  
  
  if(variable == "TMIN")
  {
    y <- "Grados_Centigrados"
    title <- "Temperatura Mínima Promedio"
  }
  
  x <- "Dias"
  
  
  #Read file
  file <- read.table(list.files(here::here('Data'),full.names = T, pattern = paste0(name_station, "_", variable)), header=T)
  
  #Format
  file$Dates <- as.Date(as.character(file$Dates), format = "%Y-%m-%d")
  file$Value <- as.numeric(file$Value)
  
  #Minimun and maximun value
  min_value <- min(file$Dates)
  max_value <- max(file$Dates)
  
  #Change per month 
  file$Dates <- format(file$Dates, "%m")
  aux <- plyr::ddply(file, ~Dates,summarise,mean=mean(Value))
  
  months_aux <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  
  
  #Change number per month
   aux$Dates[aux$Dates=="01"] <- "Ene"
   aux$Dates[aux$Dates=="02"] <- "Feb"
   aux$Dates[aux$Dates=="03"] <- "Mar"
   aux$Dates[aux$Dates=="04"] <- "Abr"
   aux$Dates[aux$Dates=="05"] <- "May"
   aux$Dates[aux$Dates=="06"] <- "Jun"
   aux$Dates[aux$Dates=="07"] <- "Jul"
   aux$Dates[aux$Dates=="08"] <- "Ago"
   aux$Dates[aux$Dates=="09"] <- "Sep"
   aux$Dates[aux$Dates=="10"] <- "Oct"
   aux$Dates[aux$Dates=="11"] <- "Nov"
   aux$Dates[aux$Dates=="12"] <- "Dic"
   
   aux <- aux[order(match(aux$Dates, months_aux )),]
   aux <- within(aux, Dates <- factor(Dates, levels=(months_aux)))
 

  
  #Data
  colnames(file)[3] <- c("Data")
  levels(file$Data) <- c("Datos_Reales", "Datos_Estimados")
  
  
  #Plot
  #ggplot(file, aes(x=Dates, y=Value, colour= Data)) + geom_point() + labs(y = y, x = x ) +
  #ggtitle(paste0("Estación ", name_station, "\n", title))+ theme(plot.title = element_text(hjust = 0.5))
  #ggsave(paste0("./Graphics/",name_station, "_", variable, ".jpg"))
  
  if (menu == 1)
  {
    ggplot(aux, aes(x=Dates, y=mean, group=1))  + geom_line(color="blue")+
    geom_point(color="red") + labs(y = y, x = x ) +
    ggtitle(paste0("Estación ", name_station, "\n", title, "\n", "Durante el periodo ", min_value, " y ", max_value  ))+ theme(plot.title = element_text(hjust = 0.5))
    ggsave(paste0("./Graphics/",name_station, "_", variable, ".jpg"))
    
  }
  
  if(menu== 2)
  {
    
  }
 
  
  
  
  
  return (aux)
  
  
}

 
