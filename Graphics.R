#libraries 

library(ggplot2)
library(dplyr)
library(Hmisc)
library(ggpubr)

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
#Arguments  - name_station  = name of station e.g. (21255160, MixIbague, Piedras) 
#                           
#           - variables ESOL = Solar Irradiation
#                       RAIN = Precipitation
#                       RHUM = Relative Humidity
#                       TMAX = Maximun Temperature
#                       TMIN = Minimun Temperature
#           - period         = period of graphic
#           - menu graphs   1 = Graphs only variable
#                           2 = Graphs temperature maximun and minimum
#                           3 = Graphs precipitation and temperature
#                           4 = Graphs Maximun Temperature
#                           5 = Graphs Minimun Temperature
#                           6 = Graphs Precipitation Bars
#                           7 = Graphs Maximun and Minimun Temperature
#                           8 = Graphs Precipitation Lines
#Return    - graph of station 

Graph_station <- function (name_station, variable, period=NULL, menu)
{
  
  
  if(variable == "ESOL")
  {
    y <- "calorias_cm2_diarios"
    title <- "Radiación Solar Promedio Diario"
  }
  
  if(variable == "RAIN")
  {
    y <- "Mililitros"
    title <- "Precipitación Acumulada"
  }
  
  if(variable == "RHUM")
  {
    y <- "Valor"
    title <- "Humedad Relativa Promedio Diario"
  }
  
  if(variable == "TMAX")
  {
    y <- "Grados_Centigrados"
    title <- "Temperatura Máxima Promedio Diario"
  }
  
  
  if(variable == "TMIN")
  {
    y <- "Grados_Centigrados"
    title <- "Temperatura Mínima Promedio Diario"
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
   aux$mean <- round(aux$mean, digits = 0)

  
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
    geom_point(color="red") + labs(y = y, x = x ) + geom_text(aes(label=mean),hjust=0, vjust=0) + 
    theme(panel.background = element_blank())+ ggtitle(paste0("Estación ", name_station, "\n", title, "\n", "Durante el periodo ", min_value, " y ", max_value  ))+ theme(plot.title = element_text(hjust = 0.5)) +
    ggsave(paste0("./Graphics/",name_station, "_", variable, ".jpg"))
    
  }
  
  if(menu== 2)
  {
    
    values_temp_max <- read.table(list.files(here::here('Data'),full.names = T, pattern = paste0(name_station, "_", "TMAX")), header=T)
    values_temp_min <- read.table(list.files(here::here('Data'),full.names = T, pattern = paste0(name_station, "_", "TMIN")), header=T)
    
    #Format
    values_temp_max$Dates <- as.Date(as.character(values_temp_max$Dates), format = "%Y-%m-%d")
    values_temp_min$Dates <- as.Date(as.character(values_temp_min$Dates), format = "%Y-%m-%d")
    values_temp_max$Value <- as.numeric(values_temp_max$Value)
    values_temp_min$Value <- as.numeric(values_temp_min$Value)
    
    
    
    #Minimun and maximun value
    min_value <- min(values_temp_max$Dates)
    max_value <- max(values_temp_max$Dates)
    
    #Change per month 
    values_temp_max$Dates <- format(values_temp_max$Dates, "%m")
    values_temp_min$Dates <- format(values_temp_min$Dates, "%m")
    
    
    aux_max <- plyr::ddply(values_temp_max, ~Dates,summarise,mean=mean(Value))
    aux_min <- plyr::ddply(values_temp_min, ~Dates,summarise,mean=mean(Value))
    
    months_aux <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
    
    
    #Change number per month max
    aux_max$Dates[aux_max$Dates=="01"] <- "Ene"
    aux_max$Dates[aux_max$Dates=="02"] <- "Feb"
    aux_max$Dates[aux_max$Dates=="03"] <- "Mar"
    aux_max$Dates[aux_max$Dates=="04"] <- "Abr"
    aux_max$Dates[aux_max$Dates=="05"] <- "May"
    aux_max$Dates[aux_max$Dates=="06"] <- "Jun"
    aux_max$Dates[aux_max$Dates=="07"] <- "Jul"
    aux_max$Dates[aux_max$Dates=="08"] <- "Ago"
    aux_max$Dates[aux_max$Dates=="09"] <- "Sep"
    aux_max$Dates[aux_max$Dates=="10"] <- "Oct"
    aux_max$Dates[aux_max$Dates=="11"] <- "Nov"
    aux_max$Dates[aux_max$Dates=="12"] <- "Dic"
    
    #Change number per month max
    aux_min$Dates[aux_min$Dates=="01"] <- "Ene"
    aux_min$Dates[aux_min$Dates=="02"] <- "Feb"
    aux_min$Dates[aux_min$Dates=="03"] <- "Mar"
    aux_min$Dates[aux_min$Dates=="04"] <- "Abr"
    aux_min$Dates[aux_min$Dates=="05"] <- "May"
    aux_min$Dates[aux_min$Dates=="06"] <- "Jun"
    aux_min$Dates[aux_min$Dates=="07"] <- "Jul"
    aux_min$Dates[aux_min$Dates=="08"] <- "Ago"
    aux_min$Dates[aux_min$Dates=="09"] <- "Sep"
    aux_min$Dates[aux_min$Dates=="10"] <- "Oct"
    aux_min$Dates[aux_min$Dates=="11"] <- "Nov"
    aux_min$Dates[aux_min$Dates=="12"] <- "Dic"
    
    
    aux_max <- aux_max[order(match(aux_max$Dates, months_aux )),]
    aux_max <- within(aux_max, Dates <- factor(Dates, levels=(months_aux)))
    
    aux_min <- aux_min[order(match(aux_min$Dates, months_aux )),]
    aux_min <- within(aux_min, Dates <- factor(Dates, levels=(months_aux)))
    
    
    
    data <- data.frame (Tipo_Temperatura = factor(c(rep(c("Temperatura_Máxima"), 12), rep(c("Temperatura_Mínima"), 12))),
                        Mes = factor(rep(months_aux, 2), levels=months_aux ),
                        Values= c(round(as.numeric(aux_max$mean), digits = 0), round(as.numeric(aux_min$mean ), digits = 0)))
    
 
    ggplot(data, aes(x=Mes, y=Values, group=Tipo_Temperatura, shape=Tipo_Temperatura)) + geom_line(aes(col=Tipo_Temperatura)) + geom_point(aes(col=Tipo_Temperatura)) + geom_text(aes(label=Values),hjust=0, vjust=0)     + 
    ggtitle(paste0("Estación ", name_station, "\n", "Temperatura Máxima y Mínima Promedio", "\n", "Durante el periodo ", min_value, " y ", max_value)) +  theme(panel.background = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) + labs(y = "Grados Centígrados")  + theme(axis.line = element_line(colour = "black")) + 
    ggsave(paste0("./Graphics/",name_station, "_", "TMaxTmin", ".jpg"))
    
    
    
  }
 
  if(menu== 3)
  {
    values_temp_prec <- read.table(list.files(here::here('Data'),full.names = T, pattern = paste0(name_station, "_", "RAIN")), header=T)
    values_temp_max <- read.table(list.files(here::here('Data'),full.names = T, pattern = paste0(name_station, "_", "TMAX")), header=T)
    values_temp_min <- read.table(list.files(here::here('Data'),full.names = T, pattern = paste0(name_station, "_", "TMIN")), header=T)
    
    #Format
    values_temp_prec$Dates <- as.Date(as.character(values_temp_prec$Dates), format = "%Y-%m-%d")
    values_temp_max$Dates <- as.Date(as.character(values_temp_max$Dates), format = "%Y-%m-%d")
    values_temp_min$Dates <- as.Date(as.character(values_temp_min$Dates), format = "%Y-%m-%d")
    values_temp_max$Value <- as.numeric(values_temp_max$Value)
    values_temp_min$Value <- as.numeric(values_temp_min$Value)
    values_temp_prec$Value <- as.numeric(values_temp_prec$Value)
    
    
    #Minimun and maximun value
    min_value <- min(values_temp_prec$Dates)
    max_value <- max(values_temp_prec$Dates)
    
    #Change per month 
    values_temp_prec$Dates <- format(values_temp_prec$Dates, "%m")
    values_temp_max$Dates <- format(values_temp_max$Dates, "%m")
    values_temp_min$Dates <- format(values_temp_min$Dates, "%m")
    
    aux_prec <- plyr::ddply(values_temp_prec, ~Dates,summarise,suma=sum(Value))
    aux_max <- plyr::ddply(values_temp_max, ~Dates,summarise,mean=mean(Value))
    aux_min <- plyr::ddply(values_temp_min, ~Dates,summarise,mean=mean(Value))
    
    months_aux <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
    
    #Change number per month max
    aux_max$Dates[aux_max$Dates=="01"] <- "Ene"
    aux_max$Dates[aux_max$Dates=="02"] <- "Feb"
    aux_max$Dates[aux_max$Dates=="03"] <- "Mar"
    aux_max$Dates[aux_max$Dates=="04"] <- "Abr"
    aux_max$Dates[aux_max$Dates=="05"] <- "May"
    aux_max$Dates[aux_max$Dates=="06"] <- "Jun"
    aux_max$Dates[aux_max$Dates=="07"] <- "Jul"
    aux_max$Dates[aux_max$Dates=="08"] <- "Ago"
    aux_max$Dates[aux_max$Dates=="09"] <- "Sep"
    aux_max$Dates[aux_max$Dates=="10"] <- "Oct"
    aux_max$Dates[aux_max$Dates=="11"] <- "Nov"
    aux_max$Dates[aux_max$Dates=="12"] <- "Dic"
    
    #Change number per month max
    aux_min$Dates[aux_min$Dates=="01"] <- "Ene"
    aux_min$Dates[aux_min$Dates=="02"] <- "Feb"
    aux_min$Dates[aux_min$Dates=="03"] <- "Mar"
    aux_min$Dates[aux_min$Dates=="04"] <- "Abr"
    aux_min$Dates[aux_min$Dates=="05"] <- "May"
    aux_min$Dates[aux_min$Dates=="06"] <- "Jun"
    aux_min$Dates[aux_min$Dates=="07"] <- "Jul"
    aux_min$Dates[aux_min$Dates=="08"] <- "Ago"
    aux_min$Dates[aux_min$Dates=="09"] <- "Sep"
    aux_min$Dates[aux_min$Dates=="10"] <- "Oct"
    aux_min$Dates[aux_min$Dates=="11"] <- "Nov"
    aux_min$Dates[aux_min$Dates=="12"] <- "Dic"
    
    
    #Change number per month prec
    aux_prec$Dates[aux_prec$Dates=="01"] <- "Ene"
    aux_prec$Dates[aux_prec$Dates=="02"] <- "Feb"
    aux_prec$Dates[aux_prec$Dates=="03"] <- "Mar"
    aux_prec$Dates[aux_prec$Dates=="04"] <- "Abr"
    aux_prec$Dates[aux_prec$Dates=="05"] <- "May"
    aux_prec$Dates[aux_prec$Dates=="06"] <- "Jun"
    aux_prec$Dates[aux_prec$Dates=="07"] <- "Jul"
    aux_prec$Dates[aux_prec$Dates=="08"] <- "Ago"
    aux_prec$Dates[aux_prec$Dates=="09"] <- "Sep"
    aux_prec$Dates[aux_prec$Dates=="10"] <- "Oct"
    aux_prec$Dates[aux_prec$Dates=="11"] <- "Nov"
    aux_prec$Dates[aux_prec$Dates=="12"] <- "Dic"
    
    
    aux_max <- aux_max[order(match(aux_max$Dates, months_aux )),]
    aux_max <- within(aux_max, Dates <- factor(Dates, levels=(months_aux)))
    
    aux_min <- aux_min[order(match(aux_min$Dates, months_aux )),]
    aux_min <- within(aux_min, Dates <- factor(Dates, levels=(months_aux)))
    
    aux_prec <- aux_prec[order(match(aux_prec$Dates, months_aux )),]
    aux_prec <- within(aux_prec, Dates <- factor(Dates, levels=(months_aux)))    
    
    aux_prec$temp_mean <- (aux_max$mean + aux_min$mean)/2
    
    
    
    data <- data.frame(Months = seq(1:12), Values_Preci = as.numeric(aux_prec$suma), Values_Temp = as.numeric(aux_prec$temp_mean))
    
    jpeg(paste0("./Graphics/",name_station, "_", "Preci_Tempe", ".jpg"), width = 7, height = 7, units = "in", res=90)
    par(mar=c(5,5,2,5))
    matrix_grap <- matrix(nrow=1, ncol=12)
    colnames(matrix_grap) <- months_aux
    matrix_grap[1,] <- as.numeric(aux_prec$suma) 
    barplot(as.numeric(aux_prec$suma), col= "blue", names.arg= months_aux, ylim= c(0, max(aux_prec$suma)), ylab = "Milimetros", cex.names=0.8, main = paste0("Diagrama de la Temperatura y Precipitación Promedio", "\n", "Estacion ", name_station, " Durante ", min_value, " y ", max_value), cex.main= 0.8 )
    par(new = T)
    with(data, plot(Months, Values_Temp, type="b", pch=16,  axes=F, xlab=NA, ylab=NA, cex=1.2, col= "red", ylim = c(min(Values_Temp),max(Values_Temp))))
    axis(side = 4)
    mtext(side = 4, line = 3, text= 'Grados Centígrados', cex=1)
    legend("topleft",legend=c("Precipitación", "Temperatura"), lty=c(1,1), pch=c(15, 16), col=c("blue", "red"), cex = 0.8)  
    dev.off()
    
  }
  
  if(menu == 4)
  {
    
    values_temp_max <- read.table(list.files(here::here('Data'),full.names = T, pattern = paste0(name_station, "_", "TMAX")), header=T)
    
    #Format
    values_temp_max$Dates <- as.Date(as.character(values_temp_max$Dates), format = "%Y-%m-%d")
    values_temp_max$Value <- as.numeric(values_temp_max$Value)
    
    
    #Minimun and maximun value
    min_value <- min(values_temp_prec$Dates)
    max_value <- max(values_temp_prec$Dates)
    
    #Put year  
    values_temp_max$Year <- as.numeric(format(values_temp_max$Dates, "%Y"))
    

    #Change per month 
    values_temp_max$Dates <- format(values_temp_max$Dates, "%m")

    
    #Split per year 
    split_year <- split(values_temp_max,values_temp_max$Year)
    
    
    #Values per month
    change_names <- lapply(split_year, function(x)
                                       {
                                        year <- unique(as.numeric(x$Year))
                                        x <- plyr::ddply(x, ~Dates,summarise,mean=mean(Value))
                                        
                                        #Year 
                                        x$Year <- year
                                        
                                        #Change number per month prec
                                        x$Dates[x$Dates=="01"] <- "Ene"
                                        x$Dates[x$Dates=="02"] <- "Feb"
                                        x$Dates[x$Dates=="03"] <- "Mar"
                                        x$Dates[x$Dates=="04"] <- "Abr"
                                        x$Dates[x$Dates=="05"] <- "May"
                                        x$Dates[x$Dates=="06"] <- "Jun"
                                        x$Dates[x$Dates=="07"] <- "Jul"
                                        x$Dates[x$Dates=="08"] <- "Ago"
                                        x$Dates[x$Dates=="09"] <- "Sep"
                                        x$Dates[x$Dates=="10"] <- "Oct"
                                        x$Dates[x$Dates=="11"] <- "Nov"
                                        x$Dates[x$Dates=="12"] <- "Dic"                          
           
                                        x <- x[order(match(x$Dates, months_aux )),]
                                        x <- within(x, Dates <- factor(Dates, levels=(months_aux)))
                                        
                                        return(x)
                                        
                                       })
    
    #Joint Elements list
    join_list <- do.call("rbind", change_names)
    join_list$Year <- as.factor(join_list$Year)
    
    ggplot(join_list, aes(x=Dates, y=mean, colour= Year, group= Year)) + geom_line() + ggtitle(paste("Estación ", name_station, "\n", "Temperatura Máxima Promedio Mensual")) + theme(plot.title = element_text(hjust = 0.5)) +
    labs(y = "Grados Centígrados", x= "Mes")  
    
    ggsave(paste0("./Graphics/",name_station, "_", "TMaxTotal", ".jpg"))
    
  }
  
  if(menu == 5)
  {
    
    values_temp_min <- read.table(list.files(here::here('Data'),full.names = T, pattern = paste0(name_station, "_", "TMIN")), header=T)
    
    #Format
    values_temp_min$Dates <- as.Date(as.character(values_temp_min$Dates), format = "%Y-%m-%d")
    values_temp_min$Value <- as.numeric(values_temp_min$Value)
    
    #Put year  
    values_temp_min$Year <- as.numeric(format(values_temp_min$Dates, "%Y"))
    
    
    #Change per month 
    values_temp_min$Dates <- format(values_temp_min$Dates, "%m")
    
    
    #Split per year 
    split_year <- split(values_temp_min,values_temp_min$Year)
    
    
    #Values per month
    change_names <- lapply(split_year, function(x)
    {
      year <- unique(as.numeric(x$Year))
      x <- plyr::ddply(x, ~Dates,summarise,mean=mean(Value))
      
      #Year 
      x$Year <- year
      
      #Change number per month prec
      x$Dates[x$Dates=="01"] <- "Ene"
      x$Dates[x$Dates=="02"] <- "Feb"
      x$Dates[x$Dates=="03"] <- "Mar"
      x$Dates[x$Dates=="04"] <- "Abr"
      x$Dates[x$Dates=="05"] <- "May"
      x$Dates[x$Dates=="06"] <- "Jun"
      x$Dates[x$Dates=="07"] <- "Jul"
      x$Dates[x$Dates=="08"] <- "Ago"
      x$Dates[x$Dates=="09"] <- "Sep"
      x$Dates[x$Dates=="10"] <- "Oct"
      x$Dates[x$Dates=="11"] <- "Nov"
      x$Dates[x$Dates=="12"] <- "Dic"                          
      
      x <- x[order(match(x$Dates, months_aux )),]
      x <- within(x, Dates <- factor(Dates, levels=(months_aux)))
      
      return(x)
      
    })
    
    #Joint Elements list
    join_list <- do.call("rbind", change_names)
    join_list$Year <- as.factor(join_list$Year)
    
    ggplot(join_list, aes(x=Dates, y=mean, colour= Year, group= Year)) + geom_line() + ggtitle(paste("Estación ", name_station, "\n", "Temperatura Mínima Promedio Mensual")) + theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = "Grados Centígrados", x= "Mes")  
    
    ggsave(paste0("./Graphics/",name_station, "_", "TMinTotal", ".jpg"))
    
  }
  
  
  if(menu == 6)
  {
    
    values_temp_prec <- read.table(list.files(here::here('Data'),full.names = T, pattern = paste0(name_station, "_", "RAIN")), header=T)
    
    #Format
    values_temp_prec$Dates <- as.Date(as.character(values_temp_prec$Dates), format = "%Y-%m-%d")
    values_temp_prec$Value <- as.numeric(values_temp_prec$Value)
    
    #Put year  
    values_temp_prec$Year <- as.numeric(format(values_temp_prec$Dates, "%Y"))
    
    
    #Change per month 
    values_temp_prec$Dates <- format(values_temp_prec$Dates, "%m")
    
    
    #Split per year 
    split_year <- split(values_temp_prec,values_temp_prec$Year)
    
    
    #Values per month
    change_names <- lapply(split_year, function(x)
    {
      year <- unique(as.numeric(x$Year))
      x <- plyr::ddply(x, ~Dates,summarise,suma=sum(Value))
      
      #Year 
      x$Year <- year
      
      #Change number per month prec
      x$Dates[x$Dates=="01"] <- "Ene"
      x$Dates[x$Dates=="02"] <- "Feb"
      x$Dates[x$Dates=="03"] <- "Mar"
      x$Dates[x$Dates=="04"] <- "Abr"
      x$Dates[x$Dates=="05"] <- "May"
      x$Dates[x$Dates=="06"] <- "Jun"
      x$Dates[x$Dates=="07"] <- "Jul"
      x$Dates[x$Dates=="08"] <- "Ago"
      x$Dates[x$Dates=="09"] <- "Sep"
      x$Dates[x$Dates=="10"] <- "Oct"
      x$Dates[x$Dates=="11"] <- "Nov"
      x$Dates[x$Dates=="12"] <- "Dic"                          
      
      x <- x[order(match(x$Dates, months_aux )),]
      x <- within(x, Dates <- factor(Dates, levels=(months_aux)))
      
      return(x)
      
    })
    
    #Joint Elements list
    join_list <- do.call("rbind", change_names)
    join_list$Year <- as.factor(join_list$Year)
    
    #ggplot(join_list, aes(x=Dates, y=mean, colour= Year, group= Year)) + geom_bar() + ggtitle(paste("Estación ", name_station, "\n", "Precipitación Acumulada Mensual")) + theme(plot.title = element_text(hjust = 0.5)) +
    #  labs(y = "Milímetros", x= "Mes")  
    
    names <- colnames(join_list)[3] <- "Año"
    ggbarplot(join_list, x="Dates", y= "suma", fill= "Año", color = "white") + labs(y = "Milímetros", x= "Mes") + ggtitle(paste("Estación ", name_station, "\n", "Precipitación Acumulada Mensual")) + theme(plot.title = element_text(hjust = 0.5)) 
    
    ggsave(paste0("./Graphics/",name_station, "_", "PrecipTotal", ".jpg"))
    
  }
  
  if(menu == 7)
  {
    values_temp_max <- read.table(list.files(here::here('Data'),full.names = T, pattern = paste0(name_station, "_", "TMAX")), header=T)
    values_temp_min <- read.table(list.files(here::here('Data'),full.names = T, pattern = paste0(name_station, "_", "TMIN")), header=T)
    
    #Format
    values_temp_max$Dates <- as.Date(as.character(values_temp_max$Dates), format = "%Y-%m-%d")
    values_temp_max$Value <- as.numeric(values_temp_max$Value)
    values_temp_min$Dates <- as.Date(as.character(values_temp_min$Dates), format = "%Y-%m-%d")
    values_temp_min$Value <- as.numeric(values_temp_min$Value)
    
    
    #Minimun and maximun value
    min_value <- min(values_temp_max$Dates)
    max_value <- max(values_temp_max$Dates)
    
    #Put year  
    values_temp_max$Year <- as.numeric(format(values_temp_max$Dates, "%Y"))
    values_temp_min$Year <- as.numeric(format(values_temp_min$Dates, "%Y"))
    
    #Change per month 
    values_temp_max$Dates <- format(values_temp_max$Dates, "%m")
    values_temp_min$Dates <- format(values_temp_min$Dates, "%m")
    
    
    #Split per year 
    split_year_max <- split(values_temp_max,values_temp_max$Year)
    split_year_min <- split(values_temp_min,values_temp_min$Year)
    
    
    #Values per month
    change_names_max <- lapply(split_year_max, function(x)
    {
      year <- unique(as.numeric(x$Year))
      x <- plyr::ddply(x, ~Dates,summarise,mean=mean(Value))
      
      #Year 
      x$Year <- year
      
      #Change number per month prec
      x$Dates[x$Dates=="01"] <- "Ene"
      x$Dates[x$Dates=="02"] <- "Feb"
      x$Dates[x$Dates=="03"] <- "Mar"
      x$Dates[x$Dates=="04"] <- "Abr"
      x$Dates[x$Dates=="05"] <- "May"
      x$Dates[x$Dates=="06"] <- "Jun"
      x$Dates[x$Dates=="07"] <- "Jul"
      x$Dates[x$Dates=="08"] <- "Ago"
      x$Dates[x$Dates=="09"] <- "Sep"
      x$Dates[x$Dates=="10"] <- "Oct"
      x$Dates[x$Dates=="11"] <- "Nov"
      x$Dates[x$Dates=="12"] <- "Dic"                          
      
      x <- x[order(match(x$Dates, months_aux )),]
      x <- within(x, Dates <- factor(Dates, levels=(months_aux)))
      
      return(x)
      
    })
    
    change_names_min <- lapply(split_year_min, function(x)
    {
      year <- unique(as.numeric(x$Year))
      x <- plyr::ddply(x, ~Dates,summarise,mean=mean(Value))
      
      #Year 
      x$Year <- year
      
      #Change number per month prec
      x$Dates[x$Dates=="01"] <- "Ene"
      x$Dates[x$Dates=="02"] <- "Feb"
      x$Dates[x$Dates=="03"] <- "Mar"
      x$Dates[x$Dates=="04"] <- "Abr"
      x$Dates[x$Dates=="05"] <- "May"
      x$Dates[x$Dates=="06"] <- "Jun"
      x$Dates[x$Dates=="07"] <- "Jul"
      x$Dates[x$Dates=="08"] <- "Ago"
      x$Dates[x$Dates=="09"] <- "Sep"
      x$Dates[x$Dates=="10"] <- "Oct"
      x$Dates[x$Dates=="11"] <- "Nov"
      x$Dates[x$Dates=="12"] <- "Dic"                          
      
      x <- x[order(match(x$Dates, months_aux )),]
      x <- within(x, Dates <- factor(Dates, levels=(months_aux)))
      
      return(x)
      
    })
    
    
    #Joint Elements list
    join_list_min <- do.call("rbind", change_names_min)
    colnames(join_list_min)[2] <- "TMIN"
    join_list_max <- do.call("rbind", change_names_max)
    colnames(join_list_max)[2] <- "TMAX"
    
    #Join two list 
    join_list_final <- cbind(join_list_min,join_list_max)
    
    #Delete repetead columns 
    join_list_final <-join_list_final[, c(-4,-6)]
    
    #Year as factor
    join_list_final$Year <- as.factor(join_list_final$Year)
    
    
    #join_list_final$Year <- as.factor(join_list_final$Year)
    
    #ggplot(join_list_final, aes(x=Dates, y=mean, colour= Year, group= Year)) + geom_line() + ggtitle(paste("Estación ", name_station, "\n", "Temperatura Máxima Promedio Mensual")) + theme(plot.title = element_text(hjust = 0.5)) +
     # labs(y = "Grados Centígrados", x= "Mes")  
    
    ggplot(join_list_final, aes(x=Dates, colour= Year, group=Year)) + geom_line(aes(y=TMAX)) + geom_line(aes(y=TMIN)) +  ggtitle(paste("Estación ", name_station, "\n", "Temperatura Máxima Y Mínima Promedio Mensual")) + theme(plot.title = element_text(hjust = 0.5)) +
     labs(y = "Grados Centígrados", x= "Mes")  
    
    
    ggsave(paste0("./Graphics/",name_station, "_", "JoinTmaxTmin", ".jpg"))
   
    
  }
   
  if(menu == 8)
  {
    
    values_temp_prec <- read.table(list.files(here::here('Data'),full.names = T, pattern = paste0(name_station, "_", "RAIN")), header=T)
    
    #Format
    values_temp_prec$Dates <- as.Date(as.character(values_temp_prec$Dates), format = "%Y-%m-%d")
    values_temp_prec$Value <- as.numeric(values_temp_prec$Value)
    
    #Put year  
    values_temp_prec$Year <- as.numeric(format(values_temp_prec$Dates, "%Y"))
    
    
    #Change per month 
    values_temp_prec$Dates <- format(values_temp_prec$Dates, "%m")
    
    
    #Split per year 
    split_year <- split(values_temp_prec,values_temp_prec$Year)
    
    
    #Values per month
    change_names <- lapply(split_year, function(x)
    {
      year <- unique(as.numeric(x$Year))
      x <- plyr::ddply(x, ~Dates,summarise,suma=sum(Value))
      
      #Year 
      x$Year <- year
      
      #Change number per month prec
      x$Dates[x$Dates=="01"] <- "Ene"
      x$Dates[x$Dates=="02"] <- "Feb"
      x$Dates[x$Dates=="03"] <- "Mar"
      x$Dates[x$Dates=="04"] <- "Abr"
      x$Dates[x$Dates=="05"] <- "May"
      x$Dates[x$Dates=="06"] <- "Jun"
      x$Dates[x$Dates=="07"] <- "Jul"
      x$Dates[x$Dates=="08"] <- "Ago"
      x$Dates[x$Dates=="09"] <- "Sep"
      x$Dates[x$Dates=="10"] <- "Oct"
      x$Dates[x$Dates=="11"] <- "Nov"
      x$Dates[x$Dates=="12"] <- "Dic"                          
      
      x <- x[order(match(x$Dates, months_aux )),]
      x <- within(x, Dates <- factor(Dates, levels=(months_aux)))
      
      return(x)
      
    })
    
    #Joint Elements list
    join_list <- do.call("rbind", change_names)
    join_list$Year <- as.factor(join_list$Year)
    
    #ggplot(join_list, aes(x=Dates, y=mean, colour= Year, group= Year)) + geom_bar() + ggtitle(paste("Estación ", name_station, "\n", "Precipitación Acumulada Mensual")) + theme(plot.title = element_text(hjust = 0.5)) +
    #  labs(y = "Milímetros", x= "Mes")  
    
    colnames(join_list)[3] <- "Año"
    
    ggplot(join_list, aes(x=Dates, y = suma, colour= Año, group=Año)) + geom_line()  +  ggtitle(paste("Estación ", name_station, "\n", "Precipitación Acumulada Mensual")) + theme(plot.title = element_text(hjust = 0.5)) +
    labs(y = "Milímetros", x= "Mes") 
    
    
    
    ggsave(paste0("./Graphics/",name_station, "_", "PrecipTotalCurves", ".jpg"))
    
  } 
  
  
  
  return (join_list)
  
  
}

 
