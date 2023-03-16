

################
#Paquetes a ejecutar - recordar instalarlos previamente por medio de:
### install.packages("nombre")
library(vegan)
library(reshape)
library(tidyverse)
library(reshape2)


##########################################################################
##### Elegimos una carpeta donde extraer y grabar todo
setwd("G:/biato/Dagma-Icesi/R_dagma/Aves")
############
#abrimos la el .CSV - debes remplazar el nombre a continuacion
Bi=read.csv("Datos aves (1).csv", header=T, sep=",", dec=".")
View(Bi)
### Asegurate de que se vean bien las separaciones entre columnas con el siguiente
head(Bi,2)
#####################################################################
#df<-Bi %>% group_by(locality) %>% count(specie )
#head(df)
#####################################################################

################### Extraemos cuantas especies hay por localidad y dia de muestreo
df<-Bi %>% group_by(day, locality ) %>% count(species )
######## asegurate que se ve bien##########
head(df, 3)
#view(df)

########################## reordenamos las columnas por especie
df.wide <- pivot_wider(df, names_from = species, values_from =  n) 
############## cambiamos los NA por 0 cuando no hay presencia de la especie
df.wide[is.na(df.wide)] <- 0
############################### Reviza que todo este bien a continuacion
head(df.wide, 5)
#######

################################ creamos dos data frame diferentes

####### este es para la curva de acumulacion general de todo el parque
df2=data.frame(df.wide)
dfA<- df2 %>% select(-c(locality, day))
head(dfA,2)
####### este es para los indices beta dependiendo la cobertura
dfB<- df2 %>% select(c(locality, day))
head(dfB,2)

################# Analisis de indicadores por cobertura general
###### Anotalos paea tu informe crack
attach(dfB)
pool_2 <- specpool(dfA, locality)
pool_2
write.csv(pool_2, "Diversidad_beta_general.csv", sep=",", dec=".")
################# Analisis de indicadores de todo el parque en general
###### corre esto derecho hasta obtener tu grafica 
pool_3 <- specpool(dfA)
write.csv(pool_3, "Diversidad_beta_general_total.csv", sep=",", dec=".")

pool_1 <- poolaccum(dfA)
datas_example<-summary(pool_1, display = "chao")
typeof(pool_1)
names(pool_1)
####
chao <- data.frame(summary(pool_1)$chao,check.names = FALSE)
colnames(chao) <- c("N", "Chao", "lower2.5", "higher97.5", "std")
chao_melt <- melt(chao, id.vars = c("N","std", "lower2.5", "higher97.5"))
####
jack_1 <- data.frame(summary(pool_1)$jack1,check.names = FALSE)
colnames(jack_1) <- c("N", "jack1", "lower2.5", "higher97.5", "std")
jack1_melt <- melt(jack_1, id.vars = c("N","std", "lower2.5", "higher97.5"))
####
jack_2 <- data.frame(summary(pool_1)$jack2,check.names = FALSE)
colnames(jack_2) <- c("N", "jack2", "lower2.5", "higher97.5", "std")
jack2_melt <- melt(jack_2, id.vars = c("N","std", "lower2.5", "higher97.5"))
####
s <- data.frame(summary(pool_1)$S,check.names = FALSE)
colnames(s) <- c("N", "S", "lower2.5", "higher97.5", "std")
s_melt <- melt(s, id.vars = c("N","std", "lower2.5", "higher97.5"))
####
Bootstrap <- data.frame(summary(pool_1)$boot,check.names = FALSE)
colnames(Bootstrap) <- c("N", "boot", "lower2.5", "higher97.5", "std")
Bootstrap_melt <- melt(Bootstrap, id.vars = c("N","std", "lower2.5", "higher97.5"))

############################
#####################################3 Detenteeeeeeeeeeeeeeeee
#########################################3##### que te detengassssssss
#######
#### borra los indicadores que no necesites segun tus necesitades taxonomicas

df<- rbind(chao_melt,jack1_melt, jack2_melt, s_melt, Bootstrap_melt )
head(df)
############################
#####################################3
### debes eliminar de forma simultanea los siguientes colores
#####
#### ejemplo chao_melt posee el color #000000 y jack1_melt el color #E69F00
###### debes eliminar entonces ambos en caso que no los quieras en tu grafica
###### tanto el indicador como su color abajo
#########################################3##### 

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442")

########
########## Listo crack ahora solo es graficar
### corre lo siguiente sin mente

p<-df %>% ggplot(aes(x=N, y=value, ymin=lower2.5, ymax=higher97.5,
                             fill=variable, linetype=variable)) + 
  theme_bw(base_size = 18) + 
  geom_line(size = 1, alpha = 10) + 
  geom_ribbon(alpha=0.3) + 
  scale_x_log10() + 
  scale_y_log10() + 
  scale_fill_brewer(palette="Greens") +
  xlab("Días de Muestreo") + 
  ylab("Número de Especies") +
  theme(legend.position="bottom")
 
####### Tu grafica a continuacion:
p 

########## para exportar tu grafica en alta definicion y de forma estandar 
###### para todos los grupor porfa descarga el paquete "gridExtra" y ejecutalo

#install.packages("gridExtra")
#library(gridExtra)


######################################### este ya lo exporta con lo siguiente
########### cambia el nombre donde dice Example_1.png coloca por ejemplo
########### mamiferos_1.png o como desees crack
#ggsave(filename = "Aves_Beta.png",
#       plot = p, width = 18, height = 15, dpi = 350, units = "cm")

###### listo ya lo tienes, busca en la carpeta donde esta todo y veras tu grafica
##### :)
