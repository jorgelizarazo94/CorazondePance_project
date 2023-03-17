
################
#Paquetes a ejecutar - recordar instalarlos previamente por medio de:
### install.packages("nombre")
library(vegan)
library(iNEXT)
library(tidyverse)
library(data.table)
library(ggplot2)

##########################################################################
##### Elegimos una carpeta donde extraer y grabar todo
setwd("G:/biato/Dagma-Icesi/R_dagma/Aves/Alfa_chao2016")

############
#abrimos la el .CSV - debes remplazar el nombre a continuacion
Bi=read.csv("Datos aves (1).csv", header=T, sep=",", dec=".")
### Asegurate de que se vean bien las separaciones entre columnas con el siguiente
head(Bi,2)

### filtrar por covertura y contar numero de especies en cada zona 
df<-Bi %>% group_by(locality ) %>% count(species )
head(df)
#########################
#df %>% mutate(locality = "restauracion")
###########################
### transponer los datos por localidad con cantidad de cada especie
df$locality <- factor(df$locality, levels = c("bosque", "restauracion", "potrero"))
df.wide <- pivot_wider(df, names_from = locality, values_from = n )
############## cambiamos los NA por 0 cuando no hay presencia de la especie
df.wide[is.na(df.wide)] <- 0

################# Analisis de indicadores por cobertura general
###### Anotalos para tu informe crack
dataalfa<- ChaoRichness(df.wide[,c("bosque","restauracion", "potrero")])
write.csv(dataalfa, file = "datos_alfa_covertura_aves.csv", sep=",", dec=".")
##### dato general de todo el muestreo
vegan.out <-estimateD(df.wide$bosque)
vegan.out
### toma los datos 

####
################# clasificar datos de presencia en cada covertura como numerico 
################## para evitar errores posteriores en su clasificacion
df.wide<- transform(df.wide, bosque = as.numeric(bosque),
                restauracion = as.numeric(restauracion),
                potrero = as.numeric(potrero))

#################3
######### Extraer los datos para graficar
str(df.wide)
i.out <- iNEXT(df.wide[,c("bosque","restauracion", "potrero")], q=c(0, 1, 2),
               datatype="abundance")
z<-data.frame(i.out$DataInfo)  
x<-data.frame(i.out$iNextEst$size_based)
x2<-data.frame(i.out$iNextEst$coverage_based)
y<-  data.frame(i.out$AsyEst)

write.csv(z, file = "DataInfo_gesamt_birds.csv")
write.csv(x, file = "size_based_gesamt_birds.csv")
write.csv(x2, file = "coverage_based_gesamt_birds.csv")
write.csv(y, file = "AsyEst_gesamt_birds.csv")



#g<-ggiNEXT(i.out, type=1, facet.var="order")
#g
g1<-i.out %>% ggiNEXT(type=1, facet.var="Order.q", color.var="Assemblage") +xlab("Número de Inidividuos") +
  ylab("Diversidad de especies")  +##theme(legend.position="bottom") +
  theme_bw(base_size = 18) + theme(legend.position="bottom") +
  scale_colour_manual(values=c("#698B22", "#8B5A00", "#548B54")) +
  scale_fill_manual(values=c("#698B22", "#8B5A00", "#548B54")) 


####### Tu grafica a continuacion:
g1



##############################################################

g2<-i.out %>% ggiNEXT(type=3,  facet.var="Order.q", color.var="Assemblage") +
  xlab("Cobertura de Muestreo") +
  ylab("Diversidad de Especies")  +##theme(legend.position="bottom") +
  theme_bw(base_size = 18) + theme(legend.position="bottom") +
  scale_colour_manual(values=c("#698B22", "#8B5A00", "#548B54")) +
  scale_fill_manual(values=c("#698B22", "#8B5A00", "#548B54")) 




####### Tu grafica a continuacion:
g2

########## para exportar tus graficas en alta definicion y de forma estandar 
###### para todos los grupor porfa descarga el paquete "gridExtra" y ejecutalo
library(gridExtra)
######################################### este ya lo exporta con lo siguiente
########### cambia el nombre donde dice Example_1.png coloca por ejemplo
########### mamiferos_1.png o como desees crack
ggsave(filename = "alfa_2_aves.png",
       plot = g1, width = 19, height = 15, dpi = 350, units = "cm")


ggsave(filename = "alfa_3_aves.png",
       plot = g2, width = 23, height = 15, dpi = 350, units = "cm")




######################################################################################
##### lo que viene a continuacion son para ensayar otras cosas
######## me avisan si se necesitan mas cosas y podemos ensayarlas...


#g<-i.out %>% ggiNEXT(type=1) +xlab("Número de Inidividuos") +
 # ylab("Diversidad de especies")  +##theme(legend.position="bottom") +
  #theme_bw(base_size = 18) + theme(legend.position="bottom") +
  #scale_colour_manual(values=c("#698B22", "#8B5A00", "#548B54")) +
  #scale_fill_manual(values=c("#698B22", "#8B5A00", "#548B54")) 


#g
#gb3 <- ggplot_build(g)
#gb3$data[[1]]$size <- 3
#gb3$data[[2]]$size <- 1
#gt3 <- ggplot_gtable(gb3)
#grid.draw(gt3)

#g<-ggiNEXT(i.out, type=1) 

#g + xlab("Número de Inidividuos") +
#  ylab("Diversidad de especies") +
#  theme_minimal(base_size = 12, base_family = "Georgia") +
#  geom_line(size = 0.2)



#######################################################

#df <- fortify(i.out, type=1)
#head(df)


#df.point <- df[which(df$method=="observed"),]
#df.line <- df[which(df$method!="observed"),]
#df.line$method <- factor(df.line$method,
 #                        c("interpolated", "extrapolated"),
  #                       c("interpolation", "extrapolation"))


#ggplot(df, aes(x=x, y=y, colour=Assemblage)) +
#  geom_point(aes(shape=Assemblage), size=5, data=df.point) +
 # geom_line(aes(linetype=method), lwd=1.5, data=df.line) +
  #geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
   #               fill=Assemblage, colour=NULL), alpha=0.2) +
  #labs(x="Number of individuals", y="Species diversity") +
  #theme(legend.position = "bottom",
   #     legend.title=element_blank(),
    #    text=element_text(size=18))


