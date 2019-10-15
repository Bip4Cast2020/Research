# 1. Crear un nuevo proyecto denominado practica 4.
# 2. Mediante la libreria readr, o mediante los menus de RStudio, leer los datasets sleep.csv  y activities.csv
# ambos archivos deben estar previamente en la carpeta del proyecto creado
library(readr)
sleep <- read_csv("sleep.csv")
actividades<-read_csv("activities.csv")

# 3.Comprobar el contenido  con View y contar cuantos NAs hay en la columna GPS del dataset activities

View(actividades, "Actividades")
sum(is.na(actividades$GPS))

# 4. Crear un objeto R denominado act_new que contenga solo las variables 
# siguientes: 1,2,5-6
act_new<-select(actividades, 1:2, 5:6)
act_new<-select(actividades, -(3:4),-(7:9))
act_new<-select(actividades, 1,2,5,6)

# 5. Renombrar la variable 'Activity type' con el nombre 'tipo' y la variable 'Time zone' como 'ciudad'
act_new<-rename(act_new, tipo='Activity type', ciudad='Timezone' )

# 6. Realizar un recuento de tipo de actividad con summary. Para ello 
# debes transformar previamente la variable tipo a factor con as.factor.
# Crea un grafico de barras con dicha variable par visualizar las frecuencias.
# Haz lo mismo para la variable ciudad

act_new$tipo<-as.factor(act_new$tipo)
summary(act_new$tipo)
plot(act_new$tipo, main="Actividades realizadas")
act_new$ciudad<-as.factor(act_new$ciudad)
summary(as.factor(act_new$ciudad))
plot(act_new$ciudad, main="Ciudades visitadas")

#7. Filtrar los registros de act_new que correspondan con ciudad Amsterdam en otro objeto
# y lo mismo con Madrid. Con esos nuevos objetos determina los deportes que 
# no se practican en Amsterdam y sí en Madrid y viceversa. Genera graficos para visualizar los resultados

act_new_Amsterdam<-filter(act_new, ciudad=="Europe/Amsterdam")
summary(act_new_Amsterdam$tipo)
plot(act_new_Amsterdam$tipo, main="Actividades realizadas en Amsterdam")

act_new_Madrid<-filter(act_new, ciudad=="Europe/Madrid")
summary(act_new_Madrid$tipo)
plot(act_new_Madrid$tipo, main="Actividades realizadas en Madrid")

#8. Encontrar las fechas en las que se ha practicado bicicleta o pilates en Amsterdam en el año 2019
filter(act_new, ciudad=="Europe/Amsterdam", tipo %in% c("Cycling", "Pilates"))


#9. Crear una nueva variable dif con los minutos de realización de cada actividad en Amsterdam
# y realizar una representación gráfica de los resultados con plot y determinar que deporte o deportes
# se han practicado durante dos horas o mas
act_new_Amsterdam<-mutate(act_new, dif=a-de)
plot(act_new_Amsterdam$tipo, act_new_Amsterdam$dif, xlab="deporte",ylab="minutos")

#10. Guardar el nuevo dataset en un archivo llamado  "act_new.csv"
write.csv(act_new, "act_new.csv")

#-------------------------------
#-----SEGUNDA PARTE-------------
# 11. Cargar el dataset sleep en un objeto llamado sleep
sleep<-read_csv("sleep.csv")
#12. crear un nuevo data set llamado sleep_new que contenga solo las variables
#que contengan información, que no sean todo cero.

sum(sleep$`Snoring (s)`, na.rm=TRUE) #compruebo que tiene info
sleep_new<-select(sleep, 1:4, 6:10)

#13. Renombrar las variables de sleep_new a nombres cortos:
sleep_new<-rename(sleep_new, ligero='ligero (s)', profundo='profundo (s)', nroncar='Snoring (s)')


#14. Eliminar todas las filas que contengan algún NA
sleep_new<-filter(sleep_new, !(is.na(ligero)|is.na(profundo)|is.na(nroncar)))

# 15. Calcular cuanto tiempo en total se ha dormido cada noche: ligero+profundo
sleep_new<-mutate(sleep_new, totalsleep=ligero+profundo)


# 16. Visualizacion de la relacion ligero-profundo-total
ggplot(sleep_new)+
  geom_point(mapping=aes(x=ligero, y= profundo  ,color=nroncar))

ggplot(sleep_new)+
  geom_point(mapping=aes(x=ligero, y= totalsleep  ,color=nroncar))

ggplot(sleep_new)+
  geom_point(mapping=aes(x=profundo, y= totalsleep  ,color=nroncar))
# A la vista de los resultados, que tipo de sueño es mas relevante?
# 17. Realizar un analisis de diferencias entre los dos tipos de sueño e interpretar los resultados
# usar la función ICalpha o el 'One sample t-test' de TeachingDemos: t.test()
ICalpha(sleep_new$ligero,sleep_new$profundo,0.05)


#18. Crear una nueva variable 'ciudad' en sleep_new con la informacion de act_new.
sleep_new$ciudad<-NA #truco para crear el espacio

for (i in 1:length(sleep_new$de)){
  for (j in (1:length(act_new$de))) {
    if (as.Date(sleep_new$de[i])==as.Date(act_new$de[j])) sleep_new$ciudad[i]<-act_new$ciudad[j]
    
  }
}
sleep_new$ciudad<-as.factor(sleep_new$ciudad)
#19. Representar la relación totalsleep y profundo usando como facetas el factor ciudad

ggplot(sleep_new)+
  geom_point(mapping=aes(x=profundo, y= totalsleep , color=ciudad))

ggplot(sleep_new)+
  geom_point(mapping=aes(x=profundo, y= totalsleep))+
 facet_grid(.~ciudad)

#20. Guardar el dataset sleep_new en un archivo "sleep_new.csv"
write.csv(sleep_new, "sleep_new.csv")
#21. Guardar el proyecto completo. Subir la carpeta del proyecto al campus.

