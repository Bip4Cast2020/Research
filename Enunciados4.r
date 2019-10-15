
# ---------------PARTE 1 (2 PUNTOS)--------------------------------

# 1. Crear un nuevo proyecto denominado practica 4.

# 2. Mediante la libreria readr, o mediante los menus de RStudio, leer el dataset activities.csv
# ambos archivos deben estar previamente en la carpeta del proyecto creado



# 3.Comprobar el contenido  con View y contar cuantos NAs hay en la columna GPS del dataset activities



# 4. Crear un objeto R denominado act_new que contenga solo las variables 
# siguientes: 1,2,5-6


# 5. Renombrar la variable 'Activity type' con el nombre 'tipo' y la variable 'Time zone' como 'ciudad'


# 6. Realizar un recuento de tipo de actividad con summary. Para ello 
# debes transformar previamente la variable tipo a factor con as.factor.
# Crea un grafico de barras con dicha variable par visualizar las frecuencias.
# Haz lo mismo para la variable ciudad



#7. Filtrar los registros de act_new que correspondan con ciudad Amsterdam en otro objeto
# y lo mismo con Madrid. Con esos nuevos objetos determina los deportes que 
# no se practican en Amsterdam y sí en Madrid y viceversa. Genera graficos para visualizar los resultados



#8. Encontrar las fechas en las que se ha practicado bicicleta o pilates en Amsterdam en el año 2019



#9. Crear una nueva variable 'total' con los minutos de realización de cada actividad en Amsterdam
# y realizar una representación gráfica de los resultados con plot y determinar que deporte o deportes
# se han practicado durante dos horas o mas

#10. Guardar los nuevos datasets en  archivos llamados  "act_new.csv", etc.


#-------------------------------
#-----SEGUNDA PARTE-(3PUNTOS) ----------------
# 11. Cargar el dataset sleep en un objeto llamado sleep


#12. Crear un nuevo data set llamado sleep_new que contenga solo las variables
#que contengan información, que no sean todo cero.




#13. Renombrar las variables de sleep_new a nombres cortos:


#14. Eliminar todas las filas que contengan algún NA

# 15. Calcular cuanto tiempo en total se ha dormido cada noche: ligero+profundo


# 16. Visualizacion de la relacion ligero-profundo-total

# A la vista de los resultados, que tipo de sueño es mas relevante?
# 17. Realizar un analisis de diferencias entre los dos tipos de sueño e interpretar los resultados
# usar la función ICalpha o el 'One sample t-test' de TeachingDemos: t.test()



#18. Crear una nueva variable 'ciudad' en sleep_new con la informacion de act_new.


#19. Representar la relación totalsleep y profundo usando como facetas el factor ciudad


#20. Guardar el dataset sleep_new en un archivo "sleep_new.csv"

#21. Guardar el proyecto completo. Subir la carpeta del proyecto al campus.