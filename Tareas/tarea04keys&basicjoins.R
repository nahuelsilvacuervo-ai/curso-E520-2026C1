#9.2.4 Ejercicios

# 1.(consigna) Olvidamos dibujar la relación entre weathery airportsen la Figura  19.1 . ¿Cuál es la relación y cómo debería aparecer en el diagrama?

# 2. (consigna) weatherSolo contiene información para los tres aeropuertos de origen en Nueva York. Si contuviera registros meteorológicos para todos los aeropuertos de los EE. UU., ¿qué conexión adicional establecería con flights?

# 3. (consigna) Las variables year, month, day, hour, y origincasi forman una clave compuesta para weather, pero hay una hora que tiene observaciones duplicadas. ¿Puedes averiguar qué tiene de especial esa hora?

# 4. (consigna) Sabemos que algunos días del año son especiales y que menos personas viajan en avión (por ejemplo, Nochebuena y Navidad). ¿Cómo se podría representar esa información como un marco de datos? ¿Cuál sería la clave primaria? ¿Cómo se conectaría con los marcos de datos existentes?

# 5. (consigna) Dibuja un diagrama que ilustre las conexiones entre los marcos de datos Batting, People, y Salariesen el paquete Lahman. Dibuja otro diagrama que muestre la relación entre People, Managers, AwardsManagers. ¿Cómo caracterizarías la relación entre los marcos de datos Batting, Pitching, y ?Fielding


# 1.(respuesta) La relación entre weather y airports es que weather contiene información meteorológica para cada aeropuerto. En el diagrama, debería haber una conexión entre weather y airports, indicando que weather tiene una clave foránea que hace referencia a la clave primaria de airports (probablemente el código de aeropuerto).

# 2. (respuesta) Si weather contuviera registros meteorológicos para todos los aeropuertos de los EE. UU., establecería una conexión adicional con flights a través de la clave de aeropuerto de origen y destino. Esto permitiría relacionar las condiciones meteorológicas con cada vuelo específico, proporcionando información sobre cómo el clima podría haber afectado el vuelo.  

# 3. (respuesta) en wheater la combinación de año, mes, día, hora y aeropuerto casi funciona como clave compuesta.Sin embargo, hay una hora con registros duplicados.Esto ocurre por el cambio de horario de verano. En otoño, el reloj se atrasa una hora y esa hora se repite dos veces. Por eso aparecen duplicados legítimos en los datos.

# 4. (respuesta) Se podría crear un marco de datos llamado "Holidays" que contenga información sobre los días festivos. La clave primaria podría ser una combinación de las variables "year", "month", y "day". Este marco de datos se conectaría con el marco de datos "flights" a través de estas mismas variables, permitiendo identificar qué vuelos ocurrieron en días festivos y analizar cómo esto afecta el tráfico aéreo.

#19.3.4 Ejercicios

library(tidyverse)
library(nycflights13)
library(dplyr)

#1. (consigna) Identifica las 48 horas (a lo largo del año) con los mayores retrasos. Compáralas con los weatherdatos. ¿Observas algún patrón?

#1. (respuesta) 

flights |> 
  group_by(year, month, day, hour) |> 
  summarize(mean_delay = mean(arr_delay, na.rm = TRUE)) |> 
  slice_max(mean_delay, n = 48)
#agrupa los vuelos por fecha y hora, calcula el retraso promedio y selecciona las 48 horas con mayor demora

flights |> 
  group_by(year, month, day, hour, origin) |> 
  summarize(mean_delay = mean(arr_delay, na.rm = TRUE)) |> 
  slice_max(mean_delay, n = 48) |> 
  left_join(weather, by = c("year","month","day","hour","origin"))

#une esas horas con la tabla weather, comparás retrasos con condiciones meteorológicas. El patrón esperado es que las horas con mayores demoras coincidan con tormentas, nieve o baja visibilidad. Esto muestra cómo factores externos (clima) afectan la variable de interés (retraso)

#2. (consigna) Imagina que has encontrado los 10 destinos más populares usando este código¿Cómo puedes encontrar todos los vuelos a esos destinos?

#2. (respuesta)

top_dest <- flights |> 
  count(dest, sort = TRUE) |> 
  head(10)

flights |> semi_join(top_dest, by = "dest")


#Primero contas los destinos y seleccionas los 10 más frecuentes. Luego filtras todos los vuelos hacia esos destinos.
#usás un semi join para quedarte solo con las filas de flights2 que tienen coincidencia en top_dest.

#3. (consigna) ¿Cada vuelo de salida dispone de datos meteorológicos correspondientes a esa hora?

#3. (respuesta)

flights |> 
  anti_join(weather, by = c("year","month","day","hour","origin"))

#Muestra los vuelos que no tienen coincidencia en la tabla de clima.
#el anti join revela huecos en los datos. Esto enseña que no siempre hay correspondencia perfecta entre tablas, y que las claves compuestas pueden fallar por duplicados o registros faltantes.

# 4.(consigna) ¿Qué tienen en común los números de cola que no tienen un registro coincidente planes? (Pista: una variable explica aproximadamente el 90% de los problemas).

# 4. (respuesta)
flights |> 
  anti_join(planes, by = "tailnum") |> 
  count(is.na(tailnum))


#El 90% de los problemas se deben a que el número de cola (tailnum) es NA (desconocido). Esto sugiere que la mayoría de los vuelos sin registro en planes no tienen un número de cola registrado, lo que dificulta la identificación del avión específico utilizado en esos vuelos.

#5. (consigna) Agrega una columna que planesliste a todos carrierlos que han volado en ese avión. Podrías esperar que exista una relación implícita entre avión y aerolínea, ya que cada avión es operado por una sola aerolínea. Confirma o refuta esta hipótesis utilizando las herramientas que aprendiste en capítulos anteriores.

flights |> 
  group_by(tailnum) |> 
  summarize(carriers = n_distinct(carrier))
#Agrupa por número de cola y cuenta el número de aerolíneas distintas que han operado ese avión. Si la mayoría de los aviones tienen solo una aerolínea asociada, eso confirmaría la hipótesis de que cada avión es operado por una sola aerolínea. Sin embargo, si hay muchos aviones con múltiples aerolíneas, eso refutaría la hipótesis.

# 6. (consigna) Agregue la latitud y la longitud del aeropuerto de origen yflights destino a . ¿Es más fácil renombrar las columnas antes o después de la unión?

# 6. (respuesta)

flights |> 
  left_join(airports, by = c("origin" = "faa")) |> 
  left_join(airports, by = c("dest" = "faa"), suffix = c("_origin","_dest"))


#Agrega coordenadas de origen y destino.
#al unir dos veces la misma tabla, aparecen conflictos de nombres. Por eso es más fácil renombrar después de la unión. Esto muestra cómo manejar claves primarias (faa) en múltiples roles (origen y destino).

# 7. (consigna) Calcula el retraso promedio por destino y luego combina los airportsdatos para mostrar la distribución espacial de los retrasos

avg_delay <- flights |> 
  group_by(dest) |> 
  summarize(mean_delay = mean(arr_delay, na.rm = TRUE))

airports |> 
  inner_join(avg_delay, by = c("faa" = "dest")) |> 
  ggplot(aes(x = lon, y = lat, color = mean_delay)) +
  annotation_borders("state") +
  geom_point() +
  coord_quickmap()

#Calcula retraso promedio por destino y une con airports para obtener coordenadas. Luego visualiza la distribución espacial de los retrasos usando un mapa. Esto muestra cómo combinar datos tabulares con información geográfica para obtener insights espaciales.

# 8. (consigna) ¿Qué ocurrió el 13 de junio de 2013? Dibuja un mapa de los retrasos y luego usa Google para compararlo con la información meteorológica.

# 8. (respuesta) 
flights |> 
  filter(year == 2013, month == 6, day == 13) |> 
  group_by(dest) |> 
  summarize(mean_delay = mean(arr_delay, na.rm = TRUE)) |> 
  inner_join(airports, by = c("dest" = "faa")) |> 
  ggplot(aes(x = lon, y = lat, color = mean_delay)) +
  annotation_borders("state") +
  geom_point() +
  coord_quickmap()
#Ese día hubo una gran tormenta en Nueva York. El mapa muestra retrasos concentrados en JFK, LGA y EWR.
#este ejercicio conecta datos de vuelos con hechos externos (clima real).

