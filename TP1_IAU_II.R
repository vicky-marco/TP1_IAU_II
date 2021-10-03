#Trabajo Práctico 1
#Alumna: María Victoria Marco

#En este trabajo práctico se buscará analizar los datos disponibles (a partir de una base obtenida de internet) sobre Airbnb, para poder caracterizar dicho fenómeno en la Ciudad de Buenos Aires.


library(tidyverse)

#En primer lugar, se descargará la base de datos

data <- read.csv("https://query.data.world/s/v7xpthpx5kvhyccn2gy2vukmx47qnx", header=TRUE, stringsAsFactors=FALSE)

summary(data)


#Se hará una selección de las variables que son de interés para este análisis, con el fin de trabajar con una base menos pesada  
data2 <- data %>% 
  select(id, listing_url, last_scraped, summary, description, host_id, host_since, latitude, 
         longitude, property_type, bathrooms, bedrooms, square_feet, price, monthly_price, 
         minimum_nights, maximum_nights, availability_30, availability_60, number_of_reviews, 
         review_scores_rating, review_scores_location, review_scores_value)

#1.Se analizarán los anfitriones:

#Resulta interesante conocer si hay anfitriones que ofrecen más de 1 unidad. Para ello:

id_hosts <- data2 %>%
  group_by(host_id) %>%
  summarise(cantidad=n()) %>% 
  arrange(desc(cantidad))

head(id_hosts,30)
#El usuario 4442974 tiene 92 unidades y lidera el ranking. Luego le sigue otro usuario con 79, otro con 75 y otro con 51. El resto es anfitrion de 50 o menos de 50 unidades.

#Para chequear lo anterior:
id_hosts %>% 
  slice_max(cantidad)


id_hosts %>% 
  summarise(promedio = sum(cantidad)/length(unique(host_id)))
#En promedio, las personas son anfitriones de 1,52 unidades (lo cual no es posible, pero permite pensar que gran parte de los anfitriones tiene más de una unidad)

id_hosts %>% 
  summarise(median(cantidad))
#Por otro lado, la mediana es 1, por lo tanto, se observa que más de la mitad de los anfitriones tienen solo 1 unidad, ya que la mediana es el valor que divide a las frecuencias en dos partes iguales. 


#2. Se analizará el tipo de propiedad, para conocer cómo son las unidades de Airbnb
summary(data2$property_type)

class(data2$property_type)

#Se mutará el dataset con el fin de renombrar bajo la categoría "Other" a aquellos tipos de propiedades que son menos de 10 unidades, con el fin de simplificar la base.
data3 <- data2 %>% 
  mutate( property_type= case_when(
    property_type == "Chalet"  ~ "Other",
    property_type == "Pension (South Korea)"  ~ "Other",
    property_type == "Villa"  ~ "Other",
    property_type == "Bungalow"  ~ "Other",
    property_type == "Camper/RV"  ~ "Other",
    property_type == "Earth house"  ~ "Other",
    property_type == "Boat"  ~ "Other",
    property_type == "Cabin"  ~ "Other",
    property_type == "Dome house"  ~ "Other",
    property_type == "In-law"  ~ "Other",
    property_type == "Resort"  ~ "Other",
    property_type == "Castle"  ~ "Other",
    property_type == "Dorm"  ~ "Other",
    property_type == "Ryokan (Japan)"  ~ "Other",
    TRUE ~ property_type))

data3 <- mutate(data3, property_type=as.factor(property_type))

summary(data3$property_type)

prop_type_data <- data3 %>%
  group_by(property_type) %>%
  summarise(cantidad=n()) %>% 
  arrange(desc(cantidad))
#De esta forma se evidencia que gran parte de las unidades de Airbnb son departamentos (precisamente, 14826).

calcular_pct <- function(data){
  round(data/(sum(data, na.rm = FALSE))*100,1)}

prop_type_data <- prop_type_data %>% 
  mutate(pct=calcular_pct(data= cantidad))
#Los departamentos constituyen el 79,2% de la oferta, seguidos muy por debajo por las casas:7,7%. 


#3. Se analizará la cantidad de dormitorios disponibles en las unidades 

dormitorios <- data2 %>% 
  select(latitude, longitude,bedrooms,price, property_type) %>% 
  filter(!is.na(bedrooms))

dormitorios %>% 
  summarise(mean(bedrooms))
#La media de los dormitorios es de 1,12; es decir, la mayor parte de las unidades poseen un solo dormitorio. 

dormitorios_deptos <- dormitorios %>% 
  filter(property_type=="Apartment")

dormitorios_deptos %>% 
  summarise(mean(bedrooms))
#En el caso de los departamentos (que son la gran mayoría de las unidades), en promedio, continen un domritorio (1,09)

#4. Ahora se estudiará la fecha de último scrapeo, para conocer de cuándo son los datos

library(lubridate)

class(data3$last_scraped)

#Como la fecha de último scrapeo está como character, la pasaremos a date
data3 <- data3 %>%
  mutate(last_scraped=ymd(last_scraped))

class(data3$last_scraped)
#Ahora sí se puede hacer operaciones con la fecha

fecha_data <- data3 %>%
  group_by(last_scraped) %>%
  summarise(cantidad=n()) %>% 
  arrange(desc(cantidad))
#A partir de esto se puede observar que la mayoría de las unidades fueron recogidas de la web entre el 17 de Abril y el 18 de Abril del 2019, y solamente una el 5 de Mayo del 2019. Por lo tanto, esta data corresponde a registros pre-pandemia
#Esta información resulta de utilidad ya que será utilizada para obtener el valor del dólar en aquel momento y poder llevar a dicha moneda los valores de alquiler de las unidades de Airbnb.

#5. Ahora se realizará un breve análisis de los precios de alquiler de las unidades

precios_data <- data3 %>% 
  select(latitude, longitude, property_type, bathrooms, bedrooms, price, review_scores_rating, square_feet)

library(stringr)  

class(precios_data$price)

#Se eliminará el signo de pesos para poder hacer operaciones con los valores
precios_data <- precios_data %>% 
  filter(!is.na(price)) %>% 
  mutate(precio=str_sub(price, 2,6)) 

precios_data <- mutate(precios_data, precio2=str_replace(precio, ",", ""))

#Se convertirán los valores a dólares (tomando el tipo de cambio del 18/04/2019: valor del dólar para la venta:$42.95).

precios_data <- precios_data %>% 
  mutate(precio3=(as.numeric(precio2)/42.95))


#Ahora, se realizarán dos gráficos para poder tener una vista macro de los precios de alquiler
library(ggplot2)

class(precios_data$precio3)

ggplot(precios_data)+
  geom_histogram(aes(x=as.numeric(precio2), fill="coral2"))+
  scale_x_continuous(breaks=seq(0,6000,1000))+
  labs(title = "Precio de unidades de Airbnb (en pesos)",
       subtitle = "Abril / Mayo 2019",
       x = "Precio",
       y = "Cantidad")+
  theme_minimal()+
  theme(legend.position = "none")

#La mayoría de los valores se encuentran por debajo de los $2.000

ggplot(precios_data)+
  geom_histogram(aes(x=as.numeric(precio3), fill="coral2"))+
  labs(title = "Precio de unidades de Airbnb (en dólares)",
       subtitle = "Abril / Mayo 2019",
       x = "Precio",
       y = "Cantidad")+
  theme_minimal()+
  theme(legend.position = "none")
#La mayoría de los valores se encuentran por debajo de los USD 50.

#A partir de la información obtenida sobre el valor de alquiler de las unidades, es posible calcular el valor del m2 

valor_m2 <- precios_data %>% 
  filter(!is.na(square_feet)) %>% 
  filter(!square_feet==0)

class(valor_m2$square_feet)

class(valor_m2$precio2)

class(valor_m2$precio3)

valor_m2 <- valor_m2 %>% 
  mutate(valor_m2_pesos=as.numeric(precio2)/square_feet) %>% 
  mutate(valor_m2_usd=precio3/square_feet)

valor_m2 %>% 
  summarise(mean(valor_m2_pesos)) 
#El valor promedio del alquiler del m2 en pesos es de $10,31

valor_m2 %>% 
  summarise(mean(valor_m2_usd))
#Mientras que, en el caso del valor de alquiler en dólares, el valor promedio del m2 es de 0,24USD. 

#Estos valores podrían resultar un poco extraños, ya que parecen ser muy bajos. Por lo tanto, se intentará conocer la mediana

valor_m2 %>% 
  summarise(median(valor_m2_pesos))
#En el caso de los valores en pesos, la mediana es de $4,45. Por lo tanto, se evidencia que hay dispersión en los datos porque al menos para la mitad de las unidades de Airbnb el valor del m2 es $4,48 o más.

#Por otro lado, resultaría interesante conocer si el valor del m2 varía según el barrio en el que se encuentra la unidad de Airbnb en alquiler. Para ello:

barrios <- st_read("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson")

library(sf)

valor_m2_barrio <- valor_m2 %>% 
  filter(!is.na(latitude), !is.na(longitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

str(valor_m2_barrio)
str(barrios)

barrios <- st_transform(barrios, crs=st_crs(valor_m2_barrio))

valor_m2_barrio <-st_join (valor_m2_barrio, barrios)

valor_m2_barrio_resumen <- valor_m2_barrio %>% 
  group_by(BARRIO, valor_m2_pesos) %>% 
  summarise(mean(valor_m2_pesos))




library(leaflet)

library(ggmap)
library(osmdata)

#6. SE mapearan las unidades de Airbnb en alquiler, con el fin de conocer sus ubicaciones en CABA
data3_geo <- data3 %>% 
  filter(!is.na(latitude), !is.na(longitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


bbox_caba <- getbb("Ciudad Autónoma de Buenos Aires, Argentina")

bbox_caba

mapa_caba <- get_stamenmap(bbox = bbox_caba,
                          zoom=12)

ggmap(mapa_caba)

ggmap(mapa_caba)+
  geom_sf(data=data3_geo, size=1, alpha=0.5, color="blue", inherit.aes = FALSE)+
  labs(title="Unidades de Airbnb en CABA (2019)",
       caption="Fuente: Open Street Map")+
  theme_void()
#Este mapa permite ver la gran cantidad de unidades de Airbnb en el Microcentro y Corredor Norte de la Ciudad, pero no permite ver en detalle la densidad (ya que puede haber puntos superpuestos)

#Para ver la densidad,se hará el siguiente mapa:
ggmap(mapa_caba) +
  geom_bin2d(data = data3, 
             aes(x = longitude, y = latitude), bins=50)+
  scale_fill_viridis_c(option = "magma", direction=-1)+
  labs(title = "Densidad de unidades de Airbnb",
       caption = "Fuente: SF Data",
       x="Longitud",y="Latitud")
#A partir de este mapa se evidencia mejor la densidad de las unidades, permitiendo ver que la mayor parte de las unidades de Airbnb se localizan en la zona del Microcentro, Retiro, Recoleta y Palermo.
#Por el contrario, en el sur de la Ciudad hay muy pocos (o no hay) unidades de Airbnb.


POR ACA!!!!



#Se sumará la base de paradas del Bus Turístico de la Ciudad de Buenos Aires para poder evaluar si dichas paradas se encuentran cerca de las unidades de Airbnb.
bus_turis <- read.csv("https://raw.githubusercontent.com/vicky-marco/TP1_IAU_II/master/paradas_bus_turistico%20(1).csv", stringsAsFactors = TRUE,
                      encoding = "UTF-8")

head(bus_turis)
#Se puede observar que esta base posee información sobre las paradas del Bus Turístico: geolocalización, recorrido, atracciones cercanas, barrio, comuna, etc.

#Se realizará una selección de las columnas que son de interés para este estudio
bus_turis2 <- bus_turis %>% 
  select(1:8, 13:14)


bus_turis_recorridos <- bus_turis2 %>%
  group_by(recorrido) %>%
  summarise(cantidad=n()) %>% 
  arrange(desc(cantidad))
#A partir de esto se puede observar que el recorrido con mayor cantidad de paradas es el Azul.



#Ahora, se realizará otro mapa para poder cruzar la densidad de las unidades de Airbnb y las paradas del Bus Turístico
ggmap(mapa_caba) +
  geom_bin2d(data = data3, 
             aes(x = longitude, y = latitude), bins=50)+
  scale_fill_viridis_c(option = "magma", direction=-1)+
  geom_point(data = bus_turis2, aes(y=lat, x=long), color="deepskyblue4")+
  labs(title = "Densidad de unidades de Airbnb y paradas del bus turístico de CABA",
       caption = "Fuente: SF Data",
       x="Latitud",y="Longitud")
#A través de este mapa se puede observar que las áreas más densas, en cuanto a unidades de Airbnb, se encuentran cerca de las paradas del Bus Turístico.

#Ahora se analizarán las paradas de los diferentes recorridos del Bus Turístico
ggmap(mapa_caba) +
  geom_bin2d(data = data3, 
             aes(x = longitude, y = latitude), bins=70, alpha=0.7)+
  scale_fill_viridis_c(option = "magma", direction=-1)+
  geom_point(data = bus_turis2, aes(y=lat, x=long, color=recorrido))+
  scale_color_manual(values = c("dodgerblue4", "firebrick", "forestgreen"))+
  labs(title = "Densidad de unidades de Airbnb y paradas del bus turístico (según recorrido) de CABA",
       caption = "Fuente: SF Data",
       x="Latitud",y="Longitud")
#Se puede observar que todos los recorridos tienen al menos una parada cerca de las grandes densidades de unidades de Airbnb. Sin embargo, es llamativo el recorrido rojo, ya que gran parte de las unidades de Airbnb se encuentran cerca de todas sus paradas.
  
#Estos mapas permiten observar que las paradas se encuentran en el corredor Norte de la Ciudad y en el Microcentro/San Telmo/La Boca.
#Por lo tanto, resulta interesante intentar generar un índice sobre la cantidad de paradas según la superficie de cada barrio. 
#Para ello, primero se conocerá cuáles son los barrios que sí poseen paradas del Bus Turístico.

bus_turis_barrios <- bus_turis2 %>%
  group_by(barrio) %>%
  summarise(cantidad=n()) %>% 
  arrange(desc(cantidad))
#Se puede observar que solo 9 barrios (Palermo, Puerto Madero, Belgrano, La Boca, Recoleta, Retiro, San Nicolás, Monserrat, San Telmo) poseen paradas en su territorio.

#Ahora, se importará un dataset con información sobre los barrios. 

barrios <- read.csv("https://raw.githubusercontent.com/vicky-marco/TP1_IAU_II/master/barrios.csv", stringsAsFactors = TRUE,
                      encoding = "UTF-8")

#Ahora se realizará una unión de la información a partir del nombre de cada barrio.
bus_turis_barrios <- bus_turis_barrios %>% 
  mutate(barrio=toupper(barrio)) %>% 
  rename("cant_parada_bus"=cantidad)

bus_turis_barrios2 <- left_join(bus_turis_barrios, barrios, by=c("barrio"="barrio"))

options(scipen=100)

bus_turis_barrios2 <-bus_turis_barrios2 %>% 
  mutate(area=area/1000000) %>% 
  mutate(indice=area/cant_parada_bus) %>% 
  mutate(indice=round(indice,2))




