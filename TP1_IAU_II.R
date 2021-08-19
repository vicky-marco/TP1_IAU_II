#Trabajo Práctico 1
#Alumna: María Victoria Marco

library(tidyverse)

data <- read.csv("https://query.data.world/s/v7xpthpx5kvhyccn2gy2vukmx47qnx", header=TRUE, stringsAsFactors=FALSE)

summary(data)


#Se hará una selección de las variables que son de interés para este análisis, con el fin de trabajar con una base menos pesada  
data2 <- data %>% 
  select(id, listing_url, last_scraped, summary, description, host_id, host_since, latitude, 
         longitude, property_type, bathrooms, bedrooms, square_feet, price, monthly_price, 
         minimum_nights, maximum_nights, availability_30, availability_60, number_of_reviews, 
         review_scores_rating, review_scores_location, review_scores_value)

#1.Se analizarán los anfitriones:

#Para conocer los anfitriones con mayor cantidad de unidades de Airbnb y la cantidad de unidades
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
#En promedio, las personas son anfitriones de 1,52 unidades (lo cual no es posible, pero nos permite pensar que gran parte de los anfitriones tiene m?s de una unidad)


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
#De esta forma se evidencia que gran parte de las unidades de Airbnb son departamentos.

prop_type_data %>% 
  summarise(porcentaje = ((cantidad)/sum(cantidad))*100)

prop_type_data <- mutate(prop_type_data, porcentaje=((cantidad)/sum(cantidad))*100)
#Los departamentos constituyen el 79,2% de la oferta, seguidos muy por debajo por las casas:7,6%. 


#3. Se analizará la cantidad de dormitorios disponibles 

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
#A partir de esto, se puede observar que la mayoría de las unidades fueron registradas entre el 17 de Abril y el 18 de Abril del 2019, y solamente una el 5 de Mayo del 2019. Por lo tanto, esta data corresponde a registros pre-pandemia
#Esta información resulta de utilidad ya que será utilizada para obtener el valor del dólar en aquel momento y poder llevar a dicha moneda los valores de alquiler de las unidades de Airbnb.

#5. Ahora se realizará un breve análisis de los precios de alquiler de las unidades

precios_data <- data3 %>% 
  select(latitude, longitude, property_type, bathrooms, bedrooms, price, review_scores_rating)

library(stringr)  

class(precios_data$price)

#Se eliminará el signo de pesos para poder hacer operaciones con los valores
precios_data <- precios_data %>% 
  filter(!is.na(price)) %>% 
  mutate(precio=str_sub(price, 2,6)) 

precios_data <- mutate(precios_data, precio2=str_replace(precio, ",", ""))

#Ahora, se realizará un gráfico para poder tener una vista macro de los precios
library(ggplot2)

class(precios_data$precio2)

ggplot(precios_data)+
  geom_histogram(aes(x=as.numeric(precio2)))+
  scale_x_continuous(breaks=seq(0,6000,1000))+
  labs(title = "Precio de unidades de Airbnb",
       subtitle = "Abril / Mayo 2019",
       x = "Precio",
       y = "Cantidad")
  

library(leaflet)
library(sf)
library(ggmap)
library(osmdata)


data3_geo <- data3 %>% 
  filter(!is.na(latitude), !is.na(longitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

#No correr lo siguiente porque se tilda
#leaflet(data3_geo) %>%
  #addTiles() %>%
  #addMarkers()

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
#Este mapa permite ver la gran cantidad de unidades de Airbnb en el Microcentro y Corredor Norte de la Ciudad, pero no permite ver en detalle la densidad

#Para ver la densidad,se hará el siguiente mapa:
ggmap(mapa_caba) +
  geom_bin2d(data = data3, 
             aes(x = longitude, y = latitude), bins=50)+
  scale_fill_viridis_c(option = "magma", direction=-1)+
  labs(title = "Densidad de unidades de Airbnb",
       caption = "Fuente: SF Data",
       x="Longitud",y="Latitud")
#A partir de este mapa se evidencia mejor la densidad de las unidades, permitiendo ver que la mayor parte de las unidades de Airbnb se localizan en la zona del Microcentro, Retiro, Recoleta y Palermo.

bus_turis <- read.csv("https://raw.githubusercontent.com/vicky-marco/TP1_IAU_II/master/paradas_bus_turistico%20(1).csv", stringsAsFactors = FALSE)

#Ahora, se realizará otro mapa para poder cruzar la densidad de las unidades de Airbnb y las paradas del bus turístico
ggmap(mapa_caba) +
  geom_bin2d(data = data3, 
             aes(x = longitude, y = latitude), bins=50)+
  scale_fill_viridis_c(option = "magma", direction=-1)+
  geom_point(data = bus_turis, aes(y=lat, x=long))+
  labs(title = "Densidad de unidades de Airbnb y paradas del bus turístico de CABA",
       caption = "Fuente: SF Data",
       x="Latitud",y="Longitud")




