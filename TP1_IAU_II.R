#Trabajo Práctico 1 (Análisis de información sobre Airbnb)
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


#Ahora se realizarán dos gráficos para poder tener una vista macro de los precios de alquiler
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

library(sf)

barrios <- st_read("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson")

valor_m2_barrio <- valor_m2 %>% 
  filter(!is.na(latitude), !is.na(longitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

str(valor_m2_barrio)
str(barrios)

barrios <- st_transform(barrios, crs=st_crs(valor_m2_barrio))

valor_m2_barrio <-st_join (valor_m2_barrio, barrios)

valor_m2_barrio_resumen <- valor_m2_barrio %>% 
  group_by(BARRIO) %>%
  summarise(cantidad=n(),
            valor_m2_prom=mean(valor_m2_pesos))
  
ggplot(valor_m2_barrio_resumen) +
  geom_col(aes(x = valor_m2_prom, y= reorder(BARRIO, valor_m2_prom), fill="Valor m2 en pesos"))+
  geom_point(data= valor_m2_barrio_resumen, aes (x=cantidad, y=BARRIO, color="Cantidad de unidades en alquiler"))+
  labs(title = "Valor promedio del m2 y cantidad de unidades de Airbnb en alquiler",
       subtitle = "Año 2019",
       x = "Valor promedio del m2 (en pesos)",
       y = "Barrio")+
  theme_minimal()+
  scale_color_manual(values = c("Cantidad de unidades en alquiler"= "coral4"),name = NULL,
                     guide = guide_legend(override.aes = list(linetype = "blank")))+
  scale_fill_manual(values = c("Valor m2 en pesos"= "coral2"), name = "Referencias",
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA)))+
  theme(legend.position = "right", legend.title = element_text(size = 10), legend.text = element_text(size = 7))


#A partir del gráfico anterior se evidencia que los mayores valores promedios del m2 de alquiler de unidades Airbnb se encuentran en: San Nicolás, Boedo, Balvanera, San Cristobal y Villa Crespo.
#Vale aclarar que puede haber muchas variables interviniendo en este análisis y dando esos resultados. Por un lado, la cantidad de unidades disponibles por barrio, ya que si hay pocas unidades pero una con un alto valor del m2, "tira hacia arriba" el promedio (por ejemplo, en Boedo hay solo 2 unidades).
#Lo mismo ocurre con San Cristobal, donde solo hay 3 unidades en oferta.
#Por el contrario, en Palermo hay 96 unidades, por lo tanto, al calcular el promedio, los valores altos del m2 pueden compensarse con los bajos, ya que son muchas unidades. 
#Por otro lado, puede que esté mal registrada la cantidad de metros cuadrados también. Si están sobreregistradas, los valores promedios del m2 darán más bajos.


#Conclusiones
#A partir de este primer trabajo práctico fue posible analizar la oferta de unidades de Airbnb en Abril/Mayo del 2019, es decir, pre pandemia.
#Se puedo observar que hay algunos anfitriones que poseen más de una unidad disponible para alquiler. Sin embargo, la mayoría posee uno.
#El tipo de propiedad en alquiler a través de Airbnb que abunda es el departamento. 
#La mayoría de las unidades de alquiler disponen de solamente 1 dormitorio.
#En cuanto a los precios de alquiler, hacia Abril/Mayo 2019 el promedio era de ARS 2.000 la noche. A partir de dicho precio, se puedo conocer que el valor promedio del m2 de las unidades era de $10,31. El mayor valor promedio del m2 se registró en el barrio de San Nicolás ($29,29) y el menor, en Parque Patricios ($0,44)


