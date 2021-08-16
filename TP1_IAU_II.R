#Trabajo Práctico 1
#Alumna: María Victoria Marco

library(tidyverse)

data <- read.csv("https://query.data.world/s/v7xpthpx5kvhyccn2gy2vukmx47qnx", header=TRUE, stringsAsFactors=FALSE)

summary(data)


#Haremos una selecci?n de las variables que son de inter?s para este an?lisis, con el fin de trabajar con una base menos pesada  
data2 <- data %>% 
  select(id, listing_url, last_scraped, summary, description, host_id, host_since, latitude, 
         longitude, property_type, bathrooms, bedrooms, square_feet, price, monthly_price, 
         minimum_nights, maximum_nights, availability_30, availability_60, number_of_reviews, 
         review_scores_rating, review_scores_location, review_scores_value)

#1.Analizaremos los anfitriones:

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


#2. Analizaremos el tipo de propiedad, para conocer c?mo son las unidades de Airbnb
summary(data2$property_type)

class(data2$property_type)

#Mutaremos el dataset con el fin de renombrar bajo la categor?a "Other" a aquellos tipos de propiedades que son menos de 10 unidades, con el fin de simplificar la base.
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


#3. Analizaremos la cantidad de dormitorios disponibles 

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
#En el caso de los departamentos (que son la gran mayor?a de las unidades), en promedio, continen un domritorio (1,09)


#4. Ahora analizaremos la fecha de último scrapeo, para conocer de cuándo son los datos

library(lubridate)

class(data3$last_scraped)

#Como la fecha de último scrapeo está como character, la pasaremos a date
data3 <- data3 %>%
  mutate(last_scraped=ymd(last_scraped))

class(data3$last_scraped)
#Ahora sí podemos hacer operaciones con la fecha

fecha_data <- data3 %>%
  group_by(last_scraped) %>%
  summarise(cantidad=n()) %>% 
  arrange(desc(cantidad))
#A partir de esto, se puede observar que la mayoría de las unidades fueron registradas entre el 17 de Abril y el 18 de Abril del 2019, y solamente una el 5 de Mayo del 2019. Por lo tanto, esta data corresponde a registros pre-pandemia







