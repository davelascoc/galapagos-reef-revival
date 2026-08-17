                        ### HOBO Data Management ###

# Daniel Velasco C.
# 12 de marzo de 2025


### Cargar paquetes-------------------------------------------------------------

#list of packages
mypackages <- c("readxl", "readr", "tidyverse", "writexl")

#load packages
invisible(lapply(mypackages, library, character.only = T))

rm(mypackages)


### Importar datos--------------------------------------------------------------

#set working directory
setwd(here::here())
getwd()

#import
HOBO_0 <- read_csv("Datos/2022-23/Originales/Hobo Piloto/Hobo_2022.csv",
                   skip = 1)
#see data
print(HOBO_0)

#cols of temperature and intensity
temp_cols      <- grep("^Temp", colnames(HOBO_0), value = TRUE)
intensity_cols <- grep("^Intensidad", colnames(HOBO_0), value = TRUE)


### Data Management -----------------------------------------------------------

#data mgmt
HOBO <- HOBO_0 %>% 
  rename("N" = 1, "DateTime" = 2) %>%
  mutate(DateTime = mdy_hms(DateTime, tz = "America/Guayaquil")) %>%
  arrange(DateTime) %>%
  mutate(
    Temperature   = rowSums(select(., all_of(temp_cols)), na.rm = TRUE),
    Intensity_Lux = rowSums(select(., all_of(intensity_cols)), na.rm = TRUE),
    Deployment = apply(select(., all_of(temp_cols)), 1, function(row) {
      dep <- which(!is.na(row))
      if (length(dep) > 0) return(dep[1]) else return(NA)  # Asigna NA si no hay datos en la fila
    })
  ) %>%
  select(DateTime, Deployment, Temperature, Intensity_Lux) %>%
  filter(!is.na(Deployment)) %>%  # Eliminar filas con Deployment NA
  mutate(
    Deployment = recode(Deployment, `1` = "2", `2` = "1", .default = as.character(Deployment)), 
    Deployment = as.factor(Deployment)  # Convertir a factor
  )

#see data
HOBO

#see deployments
summary((HOBO$Deployment))

#cleaning data
HOBO_clean <- HOBO %>%
                group_by(Deployment) %>%
                mutate(
                  first_day = min(DateTime, na.rm = TRUE) + hours(2),
                  last_day  = max(DateTime, na.rm = TRUE) - hours(24),
                  Clean = case_when(
                    DateTime <= first_day ~ "Remove",  # first  2 hours
                    DateTime >= last_day  ~ "Remove",  #  last 24 hours
                    TRUE ~ "Keep" ),
                  Season = case_when(
                    month(DateTime) %in% 1:5  ~ "Warm", #Ene - May → Warm
                    month(DateTime) %in% 6:12 ~ "Cold"  #Jun - Dic → Cold
                  ) ) %>%
                select(Deployment, DateTime, Season,
                       Temperature, Intensity_Lux, Clean)     %>%
                ungroup()



### Visualizar Datos ----------------------------------------------------------

#datos crudos
HOBO  %>% ggplot(aes(x = DateTime, y = Temperature, color = Deployment)) +
          geom_line() +
          theme_minimal()

#datos limpios

HOBO_clean %>%  filter(Clean == "Keep")  %>%
                ggplot(aes(x = DateTime, y = Temperature, color = Deployment)) +
                geom_line() +
                theme_minimal()

#export clean data
#write_xlsx(HOBO_clean, path = "Datos/GRR Pilot Temperature.xlsx")

