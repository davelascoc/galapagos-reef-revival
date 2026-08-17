                      ## Crecimiento Corales Isabela ##
                              ## Temperaturas ##


library(readxl)
library(ggplot2)
library(dplyr)

temp  <- read_excel("Datos/Analizados/Hobo 2022.xlsx")

temp             <- temp %>% mutate_at(c("Season", "Year", "Month"), factor)
temp$Date_Time   <- as.Date(temp$Date_Time)

summary(temp)

attach(temp)

ggplot(temp, aes(x = Season, y = `T (°C)`)) + ggtitle("Seasonal Temperature") +
     #   geom_jitter(aes(color = Season), width = .15, size = 1, alpha = .02) +
        geom_boxplot(color = "black", alpha = 0.65, outlier.shape = NA,
        fill = NA) +  stat_boxplot(geom = "errorbar", width = 0.15) +
        theme_bw() + theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

ggplot(temp, aes(x = Season, y = `I (lux)`)) + ggtitle("Seasonal Temperature") +
        geom_jitter(aes(color = Season), width = .15, size = 1, alpha = .02) +
        geom_boxplot(color = "black", alpha = 0.65, outlier.shape = NA,
        fill = NA) +  stat_boxplot(geom = "errorbar", width = 0.15) +
        theme_bw() + theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

summary(aov(`T (°C)`~Season))


