                       ## Crecimiento Corales Isabela ##
                               ## Grupo Piloto ##


library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggsci)
library(patchwork)
library(multcompView)
library(agricolae)


## DATA MANAGEMENT ##

corals  <-  read_excel("Datos/Analizados/Corales Isabela - Piloto.xlsx",
                          sheet = "Piloto")

corals  <-  corals    %>% filter(!Phenotype %in% c("P5.2", "P9.2", "P19.2"))

corals  <-  corals    %>% mutate(Date = as.Date(Date))             %>%
            #mutate(Phenotype = factor(Phenotype, levels = c("P2",
            #"P4", "P5", "P7", "P9", "P10", "P13", "P14", "P19")))  %>%
            mutate_at(c("Measuring", "Year", "Month", "Day",
            "Plane", "State", "Fragment"), factor)

summary(corals)
summary(corals$Phenotype)



## PLOTTING GROWTH: LINEAR FUNCTIONS ##

#vertical
corals %>%
    filter(Plane %in% "Vertical")   %>%
    ggplot(aes(x = Date, y = ML.cm, group = Fragment, color = Phenotype)) +
            geom_point() + geom_line() + ggtitle("Coral Vertical Growth") +
            facet_wrap(~Phenotype) + scale_x_date(breaks = as.Date(c(
            "2022-01-08", "2022-06-20", "2023-01-12")), date_labels = "%b %Y",
            expand = c(0.2,0)) + scale_y_continuous(expand = c(0,1.5)) +
            theme_bw() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_color_hue(l =50)

#horizontal
corals %>%
    filter(Plane %in% "Horizontal") %>%
    ggplot(aes(x = Date, y = ML.cm, group = Fragment, color = Phenotype)) +
            geom_point() + geom_line() + ggtitle("Coral Horizontal Growth") +
            facet_wrap(~Phenotype)  + scale_x_date(breaks = as.Date(c(
            "2022-01-08", "2022-06-20", "2023-01-12")), date_labels = "%b %Y",
            expand = c(0.2,0)) + scale_y_continuous(expand = c(0,1.5)) +
            theme_bw() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_color_hue(l =50)

            #Customizing Time and Date Scales in ggplot2
            #https://www.statworx.com/en/content-hub/blog/customizing-time-and-date-scales-in-ggplot2/



## GROWTH RATES (Δ/day) ##

growth  <-  read_excel("Datos/Analizados/Corales Isabela - Piloto.xlsx",
                          sheet = "RatesR")

growth  <-  growth    %>% filter(!Phenotype %in% c("P5.2", "P9.2", "P19.2"))

growth  <-  growth        %>%
            filter(Plane  ==  "Horizontal")   %>%  #droplevels()           %>%
            mutate(Season    = factor(Season,
            levels = c("Warm 2022", "Cold 2022")))                        %>%
            #mutate(Phenotype = factor(Phenotype, levels = c("P2",
            #"P4", "P5", "P7", "P9", "P10", "P13", "P14", "P19")))         %>%
            mutate_at(c("Plane", "Fragment"), factor)

str(growth)
summary(growth)
summary(growth$Phenotype)

seasons <-  growth  %>% group_by(Season)  %>%
                        summarise(var1 = var(ΔML,   na.rm = T),
                                  var2 = var(pΔML,  na.rm = T))

#histogram
ggplot(growth, aes(x = ΔML, fill = Season, col = Season)) +
            geom_histogram(position = "identity") +
            geom_density(aes(y = after_stat(density)*20), alpha = .2) +
            facet_wrap(~Season, nrow = 2) + ylab("Frequency") +
            ggtitle("Seasonal Coral Net Growth") + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_npg() + scale_color_hue(l = 25)

#histogram (%)
ggplot(growth, aes(x = pΔML, fill = Season, col = Season)) +
            geom_histogram(position = "identity") +
            geom_density(aes(y = after_stat(density)*6), alpha = .2) +
            facet_wrap(~Season, nrow = 2) +
            scale_x_continuous(labels = scales::percent) +
            ggtitle("Seasonal Coral Growth Rate") + ylab("Frequency") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_npg() + scale_color_hue(l = 25)

#bloxplots
ggplot(growth, aes(x = Season, y = ΔML, col = Season)) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(aes(fill = Season)) + 
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "wilcox.test", label.x = 1.6) +
            ggtitle("Seasonal Coral Net Growth") + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = .5)) +
            scale_fill_npg() + scale_color_hue(l = 25) +
ggplot(growth, aes(x = Season, y = pΔML, col = Season)) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(aes(fill = Season)) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "wilcox.test", label.x = 1.6) +
            ggtitle("Seasonal Coral Growth Rate") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = .5)) +
            scale_y_continuous(labels = scales::percent) +
            scale_fill_npg() + scale_color_hue(l = 25)

#boxplots by phenotypes
ggplot(growth, aes(x = Season, y = ΔML, col = Season)) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(aes(fill = Season), outlier.size = 1) + 
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "wilcox.test", label = "p.signif",
            label.x = 2.25, label.y = 1.9, hide.ns = F) +
            facet_wrap(~Phenotype) +
            ggtitle("Seasonal Coral Net Growth by Phenotype") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_npg() + scale_color_hue(l = 25)

#boxplots by phenotypes (%)
ggplot(growth, aes(x = Season, y = pΔML, col = Season)) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(aes(fill = Season), outlier.size = 1) + 
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "wilcox.test", label = "p.signif",
            label.x = 2, label.y = .55, hide.ns = F) +
            facet_wrap(~Phenotype) +
            ggtitle("Seasonal Coral Growth Rate by Phenotype") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_npg() + scale_color_hue(l = 25) +
            scale_y_continuous(labels = scales::percent)


# ANOVA ASSUMPTIONS

#outliers
growth %>%  group_by(Season)             %>%  select(Season, ΔML)     %>%
            identify_outliers(ΔML)       %>%  filter(is.extreme == T)
growth %>%  group_by(Season)             %>%  select(Season, pΔML)    %>%
            identify_outliers(pΔML)      %>%  filter(is.extreme == T)

growth %>%  group_by(Season, Phenotype)  %>%  select(Season, ΔML)     %>%
            identify_outliers(ΔML)       %>%  filter(is.extreme == T)
growth %>%  group_by(Season, Phenotype)  %>%  select(Season, pΔML)    %>%
            identify_outliers(pΔML)      %>%  filter(is.extreme == T)

#normality test (Shapiro-Wilk)
growth %>%  shapiro_test(ΔML)       #si n > 50, mejor hacer qqplot porque con
growth %>%  shapiro_test(pΔML)      #tamaños de muestras grandes, el test
                                    #Shapiro-Wilk es muy sensible, incluso
                                    #a una pequeña desviación de la normalidad.
#residuals
normal1    <- lm(ΔML   ~ Season, growth)
normal2    <- lm(pΔML  ~ Season, growth)

head(normal1$fitted.values)
head(normal1$residuals)
ggqqplot(residuals(normal1)) + theme_test()  #correlación entre un dato y la
                                             #distribución normal
shapiro_test(residuals(normal1))

head(normal2$fitted.values)
head(normal2$residuals)
ggqqplot(residuals(normal2)) + theme_test()
shapiro_test(residuals(normal2))

#by seasons
ggqqplot(growth, "ΔML",  facet.by = "Season") + theme_test()
ggqqplot(growth, "pΔML", facet.by = "Season") + theme_test()

growth  %>%  group_by(Season)  %>% shapiro_test(ΔML)
growth  %>%  group_by(Season)  %>% shapiro_test(pΔML)
#by(growth$ΔML, growth$Season, shapiro_test)            #otra forma

#by phenotypes
ggqqplot(growth, "ΔML",  facet.by = "Phenotype") + theme_test()
ggqqplot(growth, "pΔML", facet.by = "Phenotype") + theme_test()

growth  %>%  group_by(Phenotype)  %>% shapiro_test(ΔML)
growth  %>%  group_by(Phenotype)  %>% shapiro_test(pΔML)

#homogeneity of variance
plot(normal1, 1)
plot(normal2, 1)

#Bartlett's test (more sensitive to deviations from normality)
bartlett.test(ΔML  ~ Season, growth)
bartlett.test(pΔML ~ Season, growth)

#Levene's test (less sensitive)
growth  %>%  levene_test(ΔML   ~ Season)   #p > .05: varianzas homogéneas
growth  %>%  levene_test(pΔML  ~ Season)   #p < .05: varianzas diferentes
              
              #si varianzas son significativamente diferentes, se puede
              #realizar un Welch ANOVA: welch_anova_test() y
              #Games-Howell como posthoc.
              #https://www.youtube.com/watch?v=2pfOkUbigr0

#ANOVAs
growth  %>%   anova_test(ΔML  ~ Season*Phenotype, detailed = T)
growth  %>%   anova_test(pΔML ~ Season*Phenotype, detailed = T)
                                #https://www.datanovia.com/en/lessons/anova-in-r/

anova(aov(ΔML ~ Season*Phenotype, growth))
anova(aov(pΔML~ Season*Phenotype, growth))
                                #https://www.maximaformacion.es/blog-dat/como-realizar-el-anova-de-una-via-en-r/
#Tukey HSD
growth  %>%   tukey_hsd(ΔML  ~ Season*Phenotype)
growth  %>%   tukey_hsd(pΔML ~ Season*Phenotype)

#TukeyHSD(aov(ΔML ~ Season*Phenotype, growth))
#TukeyHSD(aov(pΔML~ Season*Phenotype, growth))



## DISTRIBUTION - INITIAL SIZE OF FRAGMENTS ##

corals0 <-  corals  %>%   filter(Measuring == "1")            %>%
                          filter(Plane     == "Horizontal")

summary(corals0)
summary(corals0$ML.cm)                                   #variable
range(corals0$ML.cm, finite = T)                         #max & min values

quantile(corals0$ML.cm, c(1/3, 2/3), na.rm = T)          #quantiles 

m = round(mean(corals0$ML.cm, na.rm = T), 3)

#histogram & density (all)
ggplot(corals0, aes(x = ML.cm)) + geom_histogram(fill = "coral",
            col = "coral4", binwidth = 0.25, position = "identity") +
            geom_density(aes(y = ..density..*50), fill = "coral",
            col = "coral4", alpha = .2) +
            geom_vline(aes(xintercept = m), linetype = 2, col = "coral4") +
            annotate("text", x = m*1.2, y = 24, col = "coral4",
            label = paste("Mean =", m)) +
            ggtitle("Initial Size of Fragments") + theme_test() + 
            ylab("Frequency") + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5))

#histogram & density (all)
ggplot(corals0, aes(x = ML.cm)) + geom_histogram(fill = "coral",
            col = "coral4", binwidth = .25, position = "identity") +
            geom_density(aes(y = ..density..*50), fill = "coral",
            col = "coral4", alpha = .2) +
            geom_vline(aes(xintercept = m), linetype = 2, col = "coral4") +
            annotate("text", x = m*1.2, y = 24, col = "coral4",
            label = paste("Mean =", m)) +
            ggtitle("Initial Size of Fragments") + ylab("Frequency") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(sec.axis = sec_axis(trans = ~.x/60,
            name = "", labels = scales::percent))

#ordenar por media
stats0  <-  corals0  %>% group_by(Phenotype)                    %>%
            summarise(Mean = mean(ML.cm, na.rm = TRUE),
                      Max  = max( ML.cm, na.rm = TRUE))         %>%
            arrange(-Mean)
stats0

#histogram & density (by Phenotype)
ggplot(corals0, aes(x = ML.cm, fill = Phenotype, col = Phenotype)) +
            geom_histogram(position = "identity", binwidth = 1/2) +
            geom_density(aes(y = ..density..*8), alpha = .2) +
            facet_wrap(~ Phenotype) + ggtitle("Initial Size of Fragments") +
            theme_bw() + ylab("Frequency") + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) + scale_color_hue(l = 25)

#data mgmt (anova posthoc)
#stats0["L"] <-  as.data.frame.list(multcompLetters4(
#                aov(         ML.cm ~ Phenotype, data = corals0),   #anova
#                TukeyHSD(aov(ML.cm ~ Phenotype, data = corals0)), reversed = T,   #tukey
#                )$Phenotype)["Letters"]

#stats0  <-  stats0 %>% arrange(Mean)
#stats0

#corals0 <-  corals0 %>% mutate(Phenotype = factor(Phenotype,
#                                levels = stats0$Phenotype))

#data mgmt (kruskal posthoc)

stats0  <-  stats0  %>% mutate(Phenotype = as.character(Phenotype)) %>% arrange(Phenotype)
stats0

pwc0    <-  with(corals0, kruskal(ML.cm, Phenotype, alpha = .05)$groups)
pwc0    <-  pwc0 %>% rownames_to_column() %>% rename(Phenotype = rowname) %>%
            arrange(Phenotype)

pwc0

stats0["L"] <-  pwc0["groups"]
stats0
stats0   <- stats0 %>% arrange(-Mean)
stats0

corals0  <- corals0 %>% mutate(Phenotype = factor(Phenotype, levels = stats0$Phenotype))


#boxplot
ggplot(corals0, aes(x = Phenotype, y = ML.cm, fill = Phenotype,
            col = Phenotype)) + #stat_boxplot(geom = "errorbar", width = .1) + 
            geom_jitter(width = .1, size = .5, alpha= .5) +
            geom_boxplot(alpha = .65, outlier.shape = NA) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            ggtitle("Initial Size of Fragments") + theme_test() +
            theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1),
            legend.position = "none", plot.title = element_text(hjust = .5)) +
            scale_color_hue(l = 25) +
            geom_text(data = stats0, aes(Phenotype, Max+.4, label = L))

#violinplot
ggplot(corals0, aes(x = Phenotype, y = ML.cm, fill = Phenotype,
            col = Phenotype)) +# stat_boxplot(geom = "errorbar", width = .05) +
            geom_violin(trim = T, bw = 0.75, alpha = 0.3, color = NA) + 
            geom_jitter(width = .1, size = .5, alpha = .5) + 
            geom_boxplot(alpha = .65, outlier.shape = NA, width = .3) +
            stat_summary(fun.data = 'mean_se', geom = 'point', shape = 4) +
            ggtitle("Initial Size of Fragments") + theme_test() +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.title = element_text(hjust = 0.5), legend.position = "none") +
            scale_color_hue(l = 25) +
            geom_text(data = stats0, aes(Phenotype, Max+.4, label = L))

#residuals and normality test (Shapiro-Wilk)
ggqqplot(corals0, "ML.cm") + theme_test()
corals0 %>% shapiro_test(ML.cm)
ggqqplot(corals0, "ML.cm", facet.by = "Phenotype") + theme_test()
corals0 %>% group_by(Phenotype) %>% shapiro_test(ML.cm)

          # Si p < 0,05, la distribución de los datos es significativamente
          # diferente de la distribución normal. La H0 implica que los datos
          # siguen una distribución normal, y la HA indica lo contrario.



## SUMMARY DATABASE






