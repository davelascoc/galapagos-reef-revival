                  ## Crecimiento Corales Isabela - Piloto ##



library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggsci)
library(patchwork)
library(multcompView)
library(agricolae)


## DATA MANAGEMENT ##

corals  <-  read_excel("Datos/Analizados/Corales Isabela - Master Database.xlsx",
                       sheet = "Master")

#eliminar fenotipos con pocos fragmentos
corals  <-  corals     %>% filter(!Phenotype %in% c("P5.2", "P9.2", "P19.2"))

#fragmentos proyecto PILOTO
corals   <-  corals    %>% filter(Group == "Piloto")

#factors
corals   <- corals     %>% mutate(Date = as.Date(Date))             %>%
            mutate(Phenotype = factor(Phenotype, levels = c("P2",
            "P4", "P5", "P7", "P9", "P10", "P13", "P14", "P19")))   %>%
            mutate_at(c("Measuring", "Year", "Month", "Day",
            "Plane", "State", "Fragment"), factor)

corals
summary(corals)
summary(corals[["Phenotype"]])



## PLOTTING GROWTH: LINEAR FUNCTIONS ##

corals %>%
    filter(Plane %in% "Vertical")   %>%
    ggplot(aes(x = Date, y = Area.cm2, group = Fragment, color = Phenotype)) +
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
    ggplot(aes(x = Date, y = Area.cm2, group = Fragment, color = Phenotype)) +
            geom_point() + geom_line() + ggtitle("Coral Horizontal Growth") +
            facet_wrap(~Phenotype)  + scale_x_date(breaks = as.Date(c(
            "2022-01-08", "2022-06-20", "2023-01-12")), date_labels = "%b %Y",
            expand = c(0.2,0)) + scale_y_continuous(expand = c(0,1.5)) +
            theme_bw() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_color_hue(l =50)



## GROWTH RATES (d/day) ##

growth  <-  read_excel("Datos/Analizados/Corales Isabela - Master Database.xlsx",
                       sheet = "RatesR")

#eliminar fenotipos con pocos fragmentos
growth  <-  growth    %>% filter(!Phenotype %in% c("P5.2", "P9.2", "P19.2",
                                                   "P6", "P?4", "PAC.PD"))

#fragmentos proyecto PILOTO
growth  <-  growth   %>% filter(Group == "Piloto")

#factors
growth  <-  growth        %>%
            filter(Plane  ==  "Horizontal")   %>%  #droplevels()           %>%
            mutate(Season    =  factor(Season,
                                levels = c("Warm 2022", "Cold 2022")))    %>%
            mutate(Phenotype = factor(Phenotype, levels = c("P2",
            "P4", "P5", "P7", "P9", "P10", "P13", "P14", "P19")))         %>%
            mutate_at(c("Plane", "Fragment"), factor)

str(growth)
summary(growth)

growth  %>% group_by(Season)  %>%
            summarise(var1 = var(dA,   na.rm = T),
                      var2 = var(pdA,  na.rm = T))

#histogram
ggplot(growth, aes(x = dA, fill = Season, col = Season)) +
            geom_histogram(position = "identity") +
            geom_density(aes(y = after_stat(density)*180), alpha = .2) +
            facet_wrap(~Season, nrow = 2) + ylab("Frequency") +
            ggtitle("Seasonal Coral Net Growth") + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_npg() + scale_color_hue(l = 25)

#histogram (%)
ggplot(growth, aes(x = pdA, fill = Season, col = Season)) +
            geom_histogram(position = "identity") +
            geom_density(aes(y = after_stat(density)*18), alpha = .2) +
            facet_wrap(~Season, nrow = 2) +
            scale_x_continuous(labels = scales::percent) +
            ggtitle("Seasonal Coral Growth Rate") + ylab("Frequency") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_npg() + scale_color_hue(l = 25)

#bloxplots
ggplot(growth, aes(x = Season, y = dA, col = Season)) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(aes(fill = Season)) + 
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "wilcox.test",
            label.x = 1.5, label.y = 26) +
            ggtitle("Seasonal Coral Net Growth") + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = .5)) +
            scale_fill_npg() + scale_color_hue(l = 25) +
            ggplot(growth, aes(x = Season, y = pdA, col = Season)) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(aes(fill = Season)) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "wilcox.test", label.x = 1.5) +
            ggtitle("Seasonal Coral Growth Rate") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = .5)) +
            scale_y_continuous(labels = scales::percent) +
            scale_fill_npg() + scale_color_hue(l = 25)

#boxplots by phenotypes
ggplot(growth, aes(x = Season, y = dA, col = Season)) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(aes(fill = Season), outlier.size = 1) + 
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "wilcox.test", label = "p.signif",
            label.x = 2.25, label.y = 20, hide.ns = T) +
            facet_wrap(~Phenotype) +
            ggtitle("Seasonal Coral Net Growth by Phenotype") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_npg() + scale_color_hue(l = 25)

#boxplots by phenotypes (%)
ggplot(growth, aes(x = Season, y = pdA, col = Season)) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(aes(fill = Season), outlier.size = 1) + 
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "wilcox.test", label = "p.signif",
            label.x = 2.25, label.y = 2, hide.ns = T) +
            facet_wrap(~Phenotype) +
            ggtitle("Seasonal Coral Growth Rate by Phenotype") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_npg() + scale_color_hue(l = 25) +
            scale_y_continuous(labels = scales::percent)

ggplot(growth, aes(x = Phenotype, y = pdA)) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(aes(fill = Phenotype), outlier.size = 1) + 
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            facet_wrap(~Season) +
            ggtitle("Seasonal Coral Growth Rate by Phenotype") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(labels = scales::percent)


# ANOVA ASSUMPTIONS

#outliers
growth %>%  group_by(Season)             %>%  select(Season, dA)     %>%
            identify_outliers(dA)        %>%  filter(is.extreme == T)
growth %>%  group_by(Season)             %>%  select(Season, pdA)    %>%
            identify_outliers(pdA)       %>%  filter(is.extreme == T)

growth %>%  group_by(Season, Phenotype)  %>%  select(Season, dA)     %>%
            identify_outliers(dA)        %>%  filter(is.extreme == T)
growth %>%  group_by(Season, Phenotype)  %>%  select(Season, pdA)    %>%
            identify_outliers(pdA)       %>%  filter(is.extreme == T)

#normality test (Shapiro-Wilk)
growth %>%  shapiro_test(dA)       #muestra muy grande -> mucha sensibilidad
growth %>%  shapiro_test(pdA)      

#residuals
normal1    <- lm(dA   ~ Season, growth)
normal2    <- lm(pdA  ~ Season, growth)

head(normal1$fitted.values)
head(normal1$residuals)
ggqqplot(residuals(normal1)) + theme_test()

shapiro_test(residuals(normal1))

head(normal2$fitted.values)
head(normal2$residuals)
ggqqplot(residuals(normal2)) + theme_test()
shapiro_test(residuals(normal2))

#by seasons
ggqqplot(growth, "dA",  facet.by = "Season") + theme_test()
ggqqplot(growth, "pdA", facet.by = "Season") + theme_test()

growth  %>%  group_by(Season)  %>% shapiro_test(dA)
growth  %>%  group_by(Season)  %>% shapiro_test(pdA)

#by phenotypes
ggqqplot(growth, "dA",  facet.by = "Phenotype") + theme_test()
ggqqplot(growth, "pdA", facet.by = "Phenotype") + theme_test()

growth  %>%  group_by(Phenotype)  %>% shapiro_test(dA)
growth  %>%  group_by(Phenotype)  %>% shapiro_test(pdA)

#homogeneity of variance
plot(normal1, 1)
plot(normal2, 1)

#Bartlett's test (more sensitive to deviations from normality)
bartlett.test(dA  ~ Season, growth)
bartlett.test(pdA ~ Season, growth)

#Levene's test (less sensitive)
growth  %>%  levene_test(dA   ~ Season)
growth  %>%  levene_test(pdA  ~ Season)

#ANOVAs
growth  %>%   anova_test(dA  ~ Season*Phenotype, detailed = T)
growth  %>%   anova_test(pdA ~ Season*Phenotype, detailed = T)

anova(aov(dA ~ Season*Phenotype, growth))
anova(aov(pdA~ Season*Phenotype, growth))

#Tukey HSD
growth  %>%   tukey_hsd(dA  ~ Season*Phenotype)
growth  %>%   tukey_hsd(pdA ~ Season*Phenotype)

#TukeyHSD(aov(dA ~ Season*Phenotype, growth))
#TukeyHSD(aov(pdA~ Season*Phenotype, growth))

rm(normal1, normal2)



## DISTRIBUTION - INITIAL SIZE OF FRAGMENTS ##

corals0 <-  corals  %>%   filter(Measuring == "1")            %>%
                          filter(Plane     == "Horizontal")

summary(corals0)
summary(corals0[["Area.cm2"]])                              #variable
range(corals0[["Area.cm2"]], finite = T)                    #max & min values

quantile(corals0[["Area.cm2"]], c(1/3, 2/3), na.rm = T)     #quantiles 

m = round(mean(corals0[["Area.cm2"]], na.rm = T), 3)

#histogram & density (all)
ggplot(corals0, aes(x = Area.cm2)) + geom_histogram(fill = "coral",
            col = "coral4", binwidth = 1, position = "identity") +
            geom_density(aes(y = ..density..*220), fill = "coral",
            col = "coral4", alpha = .2) +
            geom_vline(aes(xintercept = m), linetype = 2, col = "coral4") +
            annotate("text", x = m*1.4, y = 40, col = "coral4",
            label = paste("Mean =", m)) +
            ggtitle("Initial Size of Fragments") + theme_test() + 
            ylab("Frequency") + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5))

#histogram & density (all)
ggplot(corals0, aes(x = Area.cm2)) + geom_histogram(fill = "coral",
            col = "coral4", binwidth = 1, position = "identity") +
            geom_density(aes(y = ..density..*220), fill = "coral",
            col = "coral4", alpha = .2) +
            geom_vline(aes(xintercept = m), linetype = 2, col = "coral4") +
            annotate("text", x = m*1.4, y = 40, col = "coral4",
            label = paste("Mean =", m)) +
            ggtitle("Initial Size of Fragments") + ylab("Frequency") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(sec.axis = sec_axis(trans = ~.x/60,
            name = "", labels = scales::percent))

#ordenar por media
stats0  <-  corals0  %>% group_by(Phenotype)                    %>%
            summarise(Mean = mean(Area.cm2, na.rm = TRUE),
            Max  = max( Area.cm2, na.rm = TRUE))         %>%
            arrange(-Mean)
stats0

#histogram & density (by Phenotype)
ggplot(corals0, aes(x = Area.cm2, fill = Phenotype, col = Phenotype)) +
            geom_histogram(position = "identity", binwidth = 1) +
            geom_density(aes(y = ..density..*25), alpha = .2) +
            geom_vline(aes(xintercept = m), linetype = 2, col = "coral4") +
            facet_wrap(~ Phenotype) + ggtitle("Initial Size of Fragments") +
            theme_test() + ylab("Frequency") + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) + scale_color_hue(l = 25)

stats0  <-  stats0  %>% mutate(Phenotype = as.character(Phenotype)) %>%
                        arrange(Phenotype)
stats0

pwc0    <-  with(corals0, kruskal(Area.cm2, Phenotype, alpha = .05)[["groups"]])

pwc0    <-  pwc0 %>%  rownames_to_column() %>% rename(Phenotype = rowname) %>%
                      arrange(Phenotype)
pwc0

stats0["L"] <-  pwc0["groups"]
stats0
stats0      <-  stats0 %>% arrange(-Mean)
stats0

corals0     <- corals0 %>% mutate(Phenotype = factor(Phenotype,
                                     levels = stats0[["Phenotype"]]))

#boxplot
ggplot(corals0, aes(x = Phenotype, y = Area.cm2, fill = Phenotype,
            col = Phenotype)) + #stat_boxplot(geom = "errorbar", width = .1) + 
            geom_jitter(width = .1, size = .5, alpha= .5) +
            geom_boxplot(alpha = .65, outlier.shape = NA) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            ggtitle("Initial Size of Fragments") + theme_test() +
            theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1),
            legend.position = "none", plot.title = element_text(hjust = .5)) +
            scale_color_hue(l = 25) +
            geom_text(data = stats0, aes(Phenotype, Max+.4, label = L))

ggplot(corals0, aes(x = Phenotype, y = Area.cm2, fill = Phenotype,
            col = Phenotype)) + #stat_boxplot(geom = "errorbar", width = .1) + 
            geom_jitter(width = .1, size = .5, alpha= .5) + facet_wrap(~Group) +
            geom_boxplot(alpha = .65, outlier.shape = NA) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            ggtitle("Initial Size of Fragments") + theme_test() +
            theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1),
            legend.position = "none", plot.title = element_text(hjust = .5)) +
            scale_color_hue(l = 25)

corals0 %>% filter(Phenotype %in% c("P4", "P9", "P13")) %>%
ggplot(aes(x = Group, y = Area.cm2, fill = Phenotype,
            col = Phenotype)) + #stat_boxplot(geom = "errorbar", width = .1) + 
            geom_jitter(width = .1, size = .5, alpha= .5) + facet_wrap(~Phenotype) +
            geom_boxplot(alpha = .65, outlier.shape = NA) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
  stat_compare_means(method = "wilcox.test", label = "p.signif",
                     label.x = 1, label.y = 15, hide.ns = F) +
            ggtitle("Initial Size of Fragments") + theme_test() +
            theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1),
            legend.position = "none", plot.title = element_text(hjust = .5)) +
            scale_color_hue(l = 25)

##


#violinplot
ggplot(corals0, aes(x = Phenotype, y = Area.cm2, fill = Phenotype,
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

rm(m, pwc0, stats0)
rm(growth, corals0, corals)






                ## Crecimiento Corales Isabela - Testigo ##


## DATA MANAGEMENT ##

corals  <-  read_excel("Datos/Analizados/Corales Isabela - Master Database.xlsx",
                       sheet = "Master")

#fragmentos grupo CONTROL (TESTIGOS)
corals   <-  corals %>% filter(Group == "Control")

#factors
corals   <- corals  %>% mutate(Date = as.Date(Date))               %>%
                        mutate(Phenotype = factor(Phenotype,
                                  levels = c("P13", "P9", "P4")))  %>%
                        mutate_at(c("Measuring", "Year", "Month", "Day",
                        "Plane", "State", "Fragment"), factor)

corals
summary(corals)
summary(corals[["Phenotype"]])



## PLOTTING GROWTH: LINEAR FUNCTIONS ##

#vertical
corals %>%
  filter(Plane %in% "Vertical") %>%
  ggplot(aes(x = Date, y = Area.cm2, group = Fragment, col = Phenotype)) +
          geom_point() + geom_line() +  facet_wrap(~Phenotype) +
          ggtitle("Coral Vertical Growth") + theme_test() +
          theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
          scale_x_date(breaks = as.Date(c("2022-08-05", "2023-01-12")),
          date_labels = "%b %Y", expand = c(0.25,0)) +
          scale_y_continuous(expand = c(0,1)) + 
          scale_color_brewer(palette = "Dark2")

#horizontal
corals %>%
  filter(Plane %in% "Horizontal") %>%
          ggplot(aes(x = Date, y = Area.cm2, group = Fragment, col = Phenotype)) +
          geom_point() + geom_line() + facet_wrap(~ Phenotype) +
          ggtitle("Coral Horizontal Growth") + theme_test() +
          theme(legend.position = "none",
          plot.title = element_text(hjust = .5)) + 
          scale_x_date(breaks = as.Date(c("2022-08-05", "2023-01-12")),
          date_labels = "%b %Y", expand = c(0.25,0)) +
          scale_y_continuous(expand = c(0,1)) +
          scale_color_brewer(palette = "Dark2")

ggplot(corals, aes(x = Date, y = Area.cm2, group = Fragment, col = Phenotype)) +
          geom_point() + geom_line() + facet_grid(Plane ~ Phenotype) +
          theme_test() + ggtitle("Coral Growth") +
          theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
          scale_x_date(breaks = as.Date(c("2022-08-05", "2023-01-12")),
          date_labels = "%b %Y", expand = c(0.25,0)) +
          scale_y_continuous(expand = c(0,1)) + 
          scale_color_brewer(palette = "Dark2")



## GROWTH RATES (d/day) ##

growth  <-  read_excel("Datos/Analizados/Corales Isabela - Master Database.xlsx",
                       sheet = "RatesR")

#fragmentos grupo CONTROL ("testigos")
growth  <-  growth   %>% filter(Group == "Testigo")

#factors
growth  <-  growth         %>%
            filter(Plane    ==  "Horizontal")   %>%  #droplevels()         %>%
            mutate(Season    =  factor(Season,
                                levels = c("Warm 2022", "Cold 2022")))    %>%
            mutate(Phenotype = factor(Phenotype,
                                levels = c("P13", "P9", "P4")))           %>%
            mutate_at(c("Plane", "Fragment"), factor)

str(growth)
summary(growth)

quantile(growth$dA,   c(1/4, 1/2, 3/4), na.rm = T)
quantile(growth$pdA,  c(1/4, 1/2, 3/4), na.rm = T)

growth  %>% group_by(Season)  %>%
            summarise(var1 = var(dA,   na.rm = T),
            var2 = var(pdA,  na.rm = T))

#histograms
ggplot(growth, aes(x = dA, fill = Season, col = Season)) +
            geom_histogram(position = "identity", binwidth = .5) +
            geom_density(aes(y = after_stat(density)*16), alpha = .2) +
            facet_wrap(~Season, nrow = 2) + ylab("Frequency") +
            ggtitle("Seasonal Coral Net Growth") + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_npg() + scale_color_hue(l = 25) +
ggplot(growth, aes(x = pdA, fill = Season, col = Season)) +
            geom_histogram(position = "identity", binwidth = .1) +
            geom_density(aes(y = after_stat(density)*3), alpha = .2) +
            facet_wrap(~Season, nrow = 2) +
            scale_x_continuous(labels = scales::percent) + ylab("") +
            ggtitle("Seasonal Coral Growth Rate") + ylab("") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_npg() + scale_color_hue(l = 25)

#bloxplots
ggplot(growth, aes(x = Season, y = dA, col = Season)) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(aes(fill = Season)) + 
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "wilcox.test",
            label.x = 1.5, label.y = 26) +
            ggtitle("Seasonal Coral Net Growth") + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = .5)) +
            scale_fill_npg() + scale_color_hue(l = 25) +
ggplot(growth, aes(x = Season, y = pdA, col = Season)) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(aes(fill = Season)) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "wilcox.test", label.x = 1.5) +
            ggtitle("Seasonal Coral Growth Rate") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = .5)) +
            scale_y_continuous(labels = scales::percent) +
            scale_fill_npg() + scale_color_hue(l = 25)

#boxplots by phenotypes
ggplot(growth, aes(x = Phenotype, y = dA, fill = Phenotype)) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(outlier.size = 1) + 
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "kruskal", label = "p.signif",
            label.x = 2.25, label.y = 9, hide.ns = T) +
            ggtitle("Control Coral Net Growth") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_brewer(palette = "Dark2") +
ggplot(growth, aes(x = Phenotype, y = pdA, fill = Phenotype)) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(outlier.size = 1) + 
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "kruskal", label = "p.signif",
            label.x = 2.25, label.y = 1.8, hide.ns = T) +
            ggtitle("Control Coral Growth Rate") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_brewer(palette = "Dark2") +
            scale_y_continuous(labels = scales::percent)



## DISTRIBUTION - INITIAL SIZE OF FRAGMENTS ##

corals0 <-  corals  %>%   filter(Measuring == "1")            %>%
                          filter(Plane     == "Horizontal")

summary(corals0)
summary(corals0[["Area.cm2"]])                              #variable
range(corals0[["Area.cm2"]], finite = T)                    #max & min values

quantile(corals0[["Area.cm2"]], c(1/3, 2/3), na.rm = T)     #quantiles 

m = round(mean(corals0[["Area.cm2"]], na.rm = T), 3)

#histogram & density (all)
ggplot(corals0, aes(x = Area.cm2)) + geom_histogram(fill = "coral",
            col = "coral4", binwidth = 1, position = "identity") +
            geom_density(aes(y = ..density..*50), fill = "coral",
            col = "coral4", alpha = .2) +
            geom_vline(aes(xintercept = m), linetype = 2, col = "coral4") +
            annotate("text", x = m*1.4, y = 7, col = "coral4",
            label = paste("Mean =", m)) +
            ggtitle("Initial Size of Fragments") + theme_test() + 
            ylab("Frequency") + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5))

#histogram & density (all)
ggplot(corals0, aes(x = Area.cm2)) + geom_histogram(fill = "coral",
            col = "coral4", binwidth = 1, position = "identity") +
            geom_density(aes(y = ..density..*50), fill = "coral",
            col = "coral4", alpha = .2) +
            geom_vline(aes(xintercept = m), linetype = 2, col = "coral4") +
            annotate("text", x = m*1.4, y = 7, col = "coral4",
            label = paste("Mean =", m)) +
            ggtitle("Initial Size of Fragments") + ylab("Frequency") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(sec.axis = sec_axis(trans = ~.x/60,
            name = "", labels = scales::percent))

#ordenar por media
stats0  <-  corals0  %>% group_by(Phenotype)                    %>%
            summarise(Mean = mean(Area.cm2, na.rm = TRUE),
            Max  = max( Area.cm2, na.rm = TRUE))         %>%
            arrange(-Mean)
stats0

#histogram & density (by Phenotype)
ggplot(corals0, aes(x = Area.cm2, fill = Phenotype, col = Phenotype)) +
            geom_histogram(position = "identity", binwidth = 1) +
            geom_density(aes(y = ..density..*20), alpha = .2) +
            geom_vline(aes(xintercept = m), linetype = 2) +
            facet_wrap(~ Phenotype) + ggtitle("Initial Size of Fragments") +
            theme_test() + ylab("Frequency") + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_brewer(palette = "Dark2") +
            scale_color_brewer(palette = "Dark2")

stats0  <-  stats0  %>% mutate(Phenotype = as.character(Phenotype)) %>%
            arrange(Phenotype)
stats0

pwc0    <-  with(corals0, kruskal(Area.cm2, Phenotype, alpha = .05)[["groups"]])

pwc0    <-  pwc0 %>%  rownames_to_column() %>% rename(Phenotype = rowname) %>%
            arrange(Phenotype)
pwc0

stats0["L"] <-  pwc0["groups"]
stats0
stats0      <-  stats0 %>% arrange(-Mean)
stats0

corals0     <- corals0 %>% mutate(Phenotype = factor(Phenotype,
                                     levels = stats0[["Phenotype"]]))

#boxplot
ggplot(corals0, aes(x = Phenotype, y = Area.cm2, fill = Phenotype)) +
          stat_boxplot(geom = "errorbar", width = .1) + 
          geom_boxplot(outlier.shape = NA) +
          geom_jitter(width = .1, size = .5, alpha= .5) +
          stat_summary(fun = 'mean', geom = 'point', shape = 4) +
          ggtitle("Initial Size of Fragments") + theme_test() +
          theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1),
          legend.position = "none", plot.title = element_text(hjust = .5)) +
          scale_fill_brewer(palette = "Dark2") + 
          geom_text(data = stats0, aes(Phenotype, Max+1, label = L))

rm(m, pwc0, stats0)
rm(growth, corals0, corals)





          ## CORALES TESTIGO VS CORALES PROYECTO PILOTO ##


## DATA MANAGEMENT ##

corals  <-  read_excel("Datos/Analizados/Corales Isabela - Master Database.xlsx",
                       sheet = "RatesR")

#fenotipos
t_vs_p   <-   corals %>%  filter(Phenotype %in% c("P4", "P9", "P13"))

#factors
t_vs_p   <-   t_vs_p %>%  filter(Plane == "Horizontal")               %>% 
                          mutate_at(c("Group", "Season",
                          "Plane", "Fragment"), factor)               %>%
                          mutate(Phenotype = factor(Phenotype,
                          levels = c("P13", "P9", "P4")))         %>%
                          mutate(Test = factor(Test, levels = c(
                          "Control - Cold", "Piloto - Cold",
                          "Piloto - Warm")))

t_vs_p
summary(t_vs_p)
summary(t_vs_p[["Phenotype"]])

#test (Area)
res.aov1 <- t_vs_p %>% welch_anova_test(dA  ~ Test)
res.aov2 <- t_vs_p %>% welch_anova_test(pdA ~ Test)


pwc1  <-    t_vs_p  %>%   games_howell_test(dA ~ Test)   %>%
                          add_xy_position(x = "Test")
pwc2  <-    t_vs_p  %>%   games_howell_test(pdA ~ Test)  %>%
                          add_xy_position(x = "Test")

pwc1$p.adj.signif <-  gsub("ns", "", pwc1$p.adj.signif)
pwc2$p.adj.signif <-  gsub("ns", "", pwc2$p.adj.signif)

ggplot(t_vs_p, aes(x = Test, y = `dA`, col = Test)) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(aes(fill = Test)) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            #stat_anova_test(label.y = 2.9) +
            stat_pvalue_manual(pwc1, hide.ns = F) + xlab("") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_color_hue(l = 25) + ylab("dA (cm2/100 days)") +
            labs(subtitle = get_test_label(res.aov1, detailed = F,
            description = "Welch’s ANOVA"), caption = get_pwc_label(pwc1)) +
ggplot(t_vs_p, aes(x = Test, y = `pdA`, col = Test)) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(aes(fill = Test)) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            #stat_welch_anova_test(label.y = .98) +
            stat_pvalue_manual(pwc2, hide.ns = F) + xlab("") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(labels = scales::percent) +
            scale_color_hue(l = 25) + ylab("dA (%/100 days)") +
            labs(subtitle = get_test_label(res.aov2, detailed = F,
            description = "Welch’s ANOVA"), caption = get_pwc_label(pwc2))

get_test_label(res.aov1, detailed = F, description = "ANOVA")

#ggsave("coral.treatment.png", width = 7, height = 4, dpi = 2000)

rm(pwc1, pwc2, res.aov1, res.aov2)


#test (ML)
res.aov1 <- t_vs_p %>% welch_anova_test(dML  ~ Test)
res.aov2 <- t_vs_p %>% welch_anova_test(pdML ~ Test)


pwc1  <-    t_vs_p  %>%   games_howell_test(dML  ~ Test)  %>%
                          add_xy_position(x = "Test")
pwc2  <-    t_vs_p  %>%   games_howell_test(pdML ~ Test)  %>%
                          add_xy_position(x = "Test")

pwc1$p.adj.signif <-  gsub("ns", "", pwc1$p.adj.signif)
pwc2$p.adj.signif <-  gsub("ns", "", pwc2$p.adj.signif)

ggplot(t_vs_p, aes(x = Test, y = `dML`, col = Test)) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(aes(fill = Test)) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            #stat_anova_test(label.y = 2.9) +
            stat_pvalue_manual(pwc1, hide.ns = F) + xlab("") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_color_hue(l = 25) + ylab("dML (cm/100 days)") +
            labs(subtitle = get_test_label(res.aov1, detailed = F,
            description = "ANOVA"), caption = get_pwc_label(pwc1)) +
  ggplot(t_vs_p, aes(x = Test, y = `pdML`, col = Test)) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(aes(fill = Test)) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            #stat_welch_anova_test(label.y = .98) +
            stat_pvalue_manual(pwc2, hide.ns = F) + xlab("") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(labels = scales::percent) +
            scale_color_hue(l = 25) + ylab("dML (%/100 days)") +
            labs(subtitle = get_test_label(res.aov2, detailed = F,
            description = "Welch’s ANOVA"), caption = get_pwc_label(pwc2))

get_test_label(res.aov1, detailed = F, description = "ANOVA")

#ggsave("coral.treatment.png", width = 7, height = 4, dpi = 2000)


t_vs_p %>%
  filter(Season == "Cold 2022")   %>%  ggplot(aes(x = Group, y = `dA`)) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(aes(fill = Phenotype), col = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "wilcox.test", label.y = 18) +
            ggtitle("Coral Growth in Cold Season") +
            facet_wrap(~ Phenotype) + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_brewer(palette = "Dark2")

t_vs_p %>%
    filter(Season == "Cold 2022")   %>%  ggplot(aes(x = Group, y = `pdA`)) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(aes(fill = Phenotype), col = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "wilcox.test") +
            ggtitle("Coral Growth in Cold Season") +
            facet_wrap(~ Phenotype) + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(labels = scales::percent) +
            scale_fill_brewer(palette = "Dark2")


# ANOVA ASSUMPTIONS (Area)

#outliers
t_vs_p %>%  group_by(Test)              %>%  select(Test, dA)        %>%
            identify_outliers(dA)
t_vs_p %>%  group_by(Test)              %>%  select(Test, pdA)       %>%
            identify_outliers(pdA)

#normality test (Shapiro-Wilk)
t_vs_p %>%  shapiro_test(dA)
t_vs_p %>%  shapiro_test(pdA)

#residuals
normal3    <- lm(dA   ~ Test, t_vs_p)
normal4    <- lm(pdA  ~ Test, t_vs_p)

head(normal3$fitted.values)
head(normal3$residuals)
ggqqplot(residuals(normal3)) + theme_test()
shapiro_test(residuals(normal3))
ggqqplot(t_vs_p, "dA", facet.by = "Test") + theme_test()

t_vs_p %>%  group_by(Test)  %>%  shapiro_test(dA)

head(normal4$fitted.values)
head(normal4$residuals)
ggqqplot(residuals(normal4)) + theme_test()
shapiro_test(residuals(normal4))
ggqqplot(t_vs_p, "pdA", facet.by = "Test") + theme_test()

t_vs_p %>%  group_by(Test)  %>%  shapiro_test(pdA)

#homogeneity of variance
plot(normal3, 1)
plot(normal4, 1)

#Bartlett's test (more sensitive to deviations from normality)
bartlett.test(dA  ~ Test, t_vs_p)
bartlett.test(pdA ~ Test, t_vs_p)

#Levene's test (less sensitive)
t_vs_p  %>%  levene_test(dA   ~ Test)
t_vs_p  %>%  levene_test(pdA  ~ Test)

#anovas
t_vs_p  %>%         anova_test(dA  ~ Test, detailed = T)   #ANOVA
t_vs_p  %>%   welch_anova_test(pdA ~ Test)                 #Welch’s ANOVA

#anova(lm(   dA ~ Test, t_vs_p))                            #ANOVA
#oneway.test(dA ~ Test, t_vs_p, var.equal = F)              # ≈ Welch’s ANOVA

#posthocs
t_vs_p  %>%   tukey_hsd(        dA  ~ Test)
t_vs_p  %>%   games_howell_test(pdA ~ Test)

#TukeyHSD(aov(dA ~ Test, t_vs_p))

t_vs_p %>%  filter(Season == "Cold 2022")  %>%  group_by(Phenotype, Group)  %>% 
            select(Phenotype, Group, dA)   %>%  identify_outliers(dA)
t_vs_p %>%  filter(Season == "Cold 2022")  %>%  group_by(Phenotype, Group)  %>%
            select(Phenotype, Group, pdA)  %>%  identify_outliers(pdA)

rm(normal3, normal4, pwc1, pwc2, res.aov1, res.aov2)


# ANOVA ASSUMPTIONS (ML)

#outliers
t_vs_p %>%  group_by(Test)              %>%  select(Test, dML)        %>%
            identify_outliers(dML)
t_vs_p %>%  group_by(Test)              %>%  select(Test, pdML)       %>%
            identify_outliers(pdML)

#normality test (Shapiro-Wilk)
t_vs_p %>%  shapiro_test(dML)
t_vs_p %>%  shapiro_test(pdML)

#residuals
normal3    <- lm(dML   ~ Test, t_vs_p)
normal4    <- lm(pdML  ~ Test, t_vs_p)

head(normal3$fitted.values)
head(normal3$residuals)
ggqqplot(residuals(normal3)) + theme_test()
shapiro_test(residuals(normal3))
ggqqplot(t_vs_p, "dML", facet.by = "Test") + theme_test()

t_vs_p %>%  group_by(Test)  %>%  shapiro_test(dML)

head(normal4$fitted.values)
head(normal4$residuals)
ggqqplot(residuals(normal4)) + theme_test()
shapiro_test(residuals(normal4))
ggqqplot(t_vs_p, "pdML", facet.by = "Test") + theme_test()

t_vs_p %>%  group_by(Test)  %>%  shapiro_test(pdML)

#homogeneity of variance
plot(normal3, 1)
plot(normal4, 1)

#Bartlett's test (more sensitive to deviations from normality)
bartlett.test(dML  ~ Test, t_vs_p)
bartlett.test(pdML ~ Test, t_vs_p)

#Levene's test (less sensitive)
t_vs_p  %>%  levene_test(dML   ~ Test)
t_vs_p  %>%  levene_test(pdML  ~ Test)

#anovas
t_vs_p  %>%         anova_test(dML  ~ Test, detailed = T)   #ANOVA
t_vs_p  %>%   welch_anova_test(pdML ~ Test)                 #Welch’s ANOVA

#anova(lm(   dML ~ Test, t_vs_p))                            #ANOVA
#oneway.test(dML ~ Test, t_vs_p, var.equal = F)              # ≈ Welch’s ANOVA

#posthocs
t_vs_p  %>%   tukey_hsd(        dML  ~ Test)
t_vs_p  %>%   games_howell_test(pdML ~ Test)

#TukeyHSD(aov(dML ~ Test, t_vs_p))

t_vs_p %>%  filter(Season == "Cold 2022")  %>%  group_by(Phenotype, Group)  %>% 
            select(Phenotype, Group, dML)   %>%  identify_outliers(dML)
t_vs_p %>%  filter(Season == "Cold 2022")  %>%  group_by(Phenotype, Group)  %>%
            select(Phenotype, Group, pdML)  %>%  identify_outliers(pdML)

rm(normal3, normal4, pwc1, pwc2, res.aov1, res.aov2)


## OTHER STATS

p9  <- t_vs_p %>% filter(Phenotype %in%  "P9", Season %in% "Cold 2022")
p13 <- t_vs_p %>% filter(Phenotype %in% "P13", Season %in% "Cold 2022")

wilcox.test(`dML` ~ Group,  data = p13, paired = F)
wilcox.test(`dML` ~ Group,  data = p9,  paired = F)

wilcox.test(`pdML`  ~ Group,  data = p13, paired = F)
wilcox.test(`pdML`  ~ Group,  data = p9,  paired = F)


compare_means(`dML`  ~ Test, data = t_vs_p, method = "kruskal.test")
compare_means(`pdML` ~ Test, data = t_vs_p, method = "kruskal.test")

compare_means(`dML`   ~ Group, filter(t_vs_p, Season == "Cold 2022"),
              method = "wilcox.test", group.by = "Phenotype")
compare_means(`pdML`  ~ Group, filter(t_vs_p, Season == "Cold 2022"),
              method = "wilcox.test", group.by = "Phenotype")

compare_means(`dML` ~ Phenotype, t_vs_p,
              method = "wilcox.test", group.by = "Test")
compare_means(`pdML`  ~ Phenotype, t_vs_p,
              method = "wilcox.test", group.by = "Test")


