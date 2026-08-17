                       ## Crecimiento Corales Isabela ##
                              ## Grupo Testigos ##


library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggsci)
library(patchwork)
library(multcompView)


## DATA MANAGEMENT ##

testigo <-  read_excel("Datos/Analizados/Corales Isabela - Testigos.xlsx",
                          sheet = "Testigos")

piloto  <-  read_excel("Datos/Analizados/Corales Isabela - Piloto.xlsx",
                          sheet = "Piloto")


testigo <-  testigo        %>% mutate(Date = as.Date(Date))             %>%
            mutate(Phenotype = factor(Phenotype,
                               levels = c("P13", "P9", "P4")))          %>%
            mutate_at(c("Measuring", "Year", "Month", "Day",
                               "Plane", "State", "Fragment"), factor)

summary(testigo)



## PLOTTING GROWTH: LINEAR FUNCTIONS ##

#vertical
testigo %>%
    filter(Plane %in% "Vertical") %>%
    ggplot(aes(x = Date, y = ML.cm, group = Fragment, col = Phenotype)) +
            geom_point() + geom_line() +  facet_wrap(~Phenotype) +
            ggtitle("Coral Vertical Growth") + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_x_date(breaks = as.Date(c("2022-08-05", "2023-01-12")),
            date_labels = "%b %Y", expand = c(0.25,0)) +
            scale_y_continuous(expand = c(0,1)) + 
            scale_color_brewer(palette = "Dark2")

#horizontal
testigo%>%
  filter(Plane %in% "Horizontal") %>%
  ggplot(aes(x = Date, y = ML.cm, group = Fragment, col = Phenotype)) +
            geom_point() + geom_line() + facet_wrap(~ Phenotype) +
            ggtitle("Coral Horizontal Growth") + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = .5)) + 
            scale_x_date(breaks = as.Date(c("2022-08-05", "2023-01-12")),
            date_labels = "%b %Y", expand = c(0.25,0)) +
            scale_y_continuous(expand = c(0,1)) +
            scale_color_brewer(palette = "Dark2")

ggplot(testigo, aes(x = Date, y = ML.cm, group = Fragment, col = Phenotype)) +
            geom_point() + geom_line() + facet_grid(Plane ~ Phenotype) +
            theme_test() + ggtitle("Coral Growth") +
            theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_x_date(breaks = as.Date(c("2022-08-05", "2023-01-12")),
            date_labels = "%b %Y", expand = c(0.25,0)) +
            scale_y_continuous(expand = c(0,1)) + 
            scale_color_brewer(palette = "Dark2")



## GROWTH RATES (Δ/day) ##

growth  <-  read_excel("Datos/Analizados/Corales Isabela - Testigos.xlsx",
                          sheet = "RatesR")

growth  <-  growth         %>% filter(Plane   %in% "Horizontal")        %>%
            mutate_at(c("Plane", "Fragment", "Season"), factor)         %>%
            mutate(Phenotype = factor(Phenotype, levels = c("P13", "P9", "P4")))
            #no usar VERTICAL! ERRORES

summary(growth)

quantile(growth$ΔML,   c(1/4, 1/2, 3/4), na.rm = T) 
quantile(growth$pΔML,  c(1/4, 1/2, 3/4), na.rm = T) 

#histograms
ggplot(growth, aes(x = ΔML)) +
            geom_histogram(color = 1, binwidth = .2, position = "identity",
            fill = "springgreen4") +
            geom_density(aes(y = after_stat(density)*6), alpha = .2) +
            ggtitle("Coral Net Growth") + theme_test() + ylab("Frequency") +
            theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
ggplot(growth, aes(x = pΔML)) +
            geom_histogram(color = 1, binwidth = .05, position = "identity",
            fill = "springgreen4") +
            geom_density(aes(y = after_stat(density)), alpha = .2) +
            ggtitle("Coral Growth Rate") + theme_test() + ylab("Frequency") +
            theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_x_continuous(labels = scales::percent)

#bloxplots
ggplot(growth, aes(x = Season, y = ΔML)) +
            stat_boxplot(geom = "errorbar", width = .05) +
            geom_boxplot(fill = "springgreen4", color = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            ggtitle("Coral Net Growth") + xlab("") + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = .5)) +
ggplot(growth, aes(x = Season, y = pΔML)) +
            stat_boxplot(geom = "errorbar", width = .05) +
            geom_boxplot(fill = "springgreen4", color = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            ggtitle("Coral Growth Rate") + xlab("") + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = .5)) +
            scale_y_continuous(labels = scales::percent)

#boxplots by phenotypes
ggplot(growth, aes(x = Phenotype, y = `ΔML`,
            fill = Phenotype)) + stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(col = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "anova", label.x = 1.6) +
            ggtitle("Coral Net Growth by Phenotype") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_brewer(palette = "Dark2") +
ggplot(growth, aes(x = Phenotype, y = pΔML,
            fill = Phenotype)) + stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(col = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "anova", label.x = 1.6) +
            ggtitle("Coral Growth Rate by Phenotype") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(labels = scales::percent) +
            scale_fill_brewer(palette = "Dark2") 


# ANOVA ASSUMPTIONS

#outliers
growth %>%  group_by(Phenotype)        %>%  select(Phenotype, ΔML)  %>%
            identify_outliers(ΔML)     #%>%  filter(is.extreme == T)
growth %>%  group_by(Phenotype)        %>%  select(Phenotype, pΔML) %>%
            identify_outliers(pΔML)    #%>%  filter(is.extreme == T)

#normality test (Shapiro-Wilk)
growth %>%  shapiro_test(ΔML, pΔML)    #si n > 50, Shapiro-Wilk es muy sensible
nrow(growth)

#residuals
normal1  <- lm(ΔML   ~ Phenotype, growth)
normal2  <- lm(pΔML  ~ Phenotype, growth)

head(normal1$fitted.values)
head(normal1$residuals)
ggqqplot(residuals(normal1)) + theme_test()

shapiro_test(residuals(normal1))

head(normal2$fitted.values)
head(normal2$residuals)
ggqqplot(residuals(normal2)) + theme_test()
shapiro_test(residuals(normal2))

#by phenotypes
ggqqplot(growth, "ΔML",  facet.by = "Phenotype") + theme_test()
ggqqplot(growth, "pΔML", facet.by = "Phenotype") + theme_test()

growth  %>%  group_by(Phenotype)  %>% shapiro_test(ΔML, pΔML)

#homogeneity of variance
plot(normal1, 1)
plot(normal2, 1)

#Bartlett's test (more sensitive)
bartlett.test(ΔML  ~ Phenotype, growth)
bartlett.test(pΔML ~ Phenotype, growth)

#Levene's test (less sensitive)
growth  %>%  levene_test(ΔML   ~ Phenotype)
growth  %>%  levene_test(pΔML  ~ Phenotype)

#varianzas significativamente diferentes: Welch ANOVA: welch_anova_test() y
#Games-Howell como posthoc. https://www.youtube.com/watch?v=2pfOkUbigr0

#anovas
anova(aov(ΔML  ~ Phenotype, data = growth))
anova(aov(pΔML ~ Phenotype, data = growth))

growth  %>%   welch_anova_test(ΔML   ~ Phenotype)
growth  %>%   welch_anova_test(pΔML  ~ Phenotype)



## CORALES TESTIGO VS CORALES PROYECTO PILOTO

t_vs_p  <-  read_excel("Datos/Analizados/Corales Isabela - Comparación.xlsx")

t_vs_p  <-  t_vs_p     %>%  filter(Plane == "Horizontal")               %>% 
            mutate_at(c("Group", "Season",
                        "Plane", "Fragment"), factor)                   %>%
            mutate(Phenotype = factor(Phenotype,
                      levels = c("P13", "P9", "P4")))         %>%
            mutate(Test      = factor(Test,
                      levels = c("Testigo - Cold", "Piloto - Cold",
                                 "Piloto - Warm")))
summary(t_vs_p)

ggplot(t_vs_p, aes(x = Phenotype, y = `ΔML`, fill = Phenotype),) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(col = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "anova", label.y = 2.4) +
            facet_wrap(~ Test) + theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_brewer(palette = "Dark2")

ggplot(t_vs_p, aes(x = Phenotype, y = `pΔML`, fill = Phenotype)) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(col = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "anova", label.y = .75) +
            facet_wrap(~ Test) + theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(labels = scales::percent) +
            scale_fill_brewer(palette = "Dark2")

res.aov1 <- t_vs_p %>% anova_test(ΔML  ~ Test)
res.aov2 <- t_vs_p %>% welch_anova_test(pΔML ~ Test)


pwc1  <-    t_vs_p  %>%   tukey_hsd(ΔML ~ Test)           %>%
                          add_xy_position(x = "Test")
pwc2  <-    t_vs_p  %>%   games_howell_test(pΔML ~ Test)  %>%
                          add_xy_position(x = "Test")

pwc1$p.adj.signif <-  gsub("ns", "", pwc1$p.adj.signif)
pwc2$p.adj.signif <-  gsub("ns", "", pwc2$p.adj.signif)

ggplot(t_vs_p, aes(x = Test, y = `ΔML`, col = Test)) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(aes(fill = Test)) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            #stat_anova_test(label.y = 2.9) +
            stat_pvalue_manual(pwc1, hide.ns = F) + xlab("") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_color_hue(l = 25) + ylab("ΔML (cm/100 days)") +
            labs(subtitle = get_test_label(res.aov1, detailed = F,
            description = "ANOVA"), caption = get_pwc_label(pwc1)) +
ggplot(t_vs_p, aes(x = Test, y = `pΔML`, col = Test)) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(aes(fill = Test)) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            #stat_welch_anova_test(label.y = .98) +
            stat_pvalue_manual(pwc2, hide.ns = F) + xlab("") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(labels = scales::percent) +
            scale_color_hue(l = 25) + ylab("ΔML (%/100 days)") +
            labs(subtitle = get_test_label(res.aov2, detailed = F,
            description = "Welch’s ANOVA"), caption = get_pwc_label(pwc2))

get_test_label(res.aov1, detailed = F, description = "ANOVA")

#ggsave("coral.treatment.png", width = 7, height = 4, dpi = 2000)

t_vs_p %>%
  filter(Season == "Cold 2022")   %>%
  ggplot(aes(x = Group, y = `ΔML`)) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(aes(fill = Phenotype), col = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "wilcox.test", label.y = 2.4) +
            ggtitle("Coral Growth in Cold Season") +
            facet_wrap(~ Phenotype) + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_brewer(palette = "Dark2")

t_vs_p %>%
  filter(Season == "Cold 2022")   %>%
  ggplot(aes(x = Group, y = `pΔML`)) +
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


# ANOVA ASSUMPTIONS

#outliers
t_vs_p %>%  group_by(Test)              %>%  select(Test, ΔML)        %>%
            identify_outliers(ΔML)
t_vs_p %>%  group_by(Test)              %>%  select(Test, pΔML)       %>%
            identify_outliers(pΔML)

#normality test (Shapiro-Wilk)
t_vs_p %>%  shapiro_test(ΔML)
t_vs_p %>%  shapiro_test(pΔML)

#residuals
normal3    <- lm(ΔML   ~ Test, t_vs_p)
normal4    <- lm(pΔML  ~ Test, t_vs_p)

head(normal3$fitted.values)
head(normal3$residuals)
ggqqplot(residuals(normal3)) + theme_test()
shapiro_test(residuals(normal3))
ggqqplot(t_vs_p, "ΔML", facet.by = "Test") + theme_test()

t_vs_p %>%  group_by(Test)  %>%  shapiro_test(ΔML)

head(normal4$fitted.values)
head(normal4$residuals)
ggqqplot(residuals(normal4)) + theme_test()
shapiro_test(residuals(normal4))
ggqqplot(t_vs_p, "pΔML", facet.by = "Test") + theme_test()

t_vs_p %>%  group_by(Test)  %>%  shapiro_test(pΔML)

#homogeneity of variance
plot(normal3, 1)
plot(normal4, 1)

#Bartlett's test (more sensitive to deviations from normality)
bartlett.test(ΔML  ~ Test, t_vs_p)
bartlett.test(pΔML ~ Test, t_vs_p)

#Levene's test (less sensitive)
t_vs_p  %>%  levene_test(ΔML   ~ Test)
t_vs_p  %>%  levene_test(pΔML  ~ Test)

#anovas
t_vs_p  %>%         anova_test(ΔML  ~ Test, detailed = T)   #ANOVA
t_vs_p  %>%   welch_anova_test(pΔML ~ Test)                 #Welch’s ANOVA

#anova(lm(   ΔML ~ Test, t_vs_p))                            #ANOVA
#oneway.test(ΔML ~ Test, t_vs_p, var.equal = F)              # ≈ Welch’s ANOVA

#posthocs
t_vs_p  %>%   tukey_hsd(        ΔML  ~ Test)
t_vs_p  %>%   games_howell_test(pΔML ~ Test)

#TukeyHSD(aov(ΔML ~ Test, t_vs_p))

t_vs_p %>%  filter(Season == "Cold 2022")  %>%  group_by(Phenotype, Group)  %>% 
            select(Phenotype, Group, ΔML)  %>%  identify_outliers(ΔML)
t_vs_p %>%  filter(Season == "Cold 2022")  %>%  group_by(Phenotype, Group)  %>%
            select(Phenotype, Group, pΔML) %>%  identify_outliers(pΔML)



## OTHER STATS

p9  <- t_vs_p %>% filter(Phenotype %in%  "P9", Season %in% "Cold 2022")
p13 <- t_vs_p %>% filter(Phenotype %in% "P13", Season %in% "Cold 2022")

wilcox.test(`ΔML` ~ Group,  data = p13, paired = F)
wilcox.test(`ΔML` ~ Group,  data = p9,  paired = F)

wilcox.test(`pΔML`  ~ Group,  data = p13, paired = F)
wilcox.test(`pΔML`  ~ Group,  data = p9,  paired = F)


compare_means(`ΔML`  ~ Test, data = t_vs_p, method = "kruskal.test")
compare_means(`pΔML` ~ Test, data = t_vs_p, method = "kruskal.test")

compare_means(`ΔML`   ~ Group, filter(t_vs_p, Season == "Cold 2022"),
              method = "wilcox.test", group.by = "Phenotype")
compare_means(`pΔML`  ~ Group, filter(t_vs_p, Season == "Cold 2022"),
              method = "wilcox.test", group.by = "Phenotype")

compare_means(`ΔML` ~ Phenotype, t_vs_p,
              method = "wilcox.test", group.by = "Test")
compare_means(`pΔML`  ~ Phenotype, t_vs_p,
              method = "wilcox.test", group.by = "Test")

