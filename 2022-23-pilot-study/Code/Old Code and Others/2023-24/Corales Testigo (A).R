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
testigo%>%
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

ggplot(testigo, aes(x = Date, y = Area.cm2, group = Fragment, col = Phenotype)) +
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

quantile(growth$ΔA,   c(1/4, 1/2, 3/4), na.rm = T) 
quantile(growth$pΔA,  c(1/4, 1/2, 3/4), na.rm = T) 

#histograms
ggplot(growth, aes(x = ΔA)) +
            geom_histogram(color = 1, binwidth = .4, position = "identity",
            fill = "springgreen4") +
            geom_density(aes(y = after_stat(density)*6), alpha = .2) +
            ggtitle("Coral Net Growth") + theme_test() + ylab("Frequency") +
            theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
ggplot(growth, aes(x = pΔA)) +
            geom_histogram(color = 1, binwidth = .08, position = "identity",
            fill = "springgreen4") +
            geom_density(aes(y = after_stat(density)), alpha = .2) +
            ggtitle("Coral Growth Rate") + theme_test() + ylab("Frequency") +
            theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_x_continuous(labels = scales::percent)

#bloxplots
ggplot(growth, aes(x = Season, y = ΔA)) +
            stat_boxplot(geom = "errorbar", width = .05) +
            geom_boxplot(fill = "springgreen4", color = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            ggtitle("Coral Net Growth") + xlab("") + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = .5)) +
ggplot(growth, aes(x = Season, y = pΔA)) +
            stat_boxplot(geom = "errorbar", width = .05) +
            geom_boxplot(fill = "springgreen4", color = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            ggtitle("Coral Growth Rate") + xlab("") + theme_test() +
            theme(legend.position = "none",
            plot.title = element_text(hjust = .5)) +
            scale_y_continuous(labels = scales::percent)

#boxplots by phenotypes
ggplot(growth, aes(x = Phenotype, y = `ΔA`,
            fill = Phenotype)) + stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(col = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            ggtitle("Coral Net Growth by Phenotype") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_brewer(palette = "Dark2") +
ggplot(growth, aes(x = Phenotype, y = pΔA,
            fill = Phenotype)) + stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(col = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            ggtitle("Coral Growth Rate by Phenotype") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(labels = scales::percent) +
            scale_fill_brewer(palette = "Dark2") 


# ANOVA ASSUMPTIONS

#outliers
growth %>%  group_by(Phenotype)        %>%  select(Phenotype, ΔA)  %>%
            identify_outliers(ΔA)     #%>%  filter(is.extreme == T)
growth %>%  group_by(Phenotype)        %>%  select(Phenotype, pΔA) %>%
            identify_outliers(pΔA)    #%>%  filter(is.extreme == T)

#normality test (Shapiro-Wilk)
growth %>%  shapiro_test(ΔA, pΔA)    #si n > 50, Shapiro-Wilk es muy sensible
nrow(growth)

#residuals
normal1  <- lm(ΔA   ~ Phenotype, growth)
normal2  <- lm(pΔA  ~ Phenotype, growth)

head(normal1$fitted.values)
head(normal1$residuals)
ggqqplot(residuals(normal1)) + theme_test()

shapiro_test(residuals(normal1))

head(normal2$fitted.values)
head(normal2$residuals)
ggqqplot(residuals(normal2)) + theme_test()
shapiro_test(residuals(normal2))

#by phenotypes
ggqqplot(growth, "ΔA",  facet.by = "Phenotype") + theme_test()
ggqqplot(growth, "pΔA", facet.by = "Phenotype") + theme_test()

growth  %>%  group_by(Phenotype)  %>% shapiro_test(ΔA, pΔA)

#homogeneity of variance
plot(normal1, 1)
plot(normal2, 1)

#Bartlett's test (more sensitive)
bartlett.test(ΔA  ~ Phenotype, growth)
bartlett.test(pΔA ~ Phenotype, growth)

#Levene's test (less sensitive)
growth  %>%  levene_test(ΔA   ~ Phenotype)
growth  %>%  levene_test(pΔA  ~ Phenotype)

#varianzas significativamente diferentes: Welch ANOVA: welch_anova_test() y
#Games-Howell como posthoc. https://www.youtube.com/watch?v=2pfOkUbigr0

#anovas
anova(aov(ΔA  ~ Phenotype, data = growth))
anova(aov(pΔA ~ Phenotype, data = growth))

growth  %>%   welch_anova_test(ΔA   ~ Phenotype)
growth  %>%   welch_anova_test(pΔA  ~ Phenotype)



## CORALES TESTIGO VS CORALES PROYECTO PILOTO

t_vs_p  <-  read_excel("Datos/Analizados/Corales Isabela - Comparación.xlsx")

t_vs_p  <-  t_vs_p     %>%  filter(Plane == "Horizontal")               %>% 
            mutate_at(c("Group", "Season",
                        "Plane", "Fragment"), factor)                   %>%
            mutate(Phenotype = factor(Phenotype,
                      levels = c("P13", "P9", "P4")))         %>%
            mutate(Test      = factor(Test,
                      levels = c("Piloto - Warm", "Piloto - Cold",
                                 "Testigo - Cold")))
summary(t_vs_p)

ggplot(t_vs_p, aes(x = Phenotype, y = `ΔA`, fill = Phenotype)) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(col = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            facet_wrap(~ Test) + theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_fill_brewer(palette = "Dark2")

ggplot(t_vs_p, aes(x = Phenotype, y = `pΔA`, fill = Phenotype)) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(col = 1) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            facet_wrap(~ Test) + theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(labels = scales::percent) +
            scale_fill_brewer(palette = "Dark2")

res.aov1 <- t_vs_p %>% welch_anova_test(ΔA  ~ Test)
res.aov2 <- t_vs_p %>% welch_anova_test(pΔA ~ Test)


pwc1  <-    t_vs_p  %>%   games_howell_test(ΔA ~ Test)   %>%
                          add_xy_position(x = "Test")
pwc2  <-    t_vs_p  %>%   games_howell_test(pΔA ~ Test)  %>%
                          add_xy_position(x = "Test")

pwc1$p.adj.signif <-  gsub("ns", "", pwc1$p.adj.signif)
pwc2$p.adj.signif <-  gsub("ns", "", pwc2$p.adj.signif)

ggplot(t_vs_p, aes(x = Test, y = `ΔA`, col = Test)) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(aes(fill = Test)) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            #stat_anova_test(label.y = 2.9) +
            stat_pvalue_manual(pwc1, hide.ns = F) + xlab("") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_color_hue(l = 25) + ylab("ΔA (cm2/100 days)") +
            labs(subtitle = get_test_label(res.aov1, detailed = F,
            description = "Welch’s ANOVA"), caption = get_pwc_label(pwc1)) +
ggplot(t_vs_p, aes(x = Test, y = `pΔA`, col = Test)) +
            stat_boxplot(geom = "errorbar", width = 0.1) +
            geom_boxplot(aes(fill = Test)) +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            #stat_welch_anova_test(label.y = .98) +
            stat_pvalue_manual(pwc2, hide.ns = F) + xlab("") +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(labels = scales::percent) +
            scale_color_hue(l = 25) + ylab("ΔA (%/100 days)") +
            labs(subtitle = get_test_label(res.aov2, detailed = F,
            description = "Welch’s ANOVA"), caption = get_pwc_label(pwc2))

get_test_label(res.aov1, detailed = F, description = "ANOVA")

#ggsave("coral.treatment.origin1.png", width = 7, height = 4, dpi = 2000)

t_vs_p %>%
  filter(Season == "Cold 2022")   %>%
  ggplot(aes(x = Group, y = `ΔA`)) +
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
  filter(Season == "Cold 2022")   %>%
  ggplot(aes(x = Group, y = `pΔA`)) +
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
t_vs_p %>%  group_by(Test)              %>%  select(Test, ΔA)        %>%
            identify_outliers(ΔA)
t_vs_p %>%  group_by(Test)              %>%  select(Test, pΔA)       %>%
            identify_outliers(pΔA)

#normality test (Shapiro-Wilk)
t_vs_p %>%  shapiro_test(ΔA)
t_vs_p %>%  shapiro_test(pΔA)

#residuals
normal3    <- lm(ΔA   ~ Test, t_vs_p)
normal4    <- lm(pΔA  ~ Test, t_vs_p)

head(normal3$fitted.values)
head(normal3$residuals)
ggqqplot(residuals(normal3)) + theme_test()
shapiro_test(residuals(normal3))
ggqqplot(t_vs_p, "ΔA", facet.by = "Test") + theme_test()

t_vs_p %>%  group_by(Test)  %>%  shapiro_test(ΔA)

head(normal4$fitted.values)
head(normal4$residuals)
ggqqplot(residuals(normal4)) + theme_test()
shapiro_test(residuals(normal4))
ggqqplot(t_vs_p, "pΔA", facet.by = "Test") + theme_test()

t_vs_p %>%  group_by(Test)  %>%  shapiro_test(pΔA)

#homogeneity of variance
plot(normal3, 1)
plot(normal4, 1)

#Bartlett's test (more sensitive to deviations from normality)
bartlett.test(ΔA  ~ Test, t_vs_p)
bartlett.test(pΔA ~ Test, t_vs_p)

#Levene's test (less sensitive)
t_vs_p  %>%  levene_test(ΔA   ~ Test)
t_vs_p  %>%  levene_test(pΔA  ~ Test)

#anovas
t_vs_p  %>%         anova_test(ΔA  ~ Test, detailed = T)   #ANOVA
t_vs_p  %>%   welch_anova_test(pΔA ~ Test)                 #Welch’s ANOVA

#anova(lm(   ΔA ~ Test, t_vs_p))                            #ANOVA
#oneway.test(ΔA ~ Test, t_vs_p, var.equal = F)              # ≈ Welch’s ANOVA

#posthocs
t_vs_p  %>%   tukey_hsd(        ΔA  ~ Test)
t_vs_p  %>%   games_howell_test(pΔA ~ Test)

#TukeyHSD(aov(ΔA ~ Test, t_vs_p))

t_vs_p %>%  filter(Season == "Cold 2022")  %>%  group_by(Phenotype, Group)  %>% 
            select(Phenotype, Group, ΔA)  %>%  identify_outliers(ΔA)
t_vs_p %>%  filter(Season == "Cold 2022")  %>%  group_by(Phenotype, Group)  %>%
            select(Phenotype, Group, pΔA) %>%  identify_outliers(pΔA)



## OTHER STATS

p9  <- t_vs_p %>% filter(Phenotype %in%  "P9", Season %in% "Cold 2022")
p13 <- t_vs_p %>% filter(Phenotype %in% "P13", Season %in% "Cold 2022")

wilcox.test(`ΔA` ~ Group,  data = p13, paired = F)
wilcox.test(`ΔA` ~ Group,  data = p9,  paired = F)

wilcox.test(`pΔA`  ~ Group,  data = p13, paired = F)
wilcox.test(`pΔA`  ~ Group,  data = p9,  paired = F)


compare_means(`ΔA`  ~ Test, data = t_vs_p, method = "kruskal.test")
compare_means(`pΔA` ~ Test, data = t_vs_p, method = "kruskal.test")

compare_means(`ΔA`   ~ Group, filter(t_vs_p, Season == "Cold 2022"),
              method = "wilcox.test", group.by = "Phenotype")
compare_means(`pΔA`  ~ Group, filter(t_vs_p, Season == "Cold 2022"),
              method = "wilcox.test", group.by = "Phenotype")

compare_means(`ΔA` ~ Phenotype, t_vs_p,
              method = "wilcox.test", group.by = "Test")
compare_means(`pΔA`  ~ Phenotype, t_vs_p,
              method = "wilcox.test", group.by = "Test")

