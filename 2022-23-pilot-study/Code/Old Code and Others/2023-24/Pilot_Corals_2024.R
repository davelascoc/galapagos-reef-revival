                  ## Crecimiento Corales Isabela - Piloto ##
                             ## Reef Revival ##


library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggsci)
library(ggtext)
library(patchwork)
library(multcompView)
library(agricolae)

   #pdf(file = "output.pdf", paper = "a4r")

## SIZE VALUES || MONITOREO

## DATA MANAGEMENT ##

sizes  <-  read_excel("Datos/Analizados/Corales Isabela - Master Database.xlsx",
                          sheet = "Master")

sizes  <-  sizes   %>% filter(!Phenotype %in% c("P5.2", "P9.2", "P19.2",
                                                "P6", "P?4", "PAC.PD"))

sizes  <-  sizes   %>% filter(Plane == "Horizontal")             %>%
            mutate(Date = as.Date(Date),
                   Phenotype = factor(Phenotype, levels = c("P2",
            "P4", "P5", "P7", "P9", "P10", "P13", "P14", "P19")))  %>%
            mutate_at(c("Measuring", "Year", "Month", "Day",
            "Plane", "State", "Fragment"), factor)

summary(sizes)
summary(sizes$Phenotype)

# 

#horizontal
sizes %>%
    filter(Plane %in% "Horizontal") %>%
    filter(Group == "Piloto")  %>%
    ggplot(aes(x = Date, y = Area.cm2, group = Fragment, color = Phenotype)) +
            geom_point() + geom_line() +
            facet_wrap(~Phenotype, ncol = 3)  +
            labs(x = element_blank(), y = "Area (cm²)")+
            scale_x_date(breaks = as.Date(c(
            "2022-01-08", "2022-06-20", "2023-01-12")), date_labels = "%b %Y",
            expand = c(0.2,0)) + scale_y_continuous(expand = c(0,1.5)) +
            theme_test() + theme(legend.position = "none",
            text = element_text(family = "serif", size = 12)) +
            scale_color_hue(l =50)

#ggsave("R_outputs/growth.monitoring.png", width = 9, height = 5, dpi = 500)


## GROWTH RATES || WARM v COLD SEASON (Δ/day) ##

## IMPORT master database
corals  <-  read_excel("Datos/Analizados/Corales Isabela - Master Database.xlsx",
                       sheet = "RatesR")

summary(corals)  #resumen

# DATA MANAGEMENT

# eliminar fenotipos y seleccionar grupo piloto
growth  <-  corals    %>% filter(!Phenotype %in% c("P5.2", "P9.2", "P19.2"),
                                 Group == "Piloto",
                                 Plane  ==  "Horizontal")   %>% 
            mutate(Season    =  factor(Season,
                      levels = c("Warm 2022", "Cold 2022")))              %>%
            mutate(Phenotype = factor(Phenotype, levels = c(
            "P2", "P3", "P4", "P5", "P6", "P7", "P9", "P10",
            "P13", "P14", "P19", "PAC.PD")))         %>%
            mutate_at(c("Plane", "Fragment"), factor)

str(growth)
summary(growth)
summary(growth[["Phenotype"]])


growth  %>% group_by(Season)  %>%
            summarise(meanpdA = mean(pdA,  na.rm = T),
                      varpdA  =  var(pdA,  na.rm = T),
                      count = n())

growth  %>% group_by(Phenotype)  %>%
            summarise(meanpdA = mean(pdA,  na.rm = T),
                      varpdA  =  var(pdA,  na.rm = T),
                      count = n())

growth  %>% group_by(Phenotype, Season)  %>%
            summarise(varpdA = var(pdA,  na.rm = T),
                      count = n())

#¿datos son paramétrico?
#------------------------------------------------------------------------------
#identify outliers
growth %>%  group_by(Season, Phenotype)   %>%  select(Season, pdA)    %>%
            identify_outliers(pdA)    #    %>%  filter(is.extreme == T)

#residuals
normal    <- lm(pdA  ~ Season, growth)

nrow(growth)

head(normal$fitted.values)
head(normal$residuals)
ggqqplot(residuals(normal)) + theme_test()

shapiro_test(residuals(normal))


#by seasons
ggqqplot(growth, "pdA", facet.by = "Season") + theme_test()

growth  %>%  group_by(Season)  %>% shapiro_test(pdA)

#by phenotypes
ggqqplot(growth, "pdA", facet.by = "Phenotype") + theme_test()

growth  %>%  group_by(Phenotype)  %>% shapiro_test(pdA)

#homogeneity of variance
plot(normal, 1)

#Bartlett's test (more sensitive to deviations from normality)
bartlett.test(pdA ~ Season, growth)

#Levene's test (less sensitive)
growth  %>%  levene_test(pdA  ~ Season)

#------------------------------------------------------------------------------

#some stats for plots
t.test(pdA ~ Season, growth, paired = F, var.equal = F)

res.t    <- growth  %>%  t_test(pdA ~ Season) %>%  add_xy_position(x = "Season")
res.t
res.t$statistic
res.t$df
res.t$p

res.tky    <- growth  %>%  tukey_hsd(pdA ~ Season) %>%
                            add_xy_position(x = "Season")
res.tky

#boxplots by phenotypes (%)
ggplot(growth, aes(x = Season, y = pdA, col = Season)) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(aes(fill = Season), outlier.size = 1) + 
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
            stat_compare_means(method = "t.test", label = "p.signif",
            label.x = 2.25, label.y = 2, hide.ns = T) +
            facet_wrap(~Phenotype, ncol = 4) +
            #ggtitle("Seasonal Coral Growth Rate by Morphotype") +
            ylab("ΔA (%/100 days)") +
            theme_test() + theme(legend.position = "none",
            #plot.title = element_text(hjust = 0.5),
            text = element_text(family = "serif", size = 12)) +
            scale_color_manual(values = c("#6b1414", "#103c5a")) +
            scale_fill_manual(values  = c("#D62728", "#1F77B4")) +
            scale_y_continuous(labels = scales::percent)

#ggsave("R_outputs/morphotypes.png", width = 8, height = 5, dpi = 500)

res.aov.morho <-  growth %>%  group_by(Season)  %>%
                              welch_anova_test(pdA ~ Phenotype)
res.aov.morho

res.tuk.morho <-  growth %>%  group_by(Season)  %>%
                              tukey_hsd(pdA ~ Phenotype)

res.tuk.morho

library(multcompView)

warm <- res.tuk.morho %>% filter(Season == "Warm 2022") %>%
  select(group1, group2, p.adj)

comparisons_warm <- with(warm, setNames(p.adj, paste(group1, group2, sep = "-")))

letters_warm <- multcompLetters(comparisons_warm)

print(letters_warm$Letters)



cold <- res.tuk.morho %>% filter(Season == "Cold 2022") %>%
  select(group1, group2, p.adj)

comparisons_cold <- with(cold, setNames(p.adj, paste(group1, group2, sep = "-")))

letters_cold <- multcompLetters(comparisons_cold)

print(letters_cold$Letters)


letters_warm_df <- data.frame(
  Phenotype = names(letters_warm$Letters),
  Season = "Warm 2022",
  Letters = letters_warm$Letters
)

# DataFrame para Cold 2022
letters_cold_df <- data.frame(
  Phenotype = names(letters_cold$Letters),
  Season = "Cold 2022",
  Letters = letters_cold$Letters
)

# Combinar ambos
letters_df <- rbind(letters_warm_df, letters_cold_df)

positions <- growth %>%
  group_by(Phenotype, Season) %>%
  summarise(max_value = max(pdA, na.rm = TRUE)+0.05) %>%
  ungroup()

# Combinar con las letras
letters_positions <- letters_df %>%
  left_join(positions, by = c("Phenotype", "Season"))

ggplot(growth, aes(x = Phenotype, y = pdA)) +
  stat_boxplot(geom = "errorbar", width = .1) +
  geom_boxplot(aes(fill = Phenotype), outlier.size = 1) + 
  stat_summary(fun = 'mean', geom = 'point', shape = 4) +
  facet_wrap(~Season) +
  ylab("ΔA (%/100 days)") +
  stat_compare_means(method = "kruskal.test", label = "p.signif",
                     label.x = 1, label.y = 2.3, hide.ns = TRUE) +
  theme_test() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "serif", size = 12)) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = letters_positions, 
            aes(x = Phenotype, y = max_value + 0.05 * max_value, label = Letters),
            inherit.aes = FALSE)  # Asegúrate de que las letras no usen el mapeo global


#ggsave("R_outputs/morphotypes2.png", width = 11, height = 5, dpi = 500)


p1      <-  ggplot(growth, aes(x = Season, y = pdA, col = Season), col = 1) +
            stat_boxplot(geom = "errorbar", width = .1) +
            geom_boxplot(aes(fill = Season)) + xlab("") +
            ylab("ΔA (%/100 days)") +
            stat_pvalue_manual(res.tky, hide.ns = F, label = "p.adj.signif") +
            stat_summary(fun = 'mean', geom = 'point', shape = 4) +
         #   stat_compare_means(method = "t.test", label.x = 1.5) +
            theme_test() + theme(legend.position = "none",
            plot.title = element_text(hjust = .5),
            text = element_text(family = "serif", size = 12)) +
            scale_y_continuous(labels = scales::percent, limits = c(0, 2.50)) +
            scale_fill_manual( values = c("#D62728", "#1F77B4")) +
            scale_color_manual(values = c("#6b1414", "#103c5a")) +
            labs(subtitle = get_test_label(res.t, detailed = F,
            description = "Student's t-test"))

growth %>%  group_by(Season) %>% summarize(MEAN = round(mean(pdA, na.rm = T)*100, 2))



#effsize

#t.tests por grupo
res.t.phen <-     growth  %>% group_by(Phenotype)  %>%
                              t_test(pdA ~ Season, var.equal = F)
print(res.t.phen)

#writexl::write_xlsx(res.t.phen, "R_outputs/t_test_Results.xlsx",)

#effect size - Cohen's d Measure
res.cohen <- growth  %>%  group_by(Phenotype)  %>%
                                cohens_d(pdA ~ Season, var.equal = F)

print(res.cohen)

#writexl::write_xlsx(res.cohen, "R_outputs/Cohens_Results.xlsx",)

res.cohen$Phenotype <- factor(res.cohen$Phenotype,
                               levels = rev(levels(res.cohen$Phenotype)))

#barplot
ggplot(res.cohen, aes(y = Phenotype, x = effsize)) + coord_flip() +
      geom_bar(stat = "identity", show.legend = F, aes(fill = effsize),
               col = NA) +
      labs(x = "Effect Size (Cohen's d)", y = "Morphotype") +
      theme_pubclean() +
      theme(text = element_text(family = "serif", size = 12)) +
      geom_vline(xintercept = 0, col = "grey30", lwd = .5) + #xlim(c(-1.5,1.5))
      scale_fill_gradient2(low = "#195f90", high = "#D62728")

#ggsave("R_outputs/effsize.png", width = 7, height = 4, dpi = 1000)

## CORALES TESTIGO VS CORALES PROYECTO PILOTO ##

#fenotipos
t_vs_p   <-   corals %>%  filter(Phenotype %in% c("P4", "P9", "P13"))

#factors
t_vs_p   <-   t_vs_p %>%  filter(Plane == "Horizontal",
                                 Test  != "Piloto - Warm")            %>% 
                          mutate_at(c("Group", "Season",
                          "Plane", "Fragment"), factor)               %>%
                          mutate(Phenotype = factor(Phenotype,
                          levels = c("P13", "P9", "P4")))             %>%
                          mutate(Test = factor(Test, levels = c(
                          "Piloto - Cold", "Control - Cold")))

t_vs_p
summary(t_vs_p)
summary(t_vs_p[["Phenotype"]])

#test (Area)
res.aov  <-   t_vs_p %>% t_test(pdA ~ Test)

res.aov
res.aov$statistic
res.aov$p

pwc      <-   t_vs_p  %>%    tukey_hsd(pdA ~ Test)  %>%
                              add_xy_position(x = "Test")
pwc
pwc$p.adj
pwc$p.adj.signif

#pwc$p.adj.signif <-  gsub("ns", "", pwc$p.adj.signif)

p2      <-  ggplot(t_vs_p, aes(x = Test, y = `pdA`), color = "#103c5a") +
            stat_boxplot(geom = "errorbar", width = 0.1, color = "#103c5a") +
            geom_boxplot(fill = "#1F77B4", col = "#103c5a") +
            stat_summary(fun = 'mean', geom = 'point', shape = 4,
                         color = "#103c5a") +
            #stat_welch_anova_test(label.y = .98) +
            stat_pvalue_manual(pwc, hide.ns = F) + xlab("") + ylab("") +
            theme_bw() + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            text = element_text(family = "serif", size = 12)) +
            scale_y_continuous(labels = scales::percent, limits = c(0, 2.50)) +
            scale_x_discrete(labels = c("Pilot", "Control")) +
            labs(subtitle = get_test_label(res.aov, detailed = F, 
                                           description = "Student's t-test"))
            

wrap_plots(p1, p2, widths = c(0.5, 0.5))

rm(p1, p2, pwc, res.aov, res.t, res.tky, t_vs_p, normal, res.aov.morho,
   res.cohen, res.t.phen)

#ggsave("R_outputs/coral.treatment.png", width = 7, height = 4, dpi = 500)


t_vs_p %>%  group_by(Test) %>% summarize(MEAN = mean(pdA, na.rm = T)*100)


#scale_color_manual(values = c("#6b1414", "#103c5a", "#165016")) +
#scale_fill_manual(values  = c("#D62728", "#1F77B4", "#2CA02C")) +


## MORTALIDAD ##

library(readxl)
library(tidyverse)
library(patchwork)
library(ggpubr)
library(ggsci)
library(ggtext)
library(ggrepel)
library(glue)
library(ggpmisc)


#scatter plot, linear regression || coral growth v. deadth by seasons
#ggplot(mortal, aes(x = `mean%dA`, y = `Mortality (%)`, group = Season,
#        col = Season, fill = Season)) +
#        geom_hline(yintercept = 0, linetype = "dashed", col = "grey40") +
#        geom_point() + geom_smooth(method = "glm", se = T) +
#        stat_poly_eq(aes(label = paste(after_stat(eq.label), ..rr.label..,
#        ..p.value.label.., sep = "*\";   \"*")), 
#        label.x = "right", label.y = "top", size = 3) +
#        geom_text_repel(label = mortal$Phenotype, size = 3.5,
#        hjust=1.25, vjust=0.1) +
#        labs(title = "Corals from the Nursery in Isabela",
#        subtitle = "<span style='color:#ab1f20'>**Warm**</span> and
#        <span style='color:#195f90'>**Cold**</span> Seasons in 2022",
#        x = "x̄ Growth Rate in 100 days",
#        y = "Coral Mortality") +
#        theme_test() +
#        theme(
#        text = element_text(family = "serif", size = 12),
#        plot.title = element_text(size = 16, face = "bold"),
#        legend.position = "none",
#        plot.subtitle = element_markdown(size = 14)) +
#        scale_x_continuous(labels = scales::percent) +
#        scale_y_continuous(labels = scales::percent) +
#        scale_color_manual(values =  c("#ab1f20", "#195f90")) +
#        scale_fill_manual(values =  c("#D62728", "#1F77B4")) +
#        coord_cartesian(ylim = c(-0.1, .75)) +
  
#ggplot(mortal, aes(x = `mean%dA`, y = meanDeadTissue, group = Season,
#        col = Season, fill = Season)) +
#        geom_hline(yintercept = 0, linetype = "dashed", col = "grey40") +
#        geom_point() + geom_smooth(method = "glm", se = T) +
#        stat_poly_eq(aes(label = paste(after_stat(eq.label), ..rr.label..,
#        ..p.value.label.., sep = "*\";   \"*")), 
#        label.x = "right", label.y = "top", size = 3) +
#        geom_text_repel(label = mortal$Phenotype, size = 3.5,
#        hjust=1.25, vjust=0.1) +
#        labs(x = "", y = "x̄ Dead Tissue") +
#        theme_test() +
#        theme(
#        text = element_text(family = "serif", size = 12),
#        legend.position = "none") +
#        scale_x_continuous(labels = scales::percent) +
#        scale_y_continuous(labels = scales::percent) +
#        scale_color_manual(values =  c("#ab1f20", "#195f90")) +
#        scale_fill_manual(values =  c("#D62728", "#1F77B4")) +
#        coord_cartesian(ylim = c(-0.1, .75))

#ggsave("R_outputs/death_vs_growth(2).png", width = 9, height = 5, dpi = 1000)


## SLOPE TEST

# Extract slope coefficients and standard errors

#mortality
#beta1 <- coef(lma)["`mean%dA`"]
#beta1
#beta2 <- coef(lmc)["`mean%dA`"]
#beta2

#SE1 <- summary(lma)$coefficients["`mean%dA`", "Std. Error"]
#SE1
#SE2 <- summary(lmc)$coefficients["`mean%dA`", "Std. Error"]
#SE2

# Compute the test statistic
#test_statistic <- (beta1 - beta2) / sqrt(SE1^2 + SE2^2)

# Degrees of freedom
#df <- min(df.residual(lma), df.residual(lmc))

# Calculate p-value
#p_value <- 2 * pt(abs(test_statistic), df = df, lower.tail = FALSE)

# Print test statistic and p-value for coral mortality
#cat("Test Statistic:", test_statistic, "\n")
#cat("P-value:", p_value, "\n")


#dead tissue
#beta3 <- coef(lmb)["`mean%dA`"]
#beta3
#beta4 <- coef(lmd)["`mean%dA`"]
#beta4

#SE3 <- summary(lmb)$coefficients["`mean%dA`", "Std. Error"]
#SE3
#SE4 <- summary(lmd)$coefficients["`mean%dA`", "Std. Error"]
#SE4

# Compute the test statistic
#test_statistic2 <- (beta3 - beta4) / sqrt(SE3^2 + SE4^2)

# Degrees of freedom
#df2 <- min(df.residual(lmb), df.residual(lmd))

# Calculate p-value
#p_value2 <- 2 * pt(abs(test_statistic2), df = df2, lower.tail = FALSE)

# Print test statistic and p-value for dead tissue
#cat("Test Statistic:", test_statistic2, "\n")
#cat("P-value:", p_value2, "\n")

#dumbbell plot || coral performance
#ggplot(mortal, aes(x = `mean%dA`, y = Phenotype)) +
#        geom_line() + geom_point(aes(color = Season), size = 3) +
#        labs(x = "x̄ Growth Rate") + theme_bw() +
#        theme(legend.position = "none", axis.title.y = element_blank(),
#              axis.text.y = element_blank(), axis.ticks.y = element_blank(),
#              text = element_text(family = "serif", size = 12)) + 
#        scale_color_manual(values =  c("#ab1f20", "#195f90")) +
#        scale_x_continuous(labels = scales::percent)

#ggsave("R_outputs/warm_vs_cold.png", width = 9, height = 5, dpi = 1000)


# scatter plot, linear regression || coral growth v. deadth
mortal.net <- read_excel("Datos/Analizados/Coral Mortality - Pilot.xlsx",
                         sheet = "Net")

mortal.net <- mortal.net %>% mutate(Phenotype = factor(Phenotype, levels = c(
                              "P2", "P3", "P4", "P5", "P6", "P7", "P9",
                              "P10", "P13", "P14", "P19", "PAC-PD")))

ggplot(mortal.net, aes(x = `mean%dA`, y = `Mortality`)) +
        geom_hline(yintercept = 0, linetype = "dashed", col = "grey40") +
        geom_point() + geom_smooth(method = "glm", se = T, col = 1, fill = "grey50") +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label..,
        ..p.value.label.., sep = "*\";   \"*")), 
        label.x = "right", label.y = "top", size = 3) +
        geom_text_repel(label = mortal.net$Phenotype, size = 3.5) +
        labs(y = "Coral Mortality", x = "ΔA") +
        theme_test() +
        theme(text = element_text(family = "serif", size = 12),
        legend.position = "none") +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(ylim = c(-0.1, 1.05)) +

ggplot(mortal.net, aes(x = `mean%dA`, y = FinalDeadTissue)) +
        geom_hline(yintercept = 0, linetype = "dashed", col = "grey40") +
        geom_point() + geom_smooth(method = "glm", se = T, col = 1, fill = "grey50") +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label..,
        ..p.value.label.., sep = "*\";   \"*")), 
        label.x = "right", label.y = "top", size = 3) +
        geom_text_repel(label = mortal.net$Phenotype,  size = 3.5) +
        labs(x = "", y = "Dead Tissue") +
        theme_test() +
        theme(
        text = element_text(family = "serif", size = 12),
        legend.position = "none") +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(ylim = c(-0.1, 1.05))

#ggsave("R_outputs/death_vs_growth.png", width = 9, height = 5, dpi = 500)

lm1 <-  lm(`Mortality`~ `mean%dA`, data =  mortal.net)
lm1

summary(lm1)


lm2 <-  lm(FinalDeadTissue~ `mean%dA`, data =  mortal.net)
lm2

summary(lm2)


rm(beta1, beta2, beta3, beta4, df, df2, p_value, p_value2, SE1, SE2, SE3, SE4,
   test_statistic, test_statistic2, res.aov.morho, res.cohen, res.t.phen,
   lm1, lm2, lma, lmb, lmc, lmd)




## BARPLOT - Mortalidad Fragmentos


mortal.month  <-    read_excel("Datos/Analizados/Coral Mortality - Pilot.xlsx",
                               sheet = "Monthly")

mortal.month  <-    mortal.month  %>%
                    mutate(Month = factor(Month, levels = c("May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                      Morphotype = factor(Morphotype, levels = c(
                        "P2", "P3", "P4", "P5", "P6", "P7", "P9",
                        "P10", "P13", "P14", "P19", "PAC-PD" 
                      )))


## puntos y lineas

head(mortal.month)

ggplot(mortal.month, aes(x = Month, y = Mortality,
            group = Morphotype, col = Morphotype)) +
            geom_point(size = 2) + geom_line(linewidth = .7) +
            theme_test() + theme(text = element_text(family = "serif", size = 14),
            legend.position = "none", plot.title = element_text(hjust = .5)) +
            scale_color_hue(l = 40) + labs(x = element_blank()) +
            scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
            geom_vline(xintercept = 1.5, linetype = "dashed", col = "grey40") +
ggplot(mortal.month, aes(x = Month, y = `DeadTissue`,
            group = Morphotype, col = Morphotype)) +
            geom_point(size = 2) + geom_line(linewidth = .7, show.legend = F) +
            theme_test() + theme(text = element_text(family = "serif", size = 14),
            plot.title = element_text(hjust = .5)) +
            scale_color_hue(l = 40) + labs(x = element_blank()) +
            scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
            geom_vline(xintercept = 1.5, linetype = "dashed", col = "grey40")

#ggsave("R_outputs/coralmortality.png", width = 10, height = 4, dpi = 500)


myplot <- ggplot(mortal.month, aes(x = Month, y = `DeadTissue`,
            group = Morphotype, col = Morphotype)) +
            geom_point(size = 2) + geom_line(linewidth = .7, show.legend = F) +
            theme_test() + theme(text = element_text(family = "serif", size = 14),
            plot.title = element_text(hjust = .5)) +
            scale_color_hue(l = 40) + labs(x = element_blank()) +
            scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
            geom_vline(xintercept = 1.5, linetype = "dashed", col = "grey40")


## SUPER PLOTTT


plot1 <-  ggplot(mortal.month, aes(x = Month, y = Mortality,
            group = Morphotype, col = Morphotype)) +
            geom_point(size = 2) + geom_line(linewidth = .7) +
            theme_test() + theme(text = element_text(family = "serif", size = 14),
            legend.position = "none", plot.title = element_text(hjust = .5)) +
            scale_color_hue(l = 40) + labs(x = element_blank()) +
            scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
            geom_vline(xintercept = 1.5, linetype = "dashed", col = "grey40") 

plot2 <-  ggplot(mortal.month, aes(x = Month, y = DeadTissue,
            group = Morphotype, col = Morphotype)) +
            geom_point(size = 2) + geom_line(linewidth = .7) +
            theme_test() + theme(text = element_text(family = "serif", size = 14),
            legend.position = "none", plot.title = element_text(hjust = .5)) +
            scale_color_hue(l = 40) + labs(x = element_blank()) +
            ylab("Dead Tissue") +
            scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
            geom_vline(xintercept = 1.5, linetype = "dashed", col = "grey40") 

plot3 <-  ggplot(mortal.month, aes(x = Month, y = Bleaching,
            group = Morphotype, col = Morphotype)) +
            geom_point(size = 2) + geom_line(linewidth = .7) +
            theme_test() + theme(text = element_text(family = "serif", size = 14),
            legend.position = "none", plot.title = element_text(hjust = .5)) +
            scale_color_hue(l = 40) + labs(x = element_blank()) +
            ylab("Bleached Tissue") +
            scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
            geom_vline(xintercept = 1.5, linetype = "dashed", col = "grey40") 

plot4 <-  ggplot(mortal.month, aes(x = Month, y = AlgalCover,
            group = Morphotype, col = Morphotype)) +
            geom_point(size = 2) + geom_line(linewidth = .7) +
            theme_test() + theme(text = element_text(family = "serif", size = 14),
            legend.position = "none", plot.title = element_text(hjust = .5)) +
            scale_color_hue(l = 40) + labs(x = element_blank()) +
            ylab("Algal Cover") +
            scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
            geom_vline(xintercept = 1.5, linetype = "dashed", col = "grey40") 


#combine plots
combined_plot  <- cowplot::plot_grid(plot1, NULL, plot2, NULL,
                            plot3, NULL, plot4, NULL,
                            ncol = 4, rel_widths   = c(1, 0.03, 1, 0.05))


#extract legend
legend         <- get_legend(myplot)

#add legend
final_plot <-      cowplot::plot_grid(combined_plot, legend,
                                      rel_widths = c(1, 0.14)) +
                   theme(plot.background = element_rect(fill   = "white",
                                                        color  = "white"),
                         panel.background = element_rect(fill  = "white",
                                                         color = NA))

#ggsave("R_outputs/coralmortality_health.png", width = 10, height = 5.5, dpi = 500)


#windows(width = 8, height = 4)

ggplot(mortal.month, aes(x = Month, y = Survival,
            group = Morphotype, col = Morphotype)) +
            geom_point(size = 2) + geom_line(linewidth = .7) +
            theme_test() + theme(text = element_text(family = "serif", size = 14),
            legend.position = "none", plot.title = element_text(hjust = .5)) +
            scale_color_hue(l = 40) + labs(x = element_blank()) +
            scale_y_continuous(limits = c(.05,1.1), labels = scales::percent) + 
            geom_vline(xintercept = 1.5, linetype = "dashed", col = "grey40") +
ggplot(mortal.month, aes(x = Month, y = `Live Tissue`,
            group = Morphotype, col = Morphotype)) +
            geom_point(size = 2) + geom_line(linewidth = .7, show.legend = F) +
            theme_test() + theme(text = element_text(family = "serif", size = 14),
            plot.title = element_text(hjust = .5)) +
            scale_color_hue(l = 40) + labs(x = element_blank()) +
            scale_y_continuous(limits = c(.05,1.1), labels = scales::percent) + 
            geom_vline(xintercept = 1.5, linetype = "dashed", col = "grey40")

#ggsave("R_outputs/coralsurvival.png", width = 10, height = 4, dpi = 500)

ls()


### LOGGERS TEMPERATURAS - GRUPO PILOTO

library(lubridate)
library(hms)

#Hobo_2022 <- read_excel("Datos/Analizados/Hobo 2022.xlsx")

Hobo_2022           <- read_excel("Datos/Analizados/Temp Piloto.xlsx")

Hobo_2022$Hour      <- format(as.POSIXct(Hobo_2022$Hour, tz = "GMT"),
                         format = "%H:%M")

Hobo_2022        <- Hobo_2022 %>% mutate(Date_Time = paste(Year, Month,Day,
                                            Hour, sep = "-"),
                    Season = factor(Season,  levels = c("Warm", "Cold")),
                    Temp_Dates = as.Date(Temp_Dates, format = "%d/%m/%Y"))  %>%
                    mutate_at(c("Series", "Quality_Temp"),
                                          factor)

Hobo_2022$Date_Time <- as.POSIXct(Hobo_2022$Date_Time,
                                format = "%Y-%m-%d-%H:%M", tz = "GMT")

head(   Hobo_2022)
summary(Hobo_2022)
summary(Hobo_2022$Temp)

by(Hobo_2022$Temp, Hobo_2022$Season, mean, na.rm = T)
min(by(Hobo_2022$Temp, Hobo_2022$Month, mean, na.rm = T))

Hobo_2022  <- Hobo_2022 %>% filter(!Quality_Temp == "Error")

picos_t    <- data.frame(
              date_drop = as.POSIXct(c('2022-05-02', '2022-09-19', '2022-10-06',
                                      '2022-11-06', '2022-12-04')),
              t_drop   = c(19, 18, 18.4, 18, 18.6),
              mycolors = c("red3", "blue3", "blue3", "blue3", "blue3"),
              labels_d = c("1", "2", "3", "4", "5")
              )

ggplot(Hobo_2022, aes(x = Date_Time, y = Temp, group = Series)) +
              geom_line(size = .25) +
              labs(y = "SST (°C)", x = element_blank()) +
              scale_x_datetime(date_labels = "%b. %y",  # Formato de las fechas en el eje x
                               date_breaks = "2 month") + # Frecuencia de las etiquetas
              theme_bw() + theme(legend.position = "none",
              text = element_text(family = "serif", size = 12)) +
              ylim(c(18, 31)) +
              geom_text(data = picos_t, aes(x = date_drop, y = t_drop,
              label = labels_d),  inherit.aes = FALSE,  col = "red", size = 3)

#ggsave("R_outputs/hobo2022.png", width = 6, height = 4, dpi = 500)

ggplot(Hobo_2022, aes(x = Date_Time, y = Temp, group = Series, col = Season)) +
              geom_line(size = .25) +
              labs(y = "SST (°C)", x = element_blank()) +
              theme_bw() + theme(legend.position = "none",
              text = element_text(family = "serif", size = 12)) +
              ylim(c(18, 31)) +
              scale_x_datetime(date_labels = "%b. %y",
                               date_breaks = "2 month") +
              scale_color_hue(l =50) +
              geom_text(data = picos_t, aes(x = date_drop, y = t_drop,
              label = labels_d),  col = 1, inherit.aes = FALSE, size = 3)

#ggsave("R_outputs/hobo2022_seasons.png", width = 6, height = 4, dpi = 1000)

ggplot(Hobo_2022, aes(x = Date_Time, y = Temp, group = Series, col = Series)) +
              geom_line(size = .25) +
              labs(y = "SST (°C)", x = element_blank()) +
              scale_x_datetime(date_labels = "%b. %y",
                               date_breaks = "2 month") +
              theme_bw() + theme(legend.position = "none",
              text = element_text(family = "serif", size = 12)) +
              ylim(c(18, 31)) + scale_color_npg() +
              geom_text(data = picos_t, aes(x = date_drop, y = t_drop,
              label = labels_d),  col = 1, inherit.aes = FALSE, size = 3)

#ggsave("R_outputs/hobo2022_series.png", width = 6, height = 4, dpi = 1000)

rm(picos_t)





## GENERAL STATS


#PILOTO

sizes  <-  read_excel("Datos/Analizados/Corales Isabela - Master Database.xlsx",
                          sheet = "Master")

sizes  <-  sizes   %>%  mutate(Date = as.Date(Date))             %>%
                        mutate_if(is.character, as.factor)

summary(sizes)

table1 <-   sizes  %>%  filter(Plane %in% "Horizontal", Group == "Piloto",
                   Measuring == 1)  %>%
            group_by(Phenotype)  %>%
            summarise(n = n(),
                      ML0_mean = mean(ML.cm,    na.rm = T),
                      ML0_sd   =   sd(ML.cm,    na.rm = T),
                      A0_mean  = mean(Area.cm2, na.rm = T),
                      A0_sd    =   sd(Area.cm2, na.rm = T)) %>%
            arrange(Phenotype)

table2 <-   corals %>%  filter(Plane  ==  "Horizontal", Group == "Piloto") %>%
            group_by(Phenotype, Season) %>%
            summarise(GrowthRate = mean(pdA, na.rm = T)) %>%
            pivot_wider(names_from = Season,
                        values_from = GrowthRate)   %>%
            arrange(Phenotype)


table_final  <- left_join(table1, table2)

table_final

library(writexl)
#write_xlsx(table_final, "Table1.xlsx")


## CONTROL

table1 <-   sizes  %>%  filter(Plane %in% "Horizontal", Group == "Control",
                   Measuring == 1)  %>%
            group_by(Phenotype)  %>%
            summarise(n = n(),
                      ML0_mean = mean(ML.cm,    na.rm = T),
                      ML0_sd   =   sd(ML.cm,    na.rm = T),
                      A0_mean  = mean(Area.cm2, na.rm = T),
                      A0_sd    =   sd(Area.cm2, na.rm = T)) %>%
            arrange(Phenotype)

table2 <-   corals %>%  filter(Plane  ==  "Horizontal", Group == "Testigo") %>%
            group_by(Phenotype, Season) %>%
            summarise(GrowthRate = mean(pdA, na.rm = T)) %>%
            pivot_wider(names_from = Season,
                        values_from = GrowthRate)   %>%
            arrange(Phenotype)


table_final  <- left_join(table1, table2)

table_final



corals %>%  filter(Plane  ==  "Horizontal", Group == "Testigo") %>%
            group_by(Phenotype, Season) %>%
            summarise(n = n()) %>%
            arrange(Phenotype)

library(writexl)
#write_xlsx(table_final, "Table2.xlsx")



sizes %>%  group_by(Group, Measuring) %>%
            filter(Phenotype %in% c("P4", "P9", "P13"),
                   Plane == "Horizontal") %>%
            summarize(MEAN = mean(ML.cm, na.rm = T))


sizes %>%  group_by(Group, Measuring, Phenotype) %>%
            filter(Phenotype %in% c("P4", "P9", "P13"),
                   Plane == "Horizontal", Measuring %in% c(1, 2)) %>%
            summarize(SD   =   sd(ML.cm, na.rm = T))




