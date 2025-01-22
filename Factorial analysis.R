rm(list = ls())     # clear objects  
graphics.off() 
#ctrl+L   #to clean console
#######################################
###### Chestnuts RF ###########
#######################################


# Packages ----------------------------------------------------------------
inst <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr", 
              "ggplot2", "ggpubr", "broom", "AICcmodavg", "ggcorrplot", 
              "fpc","cluster", "readxl", "magrittr","hrbrthemes",
              "multipanelfigure","klaR","psych","MASS","ggord","devtools",
              "reshape2","RColorBrewer","SensoMineR","FactoMineR","stats",
              "dplyr","writexl","gtools","ggbiplot","ggrepel",
              "ggstatsplot", "plotly", "car", "ez", "openxlsx","reticulate",
              "rstatix", "patchwork", "FrF2")
inst(packages)
theme_set(theme_minimal())


# -------------------------- #
#    Importing Data set      #
# -------------------------- #
(df <- read_excel("RF_Chestnut.xlsx", sheet = "Larval tests"))


# -------------------------- #
#  Wrangling and subsetting  #
# -------------------------- #
df <- df %>%
  select(-c(1, 2, 8, 10, 11)) %>%
  na.omit() %>%
  setNames(c("TestID", "Carpet", "ContChestnuts", "InputT", 
             "PowerUnit", "Speed", "InferiorOutT", "MiddleOutT", 
             "SuperiorOutT", "OutputT", "DeltaT", "RecovLarvae", 
             "DeadLarvae", "LiveLarvae", "AnnihRate", "SurvRate", 
             "TotalChestnuts", "CookedChestnuts", "UncookedChestnuts", 
             "CookedRate"))

df <- df %>%
  mutate(
    TestID = factor(TestID),
    Carpet = factor(Carpet),
    ContChestnuts = factor(ContChestnuts),
    PowerUnit = factor(PowerUnit),
    Speed = factor(Speed)
  )


# -------------------------- #
#    Experimental design     #
# -------------------------- #
# Factorial design 2^2
(design_2_2 <- FrF2(nruns = 4, 
                    nfactors = 2, 
                    factor.names = list(Power = c("low", "high"), 
                                        Speed = c("40%", "50%"))))

# -------------------------- #
# Experimental design data   #
# -------------------------- #
# Creation of dataset for experimental design
df_fact <- df %>% 
  select(TestID, PowerUnit, Speed, SurvRate, CookedRate) 

# Filter PowerUnit and rename values
df_fact <- df_fact %>% 
  filter(PowerUnit %in% c("4500+4500+2000+2000", "4500+4500+2000+1000") & 
           Speed %in% c("40", "50"))

# Pre-substitution to PowerUnit for prep
df_fact$PowerUnit <- as.character(df_fact$PowerUnit)

# Replacement
df_fact$PowerUnit[df_fact$PowerUnit == "4500+4500+2000+2000"] <- "High"
df_fact$PowerUnit[df_fact$PowerUnit == "4500+4500+2000+1000"] <- "Low"

# re-conversion of PowerUnit to factor
df_fact$PowerUnit <- factor(df_fact$PowerUnit)

# Numeric SurvRate conversion and NA removal
df_fact <- df_fact %>% 
  mutate(SurvRate = as.numeric(as.character(SurvRate))) %>% 
  filter(!is.na(SurvRate))


# long format transformation
df_fact_long <- df_fact %>%
  pivot_longer(cols = c(SurvRate, CookedRate), 
               names_to = "variable", 
               values_to = "Rate"
               ) %>% 
  mutate(variable = factor(variable, levels = c("SurvRate", "CookedRate"), 
                                 labels = c("Larval Survival Rate (%)", "Cooked Chestnuts (%)")))

# Average summary of PowerUnit, Speed and variable
df_fact_avg <- df_fact_long %>%
  group_by(PowerUnit, Speed, variable) %>%
  summarize(AvgRate = mean(Rate, na.rm = TRUE), .groups = "drop")

# means bar chart
ggplot(df_fact_avg, aes(x = interaction(PowerUnit, Speed), y = AvgRate, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~ variable, scales = "free_y") + 
  scale_fill_manual(values = c("Larval Survival Rate (%)" = "darkslategray", 
                               "Cooked Chestnuts (%)" = "coral4")) + 
  labs(title = "Average Rates by PowerUnit and Speed", x = "Condition", y = "Average Rate (%)") + 
  theme_minimal() + 
  theme(legend.position = "none")


# -------------------------- #
#   ANOVA     #
# -------------------------- #
# ANOVA para Larval Survival Rate
anova_larval <- aov(SurvRate ~ PowerUnit * Speed, data = df_fact)
summary(anova_larval)

# ANOVA para Cooked Chestnuts Rate
anova_chestnuts <- aov(CookedRate ~ PowerUnit * Speed, data = df_fact)
summary(anova_chestnuts)

# Normality Test
shapiro.test(residuals(anova_larval))
shapiro.test(residuals(anova_chestnuts))

# Variance homogen
leveneTest(SurvRate ~ PowerUnit * Speed, data = df_fact)
leveneTest(CookedRate ~ PowerUnit * Speed, data = df_fact)

# Diagnostics plot
par(mfrow = c(2, 2))
plot(anova_larval)
plot(anova_chestnuts)


# Signal to Noise Ratio
# Survival Rate
summary_larval <- summary(anova_larval)
# MS Factor extraction and MS Residual
ms_power <- summary_larval[[1]]["PowerUnit", "Mean Sq"]   # MS  factor PowerUnit
ms_speed <- summary_larval[[1]]["Speed", "Mean Sq"]       # MS  factor Speed
ms_interaction <- summary_larval[[1]]["PowerUnit:Speed", "Mean Sq"]  # MS  interaction
ms_residual <- summary_larval[[1]]["Residuals", "Mean Sq"]           # MS residual

# SNR for each factor
(snr_power <- ms_power / ms_residual)
(snr_speed <- ms_speed / ms_residual)
(snr_interaction <- ms_interaction / ms_residual)

# Cooking Rate
summary_cooking <- summary(anova_chestnuts)
# MS Factor extraction and MS Residual
ms_power_c <- summary_cooking[[1]]["PowerUnit", "Mean Sq"]   # MS  factor PowerUnit
ms_speed_c <- summary_cooking[[1]]["Speed", "Mean Sq"]       # MS  factor Speed
ms_interaction_c <- summary_cooking[[1]]["PowerUnit:Speed", "Mean Sq"]  # MS  interaction
ms_residual_c <- summary_cooking[[1]]["Residuals", "Mean Sq"]           # MS residual

# SNR for each factor
(snr_powe_c <- ms_power_c / ms_residual_c)
(snr_speed_c <- ms_speed_c / ms_residual_c)
(snr_interaction_c <- ms_interaction_c / ms_residual_c)



# -------------------------- #
#   Factor interactions     #
# -------------------------- #
# mean group calculation
interaction_data <- df_fact %>%
  group_by(PowerUnit, Speed) %>%
  summarize(
    mean_SurvRate = mean(SurvRate, na.rm = TRUE),
    mean_CookedRate = mean(CookedRate, na.rm = TRUE),
    .groups = "drop"
  )

# Plotting interactions
ggplot(interaction_data, aes(x = Speed, y = mean_SurvRate, group = PowerUnit, color = PowerUnit)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Interaction Plot: Larval Survival Rate",
    x = "Speed",
    y = "Mean Survival Rate (%)",
    color = "Power Unit"
  ) +
  theme_minimal()

ggplot(interaction_data, aes(x = Speed, y = mean_CookedRate, group = PowerUnit, color = PowerUnit)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Interaction Plot: Cooked Chestnuts Rate",
    x = "Speed",
    y = "Mean Cooked Rate (%)",
    color = "Power Unit"
  ) +
  theme_minimal()




# -------------------------- #
#   Main effects    #
# -------------------------- #
# Surivival Rate
ggplot(df_fact, aes(x = PowerUnit, y = SurvRate)) +
  stat_summary(fun = "mean", geom = "bar", fill = "darkslategray", color = "black") +
  labs(title = "Main Effect of PowerUnit on Survival Rate", y = "Mean Survival Rate")

ggplot(df_fact, aes(x = Speed, y = SurvRate)) +
  stat_summary(fun = "mean", geom = "bar", fill = "darkslategray", color = "black") +
  labs(title = "Main Effect of Speed on Survival Rate", y = "Mean Survival Rate")

# Cooking Rate
ggplot(df_fact, aes(x = PowerUnit, y = CookedRate)) +
  stat_summary(fun = "mean", geom = "bar", fill = "coral4", color = "black") +
  labs(title = "Main Effect of PowerUnit on Cooking Rate", y = "Mean Cooking Rate")

ggplot(df_fact, aes(x = Speed, y = CookedRate)) +
  stat_summary(fun = "mean", geom = "bar", fill = "coral4", color = "black") +
  labs(title = "Main Effect of Speed on Cooking Rate", y = "Mean Cooking Rate")


# -------------------------- #
#  Equations of models   #
# -------------------------- #
# Surivival Rate
lm_model_Surv <- lm(SurvRate ~ PowerUnit * Speed, data = df_fact)
summary(lm_model_Surv)

# Cooking Rate
lm_model_Cooked <- lm(CookedRate ~ PowerUnit * Speed, data = df_fact)
summary(lm_model_Cooked)


coef_surv <- coef(lm_model_Surv)
coef_cooked <- coef(lm_model_Cooked)
effects_surv <- abs(coef_surv[-1])  # Ignora el intercepto
effects_cooked <- abs(coef_cooked[-1])

# Survival Rate
barplot(sort(effects_surv, decreasing = TRUE), main = "Pareto Chart - Survival Rate",
        xlab = "Effects", ylab = "Absolute Coefficients")

# Cooked Rate
barplot(sort(effects_cooked, decreasing = TRUE), main = "Pareto Chart - Cooked Rate",
        xlab = "Effects", ylab = "Absolute Coefficients")
