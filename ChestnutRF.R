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
              "rstatix", "patchwork")
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

df_summary <- df %>%
  group_by(TestID) %>%
  summarise(
    PowerUnit_values = paste(unique(PowerUnit), collapse = ", "),  # PowerUnit Unique values
    Speed_values = paste(unique(Speed), collapse = ", "),  #Speed Unique values
    .groups = "drop"  # to avoit TestID being factor after df_summary
  )

# Route to safe the file
output_file <- "df_summary.xlsx"

# File creation
write.xlsx(df_summary, file = output_file)

# Summary
df_summ <- df %>%
  mutate(
    OutputT = as.numeric(na_if(OutputT, "Not registered")),
    DeltaT = as.numeric(na_if(DeltaT, "Not registered")),
    SurvRate = as.numeric(na_if(SurvRate, "no recovery"))
  ) %>%
  group_by(TestID) %>%
  summarise(
    n_InputT = sum(!is.na(InputT)),
    avg_InputT = mean(InputT, na.rm = TRUE),
    sd_InputT = sd(InputT, na.rm = TRUE),
    n_OutputT = sum(!is.na(OutputT)),
    avg_OutputT = mean(OutputT, na.rm = TRUE),
    sd_OutputT = sd(OutputT, na.rm = TRUE),
    n_DeltaT = sum(!is.na(DeltaT)),
    avg_DeltaT = mean(DeltaT, na.rm = TRUE),
    sd_DeltaT = sd(DeltaT, na.rm = TRUE),
    n_SurvRate = sum(!is.na(SurvRate)),
    avg_SurvRate = mean(SurvRate, na.rm = TRUE),
    sd_SurvRate = sd(SurvRate, na.rm = TRUE),
    n_CookedRate = sum(!is.na(CookedRate)),
    avg_CookedRate = mean(CookedRate, na.rm = TRUE),
    sd_CookedRate = sd(CookedRate, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -TestID, 
               names_to = c("Metric", "Variable"), 
               names_pattern = "(.*)_(.*)", 
               values_to = "Value") %>%
  pivot_wider(names_from = "Variable", values_from = "Value")


# -------------------------- #
#       Visualizations      #
# -------------------------- #  
# Trend of Survival rate by heat
# Preprocess
df_clean <- df %>%
  mutate(
    SurvRate = as.numeric(na_if(SurvRate, "no recovery")),
    TestID = factor(TestID) # Asegurarnos que sea categórico
  )

# Data filter and transformation
df_scatter <- df_clean %>%
  mutate(
    OutputT = as.numeric(replace(OutputT, OutputT == "Not registered", NA)),
    SurvRate = as.numeric(replace(SurvRate, SurvRate == "no recovery", NA))
  ) %>%
  drop_na(OutputT, SurvRate)  

# Scatter plots faceted by TestID
ggplot(df_scatter, aes(x = OutputT, y = SurvRate)) +
  geom_point(color = "blue", size = 2, alpha = 0.8) +
  facet_wrap(~ TestID) +
  labs(
    title = "Scatter Plot de OutputT vs Larval Survival Rate",
    x = "Output Temperature (°C)",
    y = "Larval Survival Rate (%)"
  ) +
  theme_bw()


# Survival vs cooked results
# Preprocess
df_clean <- df %>%
  mutate(
    SurvRate = as.numeric(na_if(SurvRate, "no recovery")),
    TestID = factor(TestID) # Asegurarnos que sea categórico
  )

# Individual Plots
# boxplot of Survival rate per treatments 
ggplot(df_clean, aes(x = TestID, y = SurvRate)) +
  geom_boxplot(fill = "lightblue", color = "darkblue",alpha = 0.6) +
  geom_jitter(width = 0.2, color = "black", size = 1.5, alpha = 0.8) +
  labs(
    title = "Boxplot de Survival Rate por TestID",
    x = "TestID",
    y = "Survival Rate (%)"
  ) 

# boxplot of CookRate rate per treatments 
ggplot(df_clean, aes(x = TestID, y = CookedRate)) +
  geom_boxplot(fill = "brown", color = "darkgrey", alpha = 0.6) +
  geom_jitter(width = 0.2, color = "black", size = 1.5, alpha = 0.8) +
  labs(
    title = "Boxplot de Cooked Rate con puntos por TestID",
    x = "TestID",
    y = "Cooked Rate (%)"
  ) 


# Faceting plot
# Dataset for facet
df_long <- df_clean %>%
  select(TestID, SurvRate, CookedRate) %>%
  pivot_longer(cols = c(SurvRate, CookedRate), names_to = "Metric", values_to = "Value"
  ) %>%
  mutate(
    Metric = factor(Metric, levels = c("SurvRate", "CookedRate"), 
                    labels = c("Larval Survival Rate (%)", "Cooked Chestnuts (%)"))
  )

# Facet plotting
ggplot(df_long, aes(x = TestID, y = Value)) +
  geom_boxplot(aes(fill = Metric), alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, color = "black", size = 1.5, alpha = 0.8) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(
    title = "Boxplots de Survival Rate y Cooked Rate por TestID",
    x = "TestID",
    y = "Value (%)"
  ) +
  guides(fill = "none") +
  scale_fill_manual(values = c("Larval Survival Rate (%)" = "darkslategray", 
                               "Cooked Chestnuts (%)" = "coral4"))


# -------------------------- #
# Chestnut-Carpets positions #
# -------------------------- #
# Heatmap for heat distribution along carpets
# Pivot longer transformation for the heatmap
df_heatmap <- df_clean %>%
  select(TestID, Carpet, InferiorOutT, MiddleOutT, SuperiorOutT) %>%
  pivot_longer(
    cols = c(InferiorOutT, MiddleOutT, SuperiorOutT),
    names_to = "TemperatureLevel",
    values_to = "Temperature"
  ) %>%
  filter(Temperature != "Not registered") %>%
  mutate(Temperature = as.numeric(Temperature))

# Heat map plot
ggplot(df_heatmap, aes(x = Carpet, y = TemperatureLevel, fill = Temperature)) +
  geom_tile() +
  facet_wrap(~ TestID) +
  labs(
    title = "Heatmap of chestnut internal temperatures by carpets",
    x = "Carpet (#)",
    y = "Temperature Level",
    fill = "Temperature (°C)"
  ) +
  scale_fill_gradient(low = "blue", high = "red") +  # Heatmap color adjustment
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   # X axis rotations

## Stat analysis
# ANOVA data transformation
df_temp <- df_clean %>%
  select(TestID, InferiorOutT, MiddleOutT, SuperiorOutT) %>%
  pivot_longer(cols = c(InferiorOutT, MiddleOutT, SuperiorOutT),
               names_to = "TemperatureLevel",
               values_to = "Temperature") %>%
  filter(Temperature != "Not registered") %>%
  mutate(Temperature = as.numeric(Temperature),
         TemperatureLevel = factor(TemperatureLevel, levels = c("InferiorOutT", "MiddleOutT", "SuperiorOutT")))

# ANOVA for mean of chestnut at different carpet positions
anova_result <- aov(Temperature ~ TemperatureLevel, data = df_temp)

# ANOVA summary
summary(anova_result)

# Tukey post-hoc if signnificant
if(summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  summary(tukey_result)
}



# -------------------------- #
#       Visualizations 2.5   #
# -------------------------- #  
# Filtering TestID == "2.5"
df_filtered <- df %>% filter(TestID == "2.5")

# Duplicate observations
df_duplicated <- df_filtered

# Renoming PowerUnit
df_duplicated <- df_duplicated %>%
  mutate(TestID = case_when(
    PowerUnit == "4500+4500+2000+3000" ~ "2.5a",
    PowerUnit == "4500+4500+2000+2000" ~ "2.5b",
    TRUE ~ TestID # Mantener el TestID original si no coincide con los valores especificados
  ))

# Bind new labaled data with original
df_final <- bind_rows(df_filtered, df_duplicated)

# Remove no numeric 
df_clean_2.5 <- df_final %>%
  mutate(
    SurvRate = as.numeric(na_if(SurvRate, "no recovery")),
    TestID = factor(TestID) # Asegurarnos que sea categórico
  )

# Faceting plot
# Dataset for facet
df_long_2.5 <- df_clean_2.5 %>%
  select(TestID, SurvRate, CookedRate) %>%
  pivot_longer(cols = c(SurvRate, CookedRate), names_to = "Metric", values_to = "Value"
  ) %>%
  mutate(
    Metric = factor(Metric, levels = c("SurvRate", "CookedRate"), 
                    labels = c("Larval Survival Rate (%)", "Cooked Chestnuts (%)"))
  )

# Facet plotting
ggplot(df_long_2.5, aes(x = TestID, y = Value)) +
  geom_boxplot(aes(fill = Metric), alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, color = "black", size = 1.5, alpha = 0.8) +
  facet_wrap(~ Metric) +
  labs(
    title = "Survival Rate & Cooked Rate Boxplots of TestID 2.5",
    x = "TestID",
    y = "Value (%)"
  ) +
  guides(fill = "none") +
  scale_fill_manual(values = c("Larval Survival Rate (%)" = "darkslategray", 
                               "Cooked Chestnuts (%)" = "coral4"))
