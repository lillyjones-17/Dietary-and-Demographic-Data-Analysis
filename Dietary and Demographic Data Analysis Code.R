library(readxl)
cancer_risk <- read_excel("Cancer Risk.xlsx")

# Question 1
variable_table <- data.frame(
  "Variable Name" = c("AGE", "GENDER", "SMOKESTAT", "CALORIES", "FAT"),
  "General Type" = c("Quantitative", "Categorical", "Categorical", "Quantitative", "Quantitative"),
  "Specific Type" = c("Continuous", "Nominal", "Nominal", "Continuous", "Continuous"),
  "Measurement Units" = c("Years", "N/A", "N/A", "Calories per day", "Grams per day"),
  check.names = FALSE
)

variable_table



# Question 2
summary(cancer_risk$Age)
sd(cancer_risk$Age)
var(cancer_risk$Age)
IQR(cancer_risk$Age)
range(cancer_risk$Age)



summary(cancer_risk$Calories)
sd(cancer_risk$Calories)
var(cancer_risk$Calories)
IQR(cancer_risk$Calories)
range(cancer_risk$Calories)



summary(cancer_risk$Fat)
sd(cancer_risk$Fat)
var(cancer_risk$Fat)
IQR(cancer_risk$Fat)
range(cancer_risk$Fat)



# Question 3
library(ggplot2)

# Calories histogram and boxplot
ggplot(data = cancer_risk, aes(x = Calories)) +
  geom_histogram(color = "black", fill = "orchid3") +
  labs(
    title = "Figure 1: Distribution of Calories",
    x = "Calories Consumed per Day",
    y = "Frequency") +
  theme_classic()

ggplot(data = cancer_risk, aes(x = Calories)) +
  geom_boxplot(color = "black", fill = "orchid3") +
  labs(
    title = "Figure 2: Calories Boxplot",
    x = "Calories Consumed per Day",
    y = "") +
  theme_classic()

# Age histogram and boxplot
ggplot(data = cancer_risk, aes(x = Age)) +
  geom_histogram(color = "black", fill = "deeppink") +
  labs(
    title = "Figure 3: Distribution of Age",
    x = "Age (years)",
    y = "Frequency") +
  theme_classic()

ggplot(data = cancer_risk, aes(x = Age)) +
  geom_boxplot(color = "black", fill = "deeppink") +
  labs(
    title = "Figure 4: Age Boxplot",
    x = "Age (years)",
    y = "") +
  theme_classic()

# Fat histogram and boxplot
ggplot(data = cancer_risk, aes(x = Fat)) + 
  geom_histogram(color = "black", fill = "plum") +
  labs(
    title = "Figure 5: Distribution of Fat",
    x = "Grams of Fat per Day",
    y = "Frequency") +
  theme_classic()

ggplot(data = cancer_risk, aes(x = Fat)) +
  geom_boxplot(color = "black", fill = "plum") +
  labs(
    title = "Figure 6: Fat Boxplot",
    x = "Grams of Fat per Day",
    y = "") +
  theme_classic()



# agecat
agecat <- cancer_risk$Age |>
  cut(
    breaks = c(-Inf, 35, 45, 55, Inf),
    labels = c("Age <= 35", "35 < Age <= 45", "45 < Age <= 55", "Age > 55"))

agecat_freq <- table(agecat)
agecat_freq

ggplot(data = cancer_risk, aes(x = "", fill = agecat)) +
  geom_bar(width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = c("deeppink4", "violet", "thistle3", "purple4")) +
  labs(
    title = "Figure 7: Age Category Distribution",
    fill = "Age Category") +
  theme_void()

agecat_ordered <- sort(agecat_freq, decreasing = TRUE)
agecat_ordered <- as.data.frame(agecat_ordered)

agecat_ordered$agecat <- factor(
  agecat_ordered$agecat,
  levels = agecat_ordered$agecat)

ggplot(data = agecat_ordered, aes(x = agecat, y = Freq, fill = agecat)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("turquoise3", "darkseagreen2", "lightskyblue2", "darkolivegreen4")) +
  labs(
    title = "Figure 8: Age Category Distribution",
    x = "Age Category",
    y = "Frequency") +
  theme_classic()



# Question 4
cancer_risk$Gender <- factor(cancer_risk$Gender, levels = c(1, 2), labels = c("Male", "Female"))
cancer_risk$SmokeStat <- factor(cancer_risk$SmokeStat, levels = c(1, 2, 3), labels = c("Never", "Former", "Current Smoker"))
smoke_by_gender <- table(cancer_risk$Gender, cancer_risk$SmokeStat)
smoke_by_gender

round(prop.table(smoke_by_gender) * 100, 2)

round(prop.table(smoke_by_gender, margin = 1) * 100, 2)

round(prop.table(smoke_by_gender, margin = 2) * 100, 2)



# Question 5
ggplot(data = cancer_risk, aes(x = SmokeStat, fill = Gender)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("darkseagreen1", "plum1")) +
  labs(
    title = "Figure 9: Smoking Status by Gender",
    x = "Smoking Status",
    y = "Proportion",
    fill = "Gender") +
  theme_classic()



# Question 6
library(dplyr)
by(
  data = cancer_risk$Fat,
  INDICES = agecat,
  FUN = mean
)

# Question 7
t.test(cancer_risk$Calories, conf.level = 0.95)

# Question 8
# Scatterplot
ggplot(data = cancer_risk, aes(x = Age, y = Calories)) +
  geom_point() +
  labs(
    title = "Figure 10: Relationship Between Age and Calories",
    x = "Age",
    y = "Calories Consumed per Day") +
  theme_classic()

# Side-by-side boxplots
ggplot(data = cancer_risk, aes(x = Calories, y = agecat)) +
  geom_boxplot(fill = "orchid3", color = "black") +
  labs(
    title = "Figure 11: Calories by Age Category",
    x = "Calories Consumed per Day",
    y = "Age Category") +
  theme_classic()

# Stratified confidence intervals for calories
by(
  data = cancer_risk$Calories,
  INDICES = agecat,
  FUN = function(x) {
    t.test(x, conf.level = 0.95)$conf.int})

# Contingency table
calorie_q1 <- quantile(cancer_risk$Calories, 0.25)
calorie_q3 <- quantile(cancer_risk$Calories, 0.75)

caloriecat <- cancer_risk$Calories |>
  cut(
    breaks = c(-Inf, calorie_q1, calorie_q3, Inf),
    labels = c("Low", "Moderate", "High"))

calories_by_age <- table(agecat, caloriecat)
calories_by_age

# 100% Stacked bar charts
cancer_risk$agecat <- agecat
cancer_risk$caloriecat <- caloriecat

ggplot(data = cancer_risk, aes(x = agecat, fill = caloriecat)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("paleturquoise2", "mediumvioletred", "chartreuse3")) +
  labs(
    title = "Figure 12: Calorie Consumption by Age Category",
    x = "Age Category",
    y = "Proportion",
    fill = "Calorie Category") +
  theme_classic()
