library(tidyverse)
library(ggcorrplot)
library(car)

my_data <- read_csv("https://bit.ly/31FxhF4")

corr <- cor(my_data)

ggcorrplot(corr , hc.order = TRUE, type = "lower",
           lab = TRUE)

# Can house prices be predicted by (one or more of) Population, Crime (per 10000 people), 
# Average age, or Household income in a region?  We have my_data from 1,000 regions.
# First let's plot some individual graphs

ggplot(my_data, aes(x = House_price, y = Population)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(my_data, aes(x = House_price, y = Crime)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(my_data, aes(x = House_price, y = Average_age)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(my_data, aes(x = House_price, y = Household_income)) + 
  geom_point() + 
  geom_smooth(method = "lm")

corr

# First let's build a null model
model0 <- lm(House_price ~ 1, data = my_data)

# First let's build a model with all predictors
model1 <- lm(House_price ~ Population + Crime + Average_age + Household_income, data = my_data)

# Do we have any multi-colinearity issues?
vif(model1)

# Check to see if the full model is better than the null model
anova(model0, model1)

# Now get the summary of model1
summary(model1)

# Notice that Average_age and Household_income do not seem to predict house prices
# Let's drop them in model2
model2 <- lm(House_price ~ Population + Crime, data = my_data)

# Is model2 now better model1?
anova(model2, model1)

AIC(model1)
AIC(model2)

# Let's validate and look at some diagnostic plots
qqnorm(residuals(model2))
qqline(residuals(model2))

plot(model2)

durbinWatsonTest(model2)

# Now let's do some stepwise regression to see what we end up with
steplimitsboth <- step(model0, scope = list (upper = model1), direction = "both")

summary(steplimitsboth)

library(olsrr)

pmodel <- ols_step_forward_p(model1)
pmodel

library(leaps)

leapsmodels <- regsubsets(House_price ~ Population + Crime + Average_age + Household_income, data = my_data)

plot(leapsmodels, scale = "adjr2", main = "Models")

confint(steplimitsboth, level=0.95)

vif(steplimitsboth)

# ANOVA ####
# Load data
cond <- read_csv("https://bit.ly/33xm62R")

cond$Condition <- as.factor(cond$Condition)

cond %>% 
  ggplot(aes(x = Condition, y = Ability, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme(text = element_text(size = 15))

cond %>%
  group_by(Condition) %>%
  summarise(mean = mean(Ability), sd = sd(Ability))

library(afex)
library(emmeans)

model <- aov_4(Ability ~ Condition + (1 | Participant), data = cond)

summary(model)

emmeans(model, pairwise ~ Condition)

# Repeated measures ANOVA ####
rm_data <- read_csv("https://bit.ly/304Koiu")
rm_data$Condition <- as.factor(rm_data$Condition)
rm_data

rm_data %>%
  group_by(Condition) %>%
  summarise(mean = mean(Score), sd = sd (Score))

rm_data %>%
  ggplot(aes(x = Condition, y = Score, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme(text = element_text(size = 15))

rm_data %>%
  ggplot(aes(x = fct_reorder(Condition, Score), y = Score, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme(text = element_text(size = 15)) +
  labs(x = "Condition")

model <- aov_4(Score ~ Condition + (1 + Condition | Participant), data = rm_data)
anova(model)
