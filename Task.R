library(readr)
library(dplyr)

bank <- read_delim("bank.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
bank_full <- read_delim("bank-full.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
sum(is.na(bank))
sum(is.na(bank_full))

# ------- Duomenu pasiskirstymas
table(bank_full$y)
table(bank_full$y)/length(bank_full$y)*100

# ------- Skaitiniai duomenys
numericData <- bank_full %>% 
  select_if(is.numeric)
summary(numericData)

library(tidyverse)

numData <- function(data) {
  numeric_data <- data %>%
    select_if(is.numeric)
  
  num <- numeric_data %>%
    gather(var, value)
  
  boxplot_grid <- num %>%
    ggplot(aes(value)) +
    geom_boxplot() +
    facet_wrap(~ var, scales = 'free')
  
  histogram_grid <- num %>%
    ggplot(aes(value)) +
    geom_histogram(binwidth = 5, fill = "black", color = "black") +
    facet_wrap(~ var, scales = 'free')
  
  return(list(boxplot_grid, histogram_grid))
}

numeric_plots <- numData(bank_full)
numeric_plots[[1]] # Boxplot grid
numeric_plots[[2]] # Histogram grid

plot(bank_full$previous)
max_prev <- which.max(bank_full$previous)
bank_full[max_prev, ]
bank_full <- bank_full[-max_prev, ]

bank_full <- bank_full %>% 
  mutate(age_group = cut(age, breaks = c(0, 30, 40, 50, 60, Inf), 
                         labels = c("0-30", "31-40", "41-50", "51-60", "60+")))

# ------- Kategoriniai duomenys
catData <- function(data) {
  categorical_data <- data %>%
    select_if(is.character)
  
  cat_data_long <- categorical_data %>%
    gather(var, value)
  
  plot <- cat_data_long %>%
    ggplot(aes(x = value)) +
    geom_bar() +
    facet_wrap(~ var, scales = 'free') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(plot)
}

catData(bank_full)

for (column in names(bank_full)[sapply(bank_full, is.character)]) {
  print(prop.table(table(bank_full[[column]]))*100)
}

# ------- Kategoriniai -> Faktorius
bank_full$y <- ifelse(bank_full$y == "yes", 1, 0)

for (col in names(bank_full)) {
  if (is.character(bank_full[[col]])) {
    bank_full[[col]] <- as.factor(bank_full[[col]])
  }
}

# ------- Koreliacijos matrica
library(corrplot)

correlations <- cor(bank_full[, -c(2:5, 7:9, 11, 16:17)])
corrplot(correlations, type = "lower", method = "color", tl.col = "black",
         addCoef.col = "black", tl.cex=0.8, number.cex = 0.6)

# ------- Testavimo ir mokymo aibes
set.seed(1234) 
n_rows <- nrow(bank_full) 
train_rows <- sample(seq_len(n_rows), size = round(0.7 * n_rows)) 
train_data <- bank_full[train_rows, ] 
test_data <- bank_full[-train_rows, ] 

# ------- Logit modelis
logit <- glm(formula = y ~ .,
                  family = binomial(logit), data = train_data)

summary(logit)

# ------- Multikolinearumas
library(performance)
check_collinearity(logit)

# Nereiksmingos: ("age", "default", "pdays")

# ------- Išskirtys
par(mfrow=c(1,2))
cooksd <- cooks.distance(logit)
plot(cooksd, pch="*", cex=2, main = "Cooks distance")

library(boot)
pearson_residuals <- glm.diag(logit)$rp
plot(pearson_residuals, pch="*", cex=2, main="Pearson residuals") 

# residuals <- which(pearson_residuals %in% pearson_residuals[pearson_residuals <= -50])
# train_data1 <- train_data[-residuals,]

# ------ Modelio optimizavimas
remove <- c("age", "default", "pdays")
train_data1 <- train_data[, !names(train_data) %in% remove]
logit_1 <- glm(formula = y ~ .,
                    family = binomial(logit), data = train_data1)

summary(logit_1)

# ------ Klasifikavimo matrica
library(caret)
prediction <- round(logit_1$fitted.values)
confusionMatrix(data = as.factor(prediction), reference = as.factor(train_data1$y))

library(QuantPsyc)
ClassLog(logit_1, train_data1$y)

# ------- ROC kreive
par(mfrow=c(1,1))
plot(roc(logit_1$y, logit_1$fitted.values), 
     col="red", lwd=3, main="ROC curve of Logistic Regression", ylim = c(0, 1))

# ------- Exponentiated coefficients ("odds ratios")
exp(coef(logit_1))


# ------- ------------------------------------ probit modelis
probit <- glm(formula = y ~ .,
                   family = binomial(probit), data = train_data1)

summary(probit)


# ------- ANOVA probit ir logit
anova(logit_1, probit, test="Chisq")

# ------- Modelio tikslumas
prediction <- predict(logit_1, test_data, type = "response")
threshold <- 0.5
binary_pred <- ifelse(prediction >= threshold, 1, 0)
actual_outcomes <- test_data$y
accuracy <- mean(binary_pred == actual_outcomes)
accuracy
table(actual_outcomes, binary_pred)


prediction1 <- predict(probit, test_data, type = "response")
binary_pred1 <- ifelse(prediction1 >= threshold, 1, 0)
accuracy1 <- mean(binary_pred1 == actual_outcomes)
accuracy1
table(actual_outcomes, binary_pred1)
