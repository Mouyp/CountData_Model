# Load Libraries

install.packages("Hmisc")
install.packages("VGAM")
install.packages('corrplot')
install.packages("MASS")
install.packages("glmnet")
install.packages("car")

library(corrplot)
library(MASS)
library(AER)
library(VGAM)
library(ggplot2)
library(tidyverse) 
library(lubridate)
library(Hmisc)
library(glmnet)
library(car)
library(ggplot2)
library(viridis)

# -----------------------------------------------------------------------------
#Load Data

energy_data <- read.csv("customer_data.csv")

nrow(energy_data)
View(energy_data)
str(energy_data)

# ------------------ Clean Data ---------------------------------------------------

# [1] Check for missing values

sum(is.na(energy_data))
## NA = 1560 (sec_chatbot + sec_service)

# Missing values in each column
sapply(energy_data, function(x) sum(is.na(x)))

#  hh_size  = 139, solar_panels_since = 1402 , sec_service = 871,  second_chatbot = 689

# Impute hh_size with median
energy_data$hh_size[is.na(energy_data$hh_size)] <- median(energy_data$hh_size, na.rm = TRUE)

# Impute with 0
# 1) solar_panels_since
energy_data$solar_panels_since[is.na(energy_data$solar_panels_since)] <- 2022

# 2) sec_service
energy_data$sec_service[is.na(energy_data$sec_service)] <- 0

# 3) second_chatbot
energy_data$second_chatbot[is.na(energy_data$second_chatbot)] <- 0

energy_data$hh_size[is.na(energy_data$hh_size)] <- 0


# [2] Duplicate column
# email_newsletter & email_information

# --------------- Convert Data & Create New Variables --------------------------------------------

# Date Format
energy_data$customer_since <- as.Date(energy_data$customer_since)

energy_data$sec_service <- as.Date(energy_data$sec_service, format = "%Y-%m-%d")
energy_data$second_chatbot <- as.Date(energy_data$second_chatbot, format = "%Y-%m-%d")

# Convert to integer
energy_data$hh_size <- as.integer(energy_data$hh_size)


# Convert into factor
energy_data$urban <- factor(energy_data$urban,
                            levels = c(0, 1),
                            labels = c("0", "1"))

energy_data$solar_panels <- factor(energy_data$solar_panels,
                                   levels = c(0, 1),
                                   labels = c("0", "1"))

energy_data$has_sec_service <- as.factor(energy_data$has_sec_service)
energy_data$has_sec_chatbot <- as.factor(energy_data$has_sec_chatbot)

# Remove column [X]
energy_data <- energy_data[,-1]



#---------------------- Create New Variables ----------------------------

# Create New Variables for "customer service"

# [1] Customers Service Duration (Relationship Length)
energy_data$first_service <- as.Date(energy_data$first_service, format = "%Y-%m-%d")
energy_data$last_service <- as.Date(energy_data$last_service, format = "%Y-%m-%d")

energy_data$service_duration <- as.numeric(difftime(energy_data$last_service, energy_data$first_service, units = "days"))


# [2] Whether customers have second service??
energy_data$has_sec_service <- ifelse(!is.na(energy_data$sec_service) & energy_data$sec_service != 0, 1, 0)


# Create new variables for "chatbot"

# [1] Chatbot Duration (Relationship Length)
energy_data$first_chatbot <- as.Date(energy_data$first_chatbot, format = "%Y-%m-%d")
energy_data$last_chatbot <- as.Date(energy_data$last_chatbot, format = "%Y-%m-%d")

energy_data$chatbot_duration <- as.numeric(difftime(energy_data$last_chatbot, energy_data$first_chatbot, units = "days"))

# [2] Whether customers have second chatbot use??
energy_data$has_sec_chatbot <- ifelse(!is.na(energy_data$second_chatbot) & energy_data$second_chatbot != 0, 1, 0)


# Create new variables for "diff_service"
energy_data <- energy_data %>%
  mutate(diff_service = pos_service - neg_service)

# Create new variables for "diff_chatbot"
energy_data <- energy_data %>%
  mutate(diff_chatbot = pos_chatbot - neg_chatbot)

# first_service and first_chatbot (Date format)
energy_data$days_since_first_service <- as.numeric(difftime(energy_data$first_service, 
                                                            min(energy_data$first_service), 
                                                            units = "days"))
energy_data$days_since_first_chatbot <- as.numeric(difftime(energy_data$first_chatbot, 
                                                            min(energy_data$first_chatbot), 
                                                            units = "days"))


# solar_panels_since and customer_since_yr (integer format)

current_year <- 2022  # Current year
energy_data$years_since_solar_panels <- current_year - energy_data$solar_panels_since
energy_data$years_since_customer <- current_year - energy_data$customer_since_yr


# New variable for having "high positive call center service experience"
threshold1 <- quantile(energy_data$pos_service, 0.75)
energy_data$high_pos_service <- ifelse(energy_data$pos_service >= threshold1, 1, 0)


# New variable for having "high positive chatbot experience"
threshold2 <- quantile(energy_data$pos_chatbot, 0.75)
energy_data$high_pos_chatbot <- ifelse(energy_data$pos_chatbot >= threshold2, 1, 0)


# -------------- Winsorizing "nrservice" & "nrchatbot" --------------------------------------

library(DescTools)

energy_data$winsorized_nrservice <- Winsorize(energy_data$nrservice)
energy_data$winsorized_nrchatbot <- Winsorize(energy_data$nrchatbot)


# -------------- Data Analysis: nrchatbot & nrservice --------------------------------------------

# -------------- PART 1: Explore Variables --------------------------------

summary(energy_data$nrservice)
summary(energy_data$winsorized_nrservice)
summary(energy_data$nrchatbot)
summary(energy_data$winsorized_nrchatbot)


# ----------------------- PART2: Visualization DEP VAR ---------------------------------------
summary(energy_data$nrservice)
summary(energy_data$nrchatbot)


# Boxplot for nrservice
ggplot(energy_data, aes(y = nrservice)) + 
  geom_boxplot(fill = "blue") +
  ggtitle("Boxplot for nrservice")


# Boxplot for nrchatbot
ggplot(energy_data, aes(y = nrchatbot)) + 
  geom_boxplot(fill = "blue") +
  theme_minimal() +
  ggtitle("Boxplot for nrchatbot")


# plot histogram for nrservice
ggplot(energy_data, aes(nrservice)) +
  geom_histogram(binwidth=1, fill="blue") +
  ggtitle("Distribution of total calls with customer service")

# plot histogram for nrservice (winsorized)
ggplot(energy_data, aes(winsorized_nrservice)) +
  geom_histogram(binwidth=1, fill="blue") +
  ggtitle("Distribution of total calls with customer service")

# plot histogram for nrchatbot
ggplot(energy_data, aes(nrchatbot)) +
  geom_histogram(binwidth=1, fill="blue") +
  ggtitle("Distribution of total interactions with online chatbot")

# plot histogram for nrchatbot (winsorized)
ggplot(energy_data, aes(winsorized_nrchatbot)) +
  geom_histogram(binwidth=1, fill="blue") +
  ggtitle("Distribution of total interactions with online chatbot")


#----------------------- PART3: Model -------------------------------


# ------------------- NRSERVICE -------------------------------------

# [4.1]: Factors impact "total number of calls" with the customer service 

sum(energy_data$nrservice == 0) #0
sum(energy_data$nrservice == 1) #871
mean(energy_data$nrservice) # 4.392
var(energy_data$nrservice) # 83.68818
mean(energy_data$winsorized_nrservice) # 3.5885
var(energy_data$winsorized_nrservice) # 16.73053

colSums(is.na(energy_data))

# Method1: Poisson Regression (Count Data) --------------

#[AIC: 9726.4/ 11vars]
cs_poisson_service <-glm(winsorized_nrservice ~ diff_chatbot + 
                           days_since_first_service + days_since_first_chatbot + satisfaction +
                           years_since_customer + hh_size + 
                           years_since_solar_panels + nr_solar_panels+
                           email_pers_solar + email_newsletter +
                           av_bill,
                         data= energy_data,
                         family='poisson')

summary(cs_poisson_service)
exp(coef(cs_poisson_service))

vif(cs_poisson_service)


# Method2: Negative Binomial Regression (Mean not equal Variance)  --------------

# Dispersion Test
disp_test_service <-dispersiontest(cs_poisson_service, trafo =2)
disp_test_service
capture.output(disp_test_service, file = "disp_test_service.txt")


cs_negbin_service <-glm.nb(winsorized_nrservice ~ diff_chatbot + 
                             days_since_first_service + days_since_first_chatbot + satisfaction +
                             years_since_customer + hh_size + 
                             years_since_solar_panels + nr_solar_panels+
                             email_pers_solar + email_newsletter +
                             av_bill,
                           data= energy_data)

summary(cs_negbin_service)
exp(coef(cs_negbin_service))

#theta=1/alpha (alpha=1/theta)
alpha1 =1/cs_negbin_service$theta
alpha1


# Method3: Truncated Count Model (No Zero) --------------

#estimated truncated negbin model. 
control = vglm.control(maxit = 100)

cs_trunc_service <-vglm(winsorized_nrservice ~ diff_chatbot + 
                          days_since_first_service + days_since_first_chatbot + satisfaction +
                          years_since_customer + hh_size + 
                          years_since_solar_panels + nr_solar_panels+
                          email_pers_solar + email_newsletter + 
                          av_bill,
                        data= energy_data,
                        family= posnegbinomial(),
                        control = control)

summary(cs_trunc_service)
exp(coef(cs_trunc_service))



# ----- Compare Model (BIC & AIC) ----
BIC(cs_poisson_service)
BIC(cs_negbin_service)
BIC(cs_trunc_service)
AIC(cs_poisson_service)
AIC(cs_negbin_service)
AIC(cs_trunc_service)


model_comparison_service <- data.frame(
  Model = c("Poisson_service", "Negative Binomial_service", "Truncated_service"),
  BIC = c(BIC(cs_poisson_service), BIC(cs_negbin_service), BIC(cs_trunc_service)),
  AIC = c(AIC(cs_poisson_service), AIC(cs_negbin_service), AIC(cs_trunc_service))
)

print(model_comparison_service)

write.table(model_comparison_service, file = "model_comparison_service.txt", sep = "\t", row.names = FALSE)




# ------------------- NRCHATBOT -------------------------------------
# [4.2]: Factors impact "the number chatbot interactions" 

sum(energy_data$nrchatbot == 0) #0
sum(energy_data$nrchatbot == 1) #689
mean(energy_data$nrchatbot) # 10.505
var(energy_data$nrchatbot) # 1013.114
mean(energy_data$winsorized_nrchatbot) # 7.2105
var(energy_data$winsorized_nrchatbot) # 121.65



## Method1: Poisson Regression (Count Data)  --------------

cs_poisson_chatbot <-glm(winsorized_nrchatbot ~ diff_service + 
                           days_since_first_service + days_since_first_chatbot + satisfaction +
                           years_since_customer + hh_size + 
                           years_since_solar_panels + nr_solar_panels+
                           email_pers_solar + email_newsletter + 
                           av_bill + av_chatbot_length,
                         data=energy_data,
                         family='poisson')

summary(cs_poisson_chatbot)
exp(coef(cs_poisson_chatbot))



## Method2: Negative Binomial Regression (Mean not equal Variance)  --------------
disp_test_bot <-dispersiontest(cs_poisson_chatbot, trafo =2)
disp_test_bot
capture.output(disp_test_bot , file = "dispersion_test_bot.txt")



cs_negbin_chatbot <-glm.nb(winsorized_nrchatbot ~ diff_service + 
                             days_since_first_service + days_since_first_chatbot + satisfaction +
                             years_since_customer + hh_size + 
                             years_since_solar_panels + nr_solar_panels+
                             email_pers_solar + email_newsletter + 
                             av_bill,
                           data=energy_data)

summary(cs_negbin_chatbot)
exp(coef(cs_negbin_chatbot))

alpha =1/cs_negbin_chatbot$theta
alpha




# Method3: Truncated Count Model (No Zero)--------------
install.packages('glmmTMB')
library(glmmTMB)


cs_trunc_chatbot <- glmmTMB(winsorized_nrchatbot ~ diff_service + 
                              days_since_first_service + days_since_first_chatbot + satisfaction +
                              years_since_customer + hh_size + 
                              years_since_solar_panels + nr_solar_panels+
                              email_pers_solar + email_newsletter + 
                              av_bill,
                            data= energy_data,
                            family = truncated_nbinom2(link = "log"))

summary(cs_trunc_chatbot)

fixed_effects <- summary(cs_trunc_chatbot)$coeff$cond
exp_fixed_effects <- exp(fixed_effects[, "Estimate"])
exp_fixed_effects



#--------------

cs_trunc_chatbot2 <-vglm(winsorized_nrchatbot ~ diff_service + 
                           days_since_first_service + days_since_first_chatbot + satisfaction +
                           years_since_customer + hh_size + 
                           years_since_solar_panels + nr_solar_panels+
                           email_pers_solar + email_newsletter + 
                           av_bill,
                         data= energy_data,
                         family= posnegbinomial(),
                         control = control)

summary(cs_trunc_chatbot2)
exp(coef(cs_trunc_chatbot2))


# ----- Compare Model (BIC & AIC) ----
BIC(cs_poisson_chatbot)
BIC(cs_negbin_chatbot)
BIC(cs_trunc_chatbot)
AIC(cs_poisson_chatbot)
AIC(cs_negbin_chatbot)
AIC(cs_trunc_chatbot)


model_comparison_chatbot <- data.frame(
  Model = c("Poisson_chatbot", "Negative Binomial_chatbot", "Truncated_chatbot"),
  BIC = c(BIC(cs_poisson_chatbot), BIC(cs_negbin_chatbot), BIC(cs_trunc_chatbot)),
  AIC = c(AIC(cs_poisson_chatbot), AIC(cs_negbin_chatbot), AIC(cs_trunc_chatbot))
)

print(model_comparison_chatbot)

write.table(model_comparison_chatbot, file = "model_comparison_chatbot.txt", sep = "\t", row.names = FALSE)

# Load Libraries

install.packages("Hmisc")
install.packages("VGAM")
install.packages('corrplot')
install.packages("MASS")
install.packages("glmnet")
install.packages("car")

library(corrplot)
library(MASS)
library(AER)
library(VGAM)
library(ggplot2)
library(tidyverse) 
library(lubridate)
library(Hmisc)
library(glmnet)
library(car)
library(ggplot2)
library(viridis)

# -----------------------------------------------------------------------------
#Load Data

energy_data <- read.csv("customer_data.csv")

nrow(energy_data)
View(energy_data)
str(energy_data)

# ------------------ Clean Data ---------------------------------------------------

energy_data <- read.csv("energy_data.csv")

# [1] Check for missing values

sum(is.na(energy_data))
## NA = 1560 (sec_chatbot + sec_service)

# Missing values in each column
sapply(energy_data, function(x) sum(is.na(x)))

#  hh_size  = 139, solar_panels_since = 1402 , sec_service = 871,  second_chatbot = 689

# Impute hh_size with median
energy_data$hh_size[is.na(energy_data$hh_size)] <- median(energy_data$hh_size, na.rm = TRUE)

# Impute with 0
# 1) solar_panels_since
energy_data$solar_panels_since[is.na(energy_data$solar_panels_since)] <- 2022

# 2) sec_service
energy_data$sec_service[is.na(energy_data$sec_service)] <- 0

# 3) second_chatbot
energy_data$second_chatbot[is.na(energy_data$second_chatbot)] <- 0

energy_data$hh_size[is.na(energy_data$hh_size)] <- 0


# [2] Duplicate column
# email_newsletter & email_information

# --------------- Convert Data & Create New Variables --------------------------------------------

# Date Format
energy_data$customer_since <- as.Date(energy_data$customer_since)

energy_data$sec_service <- as.Date(energy_data$sec_service, format = "%Y-%m-%d")
energy_data$second_chatbot <- as.Date(energy_data$second_chatbot, format = "%Y-%m-%d")

# Convert to integer
energy_data$hh_size <- as.integer(energy_data$hh_size)


# Convert into factor
energy_data$urban <- factor(energy_data$urban,
                            levels = c(0, 1),
                            labels = c("0", "1"))

energy_data$solar_panels <- factor(energy_data$solar_panels,
                                   levels = c(0, 1),
                                   labels = c("0", "1"))

energy_data$has_sec_service <- as.factor(energy_data$has_sec_service)
energy_data$has_sec_chatbot <- as.factor(energy_data$has_sec_chatbot)

# Remove column [X]
energy_data <- energy_data[,-1]



#---------------------- Create New Variables ----------------------------

# Create New Variables for "customer service"

# [1] Customers Service Duration (Relationship Length)
energy_data$first_service <- as.Date(energy_data$first_service, format = "%Y-%m-%d")
energy_data$last_service <- as.Date(energy_data$last_service, format = "%Y-%m-%d")

energy_data$service_duration <- as.numeric(difftime(energy_data$last_service, energy_data$first_service, units = "days"))


# [2] Whether customers have second service??
energy_data$has_sec_service <- ifelse(!is.na(energy_data$sec_service) & energy_data$sec_service != 0, 1, 0)


# Create new variables for "chatbot"

# [1] Chatbot Duration (Relationship Length)
energy_data$first_chatbot <- as.Date(energy_data$first_chatbot, format = "%Y-%m-%d")
energy_data$last_chatbot <- as.Date(energy_data$last_chatbot, format = "%Y-%m-%d")

energy_data$chatbot_duration <- as.numeric(difftime(energy_data$last_chatbot, energy_data$first_chatbot, units = "days"))

# [2] Whether customers have second chatbot use??
energy_data$has_sec_chatbot <- ifelse(!is.na(energy_data$second_chatbot) & energy_data$second_chatbot != 0, 1, 0)


# Create new variables for "diff_service"
energy_data <- energy_data %>%
  mutate(diff_service = pos_service - neg_service)

# Create new variables for "diff_chatbot"
energy_data <- energy_data %>%
  mutate(diff_chatbot = pos_chatbot - neg_chatbot)

# first_service and first_chatbot (Date format)
energy_data$days_since_first_service <- as.numeric(difftime(energy_data$first_service, 
                                                            min(energy_data$first_service), 
                                                            units = "days"))
energy_data$days_since_first_chatbot <- as.numeric(difftime(energy_data$first_chatbot, 
                                                            min(energy_data$first_chatbot), 
                                                            units = "days"))


# solar_panels_since and customer_since_yr (integer format)

current_year <- 2022  # Current year
energy_data$years_since_solar_panels <- current_year - energy_data$solar_panels_since
energy_data$years_since_customer <- current_year - energy_data$customer_since_yr


# New variable for having "high positive call center service experience"
threshold1 <- quantile(energy_data$pos_service, 0.75)
energy_data$high_pos_service <- ifelse(energy_data$pos_service >= threshold1, 1, 0)


# New variable for having "high positive chatbot experience"
threshold2 <- quantile(energy_data$pos_chatbot, 0.75)
energy_data$high_pos_chatbot <- ifelse(energy_data$pos_chatbot >= threshold2, 1, 0)


# -------------- Winsorizing "nrservice" & "nrchatbot" --------------------------------------

library(DescTools)

energy_data$winsorized_nrservice <- Winsorize(energy_data$nrservice)
energy_data$winsorized_nrchatbot <- Winsorize(energy_data$nrchatbot)

# --------------- Explore Data --------------------------------------------


# View the structure of the data
str(energy_data)

# View a summary of the data
summary(energy_data)

# View the first few rows of the data
head(energy_data)
View(energy_data)


# [1] Proportion of long-term customers
table(energy_data$longterm_customer)

# [2] Average number of services used by customers:
mean(energy_data$nrservice)


# [3] Average duration of services
mean(energy_data$service_duration)


# [4]Proportion of customers using secondary services:
table(energy_data$has_sec_service)


# [5] Relationship btw service_duration vs nrservice
plot(energy_data$service_duration, energy_data$nrservice, main="Service Duration vs. Number of Services", xlab="Service Duration", ylab="Number of Services", pch=19)

hist(energy_data$service_duration, main = "Distribution of Service Duration", xlab = "Service Duration", col = "blue")
hist(energy_data$nrservice, main = "Distribution of Number of Services", xlab = "Number of Services", col = "blue")

barplot(table(energy_data$longterm_customer), main = "Long-term Customer Distribution", xlab = "Long-term Customer", ylab = "Count", col = "blue")
barplot(table(energy_data$has_sec_service), main = "Secondary Service Usage Distribution", xlab = "Has Secondary Service", ylab = "Count", col = "blue")


# Comparing number of services between long-term and newer customers
with(energy_data, tapply(nrservice, longterm_customer, mean))

# Comparing service duration between long-term and newer customers
with(energy_data, tapply(service_duration, longterm_customer, mean))

# Comparing chatbot interactions between long-term and newer customers
with(energy_data, tapply(nrchatbot, longterm_customer, mean))

# Comparing gas usage between long-term and newer customers
with(energy_data, tapply(av_gas, longterm_customer, mean))

# Comparing electricity usage between long-term and newer customers
with(energy_data, tapply(av_elec, longterm_customer, mean))

# Checking solar panel prevalence among long-term and newer customers
with(energy_data, table(solar_panels, longterm_customer))

# Plotting number of services vs. solar panel adoption
with(energy_data, plot(nrservice, solar_panels, main="Number of Services vs. Solar Panel Adoption", xlab="Number of Services", ylab="Solar Panel Adoption"))

# Plotting service duration vs. solar panel adoption
with(energy_data, plot(service_duration, solar_panels, main="Service Duration vs. Solar Panel Adoption", xlab="Service Duration", ylab="Solar Panel Adoption"))


# Comparing number of services between long-term and newer customers
ggplot(energy_data, aes(x = factor(longterm_customer), y = nrservice)) +
  geom_boxplot() +
  labs(x = "Long-term Customer (1: Yes, 0: No)", y = "Number of Services", title = "Comparison of Number of Services")

# Comparing service duration between long-term and newer customers
ggplot(energy_data, aes(x = factor(longterm_customer), y = service_duration)) +
  geom_boxplot() +
  labs(x = "Long-term Customer (1: Yes, 0: No)", y = "Service Duration", title = "Comparison of Service Duration")

# Comparing chatbot interactions between long-term and newer customers
ggplot(energy_data, aes(x = factor(longterm_customer), y = nrchatbot)) +
  geom_boxplot() +
  labs(x = "Long-term Customer (1: Yes, 0: No)", y = "Chatbot Interactions", title = "Comparison of Chatbot Interactions")

# Comparing gas usage between long-term and newer customers
ggplot(energy_data, aes(x = factor(longterm_customer), y = av_gas)) +
  geom_boxplot() +
  labs(x = "Long-term Customer (1: Yes, 0: No)", y = "Average Gas Usage", title = "Comparison of Gas Usage")

# Comparing electricity usage between long-term and newer customers
ggplot(energy_data, aes(x = factor(longterm_customer), y = av_elec)) +
  geom_boxplot() +
  labs(x = "Long-term Customer (1: Yes, 0: No)", y = "Average Electricity Usage", title = "Comparison of Electricity Usage")

# Checking solar panel prevalence among long-term and newer customers
ggplot(energy_data, aes(x = factor(longterm_customer), fill = factor(solar_panels))) +
  geom_bar() +
  labs(x = "Long-term Customer (1: Yes, 0: No)", y = "Count", fill = "Solar Panels", title = "Solar Panel Prevalence")

# Plotting number of services vs. solar panel adoption
ggplot(energy_data, aes(x = nrservice, y = solar_panels)) +
  geom_point() +
  labs(x = "Number of Services", y = "Solar Panel Adoption", title = "Number of Services vs. Solar Panel Adoption")

# Plotting service duration vs. solar panel adoption
ggplot(energy_data, aes(x = service_duration, y = solar_panels)) +
  geom_point() +
  labs(x = "Service Duration", y = "Solar Panel Adoption", title = "Service Duration vs. Solar Panel Adoption")



# -------------- Data Analysis: nrchatbot & nrservice --------------------------------------------

# -------------- PART 1: Explore Variables --------------------------------

summary(energy_data$nrservice)
summary(energy_data$winsorized_nrservice)
summary(energy_data$nrchatbot)
summary(energy_data$winsorized_nrchatbot)



# ----------------------- PART2: Visualization DEP VAR ---------------------------------------
summary(energy_data$nrservice)
summary(energy_data$nrchatbot)


# Boxplot for nrservice
ggplot(energy_data, aes(y = nrservice)) + 
  geom_boxplot(fill = "blue") +
  ggtitle("Boxplot for nrservice")


# Boxplot for nrchatbot
ggplot(energy_data, aes(y = nrchatbot)) + 
  geom_boxplot(fill = "blue") +
  theme_minimal() +
  ggtitle("Boxplot for nrchatbot")


# plot histogram for nrservice
ggplot(energy_data, aes(nrservice)) +
  geom_histogram(binwidth=1, fill="blue") +
  ggtitle("Distribution of total calls with customer service")

# plot histogram for nrservice (winsorized)
ggplot(energy_data, aes(winsorized_nrservice)) +
  geom_histogram(binwidth=1, fill="blue") +
  ggtitle("Distribution of total calls with customer service")

# plot histogram for nrchatbot
ggplot(energy_data, aes(nrchatbot)) +
  geom_histogram(binwidth=1, fill="blue") +
  ggtitle("Distribution of total interactions with online chatbot")

# plot histogram for nrchatbot (winsorized)
ggplot(energy_data, aes(winsorized_nrchatbot)) +
  geom_histogram(binwidth=1, fill="blue") +
  ggtitle("Distribution of total interactions with online chatbot")


#----------------------- PART3: Model -------------------------------


# ------------------- NRSERVICE -------------------------------------

# [4.1]: Factors impact "total number of calls" with the customer service 

sum(energy_data$nrservice == 0) #0
sum(energy_data$nrservice == 1) #871
mean(energy_data$nrservice) # 4.392
var(energy_data$nrservice) # 83.68818
mean(energy_data$winsorized_nrservice) # 3.5885
var(energy_data$winsorized_nrservice) # 16.73053

colSums(is.na(energy_data))

# Method1: Poisson Regression (Count Data) --------------

#[AIC: 9726.4/ 11vars]
cs_poisson_service <-glm(winsorized_nrservice ~ diff_chatbot + 
                           days_since_first_service + days_since_first_chatbot + satisfaction +
                           years_since_customer + hh_size + 
                           years_since_solar_panels + nr_solar_panels+
                           email_pers_solar + email_newsletter +
                           av_bill,
                         data= energy_data,
                         family='poisson')

summary(cs_poisson_service)
exp(coef(cs_poisson_service))

vif(cs_poisson_service)


# Method2: Negative Binomial Regression (Mean not equal Variance)  --------------

# Dispersion Test
disp_test_service <-dispersiontest(cs_poisson_service, trafo =2)
disp_test_service
capture.output(disp_test_service, file = "disp_test_service.txt")


cs_negbin_service <-glm.nb(winsorized_nrservice ~ diff_chatbot + 
                             days_since_first_service + days_since_first_chatbot + satisfaction +
                             years_since_customer + hh_size + 
                             years_since_solar_panels + nr_solar_panels+
                             email_pers_solar + email_newsletter +
                             av_bill,
                           data= energy_data)

summary(cs_negbin_service)
exp(coef(cs_negbin_service))

#theta=1/alpha (alpha=1/theta)
alpha1 =1/cs_negbin_service$theta
alpha1


# Method3: Truncated Count Model (No Zero) --------------

#estimated truncated negbin model. 
control = vglm.control(maxit = 100)

cs_trunc_service <-vglm(winsorized_nrservice ~ diff_chatbot + 
                          days_since_first_service + days_since_first_chatbot + satisfaction +
                          years_since_customer + hh_size + 
                          years_since_solar_panels + nr_solar_panels+
                          email_pers_solar + email_newsletter + 
                          av_bill,
                        data= energy_data,
                        family= posnegbinomial(),
                        control = control)

summary(cs_trunc_service)
exp(coef(cs_trunc_service))



# ----- Compare Model (BIC & AIC) ----
BIC(cs_poisson_service)
BIC(cs_negbin_service)
BIC(cs_trunc_service)
AIC(cs_poisson_service)
AIC(cs_negbin_service)
AIC(cs_trunc_service)


model_comparison_service <- data.frame(
  Model = c("Poisson_service", "Negative Binomial_service", "Truncated_service"),
  BIC = c(BIC(cs_poisson_service), BIC(cs_negbin_service), BIC(cs_trunc_service)),
  AIC = c(AIC(cs_poisson_service), AIC(cs_negbin_service), AIC(cs_trunc_service))
)

print(model_comparison_service)

write.table(model_comparison_service, file = "model_comparison_service.txt", sep = "\t", row.names = FALSE)




# ------------------- NRCHATBOT -------------------------------------
# [4.2]: Factors impact "the number chatbot interactions" 

sum(energy_data$nrchatbot == 0) #0
sum(energy_data$nrchatbot == 1) #689
mean(energy_data$nrchatbot) # 10.505
var(energy_data$nrchatbot) # 1013.114
mean(energy_data$winsorized_nrchatbot) # 7.2105
var(energy_data$winsorized_nrchatbot) # 121.65



## Method1: Poisson Regression (Count Data)  --------------

cs_poisson_chatbot <-glm(winsorized_nrchatbot ~ diff_service + 
                           days_since_first_service + days_since_first_chatbot + satisfaction +
                           years_since_customer + hh_size + 
                           years_since_solar_panels + nr_solar_panels+
                           email_pers_solar + email_newsletter + 
                           av_bill + av_chatbot_length,
                         data=energy_data,
                         family='poisson')

summary(cs_poisson_chatbot)
exp(coef(cs_poisson_chatbot))



## Method2: Negative Binomial Regression (Mean not equal Variance)  --------------
disp_test_bot <-dispersiontest(cs_poisson_chatbot, trafo =2)
disp_test_bot
capture.output(disp_test_bot , file = "dispersion_test_bot.txt")



cs_negbin_chatbot <-glm.nb(winsorized_nrchatbot ~ diff_service + 
                             days_since_first_service + days_since_first_chatbot + satisfaction +
                             years_since_customer + hh_size + 
                             years_since_solar_panels + nr_solar_panels+
                             email_pers_solar + email_newsletter + 
                             av_bill,
                           data=energy_data)

summary(cs_negbin_chatbot)
exp(coef(cs_negbin_chatbot))

alpha =1/cs_negbin_chatbot$theta
alpha




# Method3: Truncated Count Model (No Zero)--------------
install.packages('glmmTMB')
library(glmmTMB)


cs_trunc_chatbot <- glmmTMB(winsorized_nrchatbot ~ diff_service + 
                              days_since_first_service + days_since_first_chatbot + satisfaction +
                              years_since_customer + hh_size + 
                              years_since_solar_panels + nr_solar_panels+
                              email_pers_solar + email_newsletter + 
                              av_bill,
                            data= energy_data,
                            family = truncated_nbinom2(link = "log"))

summary(cs_trunc_chatbot)

fixed_effects <- summary(cs_trunc_chatbot)$coeff$cond
exp_fixed_effects <- exp(fixed_effects[, "Estimate"])
exp_fixed_effects



#--------------

cs_trunc_chatbot2 <-vglm(winsorized_nrchatbot ~ diff_service + 
                           days_since_first_service + days_since_first_chatbot + satisfaction +
                           years_since_customer + hh_size + 
                           years_since_solar_panels + nr_solar_panels+
                           email_pers_solar + email_newsletter + 
                           av_bill,
                         data= energy_data,
                         family= posnegbinomial(),
                         control = control)

summary(cs_trunc_chatbot2)
exp(coef(cs_trunc_chatbot2))


# ----- Compare Model (BIC & AIC) ----
BIC(cs_poisson_chatbot)
BIC(cs_negbin_chatbot)
BIC(cs_trunc_chatbot)
AIC(cs_poisson_chatbot)
AIC(cs_negbin_chatbot)
AIC(cs_trunc_chatbot)


model_comparison_chatbot <- data.frame(
  Model = c("Poisson_chatbot", "Negative Binomial_chatbot", "Truncated_chatbot"),
  BIC = c(BIC(cs_poisson_chatbot), BIC(cs_negbin_chatbot), BIC(cs_trunc_chatbot)),
  AIC = c(AIC(cs_poisson_chatbot), AIC(cs_negbin_chatbot), AIC(cs_trunc_chatbot))
)

print(model_comparison_chatbot)

write.table(model_comparison_chatbot, file = "model_comparison_chatbot.txt", sep = "\t", row.names = FALSE)
