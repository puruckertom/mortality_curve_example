# this was quick, subject to change

library(ggplot2)
library(braidrm)


#eec <- 0.006
eec <- seq(from=0.0006, to=0.06, by=0.0001)
dnorm(eec, mean=0.06, sd=4.5)


ld50 <- 0.006
slope <- 4.5
# looks like this gives kernel density and not cumulative
# density and needs to be converted
z<-(slope)*(log(eec)-log(ld50))
phat<-(1/(2*pi)^0.5)*exp(-(z^2)/2)
phat

beepop <- 1/(1+(eec/ld50)^-slope)
beepop

min(phat)
max(phat)
# n, Eo, Ef, lnIDm
evalHillEqn(eec, parv=c(2,0,100,log(10^-6)))

ggplot(data = data.frame(x = eec, y = phat), aes(x = x, y = y)) +
  geom_line()

# combine concentration and added mortality
df <- data.frame(x = eec, y = phat)
df_beepop <- data.frame(x = eec, y = beepop)

# ld50 point
ld50_point <- data.frame(x = 0.006, y = 0.5)

# plot the cdf using ggplot2
ggplot(df, aes(x)) + 
  stat_ecdf(geom = "step") +
  scale_x_log10() +
  geom_point(data = ld50_point, aes(x = x, y = y), color = "red", size = 3)

# plot the cdf using ggplot2
ggplot(df_beepop, aes(x)) + 
  stat_ecdf(geom = "step") +
  scale_x_log10() +
  geom_point(data = ld50_point, aes(x = x, y = y), color = "red", size = 3)


# we still need to extract the cdf values from the cumulative
# curve to convert the doses to mortality
# the above equation only give the pdf as added mortality

library(MASS)

# Create sample data
y <- c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 6, 7, 8)
x <- c(7, 7, 8, 3, 2, 4, 4, 6, 6, 7, 5, 3, 3, 5, 8)

# Fit a linear regression model
model <- lm(y ~ x)

# Find optimal lambda for Box-Cox transformation
bc <- boxcox(y ~ x)
bc
optimal_lambda <- bc$x[which.max(bc$y)]

# Fit new linear regression model using the Box-Cox transformation
new_model <- lm(((y^optimal_lambda - 1) / optimal_lambda) ~ x)

x <- c(0.2, 0.528, 0.11, 0.260, 0.091, 1.314, 1.52, 0.244, 1.981, 0.273, 0.461, 0.366, 1.407, 0.79, 2.266)

# Use the boxcox() function to find the optimal lambda
bc <- boxcox(x)
bc

# The optimal lambda is the value that maximizes the log-likelihood
optimal_lambda <- bc$x[which.max(bc$y)]
print(optimal_lambda)


if(Sys.info()[4]=="LZ26TPURUCKE-2"){
  curve_root <- file.path("c:", "Users", "tpurucke", "git", "mortality_curve_example")
}

print(paste("Root directory location: ", curve_root, sep=""))

curve_data_in <- file.path(curve_root, "data_in")

daily_probs <- read.csv(file.path(curve_data_in,"dailprob_example.csv"), stringsAsFactors = TRUE)

dim(daily_probs)
colnames(daily_probs)
length(daily_probs$X)
length(unique(daily_probs$X))
daily_probs$index <- paste0("foundress_", daily_probs$index)
daily_probs$Day <- paste0("day_", daily_probs$Day)
hist(log(daily_probs$Survival))
daily_probs2 <- daily_probs[,-1]
View(daily_probs2)
colnames(daily_probs2)
unique(daily_probs2$Day)
unique(daily_probs2$index)
unique(daily_probs2$Survival)
length(daily_probs2$Survival)
250*365

library(tidyr)

# Convert to wide format
daily_probs_wide <- pivot_wider(daily_probs2, names_from = index, values_from = Survival)
dim(daily_probs_wide)
View(daily_probs_wide)
