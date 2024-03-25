# this was quick, subject to change

library(ggplot2)

#eec <- 0.006
eec <- seq(from=0.00001, to=0.01, by=0.00001)
ld50 <- 0.006
slope <- 4.5
# looks like this gives kernel density and not cumulative
# density and needs to be converted
z<-(slope)*(log(eec)-log(ld50))
phat<-(1/(2*pi)^0.5)*exp(-(z^2)/2)
phat

min(phat)
max(phat)


ggplot(data = data.frame(x = eec, y = phat), aes(x = x, y = y)) +
  geom_line()

# combine concentration and added mortality
df <- data.frame(x = eec, y = phat)

# ld50 point
ld50_point <- data.frame(x = 0.006, y = 0.5)

# plot the cdf using ggplot2
ggplot(df, aes(x)) + 
  stat_ecdf(geom = "step") +
  scale_x_log10() +
  geom_point(data = ld50_point, aes(x = x, y = y), color = "red", size = 3)



# we still need to extract the cdf values from the cumulative
# curve to convert the doses to mortality
# the above equation only give the pdf as added mortality
