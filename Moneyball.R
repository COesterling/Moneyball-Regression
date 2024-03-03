download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("mlb11.RData")
head(mlb11)

plot(mlb11$runs, mlb11$at_bats)

cor(mlb11$runs, mlb11$at_bats)

plot_ss(x = mlb11$at_bats, y = mlb11$runs)
plot_ss(x = mlb11$at_bats, y = mlb11$runs, showSquares = TRUE)

m1 <- lm(runs ~ at_bats, data = mlb11)
summary(m1)
m2 <- lm(runs ~ homeruns, data = mlb11)
summary(m2)

plot(mlb11$runs ~ mlb11$at_bats)
abline(m1)

plot(m1$residuals ~ mlb11$at_bats)
abline(h = 0, lty = 3)  # adds a horizontal dashed line at y = 0

hist(m1$residuals)
qqnorm(m1$residuals)
qqline(m1$residuals)  # adds diagonal line to the normal prob plot

head(mlb11)
m3 <- lm(runs ~ stolen_bases, data = mlb11)
plot(mlb11$runs ~ mlb11$stolen_bases)
abline(m3)

m4 <- lm(runs ~ stolen_bases + hits + at_bats + homeruns + strikeouts, data = mlb11)
summary(m4)
sigma(m4)

m5 <- lm(runs ~ new_onbase + new_slug + new_obs, data = mlb11)
summary(m5)

m6 <- lm(runs~ homeruns, data = mlb11)
summary(m6)
plot(m6)

