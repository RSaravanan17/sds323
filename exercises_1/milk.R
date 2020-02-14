library(mosaic)
library(tidyverse)

# read in data
milk = read.csv('./data/milk.csv')

# scatterplot of sales vs price
plot(sales ~ price, data = milk)

# scatterplot of log(sales) vs log(price)
plot(log(sales) ~ log(price), data = milk)

# linear model for line of best fit for log scatterplot
lm_ped = lm(log(sales) ~ log(price), data = milk)
coef(lm_ped)

# P = price, Q = quantity sold, N = profit, c = cost to buy from wholesaler
# N = (P - c) x Q
# P and Q are inversely related
# Therefore, N = (P - c) x f(P) since Q = f(P)
# Q = a x P^B
# log(Q) = log(a) + B x log(P)

# Intercept = 4.720604, log(price) = -1.618578
# log(Q) = 4.72 - 1.62 x log(P)
# => Q = e^{4.72} x P^{-1.62}

# N = (P - c) x e^{4.72} x P^{-1.62}
# => N = (P - c) x 112.24 x P^{-1.62}

curve((x - 1) * 112.24 * x^(-1.62), from = 1, to = 9)
curve((x - 1) * 112.24 * x^(-1.62), from = 2, to = 3)
curve((x - 1) * 112.24 * x^(-1.62), from = 2.50, to = 2.70)

# peak of the curve is at $2.61
# by rule, f(x) and log(f(x)) are maximized at the same point because they are one-to-one
# therefore, your profit is optimized when selling the milk for $2.61