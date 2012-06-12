# c = option premium
# s = current stock price
# x = strike price
# t.exp = expiration time
# t = current time
# r = risk free rate
# sigma = volatility


# callprice.bs
# Calc price of call option

d.pos <- log(s/x) + (r + 0.5 * sigma^2) * (t.exp - t)
d.pos <- d.pos/(sigma * (t.exp - t)^0.5)
d.neg <- d.pos - sigma * (t.exp - t)^0.5
s * pnorm(d.pos) - x * exp( - r * (t.exp - t)) * pnorm(d.neg)


# putprice.bs
# Calc price of put option

c <- callprice.bs(s, x, r, sigma, t.exp, t)
c - s + x * exp( - r * (t.exp - t))


vol.implied <- function(c, s, x, t.exp, t, r, sigma.init = 0.05) {
  dif <- 1
  sigma0 <- sigma.init
  while(dif > 0.001) {
    c0 <- callprice.bs(s, x, r, sigma0, t.exp, t)
   print(c0)
    v0 <- vega(s, x, t.exp, t, r, sigma0)
   print(v0)
    sigma1 <- sigma0 - (c0 - c)/v0
    print(sigma1)
    dif <- abs(sigma1 - sigma0)
    sigma0 <- sigma1
  }
}

vega <- function(s, x, t.exp, t, r, sigma) {
  d.pos <- log(s/x) + (r + 0.5 * sigma^2) * (t.exp - t)
  d.pos <- d.pos/(sigma * (t.exp - t)^0.5)
  s * dnorm(d.pos) * (t.exp - t)^0.5
}


# Example 1

# premium = 2.15
# curr price = 36.12
# strike = 35
# exp = 6 months
# curr time = week 19
# risk free rate = 0.07
# sigma.init = 0.05)

vol.implied(2.15, 36.12, 35, (26/52), ((26-7)/52), 0.07)

