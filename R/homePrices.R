# Home Prices In Mid-West

homes <- read.csv("data/homePriceData.csv")
View(homes)
names(homes)
summary(homes$price)
summary(homes$age)

states <- levels(homes$state)
avePrice <- round(mean(homes$price),2)
aveAge <- round(mean(homes$age), 0)

square <- function(x) {
  return(x^2)
}

square(aveAge)