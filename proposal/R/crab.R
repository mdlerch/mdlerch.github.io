set.seed(12)

crab <- read.csv("../data/crabs.csv")

summary(lm(FORCE ~ HEIGHT + SPECIES, crab))

crab$rulerHEIGHT <- rnorm(nrow(crab), crab$HEIGHT, 1)

summary(lm(FORCE ~ rulerHEIGHT + SPECIES, crab))

ntrain <- 30
train <- sample(1:nrow(crab), ntrain, replace = FALSE)
test <- setdiff(1:nrow(crab), train)

crab.train <- crab[train, ]
crab.test <- crab[test, ]

lmHigh <- lm(FORCE ~ HEIGHT + SPECIES, crab.train)
lmLow <- lm(FORCE ~ rulerHEIGHT + SPECIES, crab.train)

sum((crab.test$FORCE - predict(lmHigh, crab.test))^2)
sum((crab.test$FORCE - predict(lmLow, crab.test))^2)

summary(lmHigh)
summary(lmLow)


