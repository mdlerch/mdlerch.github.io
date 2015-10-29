set.seed(12)
cane <- read.csv("../data/cane.csv")

summary(lm(Tonn.Hect ~ ., cane))

cane$response <- log(cane$Tonn.Hect)
cane$water <- with(cane, Jul.96 + Aug.96 + Sep.96 + Oct.96 + Nov.96 + Dec.96 +
                         Jan.97 + Feb.97 + Mar.97 + Apr.97 + May.97 + Jun.97 +
                         Jul.97 + Aug.97 + Sep.97 + Oct.97 + Nov.97 + Dec.97)
summary( lm(response ~ water + District + as.factor(HarvestMonth) +
            as.factor(Variety) + HarvestDuration, cane))

cane

"District"         "DistrictGroup"    "DistrictPosition" "SoilID"           "SoilName"
"Area"             "Variety"          "Ratoon"           "Age"              "HarvestMonth"
"HarvestDuration"  "Tonn.Hect"        "Fibre"            "Sugar"            "Jul.96"
"Aug.96"           "Sep.96"           "Oct.96"           "Nov.96"           "Dec.96"
"Jan.97"           "Feb.97"           "Mar.97"           "Apr.97"           "May.97"
"Jun.97"           "Jul.97"           "Aug.97"           "Sep.97"           "Oct.97"
"Nov.97"           "Dec.97"           "response"         "water"


ruler <- round(rnorm(length(unique(cane$Aug.96)), unique(cane$Aug.96), 5))
cane$rulerAug.96 <- apply(cbind(cane$Aug.96), 1, function(x) ruler[which(unique(cane$Aug.96) == x)])
ruler <- round(rnorm(length(unique(cane$Sep.96)), unique(cane$Sep.96), 5))
cane$rulerSep.96 <- apply(cbind(cane$Sep.96), 1, function(x) ruler[which(unique(cane$Sep.96) == x)])
ruler <- round(rnorm(length(unique(cane$Oct.96)), unique(cane$Oct.96), 5))
cane$rulerOct.96 <- apply(cbind(cane$Oct.96), 1, function(x) ruler[which(unique(cane$Oct.96) == x)])
ruler <- round(rnorm(length(unique(cane$Nov.96)), unique(cane$Nov.96), 5))
cane$rulerNov.96 <- apply(cbind(cane$Nov.96), 1, function(x) ruler[which(unique(cane$Nov.96) == x)])

ntrain <- 3000

train <- sample(1:nrow(cane), ntrain, replace = FALSE)
test <- setdiff(1:nrow(cane), train)

cane.train <- cane[train, ]
cane.test <- cane[test, ]

lmHigh <- lm(lTonn.Hect ~ Aug.96 + Sep.96 + Oct.96 + Nov.96 + as.factor(HarvestMonth), cane.train)
lmLow <- lm(lTonn.Hect ~ rulerAug.96 + rulerSep.96 + rulerOct.96 + rulerNov.96 + as.factor(HarvestMonth), cane.train)

sum((cane.test$lTonn.Hect - predict(lmHigh, cane.test))^2) / nrow(cane.test)
sum((cane.test$lTonn.Hect - predict(lmLow, cane.test))^2) / nrow(cane.test)

set.seed(12)

cane <- read.csv("../data/cane.csv")
cane$lTonn.Hect <- log(cane$Tonn.Hect)

ruler <- round(rnorm(length(unique(cane$Aug.96)), unique(cane$Aug.96), 5) / 10 ) * 10
cane$rulerAug.96 <- apply(cbind(cane$Aug.96), 1, function(x) ruler[which(unique(cane$Aug.96) == x)])
ruler <- round(rnorm(length(unique(cane$Sep.96)), unique(cane$Sep.96), 5) / 10 ) * 10
cane$rulerSep.96 <- apply(cbind(cane$Sep.96), 1, function(x) ruler[which(unique(cane$Sep.96) == x)])
ruler <- round(rnorm(length(unique(cane$Oct.96)), unique(cane$Oct.96), 5) / 10 ) * 10
cane$rulerOct.96 <- apply(cbind(cane$Oct.96), 1, function(x) ruler[which(unique(cane$Oct.96) == x)])
ruler <- round(rnorm(length(unique(cane$Nov.96)), unique(cane$Nov.96), 5) / 10 ) * 10
cane$rulerNov.96 <- apply(cbind(cane$Nov.96), 1, function(x) ruler[which(unique(cane$Nov.96) == x)])


ntrain <- 3000

train <- sample(1:nrow(cane), ntrain, replace = FALSE)
test <- setdiff(1:nrow(cane), train)

cane.train <- cane[train, ]
cane.test <- cane[test, ]

lmHigh <- lm(lTonn.Hect ~ Aug.96 + Sep.96 + Oct.96 + Nov.96 + as.factor(HarvestMonth), cane.train)
lmLow <- lm(lTonn.Hect ~ rulerAug.96 + rulerSep.96 + rulerOct.96 + rulerNov.96 + as.factor(HarvestMonth), cane.train)

sum((cane.test$lTonn.Hect - predict(lmHigh, cane.test))^2) / nrow(cane.test)
sum((cane.test$lTonn.Hect - predict(lmLow, cane.test))^2) / nrow(cane.test)



