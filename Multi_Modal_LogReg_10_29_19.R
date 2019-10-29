library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)
library(stringr)
library(data.table)
library(plyr);
library(dplyr)
library(randomForest)
library(memisc)
library(plotly)
library(grid)





data <- read.csv("H:/VRR_Analysis_Dataset_4_22_19.csv",header =TRUE)

data_full <- filter(data, data$VehicleDeleted != -100)

train.index1 <- createDataPartition(data_full$VehicleDeleted, p = .7, list = FALSE) 
data<- data_full[ train.index1,]
hold_out_data  <- data_full[-train.index1,]    # add some categorical columns to data_numeric to see how it improves the model





## function that replicates default ggplot2 colors
## taken from [1]
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}


df <- data %>% select(PrincipalDriverAge,VehicleDeleted)
df$x <- df$PrincipalDriverAge
df$sample <- df$VehicleDeleted



## Calculate density estimates
g1 <- ggplot(df, aes(x=x, group=sample, colour=sample)) +
  geom_density(data = df) + xlim(0, 100)
gg1 <- ggplot_build(g1)

## Use these estimates (available at the same x coordinates!) for
## calculating the differences.
## Inspired by [2]
x <- gg1$data[[1]]$x[gg1$data[[1]]$group == 1]
y1 <- gg1$data[[1]]$y[gg1$data[[1]]$group == 1]
y2 <- gg1$data[[1]]$y[gg1$data[[1]]$group == 2]
df2 <- data.frame(x = x, ymin = pmin(y1, y2), ymax = pmax(y1, y2), 
                  side=(y1<y2), ydiff = y2-y1)
g2 <- ggplot(df2) +
  geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax, fill = side, alpha = 0.5)) +
  geom_density(data = df, size = 1, aes(x = x, group = sample, colour = sample)) +
  xlim(0, 100) +
  guides(alpha = FALSE, fill = FALSE)
g3 <- ggplot(df2) +
  geom_line(aes(x = x, y = ydiff, colour = side)) +
  geom_area(aes(x = x, y = ydiff, fill = side, alpha = 0.4)) +
  guides(alpha = FALSE, fill = FALSE)
## See [3]
grid.draw(rbind(ggplotGrob(g2), ggplotGrob(g3), size="last"))



df2$Driver_Age <- as.integer(df2$x)

df3 <- aggregate(df2[, 5], list(df2$Driver_Age), mean)
df3$PrincipalDriverAge <- df3$Group.1
df3$Group.1 <- NULL

df$x<- NULL
df$sample <- NULL

test <- merge(df,df3)

test <- filter(test, test$PrincipalDriverAge!= 0)

test$VehicleDeleted <- test$VehicleDeleted/100


# Logistics Regression
glm.fit <- glm(VehicleDeleted ~ x + PrincipalDriverAge, data = test, family = binomial)

summary(glm.fit)


test$VehicleDeleted_factor <- as.factor(as.character(test$VehicleDeleted))
# Density plots with semi-transparent fill
ggplot(test, aes(x=x, fill=VehicleDeleted_factor)) + geom_density(alpha=.3)




hold_out_data <- hold_out_data %>% select(PrincipalDriverAge,VehicleDeleted)

colnames(hold_out_data)<- c("PrincipalDriverAge","VehicleDeleted")

hold_out_data <- merge(hold_out_data,df3)


# hold out data graph
hold_out_data$VehicleDeleted_factor <- as.factor(as.character(hold_out_data$VehicleDeleted))
ggplot(hold_out_data, aes(x=x, fill=VehicleDeleted_factor)) + geom_density(alpha=.3)

# model summary on the hold out data
glm.fit <- glm(VehicleDeleted ~ x + PrincipalDriverAge, data = hold_out_data, family = binomial)

summary(glm.fit)








hold_out_data$prediction_overall <- hold_out_data$x


nintyfifth_percentile <- mean(hold_out_data$VehicleDeleted[hold_out_data$prediction_overall> quantile(hold_out_data$prediction_overall, .95)])/mean(hold_out_data$VehicleDeleted)

nintypercentile <-mean(hold_out_data$VehicleDeleted[hold_out_data$prediction_overall> quantile(hold_out_data$prediction_overall, .90)])/mean(hold_out_data$VehicleDeleted)

eightypercentile <-mean(hold_out_data$VehicleDeleted[hold_out_data$prediction_overall> quantile(hold_out_data$prediction_overall, .80)])/mean(hold_out_data$VehicleDeleted)

seventypercentile <-mean(hold_out_data$VehicleDeleted[hold_out_data$prediction_overall> quantile(hold_out_data$prediction_overall, .70)])/mean(hold_out_data$VehicleDeleted)

sixtypercentile <-mean(hold_out_data$VehicleDeleted[hold_out_data$prediction_overall> quantile(hold_out_data$prediction_overall, .60)])/mean(hold_out_data$VehicleDeleted)

fiftypercentile <-mean(hold_out_data$VehicleDeleted[hold_out_data$prediction_overall> quantile(hold_out_data$prediction_overall, .50)])/mean(hold_out_data$VehicleDeleted)


fiftypercentile <-mean(hold_out_data$VehicleDeleted[hold_out_data$prediction_overall> quantile(hold_out_data$prediction_overall, .50)])/mean(hold_out_data$VehicleDeleted)

belowfiftypercentile <-mean(hold_out_data$VehicleDeleted[hold_out_data$prediction_overall< quantile(hold_out_data$prediction_overall, .50)])/mean(hold_out_data$VehicleDeleted)


vehicleDeletesFound95 <- .05* nintyfifth_percentile
vehicleDeletesFound90<- .1 * nintypercentile
vehicleDeletesFound80 <- .2*eightypercentile
vehicleDeletesFound70 <- .3* seventypercentile
vehicleDeletesFound60 <- .4*sixtypercentile
vehicleDeletesFound50<- .5*fiftypercentile
vehicleDeletesFoundbelow50<- .5*belowfiftypercentile



dt <- data.table(x = c(".95", ".90",".8",".7",".6",".5","below .5"), y = c(nintyfifth_percentile,nintypercentile,eightypercentile,seventypercentile,sixtypercentile,fiftypercentile,belowfiftypercentile), 
                 z =c(vehicleDeletesFound95,vehicleDeletesFound90,vehicleDeletesFound80,vehicleDeletesFound70,vehicleDeletesFound60,
                      vehicleDeletesFound50,vehicleDeletesFoundbelow50))

dt$Predictive_Percentiles <- dt$x
dt$EfficiencyMultiplier <- dt$y
dt$VehicleDeletesFound <- dt$z
dt$x <- NULL
dt$y <- NULL
dt$z <- NULL

print(dt)


