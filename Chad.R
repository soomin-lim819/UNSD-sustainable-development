getwd()
setwd("/Users/soom/Desktop")

library(shiny)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(stats)
library(RColorBrewer)

getwd()
diff <- read_csv("diff.csv")
view(diff)

diff_chad<-read_csv("diff_chad.csv")

# We are adding up the time dimension
diff_chad$time = ifelse(diff$year >= 2015, 1, 0)

# If no effect = 0, if there is effect = 1
diff_chad$treated = ifelse(diff$country == "Nigeria", 1, 0)

# Combining the above effects together in order to draw Did
diff_chad$did = diff_chad$time * diff_chad$treated

#Regression analysis with DiD
didreg1= lm(greenfinance ~ treated + time + did, data = diff_chad)

summary(didreg1)

#Regression analysis about time dimension (our DiD analysis in didreg1)
didreg2 = lm(greenfinance ~ treated*time, data = diff_chad)
summary(didreg2)


#Exporting regression analysis result into Excel
library(broom)
tidy_results <- tidy(didreg2)
write.csv(tidy_results, "regression_results_chad.csv", row.names=FALSE)

library(ggplot2)


#Below is the Green Finance result from original data
nigeriatable <- diff_chad %>%
  select(country, year, greenfinance)

summary(nigeriatable)
head(nigeriatable)

view(nigeriatable)

# Plotting the green finance trend (2010 to 2025)
nigeriaplot <- ggplot(nigeriatable, aes(x = as.factor(year), y = greenfinance, color = country, group = country)) +
  geom_point(alpha=0.3)+
  geom_line() +
  labs(
    title = "How Did Green Finance Evolve after SDG 7 Establishment",
    subtitle = "Deploying Diff-in-Diffs before & after 2015",
    x = "Year",
    y = "Green Finance (Mil$)",
    color = "Country") +
  theme_bw() +
  scale_color_brewer(palette = "Dark2")

print(nigeriaplot)

