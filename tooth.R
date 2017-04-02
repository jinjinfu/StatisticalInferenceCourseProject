#tooth.R

## Load ToothGrowth data and perform exploratory data analyses
library(datasets)
data(ToothGrowth)
str(ToothGrowth)
head(ToothGrowth)
summary(ToothGrowth)

library(ggplot2)
t = ToothGrowth
levels(t$supp) <- c("Orange Juice", "Ascorbic Acid")
ggplot(t, aes(x=factor(dose), y=len)) + 
  facet_grid(.~supp) +
  geom_boxplot(aes(fill = supp), show.legend = FALSE) +
  labs(title="Guinea pig tooth length by dosage for each type of supplement", 
    x="Dose (mg/day)",
    y="Tooth Length")

