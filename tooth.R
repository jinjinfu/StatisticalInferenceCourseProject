#tooth.R

## Load ToothGrowth data and perform exploratory data analyses
library(datasets)
data(ToothGrowth)
dim(ToothGrowth)

head(ToothGrowth)

## Provide a basic summary of the data.
summary(ToothGrowth)

# get statistical information about lengths
c(
	round(
		mean(ToothGrowth$len),
		3
	),
	round(
		sd(ToothGrowth$len),
		3
	),
	round(
		var(ToothGrowth$len),
		3
	)
)

# Turn dosages into factors
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
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

