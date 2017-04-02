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

# Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from class, even if there’s other approaches worth considering)

library(ggplot2)

ggplot(
	ToothGrowth,
	aes(
		x=factor(dose),
		y=len,
		fill=factor(dose)
	)
) + 
    geom_boxplot(notch=F) +
    facet_grid(.~supp) +
    scale_x_discrete("Dosage (mg)") +   
    scale_y_continuous("Tooth Length") +  
    scale_fill_discrete(name="Dose (mg)") + 
    ggtitle("Effect of Supplement Type and Dosage on Tooth Growth")


#library(ggplot2)
#t = ToothGrowth
#levels(t$supp) <- c("Orange Juice", "Ascorbic Acid")
#ggplot(t, aes(x=factor(dose), y=len)) + 
#  facet_grid(.~supp) +
#  geom_boxplot(aes(fill = supp), show.legend = FALSE) +
#  labs(title="Guinea pig tooth length by dosage for each type of supplement", 
#    x="Dose (mg/day)",
#    y="Tooth Length")

