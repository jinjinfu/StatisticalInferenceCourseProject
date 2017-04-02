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


# The condifence intervals (95%) are:
alpha <- 1 - 0.95
availabledosages <- c(0.5, 1, 2)
statnames <- c("OJ-mean","OJ-lower","OJ-upper","VC-mean","VC-lower","VC-upper")
dosagematrix <- matrix(
	nrow = 0,
	ncol = length(statnames)
)

for(rowname in c(0.5, 1, 2)){
	x <- ToothGrowth$len[ToothGrowth$supp=="OJ" & ToothGrowth$dose == rowname]
	y <- ToothGrowth$len[ToothGrowth$supp=="VC" & ToothGrowth$dose == rowname]
	dosagematrix <- rbind(
		dosagematrix,
		c(
			round(
				mean(x),
				2
			),
			round(
				mean(x) - qnorm(1-alpha/2) * sd(x)/sqrt(length(x)),
				2
			),
			round(
				mean(x) + qnorm(1-alpha/2) * sd(x)/sqrt(length(x)),
				2
			),
			round(mean(y),2),
			round(mean(y) - qnorm(1-alpha/2) * sd(y)/sqrt(length(y)),2),
			round(mean(y) + qnorm(1-alpha/2) * sd(y)/sqrt(length(y)),2)
		)
	)
}

rownames(dosagematrix) <- availabledosages
colnames(dosagematrix) <- statnames

x <- ToothGrowth$len[ToothGrowth$supp=="OJ" & ToothGrowth$dose == 0.5]
y <- ToothGrowth$len[ToothGrowth$supp=="VC" & ToothGrowth$dose == 0.5]
d05 <- c(
	round(mean(x),2),
	(
		round(
			mean(x) + c(-1,1) * qnorm(1-alpha/2) * sd(x)/sqrt(length(x)),2)
		),
		round(mean(y),2),
	(
		round(mean(y) + c(-1,1) * qnorm(1-alpha/2) * sd(y)/sqrt(length(y)),2)
	)
)

x <- ToothGrowth$len[ToothGrowth$supp=="OJ" & ToothGrowth$dose == 1]
y <- ToothGrowth$len[ToothGrowth$supp=="VC" & ToothGrowth	$dose == 1]
d10 <- c(round(mean(x),2),
  (round(mean(x) + c(-1,1) * qnorm(0.975) * sd(x)/sqrt(length(x)),2)),
  round(mean(y),2),
  (round(mean(y) + c(-1,1) * qnorm(0.975) * sd(y)/sqrt(length(y)),2)))

x <- ToothGrowth$len[ToothGrowth$supp=="OJ" & ToothGrowth$dose == 2]
y <- ToothGrowth$len[ToothGrowth$supp=="VC" & ToothGrowth$dose == 2]
d20 <- c(round(mean(x),2),
  (round(mean(x) + c(-1,1) * qnorm(0.975) * sd(x)/sqrt(length(x)),2)),
  round(mean(y),2),
  (round(mean(y) + c(-1,1) * qnorm(0.975) * sd(y)/sqrt(length(y)),2)))


