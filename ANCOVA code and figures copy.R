---
title: "Rapid evolution of a sexually selected trait"
output: github_document
---

## Step 1: Prepare data

```{r}
## Read in data
## Create male and female subsets
## Create a subset just for those individuals from India
## Create male and female subsets for individuals from India
mongoose = read.csv("all mongoose data.csv")
male = subset(mongoose, sex =="M")
female = subset(mongoose, sex =="F")
india = subset(mongoose, location =="India")
india.m = subset(india, sex =="M")
india.f = subset(india, sex =="F")
```

```{r}
## Creates a function to display count, mean, standard deviation, standard error, and 95% CI
summarySE = function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    length2 = function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )
   
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  

    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}
```

## Step 2: Investigate features from the native range of India
#### Sexual dimorphism in traits and their characteristics is often a hallmark of sexual selection
#### The anal pad is an important tool for scent marking, a behavior thought to be used by males to attract mates

```{r}
## Uses the newly created "summarySE" function to summarize data of the trait "ap.c"
## and puts the results into plot-able (long) format.
## The variable "ap.c" is the size of the anal pad, after correcting for body length.
## This correction is necessary to allow for males and females to be directly compared,
## as males are larger than females.
india_plot_data = summarySE(india, measurevar = "ap.c", groupvars = "sex")
print(india_plot_data)
```

#### Visualize the data

```{r}
## loads ggplot
library(ggplot2)

## This plots means and standard errors of both sexes on same plot
ggplot(india_plot_data, aes(x = sex, y = ap.c)) + 
	geom_errorbar(aes(ymin = ap.c-se, ymax = ap.c + se), width = 0.25) + 
	geom_point() +
	labs(x = "Sex", y = "Relative Anal Pad Size", title = "Sexual Size Dimorphism in India") + 
	theme_bw() + 
	theme(
		panel.border = element_blank(), panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(), axis.line.x = element_line(color = "black"), 
		axis.line.y = element_line(color = "black"), plot.title = element_text(hjust = 0.5))
## The plot clearly shows a sex difference, but we need to be sure

```

#### Apply statistics

```{r}
## The code first uses an F-test to test for homogeneity of variance between males and females,
## then, based on the results, performs the appropriate t-test to compare means.
if (var.test(india.m$ap.c, india.f$ap.c)$p.value < 0.05) {
  t.test(india.m$ap.c, india.f$ap.c, var = F)
} else 
  t.test(india.m$ap.c, india.m$ap.c, var = T)
## The t-test revealed that males do indeed posses larger anal pads.
```

#### More evidence: visualize sexual dimorphism in the relationship between the anal pad and body condition

```{r}
## Now to provide more evidence that this trait is under sexual selection,
## we want to determine if it is related to individual condition in males and not females,
## as this is a much stronger indicator of sexual selection than sexual dimorphism.
## Let's visualize the data by plotting anal pad size and condition for both sexes on same graph:
ggplot(india, aes(x = cond2, y = ap, color = sex)) +
	geom_point() +
	geom_smooth(method = lm, se = F) +
	labs(
		x = "Condition",  y = (bquote("Anal Pad ("*mm^2*")")), title = "India") + 
	theme_bw() + 
	theme(
		panel.border = element_blank(), panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(), axis.line.x = element_line(color = "black"), 
		axis.line.y = element_line(color = "black"), plot.title = element_text(hjust = 0.5))
## The warning message is because one individual's condition metric could not be calculated

## There definitely appears to be a difference in slopes, but we need to be sure.
```

#### Apply more statistics

```{r}
## Here I use an ANCOVA (analysis of covariance) to determine if the slopes
## of the lines differ by sex. Or put another way, if the relationship between
## anal pad size and body condition co-varies with sex.
summary(aov(ap~cond2*sex, data = india))
## And the interaction term "cond2:sex" is indeed significant, suggesting the trait
## is condition dependent in males but not females. 
```

#### Conclusion: Following sexual selection theory: pretty strong indirect evidence that the anal pad is under sexual selection in males but not females

# Step 3: Compare native range to introduced range

#### Prepare the data

```{r}
## Summarizes the global data for "ap.c" and puts them into plot-able (long) form
global_plot_data = summarySE(mongoose, measurevar = "ap.c", groupvars = c("sex", "location"))
print(global_plot_data)
```

#### Visualize the data

```{r}
## plots means and standard errors of both sexes on same plot
ggplot(global_plot_data, aes(x = location, y = ap.c, color = sex)) + 
	geom_errorbar(aes(ymin = ap.c-se, ymax = ap.c + se), width = 0.25) + 
	geom_point() + 
	scale_x_discrete(limits=c("India","Mauritius","St. Croix","Hawaii","Jamaica")) + 
	labs(
		x = "Location (ordered by time since introduction)", y = "Relative Anal Pad",
		title = "Global Sexual Size Dimorphism") + 
	theme_bw() + 
	theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
	      panel.grid.minor = element_blank(), axis.line.x = element_line(color = "black"), 
	      axis.line.y = element_line(color = "black"), plot.title = element_text(hjust = 0.5))
## Notice that male anal pads seem to gradually get smaller based on the 
## length of time since introduction.
## Female anal pads do not appear to have changed.
```

#### Apply statistics

```{r}
## Perform an ANOVA (anlysis of variance) to determine if males differ in the size of their
## anal pad across locations
summary(aov(ap.c~location, data = male))
```

```{r}
## The ANOVA's p-value shows that there is a significant difference between locations
## but we need a post hoc, pairwise comparisons between locations to know which specific 
## locations are different from each other
TukeyHSD(aov(ap.c~location, data = male))
## Notice: 1) all locations are different from India, 2) no introduced locations are different
## except the Mauritius-Jamaica comparison. This is because there is such a big difference between
## these locations in the time that mongooses have been present. 
## This gradual, uniform change suggests it is a product of adaptation (i.e., evolution)
```

```{r}
## analysis of variance between females
summary(aov(ap.c~location, data = female))
## Although approaching significance, there is no difference between anal pads of females 
## from different locations
```

#### Visualize the data: any change in condition-dependence?

```{r}
## Is there a difference in the relationship between anal pad size and body condition
## in males across locations?
ggplot(male, aes(x = cond2, y = ap, color = location, shape = location)) + 
	geom_point() + 
	scale_color_manual(name = "Location", labels = c("Hawaii", "India", "Jamaica", 
	                                                 "Mauritius", "St. Croix"), values = c(1,2,3,4,5)) + 
	scale_shape_manual(name = "Location", labels = c("Hawaii", "India", "Jamaica", 
	                                                 "Mauritius", "St. Croix"), values = c(1,2,3,4,5)) + 
	geom_smooth(method = lm, se = F) + 
	labs(
		x = "Condition" ,  y = (bquote("Anal Pad ("*mm^2*")")), title = "Males") + 
	theme_bw() + 
	theme(
		panel.border = element_blank(), panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(), axis.line.x = element_line(color = "black"), 
		axis.line.y = element_line(color = "black"), plot.title = element_text(hjust = 0.5))
## The warning message is because four individuals' condition metrics could not be calculated

## The Indian slope seems steeper, but let's make sure
```

```{r}
## Here I perform another ANCOVA to test for differences in slopes of the lines
summary(aov(ap~cond2*location, data = male))
## The "cond2:location" interaction term is significant, suggesting the slopes
## of the lines are different. Or in other words, the level of condition dependence
## of the anal pad has become weaker in introduced populations, suggesting a relaxation
## of sexual selection.
```

```{r}
## What about females? Does the relationship between anal pad size and body condition
## differ across locations?
ggplot(female, aes(x = cond2, y = ap, color = location, shape = location)) + 
	geom_point() + 
	scale_color_manual(name = "Location", labels = c("Hawaii", "India", "Jamaica", 
	                                                 "Mauritius", "St. Croix"), values = c(1,2,3,4,5)) + 
	scale_shape_manual(name = "Location", labels = c("Hawaii", "India", "Jamaica", 
	                                                 "Mauritius", "St. Croix"), values = c(1,2,3,4,5)) + 
	geom_smooth(method = lm, se = F) + 
	labs(
		x = "Condition",  y = (bquote("Anal Pad ("*mm^2*")")), title = "Females") + 
	theme_bw() + 
	theme(
		panel.border = element_blank(), panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(), axis.line.x = element_line(color = "black"), 
		axis.line.y = element_line(color = "black"), plot.title = element_text(hjust = 0.5))
## It doesn't look like it, but let's make sure
```

```{r}
## Another ANCOVA
summary(aov(ap~cond2*location, data = female))
## Results suggest no change in females regarding the relationship of anal pad size and body condition
```

#### Overall conclusion 1: Anal pads shrank in size and displayed a weakened relationship with body condition in males but not females of introduced mongooses. 
#### Overall conclusion 2: Because these changes occured gradually and in fewer than 145 years suggests rapid evolution, the first ever reported of a sexually selected trait in a mammal.

