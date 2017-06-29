---
title: "Rapid evolution of a sexually selected trait"
output: md_document
---

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

# Investigate features from the native range of India
### The anal pad is an important tool for scent marking, a behavior thought to be used by males to attract mates

```{r}
## uses function to summarize descriptive statistics of the trait "ap.c"
## and puts the results into plot-able (long) format.
## "ap.c" is the size of the anal pad, after controlling for body length
india_plot_data = summarySE(india, measurevar = "ap.c", groupvars = "sex")
print(india_plot_data)
```

### Visualize the data

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

```

### Apply statistics

```{r}
## The plot clearly shows a sex difference, but we need to be sure.
## Sexual dimorphism is often a hallmark of a sexually selected trait.
## The code first uses an F-test to test for homogeneity of variance between males and females,
## then, based on the results, performs the appropriate t-test to compare means.
if (var.test(india.m$ap.c, india.f$ap.c)$p.value < 0.05) {
  t.test(india.m$ap.c, india.f$ap.c, var = F)
} else 
  t.test(india.m$ap.c, india.m$ap.c, var = T)
```

```{r}
## The t-test revealed that males do indeed posess larger anal pads.
## Now to provide more evidence that this trait is under sexual selection,
## we want to determine if it is related to individual condition in males and not females.
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
```

```{r}
## There does appear to be a difference in slopes, but to be sure, we need to perform
## an ANCOVA (analysis of covariance) to determine if the slopes differ by sex
summary(aov(ap~cond2*sex, data = india))
## And the interaction term "cond2:sex" is indeed significant, suggesting the trait
## is condition dependent in males but not females.
```

# Compare native range to introduced range
## To determine if any changes have occured since introduction

```{r}
## summarizes the global data for "ap.c" and puts it into plot-able (long) form
global_plot_data = summarySE(mongoose, measurevar = "ap.c", groupvars = c("sex", "location"))
```

```{r}
## plots means and st.err of both sexes on same plot
ggplot(global_plot_data, aes(x = location, y = ap.c, color = sex)) + 
	geom_errorbar(aes(ymin = ap.c-se, ymax = ap.c + se), width = 0.25) + 
	geom_point() + 
	scale_x_discrete(limits=c("India","Mauritius","St. Croix","Hawaii","Jamaica")) + 
	labs(
		x = "Location", y = "Relative Anal Pad", title = "Global Comparison") + 
	theme_bw() + 
	theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
	      panel.grid.minor = element_blank(), axis.line.x = element_line(color = "black"), 
	      axis.line.y = element_line(color = "black"), plot.title = element_text(hjust = 0.5))

```

```{r}
## analysis of variance between males
summary(aov(ap.c~location, data = male))
```

```{r}
## post hoc, pairwise comparisons between locations for males
TukeyHSD(aov(ap.c~location, data = male))
```

```{r}
## analysis of variance between females
summary(aov(ap.c~location, data = female))
```

```{r}
## plots relationship of anal pad and condition for each location for males
ggplot(male, aes(x = cond2, y = ap, color = location, shape = location)) + 
	geom_point() + 
	scale_color_manual(name = "Location", labels = c("Hawaii", "India", "Jamaica", "Mauritius", "St. Croix"), values = c(1,2,3,4,5)) + 
	scale_shape_manual(name = "Location", labels = c("Hawaii", "India", "Jamaica", "Mauritius", "St. Croix"), values = c(1,2,3,4,5)) + 
	geom_smooth(method = lm, se = F) + 
	labs(
		x = "Condition" ,  y = (bquote("Anal Pad ("*mm^2*")")), title = "Males") + 
	theme_bw() + 
	theme(
		panel.border = element_blank(), panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(), axis.line.x = element_line(color = "black"), 
		axis.line.y = element_line(color = "black"), plot.title = element_text(hjust = 0.5))
```

```{r}
## ancova: do the different populations of males have significantly different relationships between anal pad and body condition?
summary(aov(ap.c~cond2*location, data = male))
```

```{r}
## plots relationship of anal pad and condition for each location for females
ggplot(female, aes(x = cond2, y = ap, color = location, shape = location)) + 
	geom_point() + 
	scale_color_manual(name = "Location", labels = c("Hawaii", "India", "Jamaica", "Mauritius", "St. Croix"), values = c(1,2,3,4,5)) + 
	scale_shape_manual(name = "Location", labels = c("Hawaii", "India", "Jamaica", "Mauritius", "St. Croix"), values = c(1,2,3,4,5)) + 
	geom_smooth(method = lm, se = F) + 
	labs(
		x = "Condition",  y = (bquote("Anal Pad ("*mm^2*")")), title = "Females") + 
	theme_bw() + 
	theme(
		panel.border = element_blank(), panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(), axis.line.x = element_line(color = "black"), 
		axis.line.y = element_line(color = "black"), plot.title = element_text(hjust = 0.5))

```

```{r}
## ancova: do the different populations of females have significantly different relationships between anal pad and body condition?
summary(aov(ap.c~cond2*location, data = female))
```

