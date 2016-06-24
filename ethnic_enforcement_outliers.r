
## Prep the workspace
library("AzureML")
ws <- workspace()

## load the data: 2014 arrests by county and agency; county pop by race and gender
dat_arrests <- download.datasets(ws, "ca_doj_arrests_deidentified_2014_05-07-2016.csv")
dat_pop <- download.datasets(ws, "ca_county_population_by_race_gender_age_2005-2014_02-05-2016.csv")

## Preview the arrests data
dim(dat_arrests)
names(dat_arrests)
head(dat_arrests)

## Load necessary libraries
library(dplyr)
library(ggplot2)
library(grid)
library(stats)

## Subset arrests to only juveniles.
dat_juv <- dat_arrests[dat_arrests$age_group %in% "juvenile",]

## Group by county, then by year, then by race/ethnicity and give me the counts.
cty_ethnic <- summarise(group_by(dat_juv, county, arrest_year, race_or_ethnicity), total = n())

## Now remove those records supressed due to privacy concern.
cty_ethnic <- cty_ethnic[!(cty_ethnic$race_or_ethnicity %in% "suppressed_due_to_privacy_concern"),]

#### !! Some counties are reporting only "NA"s in their arrest totals per ethnic group. :-\
## Let's remove those from our analysis...for now.
cty_ethnic <- cty_ethnic[!is.na(cty_ethnic$total),]

## Preview
dim(cty_ethnic)
head(cty_ethnic)
tail(cty_ethnic)

## Panel bar charts: ethnic breakdown of arrests, by county.
## NOTE: this is before normalizing by ethnic population.
plot_ethnic <- ggplot(cty_ethnic, aes(x = race_or_ethnicity, y = total, fill = race_or_ethnicity)) + 
                geom_bar(stat = "identity") + coord_flip() + facet_wrap(~county) +  
                theme(axis.text.x=element_text(angle=-90,hjust=1,vjust=0.5, size = 8), axis.text.x=element_text(size = 8),
                      legend.position = "none", strip.text=element_text(size = 8), axis.title.x=element_blank(),
                      axis.title.y=element_blank()) +
                ggtitle("Ethnic Breakdown of Arrest FREQ by County")

plot_ethnic

## Stacked bar chart: ethnic breakdown of arrests, stacked between counties.
## Again, this is before normalizing by ethnic population.
plot_ethnic2 <- ggplot(cty_ethnic, aes(x = race_or_ethnicity, y = total, fill = county)) + 
                geom_bar(stat = "identity") + coord_flip() + 
                theme(axis.text.x=element_text(angle=-90,hjust=1,vjust=0.5, size = 8), axis.text.x=element_text(size = 8),
                      strip.text=element_text(size = 8), axis.title.x=element_blank(), axis.title.y=element_blank(),
                      legend.text=element_text(size= 6), legend.key.height=unit(.4, "cm")) +
                ggtitle("Cumulative Ethnic Breakdown of Arrests by County")

plot_ethnic2

## Now, let's preview the population data
dim(dat_pop)
names(dat_pop)
head(dat_pop)
tail(dat_pop)

## Looks like it's already aggregated along a number of dimensions. 
## Let's subset only the juveniles.
dat_pop_jv <- dat_pop[dat_pop$age_group %in% "Juvenile",]
head(dat_pop_jv)

## Ok, now, let's look at arrests of both genders and ignore the 'all combined' county value.
dat_pop_jv <- dat_pop_jv[dat_pop_jv$gender %in% "All Combined" & !(dat_pop_jv$county %in% "All Combined"),]

## Let's also remove the race 'all combined.'
dat_pop_jv <- dat_pop_jv[!(dat_pop_jv$race %in% "All Combined"),]

head(dat_pop_jv[dat_pop_jv$year %in% "2014",])
tail(dat_pop_jv[dat_pop_jv$year %in% "2014",])

## Looks like we'll have to do some recoding/classification, if we want to look at
## Native American, Other, or suppressed_due_to_privacy_concern populations.
unique(dat_juv$race_or_ethnicity)
unique(dat_pop_jv$race)

## Join the pop and arrests datasets.
## Start by relabeling the 'race' variable in the pop table. Also, until we've bound all years together, 
## let's subset pop figures to 2014.
names(dat_pop_jv)[3] <- "race_or_ethnicity"
names(cty_ethnic)[2] <- "year"
dat_joined <- right_join(cty_ethnic, dat_pop_jv[dat_pop_jv$year %in% "2014",], by = c("county","year","race_or_ethnicity"))

## Preview to confirm. 
head(dat_joined[!(dat_joined$county %in% "Alpine" | dat_joined$county %in% "Amador" | dat_joined$county %in% "Yuba"),])
tail(dat_joined[!(dat_joined$county %in% "Alpine" | dat_joined$county %in% "Amador" | dat_joined$county %in% "Yuba"),])

## !! Let's sub out those counties that aren't represented in the arrests file.
dat_joined <- dat_joined[!(dat_joined$county %in% "Alpine" | 
                           dat_joined$county %in% "Amador" |
                           dat_joined$county %in% "Yuba"),]

## Let's remove post-join arrest total NAs from our analysis...for now.
dat_joined <- dat_joined[!is.na(dat_joined$total),]

## Now, let's try that first panel plot, but normalized by population.
plot_ethnic_norm <- ggplot(dat_joined[!(dat_joined$race_or_ethnicity %in% "Native American"),], 
                        aes(x = race_or_ethnicity, y = total/population, fill = race_or_ethnicity), na.rm=T) + 
                        geom_bar(stat = "identity") + coord_flip() + facet_wrap(~county) +  
                        theme(axis.text.x=element_text(angle=-90,hjust=1,vjust=0.5, size = 8), axis.text.x=element_text(size = 8),
                        legend.position = "none", strip.text=element_text(size = 8), axis.title.x=element_blank(),
                        axis.title.y=element_blank()) +
                        ggtitle("Ethnic Breakdown of Arrest Rates by County")

## !! Looks like I'll need to address some out of bound issues when it comes to the Y (see warning).
plot_ethnic_norm

## Add a column just for arrest rate by ethnic population per county.
dat_joined$eth_arrest_rate <- round((dat_joined$total)/(dat_joined$population),5)

## Subset to an ethnic group.
#dat_joined_eth <- dat_joined[dat_joined$race_or_ethnicity %in% "Hispanic",]
dat_joined_eth <- dat_joined[dat_joined$race_or_ethnicity %in% "Black",]
#dat_joined_eth <- dat_joined[dat_joined$race_or_ethnicity %in% "White",]
#dat_joined_eth <- dat_joined[dat_joined$race_or_ethnicity %in% "White",]
#dat_joined_eth <- dat_joined[dat_joined$race_or_ethnicity %in% "Other",]

## Add a column for probability of seeing that arrest rate.
dat_joined_eth$rate_prob <- round(pnorm(dat_joined_eth$eth_arrest_rate, mean(dat_joined_eth$eth_arrest_rate, na.rm = T), 
                              sd(dat_joined_eth$eth_arrest_rate, na.rm = T), lower.tail = FALSE, log.p = FALSE), 5)

## View it all..
dat_joined_eth

## Calculate z-scores per rate probability.
dat_joined_eth$z_score <- qnorm(dat_joined_eth$rate_prob, lower.tail = FALSE, log.p = FALSE)

## Now, show only those counties whose z-scores are higher than 2...
## ------------------------------------------------------------------

## I.E. These are the counties whose arrest rates of the given ethnicity are statistical 'outliers' 
## among those of their California peer counties. Put another way, their exhibited arrest rates
## have a very low probability of happening purely by chance.
dat_joined_eth[dat_joined_eth$z_score >= 2,]

## IF using t-distribution
## dat_joined_eth[dat_joined_eth$t_score >= 2,]

## It'd be a good idea to check whether we're indeed working with a normal distribution, aside from
## our outliers, so let's test that.

## Test for difference between observed distribution and normal distribution (Shapiro-Wilk normality test). 
## If difference's p is < .05, then the observed distribution is not sufficiently normal.
shapiro.test(dat_joined_eth$rate_prob)

## Plot observations against norm quantiles
qqnorm(dat_joined_eth$rate_prob)
qqline(dat_joined_eth$rate_prob)

## Plot the density
plot(density(dat_joined_eth$rate_prob), main = "Arrest Rate Probability Density")

## Ok, great. The distribution problem has been fixed, and our stats are now correct.
## Granted, our population of usable county cases has shrunk by about 40%, but hey,
## it's more important to be accurate here. 

## I'll be extending this to the other ethnic groups and scaling this over the span
## of years we have arrest data for (I believe that's 2005-2014). Then, we'll apply
## this analysis to agencies, too.
