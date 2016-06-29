
## Status, as of June 28, 2016:
## - Pretty much done, although I'd like to convert the nested for loop into a window function call.
## - Would like to create a final large panel plot that highlights outliers per year.

## Prep the workspace
library("AzureML")
ws <- workspace()

## load the data: Read all arrests files and merge into one massive data frame.
## Please note: We could automate file name detection/grabbing in a local version of this, but we're
## trying to lean on Azure ML's processing power, so we've gotta' work with their obfuscation of
## dataset paths.
file_names <- grep("^ca_doj_arrests.*csv", ws$datasets$Name, value = T)

## Subset to only 2005-2014, because our population data only begins in 2005
dat_arrests <- do.call(rbind, download.datasets(ws, file_names[!grepl("200[0-4]", file_names)]))

## Remove row names
row.names(dat_arrests) <- NULL

## Then, read county pop by race and gender.
dat_pop <- download.datasets(ws, "ca_county_population_by_race_gender_age_2005-2014_02-05-2016.csv")

## Preview the arrests data
dim(dat_arrests)
names(dat_arrests)
head(dat_arrests,3)
tail(dat_arrests,3)

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
## Note: this is sheerly by count (not rate).
plot_ethnic <- ggplot(cty_ethnic[cty_ethnic$arrest_year %in% "2014",], aes(x = race_or_ethnicity, y = total, fill = race_or_ethnicity)) + 
                geom_bar(stat = "identity") + coord_flip() + facet_wrap(~county) +  
                theme(axis.text.x=element_text(angle=-90,hjust=1,vjust=0.5, size = 8), axis.text.x=element_text(size = 8),
                      legend.position = "none", strip.text=element_text(size = 8), axis.title.x=element_blank(),
                      axis.title.y=element_blank()) +
                ggtitle("Ethnic Breakdown of Arrest FREQ by County\r
2014 Only (test year)")

## Print plot
plot_ethnic

## Stacked bar chart: ethnic breakdown of arrests, stacked between counties.
plot_ethnic2 <- ggplot(cty_ethnic[cty_ethnic$arrest_year %in% "2014",], aes(x = race_or_ethnicity, y = total, fill = county)) + 
                geom_bar(stat = "identity") + coord_flip() + 
                theme(axis.text.x=element_text(angle=-90,hjust=1,vjust=0.5, size = 8), axis.text.x=element_text(size = 8),
                      strip.text=element_text(size = 8), axis.title.x=element_blank(), axis.title.y=element_blank(),
                      legend.text=element_text(size= 6), legend.key.height=unit(.4, "cm")) +
                ggtitle("Cumulative Ethnic Breakdown of Arrest Freq by County\r
2014 Only (test year)")

## Print plot
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

head(dat_pop_jv)
tail(dat_pop_jv)

## Looks like we'll have to do some recoding/classification, if we want to look at
## Native American, Other, or suppressed_due_to_privacy_concern populations.
unique(dat_juv$race_or_ethnicity)
unique(dat_pop_jv$race)

## Join the pop and arrests datasets.
## Start by relabeling the 'race' variable in the pop table. Also, until we've bound all years together, 
names(dat_pop_jv)[3] <- "race_or_ethnicity"
names(cty_ethnic)[2] <- "year"
dat_joined <- right_join(cty_ethnic, dat_pop_jv, by = c("county","year","race_or_ethnicity"))

## Preview to confirm. 
head(dat_joined[!(dat_joined$county %in% "Alpine" | dat_joined$county %in% "Amador" | dat_joined$county %in% "Yuba"),])
tail(dat_joined[!(dat_joined$county %in% "Alpine" | dat_joined$county %in% "Amador" | dat_joined$county %in% "Yuba"),])

## Let's sub out those counties that aren't represented in the arrests file.
dat_joined <- dat_joined[!(dat_joined$county %in% "Alpine" | 
                           dat_joined$county %in% "Amador" |
                           dat_joined$county %in% "Yuba"),]

## Let's remove post-join arrest total NAs from our analysis...for now.
dat_joined <- dat_joined[!is.na(dat_joined$total),]

## Now, let's panel plot arrest rates by county.
plot_ethnic_norm <- ggplot(dat_joined[!(dat_joined$race_or_ethnicity %in% "Native American") & dat_joined$year %in% "2014",], 
                        aes(x = race_or_ethnicity, y = total/population, fill = race_or_ethnicity), na.rm=T) + 
                        geom_bar(stat = "identity") + coord_flip() + facet_wrap(~county) +  
                        theme(axis.text.x=element_text(angle=-90,hjust=1,vjust=0.5, size = 8), axis.text.x=element_text(size = 8),
                        legend.position = "none", strip.text=element_text(size = 8), axis.title.x=element_blank(),
                        axis.title.y=element_blank()) +
                        ggtitle("Ethnic Breakdown of Arrest Rates by County\r
-2014 Only-")

## Print plot
plot_ethnic_norm

## Add a column just for arrest rate by ethnic population per county.
dat_joined$eth_arrest_rate <- round((dat_joined$total)/(dat_joined$population), 5)

#### Looping approach (Please don't hate me, Rocio! I'll vectorize asap :)) ####

## Create empty dataframe
dat_stats <- dat_joined[0,]
dat_stats$rate_prob <- numeric(0)
dat_stats$z_score <- numeric(0)

## Nested loop (computing stats per race/ethnic group, per year)
for(i in unique(dat_joined$year)){
    
    ## Subset to iterative year
    dat_year <- dat_joined[dat_joined$year %in% i,]
    
    for(j in unique(dat_year$race_or_ethnicity)){
        
        ## Subset to iterative race/ethnicity
        dat_race <- dat_year[dat_year$race_or_ethnicity %in% j,]
        
        ## Compute the probability of the observed arrest rate
        dat_race$rate_prob <- round(pnorm(dat_race$eth_arrest_rate, mean(dat_race$eth_arrest_rate, na.rm = T), 
                                    sd(dat_race$eth_arrest_rate, na.rm = T), lower.tail = FALSE, log.p = FALSE), 5)
        
        ## Compute the Z-score of the observed arrest rates
        dat_race$z_score <- qnorm(dat_race$rate_prob, lower.tail = FALSE, log.p = FALSE)
        
        ## Bind to burgeoning dataframe
        dat_stats <- rbind(dat_stats, dat_race)
    }
}

## Now, preview those who have evidently been outliers in enforcement upon any ethnic group for any year.
head(dat_stats[dat_stats$z_score >= 2,], 20)
head(dat_stats[dat_stats$z_score >= 2,], 20)
paste("Number of outlying instances over this time-span:", nrow(dat_stats[dat_stats$z_score >= 2,]))

## Interesting. There seem to be a lot of the same few counties represented.
## Thus, let's draw up a frequency table of the instances per outlying county from above.
table(dat_stats[dat_stats$z_score >= 2, "county"])

## Two counties stick out to this researcher: 
## Kings, for its sheer volume of outlying instances, and San Francisco, because, well, it's a 
## little strange to see San Francisco in there, given how "cosmopolitan" it's often considered.

## That said, let's first isolate Kings cases to see if there's a pattern.
dat_stats[dat_stats$z_score >= 2 & dat_stats$county %in% "Kings",]

## VERY interesting. One will note that Kings is an outlier for enforcement upon at least one ethnic
## group per year. HOWEVER, in only one of those years were they an outlier for blacks. Indeed, it
## seems their outlying arrest rates span their application to most every other race/ethnic group.
## What is so different about Kings???

## Now, let's isolate those SF cases to see if there's a pattern there.
dat_stats[dat_stats$z_score >= 2 & dat_stats$county %in% "San Francisco",]

## AH, San Francisco is an outlier for enforcement upon those classified as 'Other' in every year
## within our dataset. There's two ways one can interpret this: Either San Francisco is indeed
## extremely cosmopolitan and the high number of 'Other' designees simply points to the proportion
## of mixed race/ethnicity one might assume such a city to have, OR, for some operational policy
## reason, SFPD is simply choosing to label arrested juveniles who match some demographic conditions
## as 'Other.'

# ## Vectorization / Window function approach (for practice!)
# 
# prob_fun <- function(x){
#     round(pnorm(x, mean(x, na.rm = T), sd(x, na.rm = T), lower.tail = FALSE, log.p = FALSE), 5)
# }
# zscore_fun <- function(x){
#     round(qnorm(x$rate_prob, lower.tail = FALSE, log.p = FALSE), 5)
# }
# 
# dat_wstats <- aggregate(eth_arrest_rate ~ year + county, data = dat_joined, FUN = prob_fun, na.action = "na.pass")
# 
# #dat_wstats
# names(dat_wstats)[3] <- "rate_prob"
# 
# dat_wstats

## It'd be a good idea to check whether our assumptions over a normal distribution (because we're 
## employing z-scores) are true.

## Choose a test subset
dat_stats_test <- dat_stats[dat_stats$year %in% "2014" & dat_stats$race_or_ethnicity %in% "Hispanic",]

## Test for difference between observed distribution and normal distribution (Shapiro-Wilk normality test). 
## If difference's p is < .05, then the observed distribution is not sufficiently normal.
shapiro.test(dat_stats_test$eth_arrest_rate)

## See what happens if we exclude the outliers. Depending on the number of outliers, this should
## be even closer to normal.
shapiro.test(dat_stats_test$eth_arrest_rate[dat_stats_test$z_score < 2])

## Plot the density w/outliers
plot(density(dat_stats_test$eth_arrest_rate), 
     main = "Arrest Rate Density for Hispanics in 2014\r
(test eth and year), With Outliers")

## Plot vs. purely normal distribution
qqnorm(dat_stats_test$eth_arrest_rate, main = "Arrest Rate Observations for Hispanics in 2014\r
vs. Theoretical Normal Quantiles (outliers included)")
qqline(dat_stats_test$eth_arrest_rate)

## Save full stats df to csv and also upload DF to Azure ML
write.csv(dat_stats, "dat_stats_2005-2014.csv", row.names = F)
upload.dataset(dat_stats, ws, name = "dat_stats_2005-2014")

## Save outliers-only to csv and also upload DF to Azure ML
write.csv(dat_stats[dat_stats$z_score > 2,], "dat_stats_outliers_2005-2014.csv", row.names = F)
upload.dataset(dat_stats[dat_stats$z_score > 2,], ws, name = "dat_stats_outliers_2005-2014")

## ^Please disregard the Azure ML upload status messages the system produces...

## Next step: Create a large panel plot, faceted by year, between counties and their per-race/ethnic
## group arrest rates OR a motion chart (ala Hans Rosling), which displays the same, over the years.
