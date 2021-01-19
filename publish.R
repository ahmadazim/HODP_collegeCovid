## Read in college covid data from 3 dates (NYT dataset, raw numbers)
nytOct8 <- read.csv("./rawData/college_oct8.csv")
nytNov5 <- read.csv("./rawData/college_nov5.csv")
nytDec11 <- read.csv("./rawData/college_dec11.csv")

# row bind all dates and clean up naming 
nyt <- rbind(nytOct8, nytNov5, nytDec11)
nyt$date[nyt$date == '2020-10-08'] = "oct8"
nyt$date[nyt$date == '2020-11-05'] = "nov5"
nyt$date[nyt$date == '2020-12-11'] = "dec11"

nyt$ipeds_id <- NULL
nyt$notes <- NULL
nyt <- nyt[!is.na(nyt$cases),]
nyt <- nyt[nyt$county != 'n/a',]


# Link to census data: https://www.census.gov/rawData/tables/time-series/demo/popest/2010s-counties-total.html#par_textimage_242301767
# Read in county population data
countyPop <- read.csv('/Users/ahmadazim/Documents/Harvard/HODP/rawData/countyPop.csv', header = FALSE)
colnames(countyPop) = c('county', 'pop')

nyt$county[!paste(nyt$county,nyt$state, sep = '') %in% str_remove(countyPop$county, ' County')]
nyt$county[nyt$county == "New York City"] = "New York"   # change new york city to new york to match countyPop 
countyPop$county[countyPop$county == "Baltimore cityMaryland"] = 'Baltimore CityMaryland'    # capitalize baltimore City

countyPop$county = str_remove(countyPop$county, ' County')
nyt$mergeCS <- paste(nyt$county,nyt$state, sep = '')
nyt <- nyt[nyt$mergeCS %in% countyPop$county,]

# Getting covid numbers by county (and then dividing by )
countyCovid_oct8 <- read.csv("./rawData/countyCovid_oct8.csv")
countyCovid_nov5 <- read.csv("./rawData/countyCovid_nov5.csv")
countyCovid_dec11 <- read.csv("./rawData/countyCovid_dec11.csv")

countyCovid <- rbind(countyCovid_oct8, countyCovid_nov5, countyCovid_dec11)
countyCovid$date[countyCovid$date == '2020-10-08'] = "oct8"
countyCovid$date[countyCovid$date == '2020-11-05'] = "nov5"
countyCovid$date[countyCovid$date == '2020-12-11'] = "dec11"

countyCovid <- countyCovid[,c('date', 'county', 'state', 'cases')]
countyCovid$county[countyCovid$county == "New York City"] = "New York"
countyCovid$county[countyCovid$county == "Baltimore city"] = "Baltimore City"

countyCovid$mergeCS <- paste(countyCovid$county,countyCovid$state, sep = '')
countyCovid$mergeCS[countyCovid$mergeCS == "District of ColumbiaDistrict of Columbia"] = "Washington, D.C.Washington, D.C."

nyt <- nyt[nyt$mergeCS %in% countyCovid$mergeCS,]

nyt$countyCovidProp <- "untouched"

for(i in 1:nrow(nyt)){
  dt = nyt$date[i]
  cp = countyPop$pop[countyPop$county == nyt$mergeCS[i]]
  cc = countyCovid$cases[countyCovid$mergeCS == nyt$mergeCS[i] & countyCovid$date == dt]
  
  nyt$countyCovidProp[i] = cc/cp
}
nyt$countyCovidProp <- as.numeric(nyt$countyCovidProp)


# Adding scraped data
scrapedData <- data.frame(final)
for(i in 1:length(scrapedData$Name)){
  scrapedData[i,1] = str_replace(scrapedData$Name[i], " at ", ', ')
  #print(str_replace(scrapedData$Name[i], " at ", ', '))
}

for(i in 1:length(nyt$college)){
  nyt$college[i] = str_replace(nyt$college[i], " at ", ', ')
}

scrapedData$Name[scrapedData$Name == 'University of Wisconsin, Eau Claire'] = 'University of Wisconsin-Eau Claire'
scrapedData$Name[scrapedData$Name == 'University of Wisconsin, Green Bay'] = 'University of Wisconsin-Green Bay'
scrapedData$Name[scrapedData$Name == 'University of Wisconsin, La Crosse'] = 'University of Wisconsin-La Crosse'
scrapedData$Name[scrapedData$Name == 'University of Wisconsin, Madison'] = 'University of Wisconsin-Madison'
scrapedData$Name[scrapedData$Name == 'University of Wisconsin, Milwaukee'] = 'University of Wisconsin-Milwaukee'
scrapedData$Name[scrapedData$Name == 'University of Wisconsin, Oshkosh'] = 'University of Wisconsin-Oshkosh'
scrapedData$Name[scrapedData$Name == 'University of Wisconsin, Platteville'] = 'University of Wisconsin-Platteville'
scrapedData$Name[scrapedData$Name == 'University of Wisconsin, River Falls'] = 'University of Wisconsin-River Falls'
scrapedData$Name[scrapedData$Name == 'University of Wisconsin, Stevens Point'] = 'University of Wisconsin-Stevens Point'
scrapedData$Name[scrapedData$Name == 'University of Wisconsin, Superior'] = 'University of Wisconsin-Superior'
scrapedData$Name[scrapedData$Name == 'University of Wisconsin, Whitewater'] = 'University of Wisconsin-Whitewater'


d <- nyt[nyt$college %in% scrapedData$Name,]
d$plan <- "untouched"
d$enrollment2018 <- "untouched"
d$type <- "untouched"

for(i in 1:nrow(d)){
  d$plan[i] = scrapedData$Plan[scrapedData$Name == d$college[i]]
  d$enrollment2018[i] = scrapedData$Enrollment[scrapedData$Name == d$college[i]]
  d$type[i] = scrapedData$Type[scrapedData$Name == d$college[i]]
}



## Get county data from July 15/Aug 15....
# create df with college name, mergeCS, county covid cases, and reopening plan (and type?)
reopen <- d[,c('college', 'mergeCS', 'plan', 'type')]
reopen <- unique(reopen)

# read in august and july data
countyCovid_aug15 <- read.csv("./rawData/countyCovid_aug15.csv")
countyCovid_july1 <- read.csv("./rawData/countyCovid_july1.csv")
cnty_early <- rbind(countyCovid_aug15, countyCovid_july1)

cnty_early$date[cnty_early$date == '2020-08-15'] = "aug15"
cnty_early$date[cnty_early$date == '2020-07-01'] = "july1"
cnty_early <- cnty_early[,c('date', 'county', 'state', 'cases')]
cnty_early$county[cnty_early$county == "New York City"] = "New York"
cnty_early$county[cnty_early$county == "Baltimore city"] = "Baltimore City"

cnty_early$mergeCS <- paste(cnty_early$county, cnty_early$state, sep = '')
cnty_early$mergeCS[cnty_early$mergeCS == "District of ColumbiaDistrict of Columbia"] = "Washington, D.C.Washington, D.C."

# Repeat reopen df for two dates (aug15 and july1)
reopen <- rbind(reopen, reopen)
reopen$date <- rep(c('aug15','july1'), each = (nrow(reopen)/2))

reopen$countyCovidProp = 'untouched'
for(i in 1:nrow(reopen)){
  dt = reopen$date[i]
  cp = countyPop$pop[countyPop$county == reopen$mergeCS[i]]
  cc = cnty_early$cases[cnty_early$mergeCS == reopen$mergeCS[i] & cnty_early$date == dt]
  
  reopen$countyCovidProp[i] = cc/cp
}
reopen$countyCovidProp <- as.numeric(reopen$countyCovidProp)


## Are colleges making ADVISED reopening plans?
# Make bar plot... all plans on x-axis (categorical data) and avg county covid on y-axis

mean_aug15 = data.frame(meanCovid = tapply(reopen$countyCovidProp[reopen$date == 'aug15'], reopen$plan[reopen$date == 'aug15'], mean))
sd_aug15 = data.frame(sdCovid = tapply(reopen$countyCovidProp[reopen$date == 'aug15'], reopen$plan[reopen$date == 'aug15'], sd))
plansCovid_aug15 = cbind(mean_aug15, sd_aug15)
plansCovid_aug15$plan = rownames(plansCovid_aug15)
rownames(plansCovid_aug15) = NULL

mean_july1 = data.frame(meanCovid = tapply(reopen$countyCovidProp[reopen$date == 'july1'], reopen$plan[reopen$date == 'july1'], mean))
# use standard error instead of std deviation
SE_july1 = data.frame(SE = tapply(reopen$countyCovidProp[reopen$date == 'july1'], reopen$plan[reopen$date == 'july1'], function(x) sd(x)/sqrt(length(reopen$countyCovidProp[reopen$date == 'july1'])) ))
plansCovid_july1 = cbind(mean_july1, SE_july1)
plansCovid_july1$plan = rownames(plansCovid_july1)
rownames(plansCovid_july1) = NULL

# remove 'other' and 'undetermined'
plansCovid_aug15 = plansCovid_aug15[plansCovid_aug15$plan != "Other" & plansCovid_aug15$plan != "Undetermined",]
plansCovid_july1 = plansCovid_july1[plansCovid_july1$plan != "Other" & plansCovid_july1$plan != "Undetermined",]

aug15_plot <- ggplot(data=plansCovid_aug15, aes(x=reorder(plan, meanCovid), y=meanCovid)) +
  geom_bar(stat="identity", position=position_dodge(), fill = '#EE3838') +
  theme_hodp() + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust = 0.4), plot.title = element_text(size=15)) +
  ylab("Average Proportion of COVID-19 Cases in College County (August 15, 2020)") +
  labs(title= "Are Colleges Making Advised Reopening Plans?") + 
  xlab("Reopening Plan")
aug15_plot


#july1_props = reopen[reopen$date == 'july1' & reopen$plan != "Other" & reopen$plan != "Undetermined",]
plansCovid_july1$plan = factor(plansCovid_july1$plan, levels = c('Fully in person', 'Primarily in person', 'Hybrid', 'Primarily online','Fully online'))

july1_plot <- ggplot(data=plansCovid_july1, aes(x=plan, y=meanCovid, fill= plan)) +
  geom_bar(stat="identity", position=position_dodge()) +
  #geom_boxplot() +
  geom_errorbar(aes(ymin=meanCovid-SE, ymax=meanCovid+SE), width=.2, position=position_dodge(.9)) + 
  theme_hodp() + 
  scale_fill_manual(values = c("#78C4D4", '#4B5973', '#EE3838', '#FA9E1C', '#760000'))+
  theme(legend.position = 'none', axis.text.x = element_text(angle = 0, hjust = 0.4), plot.title = element_text(size=15)) +
  ylab("Average County COVID-19 Proportions") +
  labs(title= "County COVID-19 Proportions vs College Reopening Plan") + 
  xlab("Reopening Plan")+ 
  #geom_signif(comparisons = list(c("Primarily in person", "Fully online")), 
  #            map_signif_level=TRUE)
  geom_signif(y_position=0.0124, xmin=2, xmax=4,
            annotation="**", tip_length=0) 

# testing for significance between fully online and primarily in person
t.test(reopen$countyCovidProp[reopen$date == "july1" & reopen$plan == "Primarily online"], reopen$countyCovidProp[reopen$date == "july1" & reopen$plan == "Primarily in person"])

png("./Figures/advised.png", width = 2592, height = 1890, res = 300)
july1_plot 
dev.off()

#Try to evaluate within school type?
# No.


# Pie charts (distribution of reopening plan across school type)
pie_d = d[d$date == "oct8",]
pie_d$Type2 <- 0 
pie_d$Type2[pie_d$type == unique(pie_d$type)[1]] = 1  #"Public, 4-year"
pie_d$Type2[pie_d$type == unique(pie_d$type)[2]] = 2  #"Public, 2-year"
pie_d$Type2[pie_d$type == unique(pie_d$type)[3]] = 3  #"Private nonprofit, 4-year"

generalPlan <- data.frame(table(pie_d$plan))
generalPlan$Freq[generalPlan$Var1 == "Other"] <- generalPlan$Freq[generalPlan$Var1 == "Other"] + generalPlan$Freq[generalPlan$Var1 == "Undetermined"]
generalPlan <- generalPlan[generalPlan$Var1 != "Undetermined",]

generalPlan$Var1 <- factor(generalPlan$Var1, levels = c('Fully in person', 'Primarily in person', 'Hybrid', 'Primarily online', 'Fully online', 'Other'))
colnames(generalPlan) <- c("Plan", "Freq")
pie <- ggplot(data=generalPlan, aes(x=2, y=Freq, fill = Plan)) +
  geom_bar(stat="identity", width = 1) +
  theme_void() + 
  theme(plot.title = element_text(size=20,  family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5),
        legend.title=element_text(size=10, family="Helvetica"), 
        legend.text=element_text(size=10, family="Helvetica"),
        legend.position = "bottom",
  ) + 
  scale_fill_manual(values = c('#EE3838', '#FA9E1C', "#78C4D4", '#4B5973', '#760000', "#E2DDDB")) +
  labs(title= "All College Reopening Plans") + 
  coord_polar("y") +
  xlim(0.2,2.5)

png("./Figures/allPlan.png", width = 2592, height = 1890, res = 300)
pie
dev.off()


## Type i
pie_di <- pie_d[pie_d$Type2 == 2,] # REPLACE i WITH WHATEVER YOU WANT 
generalPlan <- data.frame(table(pie_di$plan))
generalPlan$Freq[generalPlan$Var1 == "Other"] <- generalPlan$Freq[generalPlan$Var1 == "Other"] + generalPlan$Freq[generalPlan$Var1 == "Undetermined"]
generalPlan <- generalPlan[generalPlan$Var1 != "Undetermined",]

generalPlan$Var1 <- factor(generalPlan$Var1, levels = c('Fully in person', 'Primarily in person', 'Hybrid', 'Primarily online', 'Fully online', 'Other'))
colnames(generalPlan) <- c("Plan", "Freq")
pie <- ggplot(data=generalPlan, aes(x=2, y=Freq, fill = Plan)) +
  geom_bar(stat="identity", width = 1) +
  theme_void() + 
  theme(plot.title = element_text(size=20,  family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5),
        legend.title=element_text(size=10, family="Helvetica"), 
        legend.text=element_text(size=10, family="Helvetica"),
        legend.position = "bottom",
  ) + 
  scale_fill_manual(values = c('#EE3838', '#FA9E1C', "#78C4D4", '#4B5973', '#760000', "#E2DDDB")) +
  labs(title= "Reopening Plans for 4-Year, Private Colleges") + 
  coord_polar("y") +
  xlim(0.2,2.5)

png("./Figures/private4_pie.png", width = 2592, height = 1890, res = 300)
pie
dev.off()


## Are plans effective... looking at oct8, nov5, dec11
d$adjCases = d$cases/as.numeric(d$enrollment2018)
oct8 = d[d$date == 'oct8',]
nov5 = d[d$date == 'nov5',]
dec11 = d[d$date == 'dec11',]

plot(as.numeric(oct8$countyCovidProp), oct8$adjCases, col = 'navyblue', main ="How do College COVID-19 Cases Compare \n to Their Counties?", 
     xlab = "Proportion of COVID-19 Cases in County", ylab = "Proportion of COVID-19 Cases in College")
abline(as.numeric(oct8$countyCovidProp), oct8$adjCases, col = 'red')
lines(c(0,1), c(0,1))

oct8_plot <- ggplot(data=oct8, aes(x=countyCovidProp, y=adjCases)) +
  geom_point(color = '#4B5973') +
  geom_smooth(method='lm', col = '#EE3838') +
  geom_line(aes(c(rep(0,1100),rep(.15, 5)), c(rep(0,1100),rep(0.15, 5))), size = 1.2, linetype = "dashed") +
  labs(title="College vs County COVID-19 Proportions (Oct. 8)") +
  xlab("Proportions of COVID-19 Cases in County") +
  ylab("Proportions of COVID-19 Cases in College") +
  theme_hodp() 
oct8_plot

png("./Figures/cvc_oct8.png", width = 2592, height = 1890, res = 300)
oct8_plot
dev.off()

summary(lm(oct8$adjCases ~ oct8$countyCovidProp))
# (estimate - null) / SE --> find value on t distribution
#(0.0782793-1)/0.0292046

nov5_plot <- ggplot(data=nov5, aes(x=countyCovidProp, y=adjCases)) +
  geom_point(color = '#4B5973') +
  geom_smooth(method='lm', col = '#EE3838') +
  geom_line(aes(c(rep(0,1178),rep(0.165, 5)), c(rep(0,1178),rep(0.165, 5))), size = 1.2, linetype = "dashed") +
  labs(title="College vs County COVID-19 Proportions (Nov. 5)") +
  xlab("Proportions of COVID-19 Cases in County") +
  ylab("Proportions of COVID-19 Cases in College") +
  theme_hodp() 
nov5_plot

png("./Figures/cvc_nov5.png", width = 2592, height = 1890, res = 300)
nov5_plot
dev.off()

summary(lm(nov5$adjCases ~ nov5$countyCovidProp))
# (estimate - null) / SE --> find value on t distribution
#(0.197774-1)/0.034431

dec11_plot <- ggplot(data=dec11, aes(x=countyCovidProp, y=adjCases)) +
  geom_point(color = '#4B5973') +
  geom_smooth(method='lm', col = '#EE3838') +
  geom_line(aes(c(rep(0,1229),rep(0.215, 5)), c(rep(0,1229),rep(0.215, 5))), size = 1.2, linetype = "dashed") +
  labs(title="College vs County COVID-19 Proportions (Dec. 11)") +
  xlab("Proportions of COVID-19 Cases in County") +
  ylab("Proportions of COVID-19 Cases in College") +
  theme_hodp() 
dec11_plot

png("./Figures/cvc_dec11.png", width = 2592, height = 1890, res = 300)
dec11_plot
dev.off()



### Trying a new plot

oct8$diff = oct8$adjCases - oct8$countyCovidProp
oct8$sign = 0 
oct8$sign[oct8$diff <= 0] = 'good'
oct8$sign[oct8$diff > 0] = 'bad'
oct8_plot2 <- ggplot(data=oct8, aes(x=countyCovidProp, y=diff, color = sign)) +
  geom_point() +
  #geom_smooth(method='lm', col = '#EE3838') +
  labs(title="College vs County COVID-19 Proportions (Oct. 8)") +
  scale_color_manual(values = c('#EE3838', '#4B5973'))+
  xlab("Proportions of COVID-19 Cases in County") +
  ylab("Difference in COVID-19 Proportions (College — County)") +
  geom_hline(yintercept=0, color = 'black', linetype = "dashed", size = 1.2)+
  xlim(c(0,0.216))+
  ylim(c(-0.23,0.33))+
  theme_hodp()+
  theme(legend.position="none")
oct8_plot2

png("./Figures/cvc2_oct8.png", width = 2592, height = 1500, res = 300)
oct8_plot2
dev.off()


nov5$diff = nov5$adjCases - nov5$countyCovidProp
nov5$sign = 0 
nov5$sign[nov5$diff <= 0] = 'good'
nov5$sign[nov5$diff > 0] = 'bad'
nov5_plot2 <- ggplot(data=nov5, aes(x=countyCovidProp, y=diff, color = sign)) +
  geom_point() +
  #geom_smooth(method='lm', col = '#EE3838') +
  labs(title="College vs County COVID-19 Proportions (Nov. 5)") +
  scale_color_manual(values = c('#EE3838', '#4B5973'))+
  xlab("Proportions of COVID-19 Cases in County") +
  ylab("Difference in COVID-19 Proportions (College — County)") +
  geom_hline(yintercept=0, color = 'black', linetype = "dashed", size = 1.2)+
  xlim(c(0,0.216))+
  ylim(c(-0.23,0.33))+
  theme_hodp()+
  theme(legend.position="none")
nov5_plot2

png("./Figures/cvc2_nov5.png", width = 2592, height = 1500, res = 300)
nov5_plot2
dev.off()



dec11$diff = dec11$adjCases - dec11$countyCovidProp
dec11$sign = 0 
dec11$sign[dec11$diff <= 0] = 'good'
dec11$sign[dec11$diff > 0] = 'bad'
dec11_plot2 <- ggplot(data=dec11, aes(x=countyCovidProp, y=diff, color = sign)) +
  geom_point() +
  #geom_smooth(method='lm', col = '#EE3838') +
  labs(title="College vs County COVID-19 Proportions (Dec. 11)") +
  scale_color_manual(values = c('#EE3838', '#4B5973'))+
  xlab("Proportions of COVID-19 Cases in County") +
  ylab("Difference in COVID-19 Proportions (College — County)") +
  geom_hline(yintercept=0, color = 'black', linetype = "dashed", size = 1.2)+
  xlim(c(0,0.216))+
  ylim(c(-0.23,0.33))+
  theme_hodp()+
  theme(legend.position="none")
dec11_plot2

png("./Figures/cvc2_dec11.png", width = 2592, height = 1500, res = 300)
dec11_plot2
dev.off()




## Trying something else.... (histograms)
allData = rbind(oct8, nov5, dec11)
allData$date <- factor(allData$date, levels = c('oct8','nov5','dec11'))
col1 = '#4B5973'; col2 = '#FA9E1C'; col3 = '#EE3838'
density_plot = ggplot(allData, aes(x=diff, color=date, fill=date)) + 
  #geom_histogram(aes(y=..density..), alpha=0, 
  #               position="identity")+
  xlim(c(-0.1, 0.1))+
  geom_density(alpha=.2, size = 1)+
  theme_hodp()+
  labs(title = "Differences in COVID-19 Proportions between Colleges and Counties")+
  xlab("Difference in COVID-19 Proportions (College — County)")+
  ylab('Density')+
  geom_vline(xintercept = 0, linetype = 'dashed', size = 0.85)+
  geom_segment(aes(x=median(oct8$diff), xend=median(oct8$diff),   y=0, yend=29.6), linetype = 'dotted', color = col1, size=0.8)+
  geom_segment(aes(x=median(nov5$diff), xend=median(nov5$diff),  y=0, yend=24.12), linetype = 'dotted', color = col2, size=0.8)+
  geom_segment(aes(x=median(dec11$diff), xend=median(dec11$diff), y=0, yend=15.4), linetype = 'dotted', color = col3, size=0.8)+
  theme(legend.position = c(0.9, 0.5), 
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size=17, hjust = 0.5))+
  scale_color_manual(name = "Date (Year 2020)", labels =  c("October 8", "Novermber 5", "December 11"), values = c(col1, col2, col3))+
  scale_fill_manual(name = "Date (Year 2020)", labels = c("October 8", "Novermber 5", "December 11"), values =  c(col1, col2, col3))

png("./Figures/densityPlot1.png", width = 2500, height = 1800, res = 300)
density_plot
dev.off()


# > length(oct8$diff[oct8$diff > 0])/nrow(oct8)
# [1] 0.1900452
# > length(nov5$diff[nov5$diff > 0])/nrow(nov5)
# [1] 0.2147084
# > length(dec11$diff[dec11$diff > 0])/nrow(dec11)
# [1] 0.1961102



### Try separating the densities 
oct8_den = data.frame(college = c(oct8$college, oct8$college), den = c(oct8$adjCases, oct8$countyCovidProp))
oct8_den$cc = rep(c("College", "County"), each = nrow(oct8))

ggplot(oct8_den, aes(x=den, color=cc, fill=cc)) + 
  #geom_histogram(aes(y=..density..), alpha=0, 
  #               position="identity")+
  xlim(c(0, 0.06))+
  geom_density(alpha=.2)+
  theme_hodp()+
  labs(title = "Differences in COVID-19 Proportions (College — County)")+
  xlab("Proportion of COVID-19 Cases")+
  ylab('Density')+
  theme(legend.position = "right", 
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size=17))+
  scale_fill_discrete(name = "", labels = c("College", "County"))+
  scale_color_discrete(name = "", labels = c("College", "County"))

# Not the move!


# Regression...
summary(lm(adjCases ~ state + date + countyCovidProp + plan + type, data = d))
anova(lm(adjCases ~ date + state + countyCovidProp + plan + type, data = d))
anova(lm(adjCases ~ state + countyCovidProp + plan + type, data = d[d$date == 'oct8',]))

anova(lm(adjCases ~ date + state + countyCovidProp*plan + type, data = d))
# probably overly complicated...



# Looking at average increase in cases over time (college slower)
qplot(d$date, d$countyCovidProp)

ggplot(data = d) +
  aes(x = date, y = countyCovidProp) +
  geom_jitter(width = 0.3)

ggplot(data = d) +
  aes(x = date, y = adjCases) +
  geom_jitter(width = 0.3)



# Enrollment by institution size
enrollment_data = d[d$plan != "Other" & d$plan != "Undetermined",]
enrollment_means <- data.frame(mean=tapply(as.numeric(enrollment_data$enrollment2018), enrollment_data$plan, mean))
enrollment_means$plan = rownames(enrollment_means)

enrollment_means$plan = factor(enrollment_means$plan, levels = c('Fully in person', 'Primarily in person', 'Hybrid', 'Primarily online', 'Fully online'))
enrollment_plot <- ggplot(data=enrollment_means, aes(x=plan, y=mean)) +
  geom_bar(stat="identity", position=position_dodge(), fill = '#EE3838') +
  theme_hodp()+
  ylab("Mean Enrollment") +
  xlab("Reopening Plan") +
  labs(title = "Mean Enrollment vs Reopening Plan")
enrollment_plot

png("./Figures/enrollmentMeans.png", width = 2592, height = 1890, res = 300)
enrollment_plot
dev.off()





