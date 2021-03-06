# Load in colleges.csv file
d = read.csv("./Data/colleges.csv", header = T) 

d = d[order(d$cases, decreasing = T),]
d = d[is.na(d$cases) == FALSE,]
# In article, address why we don't have to use proportions (has to do with density)
# We couldnt number of students on campus, but that isnt so bad because^

allCovid = read.csv("./Data/Oct8_allData.csv", header = T)
usData = allCovid[allCovid$Country_Region == "US",]

usData_states = unique(usData$Province_State)
d_states = unique(d$state)
usData_states[usData_states %in% d_states == FALSE]

# Remove colleges in American Samoa and Marshall Islands
d_states[d_states %in% usData_states == FALSE]
#[1] "American Samoa"   "Marshall Islands"
d = d[d$state != "Virgin Islands" & d$state != "Puerto Rico" & d$state != "American Samoa" & d$state != "Marshall Islands",] 
usData_states = unique(usData$Province_State)
d_states = unique(d$state)

# change "District of Columbia" to "Washington, D.C." in usData_states
usData$Province_State[usData$Province_State == "District of Columbia"] = "Washington, D.C."


# USED 2019 ESTIMATES FOR STATE POPULATION
statePop = read.csv("./Data/statePop.csv", header = TRUE)
statePop[,1] = substr(statePop[,1], 2, nchar(statePop[,1]))
statePop[,1][statePop[,1] == "District of Columbia"] = "Washington, D.C."

# Find state covid PROPORTION
stateCovid = matrix(0, ncol = 2, nrow = 51)
for(i in 1:51){
  stateCovid[i,1] = d_states[i]
  x <- usData[usData$Province_State == d_states[i],]
  stateCovid[i,2] = sum(x$Confirmed)/statePop[,2][statePop[,1] == d_states[i]]
}

d$state_covid = 0
for(i in 1:nrow(d)){
  d$state_covid[i] <- stateCovid[,2][stateCovid[,1] == d$state[i]]
}


pop.lm = lm(cases ~ state_covid, data = d)
summary(pop.lm)

plot(d$state_covid, d$cases)
abline(d$state_covid, d$cases, col = "red")

# Only use worst 500 colleges
w <- d[1:50,]
plot(w$state_covid, w$cases)
abline(w$state_covid, w$cases, col = "red")



##===========================================================
## State data not informative enough.... zoom in on county
##===========================================================
countyCovid <- read.csv("./Data/countyCovid_allDates.csv", header = TRUE)
unique(countyCovid$date)     #want Oct. 8 to match college covid data we have
countyOct8 <- countyCovid[countyCovid$date == "2020-10-08",]

# Add column to data to reflect county proportions... first we have to get proportions
# USED 2019 ESTIMATES FOR COUNTY POPULATION

# Working with county populations
countyPop = read.csv("./Data/countyPop.csv", header = FALSE)
names(countyPop)[names(countyPop) == "V1"] <- "pop"   #change column names
names(countyPop)[names(countyPop) == "V2"] <- "county"
names(countyPop)[names(countyPop) == "V3"] <- "state"

countyPop[,2] = substr(countyPop[,2], 2, nchar(countyPop[,2]))
countyPop[,3] = substr(countyPop[,3], 2, nchar(countyPop[,3]))

for(i in 1:nrow(countyPop)){
  if( substring(countyPop[i,2], (nchar(countyPop[i,2])-5), nchar(countyPop[i,2])) == "County"){
    countyPop[i,2] = substr(countyPop[i,2], 1, (nchar(countyPop[i,2]) - 7))
  }
}

# Glue together county and state
countyPop$name = paste(countyPop$county, countyPop$state, sep = "")
usData$countyState = paste(usData$Admin2, usData$Province_State, sep = "")
  

covid_county = matrix("untouched", ncol = 2, nrow = 3270)    # length(usData$Admin2)
for(i in 1:3270){
  x = usData$countyState[i]
  covid_county[i,1] = x
  
  xConfirmed <- usData$Confirmed[usData$countyState == x]
  xPop <- countyPop$pop[countyPop$name == x]
  
  if(length(xPop) == 1){
    covid_county[i,2] = xConfirmed/xPop
  }
  else{ next }
}


# Look at one that were untouched
d$county[d$county == "New York City"] = "New York"
d$county[d$county == "Washington, D.C."] = "District of Columbia"
d$county[d$county == "St. Louis County"] = "St. Louis"
covid_county = rbind(covid_county, c("CacheUtah", 3998/128289))

d$mergedName <- paste(d$county, d$state, sep = "")
d$county_covid <- "proportion"
for(i in 1:nrow(d)){
  x = d$mergedName[i]
  propCov = covid_county[,2][covid_county[,1] == x]
  if(length(propCov) == 1){
    d$county_covid[i] = propCov
  }
}

View(d[d$county_covid == "untouched",])
View(d[d$county_covid == "proportion",])
# NOTE THAT THERE ARE 68 UNIVERSITIES WHOSE COVID PROPS R UNFILLED

plot(d$county_covid, d$cases, xlim = c(0,0.05))




## Scraping from hronicle website (October 1, 2020)
library("RSelenium")
library("rvest")
library("tidyverse")

binman::list_versions("chromedriver")
rD <- rsDriver(browser="chrome", port=230L, verbose=T, chromever = "85.0.4183.87")
remDr <- rD[["client"]]

remDr$navigate("https://www.chronicle.com/article/heres-a-list-of-colleges-plans-for-reopening-in-the-fall/?cid2=gen_login_refresh&cid=gen_sign_in&cid2=gen_login_refresh")

# Find the next button
nextbutton_Element <-
  remDr$findElement(using = "xpath", value="/html/body/div[3]/div/main/div/article/div[3]/div/div/div/div[8]/div/div/div[1]/div[2]/div[1]/div[2]/a[2]")
# nextbutton_Element$clickElement()
Sys.sleep(2)
html <- remDr$getPageSource()[[1]]
xpath <- "/html/body/div[3]/div/main/div/article/div[3]/div/div/div/div[8]/div/div/div[1]/div[2]/table/tbody"

scraped <- matrix(0, nrow = 2958, ncol = 5)
     
for(j in 1:59){
  signals <- read_html(html) %>% # parse HTML
    html_nodes(xpath = xpath)
  
  lst = seq(2,100,2)
  for(i in 1:50){ 
    college_info = as.character(xml_child(xml_child(signals[[1]], lst[i]), 1))
    info = strsplit(strsplit(college_info, '">')[[1]][2], "<")[[1]][1]
    scraped[(50*(j-1) + i), 1] = info 
    
    pubPriv <- as.character(xml_child(xml_child(xml_child(signals[[1]], lst[i]), 1)))
    pubPriv_temp <- strsplit(strsplit(pubPriv, '">')[[1]][2], "<")[[1]][1]
    scraped[(50*(j-1) + i), 5] = pubPriv_temp 
    
    plan = as.character(xml_child(xml_child(signals[[1]], lst[i]), 3))
    p = strsplit(strsplit(plan, '">')[[1]][4],"<")[[1]][1]
    scraped[(50*(j-1) + i), 2] = p
    
    enrollment = as.character(xml_child(xml_child(signals[[1]], lst[i]), 4))
    e = strsplit(strsplit(enrollment, '">')[[1]][2],'<')[[1]][1]
    e = as.numeric(gsub(",","",e))
    scraped[(50*(j-1) + i), 3] = e
    
    county_cases = as.character(xml_child(xml_child(signals[[1]], lst[i]), 5))
    c = strsplit(strsplit(county_cases,'">')[[1]][2],"<")[[1]][1]
    c = as.numeric(gsub(",","",c))
    scraped[(50*(j-1) + i), 4] = c
  }
  
  nextbutton_Element$clickElement()
  Sys.sleep(5)
  html <- remDr$getPageSource()[[1]]
}


# Matrix to fill
which(duplicated(scraped))

# Stopped on 2950... add manually
signals <- read_html(html) %>%
  html_nodes(xpath = xpath)
lst = seq(2,16,2)
mtx <- matrix(0, nrow = 8, ncol = 5)
for(i in 1:8){ 
  college_info = as.character(xml_child(xml_child(signals[[1]], lst[i]), 1))
  info = strsplit(strsplit(college_info, '">')[[1]][2], "<")[[1]][1]
  mtx[i, 1] = info 
  
  pubPriv <- as.character(xml_child(xml_child(xml_child(signals[[1]], lst[i]), 1)))
  pubPriv_temp <- strsplit(strsplit(pubPriv, '">')[[1]][2], "<")[[1]][1]
  scraped[i, 5] = pubPriv_temp 
  
  plan = as.character(xml_child(xml_child(signals[[1]], lst[i]), 3))
  p = strsplit(strsplit(plan, '">')[[1]][4],"<")[[1]][1]
  mtx[i, 2] = p
  
  enrollment = as.character(xml_child(xml_child(signals[[1]], lst[i]), 4))
  e = strsplit(strsplit(enrollment, '">')[[1]][2],'<')[[1]][1]
  e = as.numeric(gsub(",","",e))
  mtx[i, 3] = e
  
  county_cases = as.character(xml_child(xml_child(signals[[1]], lst[i]), 5))
  c = strsplit(strsplit(county_cases,'">')[[1]][2],"<")[[1]][1]
  c = as.numeric(gsub(",","",c))
  mtx[i, 4] = c
}


final = rbind(scraped[1:2950,], mtx)
colnames(final) = c("Name", "Plan", "Enrollment", "Cases", "Type")
df <- data.frame(final)

#df <- readRDS("clare.rds")

# Wisconsin shit
df$Name[df$Name == 'University of Wisconsin at Eau Claire'] = 'University of Wisconsin-Eau Claire'
df$Name[df$Name == 'University of Wisconsin at Green Bay'] = 'University of Wisconsin-Green Bay'
df$Name[df$Name == 'University of Wisconsin at La Crosse'] = 'University of Wisconsin-La Crosse'
df$Name[df$Name == 'University of Wisconsin at Madison'] = 'University of Wisconsin-Madison'
df$Name[df$Name == 'University of Wisconsin at Milwaukee'] = 'University of Wisconsin-Milwaukee'
df$Name[df$Name == 'University of Wisconsin at Oshkosh'] = 'University of Wisconsin-Oshkosh'
df$Name[df$Name == 'University of Wisconsin at Platteville'] = 'University of Wisconsin-Platteville'
df$Name[df$Name == 'University of Wisconsin at River Falls'] = 'University of Wisconsin-River Falls'
df$Name[df$Name == 'University of Wisconsin at Stevens Point'] = 'University of Wisconsin-Stevens Point'
df$Name[df$Name == 'University of Wisconsin at Superior'] = 'University of Wisconsin-Superior'
df$Name[df$Name == 'University of Wisconsin at Whitewater'] = 'University of Wisconsin-Whitewater'



##===================
# ACTUAL HODP WORK 
##===================
# datesets that we have: 'df' and 'd'
data_fr <- d[d$college %in% df$Name,]
data_fr$Name <- data_fr$college
data_fr$college <- NULL

# df = df[c(1:905, 907:nrow(df)),] # duplicate
# df = df[c(1:1105, 1108:nrow(df)),] # duplicate

frfr <- merge(data_fr, df, by= "Name", all.x=TRUE)
frfr <- frfr[frfr$county_covid != "untouched" & frfr$county_covid != "proportion",]
frfr$county_covid <- as.numeric(frfr$county_covid)


# Pretty graphs
#### Style Guide ####
# Step 0: HODP Theme
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('hrbrthemes')) install.packages('hrbrthemes'); library(hrbrthemes)
if (!require('magick')) install.packages('magick'); library(magick)
if (!require('plotly')) install.packages('plotly'); library(plotly)
logo <- image_read("logo.png")
# Legend: https://stackoverflow.com/questions/14622421/how-to-change-legend-title-in-ggplot

monochrome <- c('#760000', '#BE1E26', '#D84742', '#FF6B61', '#FF9586')
primary <- c('#EE3838', '#FA9E1C', '#78C4D4', '#4B5973', '#E2DDDB')
sidebysidebarplot <- c("#ef3e3e", "#2c3e50")
theme_hodp <- function () { 
  theme_classic(base_size=12, base_family="Helvetica") %+replace%
    theme(
      panel.background  = element_rect(fill="#F2F2F2", colour=NA),
      plot.background = element_rect(fill="#F2F2F2", colour="#d3d3d3"),
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA),
      plot.title = element_text(size=24,  family="Helvetica", face = "bold", margin = margin(t = 0, r = 0, b = 10, l = 0)),
      plot.subtitle = element_text(size=18,  family="Helvetica", color="#717171", face = "italic", margin = margin(t = 0, r = 0, b = 10, l = 0)),
      plot.caption = element_text(size=8,  family="Helvetica", hjust = 1),
      axis.text.x =element_text(size=10,  family="Helvetica"),
      axis.title.x =element_text(size=14, family="Helvetica", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold"),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14, family="Helvetica", angle=90, face ='bold'),
      legend.title=element_text(size=10, family="Helvetica"), 
      legend.text=element_text(size=10, family="Helvetica"),
      legend.position = "bottom",
      axis.ticks = element_blank()
    )
}


a <- ggplot(data=frfr, aes(x=county_covid, y=cases)) +
  theme_hodp() 
png(".output.png", width = 480, height = 350, res = 300)
a
dev.off()







advised <- frfr
advised$Plan[advised$Plan == 'Fully in person' | advised$Plan == 'Primarily in person'] = "In Person"
advised$Plan[advised$Plan == 'Fully online' | advised$Plan == 'Primarily online'] = "Online"
advised$Plan[advised$Plan == 'Undetermined'] = "Other"
advised$casesProp <- advised$cases/as.numeric(advised$Enrollment)

high <- c('Iowa Wesleyan University', "Dickinson State University", "Adrian College")

a <- ggplot(data=advised, aes(x=county_covid, y=casesProp)) +
  geom_point(aes(colour = factor(Plan))) +
  geom_text(aes(label=ifelse(Name == high[1],as.character(Name),'')),hjust=1,vjust=-1) +
  geom_text(aes(label=ifelse(Name == high[2],as.character(Name),'')),hjust=1,vjust=-1) +
  geom_text(aes(label=ifelse(Name == high[3],as.character(Name),'')),hjust=1,vjust=-1) +
  scale_color_manual(values = c("#FA9E1C", "#EE3838", "#4B5973", "#E2DDDB"),name="Plan", labels = c('Hybrid', 'In Person', 'Online', 'Other')) +
  labs(title="Are Colleges Making Advised Reopening Plans?") +
  xlab("Proportion of COVID-19 Cases in County") +
  ylab("Number of COVID-19 Cases in College") +
  ylim(c(0,0.25)) + 
  xlim(c(0,0.065)) +
  theme(legend.position="bottom") +
  theme_hodp() 

png("./output.png", width = 2592, height = 1890, res = 300)
a
dev.off()

# Table of Ivy Cases
ivies <- c("Harvard University", "Yale University", "Princeton University", "Dartmouth College", "Brown University", "Columbia University", "University of Pennsylvania", "Stanford University", "Cornell University")
x = frfr[frfr$Name %in% ivies,]
x$casesProp <- x$cases/as.numeric(x$Enrollment)
x$date <- NULL
x$state <- NULL 
x$city <- NULL
x$ipeds_id <- NULL 
x$cases <- NULL
x$notes <- NULL 
x$state_covid <- NULL
x$mergedName <- NULL
x$Enrollment <- NULL
x$Cases <- NULL
colnames(x) <- c("University", "County Name", "County COVID-19 Proportion", "Reopening Plan", "Type", "University COVID-19 Cases")
write.csv(x, file = "./ivies.csv")

x <- read.csv("./ivies.csv", header = TRUE)[1:9,]

x <- rbind(x, x)
x$value = c(x$County.COVID.19[1:9], x$University.COVID.19[1:9])
x$Scope <- c(rep("County",9), rep("University",9))
x$County.COVID.19 <- NULL 
x$University.COVID.19 <- NULL
x$County.Name <- NULL
x$Type <- NULL

x$University <- factor(x$University, levels = c('Harvard', "Columbia", 'Princeton','UPenn',  "Brown", "Yale", 'Dartmouth',  'Stanford', 'Cornell'))
b <- ggplot(data=x, aes(x=University, y=value, fill=Scope)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_hodp() + 
  scale_fill_manual(values=c('#EE3838','#4B5973')) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust = 0.4), plot.title = element_text(size=15)) +
  ylab("Proportion of COVID-19 Cases") +
  labs(title= "Comparison of COVID-19 Proportions at the County and University Level") + 
  xlab("")
  
png("./barplot.png", width = 2592, height = 1890, res = 300)
b 
dev.off()




### Subsetting types of colleges...
frfr$Type2 <- 0 
frfr$Type2[frfr$Type == as.character(factor(frfr$Type)[1])] = 1  #"Private nonprofit, 4-year"
frfr$Type2[frfr$Type == as.character(factor(frfr$Type)[2])] = 2  #"Public, 2-year"
frfr$Type2[frfr$Type == as.character(factor(frfr$Type)[3])] = 3  #"Public, 4-year"

generalPlan <- data.frame(table(frfr$Plan))
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

png("./allPlan.png", width = 2592, height = 1890, res = 300)
pie
dev.off()



## Type 1
frfri <- frfr[frfr$Type2 == i,] # REPLACE i WITH WHATEVER YOU WANT 
generalPlan <- data.frame(table(frfri$Plan))
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
  labs(title= "Reopening Plans for 4-Year, Public Colleges") + 
  coord_polar("y") +
  xlim(0.2,2.5)

png("./allPlani.png", width = 2592, height = 1890, res = 300)
pie
dev.off()




plot(frfr$county_covid, frfr$cases_adj, col = 'navyblue', main ="How do College COVID-19 Cases Compare \n to Their Counties?", 
     xlab = "Proportion of COVID-19 Cases in County", ylab = "Proportion of COVID-19 Cases in College")
abline(frfr$county_covid, frfr$cases_adj, col = 'red')
text(0.085, 0.02, "y = 0.3085(x) + 0.0056", cex = 0.9, font = 4)




