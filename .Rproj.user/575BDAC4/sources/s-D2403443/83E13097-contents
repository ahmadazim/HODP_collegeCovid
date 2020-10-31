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




## Scraping from C2i dashboard
library("RSelenium")
library("rvest")
library("tidyverse")

binman::list_versions("chromedriver")
rD <- rsDriver(browser="chrome", port=232L, verbose=T, chromever = "85.0.4183.87")
remDr <- rD[["client"]]

remDr$navigate("https://www.chronicle.com/article/heres-a-list-of-colleges-plans-for-reopening-in-the-fall/?cid2=gen_login_refresh&cid=gen_sign_in&cid2=gen_login_refresh")

# remDr$findElement(using = "id", value = "homeMap")$sendKeysToElement(list("Vassar College"))
Sys.sleep(5) # give the page time to fully load
html <- remDr$getPageSource()[[1]]

xpath <- "/html/body/div[3]/div/main/div/article/div[3]/div/div/div/div[8]/div/div/div[1]/div[2]/table/tbody"

signals <- read_html(html) %>% # parse HTML
  html_nodes(xpath = xpath)
View(signals)

# Matrix to fill
scraped <- matrix(0, nrow = 2958, ncol = 10)
colnames(scraped) <- c("pathClass", "stroke", "strokeOpacity", "strokeWidth", 'strokeLinecap', 'strokeLinejoin', 'fill', 'fillOpacity', 'fillRule', 'd') 

for(i in 1:2958){ 
  string = as.character(xml_child(xml_child(signals[[1]], 1), i))
  split = strsplit(string, "\"")
  
  scraped[i,1] <- split[[1]][2]
  scraped[i,2] <- split[[1]][4]
  scraped[i,3] <- split[[1]][6]
  scraped[i,4] <- split[[1]][8]
  scraped[i,5] <- split[[1]][10]
  scraped[i,6] <- split[[1]][12]
  scraped[i,7] <- split[[1]][14]
  scraped[i,8] <- split[[1]][16]
  scraped[i,9] <- split[[1]][18]
  scraped[i,10] <- split[[1]][20]
}

for(i in 1:10){cat(unique(scraped[,i]), '\n')}




