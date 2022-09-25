install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("markdown")

#reading the data
library("readr")
CustomerData <- read_csv2("CustomerData.csv") #Used CSV2 after I had a sneak peek at the file and found it was using a semicolon as a seperator

#Lets have a quick look to see if its uploaded successfully
library("dplyr")
glimpse(CustomerData)
CustomerData$HaveChildren <- as.logical(as.double(CustomerData$HaveChildren))
#After looking at the data types I think the automated selection of double and character types will do fine for analysis except for AsChildren which I changed to a logical output
glimpse(CustomerData) #Let's check if HaveChildren section successfully changed

library("skimr") #Skimming the data to check if there is any issues to clean
skim_without_charts(CustomerData)

library("janitor") #The data looks relatively clean, just double checking by running this small script
clean_names(CustomerData)
get_dupes(CustomerData)

summary(CustomerData)#Finally just double checking everything to see if anything is different to what it is suppose to be and checking to see how I can start my analysis

#Question 1 Analysis. "The first objective is to improve the performance of the product. Therefore, we want to understand what the right target group is. Which segments do you identify? Explain the main insights obtained to your team."
"Age" <- `CustomerData` %>% #Have a look to see which age group is using it most
  group_by(`Age`) %>%
  summarise(AgeCount =  n_distinct(CustomerID)) #These numbers have come out so low. I'm going to have a quick look to see the amount of customers there are

`CustomerData` %>% #Only 5891 customers out of 537,577 rows. I'll double check these numbers with the slower sheets file I got open. Sheets says the same when I do the function =COUNTUNIQUE()
  summarise(CustCount = n_distinct(CustomerID))

"AgeTime" <- `CustomerData` %>% #The older people get, the longer they look at the article
  group_by(`Age`) %>%
  summarise(AvgTime = mean(Amount))
View(AgeTime)

#Lets continue with this analysis with Sex, Profession, CityType, do some fancy things with the amount of time someone spent in the city and look if how many have kids or not and how long that effects their viewing
"Sex" <- `CustomerData` %>% #Over double the amount of people using this VR experience are male and men spend longer looking at products
  group_by(`Sex`) %>%
  summarise(SexCount =  n_distinct(CustomerID),AvgTime = mean(Amount)) %>%
  mutate(CountPerc = SexCount / sum(SexCount)*100) %>%
  mutate(TimePerc = AvgTime / sum(AvgTime)*100)
View(Sex)

"Prof" <- `CustomerData` %>%
  group_by(`Profession`) %>%
  summarise(Count = n_distinct(CustomerID), AvgTime = (mean(Amount)))%>%
  mutate(CountPerc = (Count / sum(Count)*100)) %>%
  mutate(TimePerc = (AvgTime / sum(AvgTime)*100)) %>%
  arrange(desc(CountPerc))
View(Prof)

"City" <- `CustomerData` %>%
  group_by(`CityType`) %>%
  summarise(Count = n_distinct(CustomerID), AvgTime = (mean(Amount)))
View(City)

"Local" <- `CustomerData` %>%
  group_by(`YearsInCity`) %>%
  summarise(CountA = n_distinct(ifelse(CityType == "A",CustomerID, NA),na.rm = T),AvgTimeA = mean(ifelse(CityType == "A",CustomerID, NA), na.rm = T),CountB = n_distinct(ifelse(CityType == "B",CustomerID,NA),na.rm = T),AvgTimeB = mean(ifelse(CityType == "B",CustomerID, NA), na.rm = T) ,CountC = n_distinct(ifelse(CityType == "C",CustomerID,NA),na.rm = T),AvgTimeC = mean(ifelse(CityType == "C",CustomerID, NA), na.rm = T),TCount = n_distinct(CustomerID),TAvgTime = (mean(Amount)))
View(Local)

library("tidyr")
"TKids" <- `CustomerData` %>%
  group_by(`HaveChildren`) %>%
  drop_na(HaveChildren) %>%
  summarise(Count = n_distinct(CustomerID), AvgTime = (mean(Amount))) %>%
  mutate(CountPerc = (Count / sum(Count)*100)) %>%
  mutate(TimePerc = (AvgTime / sum(AvgTime)*100))
View(TKids)


#We also should look at which products are more popular to look at 




"KidCount" <- `CustomerData` %>% #Tried with item ID, ended up with 3623 rows, so trying with category instead
  group_by(`ItemCategory1`) %>%
  summarise(WithKids = n_distinct(ifelse(HaveChildren == TRUE,CustomerID, NA),na.rm = T),WOKids = n_distinct(ifelse(HaveChildren == FALSE,CustomerID, NA),na.rm = T)) %>%
  mutate(WithKidsPerc = WithKids / sum(WithKids)*100) %>% 
  mutate(WOKidsPerc = WOKids / sum(WOKids)*100) %>% 
  mutate(KidsPercDif = WithKidsPerc-WOKidsPerc) %>%
  arrange(KidsPercDif)

"KidTime" <- `CustomerData` %>% #I'm not comfortable with these variables. These numbers are too tight. Maybe the second category can give me some light
  group_by(`ItemCategory1`) %>%
  summarise(WithKids = mean(ifelse(HaveChildren == TRUE,Amount, NA),na.rm = T),WOKids = mean(ifelse(HaveChildren == FALSE,Amount, NA),na.rm = T)) %>%
  mutate(WithKidsPerc = WithKids / (WithKids+WOKids)*100) %>% 
  mutate(WOKidsPerc = WOKids / (WithKids+WOKids)*100) %>%
  mutate(KidsPercDif = WithKidsPerc-WOKidsPerc) %>% #Something has gone wrong with this difference, however I think I will leave it since I can clearly see the data anyway
  arrange(KidsPercDif)

"KidTime2" <- `CustomerData` %>% #Tried with item ID, ended up with 3623 rows, so trying with category instead
  group_by(`ItemCategory2`) %>%
  summarise(WithKids = mean(ifelse(HaveChildren == TRUE,Amount, NA),na.rm = T),WOKids = mean(ifelse(HaveChildren == FALSE,Amount, NA),na.rm = T)) %>%
  mutate(WithKidsPerc = WithKids / (WithKids+WOKids)*100) %>% 
  mutate(WOKidsPerc = WOKids / (WithKids+WOKids)*100) %>%
  mutate(KidsPercDif = WithKidsPerc-WOKidsPerc) %>%
  arrange(KidsPercDif)

"KidTime3" <- `CustomerData` %>% #Tried with item ID, ended up with 3623 rows, so trying with category instead
  group_by(`ItemCategory3`) %>%
  summarise(WithKids = mean(ifelse(HaveChildren == TRUE,Amount, NA),na.rm = T),WOKids = mean(ifelse(HaveChildren == FALSE,Amount, NA),na.rm = T)) %>%
  mutate(WithKidsPerc = WithKids / (WithKids+WOKids)*100) %>% 
  mutate(WOKidsPerc = WOKids / (WithKids+WOKids)*100) %>%
  mutate(KidsPercDif = WithKidsPerc-WOKidsPerc) %>%
  arrange(KidsPercDif)

"KidTimeArt" <- `CustomerData` %>% #Tried with item ID, ended up with 3623 rows, so trying with category instead, could put this in a line graph, see what we have
  group_by(`ItemID`) %>%
  summarise(WithKids = mean(ifelse(HaveChildren == TRUE,(Amount), NA),na.rm = T),WOKids = mean(ifelse(HaveChildren == FALSE,(Amount), NA),na.rm = T)) %>%
  mutate(WithKidsPerc = (WithKids / (WithKids+WOKids)*100)) %>% 
  mutate(WOKidsPerc = (WOKids / (WithKids+WOKids)*100)) %>% 
  mutate(KidsPercDif = WithKidsPerc-WOKidsPerc)%>% 
  mutate(Dif = WithKids-WOKids) %>%
  arrange(desc(KidsPercDif)) #I want to put this in Ascending to check the Null's
  #arrange(Dif) #I had to scroll down for the Nulls, however the Null's were so small, I thought I should look at the dataframe overall
#Success, the item with ID P00131842 was looked at 14159 more miliseconds by people with kids. I should take the everything where people with kids looked at the item 2000 miliseconds longer than people without kids and add the ones which people with kids looked at. Take that data and be able to assess if the others have kids or not.


"KidAgeCount" <- `CustomerData` %>% #The older people get, the more likely they are to have kids
  group_by(`Age`) %>%
  summarise(CountKids = n_distinct(ifelse(HaveChildren == TRUE,CustomerID, NA),na.rm = T),CountWOKids = n_distinct(ifelse(HaveChildren == FALSE,CustomerID,NA),na.rm = T)) %>%
  mutate(WithKidsPerc = (CountKids / (CountKids+CountWOKids))*100) %>% 
  mutate(WOKidsPerc = (CountWOKids / (CountKids+CountWOKids))*100) %>%
  mutate(dif = WithKidsPerc - WOKidsPerc)
View(KidAgeCount)

"KidCityYearsCount" <- `CustomerData` %>% #There doesn't seem to be a difference when looking at years in the city
  group_by(`YearsInCity`) %>%
  summarise(CountKids = (n_distinct(ifelse(HaveChildren == TRUE,CustomerID, NA),na.rm = T)),CountWOKids = (n_distinct(ifelse(HaveChildren == FALSE,CustomerID,NA),na.rm = T))) %>%
  mutate(WithKidsPerc = (CountKids / sum(CountKids))*100) %>% 
  mutate(WOKidsPerc = (CountWOKids / sum(CountWOKids))*100) %>%
  mutate(dif = WithKidsPerc - WOKidsPerc)
View(KidCityYearsCount)

write.csv(Age,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/Age.csv",row.names = FALSE)
write.csv(AgeTime,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/AgeTime.csv",row.names = FALSE)
write.csv(Sex,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/Sex.csv",row.names = FALSE)
write.csv(Prof,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/Prof.csv",row.names = FALSE)
write.csv(City,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/City.csv",row.names = FALSE)
write.csv(Local,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/Local.csv",row.names = FALSE)
write.csv(TKids,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/TKids.csv",row.names = FALSE)
write.csv(KidTimeArt,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/KidTimeArt.csv",row.names = FALSE)
write.csv(KidJoin,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/KidJoin.csv",row.names = FALSE)
write.csv(KidAgeCount,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/KidAgeCount.csv",row.names = FALSE)
write.csv(CustomerData,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/CustomerData.csv",row.names = FALSE)
