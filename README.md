---
title: "Shopping in VR"
author: "Christopher Davidson"
date: "2022-09-25"
output: pdf_document
---
Please check out the pdf version for a version that includes the dataframes produced by the executed code.
```{r installation, include=FALSE}
library("readr")
library("dplyr")
library("skimr")
library("janitor")
library("tidyr")
CustomerData <- read_csv2("CustomerData.csv")
CustomerData$HaveChildren <- as.logical(as.double(CustomerData$HaveChildren))
```

First of all I would like to thank the people who gave me this opportunity to prove my skills a data analyst and be able to showcase them. This is a Markdown document that will show all of the processes I went through whilst programming in R. Later, I exported the dataframe's I produced in R into PowerBI to be able to visualise the data in an easy way. Then create a report for the fictional stakeholders.

## Intro
I was asked to do a case study into Users using VR to shop. I was given a CSV containing the data from the time when these users were in VR. The CSV file does not contain time stamps when people used VR, however it does contain the amount of time users spent looking at certain products in VR. This was useful to understand how interested they were in a certain product.
I am to work in this fictional team consisting of the Product Owner, a UX designer, and another data analyst. Meaning that I will have to present my findings in a way that everyone understands.

I used R to prepare and analyse the data but then used PowerBI to visualise the data later. I thought it would be easier this way.

## Ask
In this setup, I was already given the questions. They are as follows:

1. Who is the main target group? Which segments do you identify?

2. What kind of data would I want to improve my analysis and back-up the insights I mentioned and why?

3. The team wants to develop new features that is personalised for each target market. Which target market should they focus on first?

4. Some users recorded whether they had children or not and others did not. The team is wanting to increase children product sales. They want to know which characteristics a user has that shows that they have children.

## Process
The data collected was already very clean after initially looking at it through dplyr's glimpse and skimr's skim_without_charts. Nothing was coming out as unusual.

```{r glimpse}
glimpse(CustomerData)

```

```{r Skim}
skim_without_charts(CustomerData)
```
After checking that there wasn't anything unusual. I decided to continue with two cleaning functions just in case.

```{r cleaning}
clean_names(CustomerData) #This is used to ensure that all the columns names are compatible for R to understand
get_dupes(CustomerData) #This ensures that none of the rows are duplicated and eliminates any that are
```


## Analyse

Afterwards it was time to analyse and this took some time. First, I was wanting to look at the data I had available. Each header telling me what was contained in that column.

```{r Head}
glimpse(CustomerData)
```

I figured out what I wanted to do. I wanted to look at the different type of users and what they were up to. That included sepearate dataframes for Age, Sex, Profession,children ,City, and the amount of time lived in that city. Whether that made a difference or not. I knew later I would want to go deeper into analysing users with children but for now I looked at the basics.

```{r Age}
"Age" <- `CustomerData` %>% #Have a look to see which age group is using it most
  group_by(`Age`) %>%
  summarise(AgeCount =  n_distinct(CustomerID))
print(Age)
```
I realised at this point that numbers were so low, so I just wanted to make sure that the data I was looking at was correct.

```{r Doublecheck}
`CustomerData` %>% #Only 5891 customers out of 537,577 rows. I'll double check these numbers with the slower sheets file I got open. Sheets says the same when I do the function =COUNTUNIQUE()
  summarise(CustCount = n_distinct(CustomerID))
```
There was only 5891 customers out of 537,577 rows. I double checked the data using the =COUNTUNIQUE function in sheets and found that I got the same number. It was correct, there was just under 6000 people who tested this product.

From the data, it was also clear that **it was mostly 26-35 year olds that wanted to use VR**. You can see this more clearly in the report I created in PowerBI showing all the visuals.

```{r Age Time}
"AgeTime" <- `CustomerData` %>% #The older people get, the longer they look at the article
  group_by(`Age`) %>%
  summarise(AvgTime = mean(Amount))
print(AgeTime)
```

This was interesting. **The older people got, the more time they would spend looking at individual products.**

```{r Sex}
"Sex" <- `CustomerData` %>% #Over double the amount of people using this VR experience are male and men spend longer looking at products. Around 70% are men and 30% are women.
  group_by(`Sex`) %>%
  summarise(SexCount =  n_distinct(CustomerID),AvgTime = mean(Amount)) %>%
  mutate(CountPerc = SexCount / sum(SexCount)*100) %>%
  mutate(TimePerc = AvgTime / sum(AvgTime)*100)
print(Sex)
```

I decided to put a percentage in this one that would compare the sum of the column with the smaller part. Showing that around **70% of the users using VR are men and just under 30% were female**. **Men even spent a little longer on average on looking at the product than women.**

```{r Profession}
"Prof" <-`CustomerData` %>%
  group_by(`Profession`) %>%
  summarise(Count = n_distinct(CustomerID), AvgTime = (mean(Amount)))%>%
  mutate(CountPerc = (Count / sum(Count)*100)) %>%
  mutate(TimePerc = (AvgTime / sum(AvgTime)*100)) %>%
  arrange(desc(CountPerc))
print(Prof)
```

This was a large file, but generally I could see that there were **people with certain professions who would be more interested in VR.** However, *the dataset was so small and it was a test run in the stores, there can be other contributing factors at play. These could include people with shift work not being able to make it on the day the store is showcasing VR or the other way around. Perhaps there could be a lot of co-workers who work in the store and surrounding stores whoc used the VR on their breaks.* If I was to look into this again, I would want to know the types of professions these people do and see if the data coincides with professional interests. For example, 3D designers could be interested in VR because the want to look at the technology progression so they could perhaps buy a VR headset for work. Or perhaps game creators are just naturally interested in the technology.

```{r City Type}
"City" <- `CustomerData` %>%
  group_by(`CityType`) %>%
  summarise(Count = n_distinct(CustomerID), AvgTime = (mean(Amount)))
print(City)
```

If this was a real study, I would ask what they City Types represent. Whether that be particular cities, city density and so on. Or if it is something else. However, the data clearly shows that **people are a lot more interested in VR in type C.** However, the tricky part is that we don't know details. *Details we would want to understand this would be what City Types represent, the amount of time VR was shown in these cities, the amount of staff available to assist customers with VR and so on.*

```{r Local}
"Local" <-`CustomerData` %>%
  group_by(`YearsInCity`) %>%
  summarise(CountA = n_distinct(ifelse(CityType == "A",CustomerID, NA),na.rm = T),AvgTimeA = mean(ifelse(CityType == "A",CustomerID, NA), na.rm = T),CountB = n_distinct(ifelse(CityType == "B",CustomerID,NA),na.rm = T),AvgTimeB = mean(ifelse(CityType == "B",CustomerID, NA), na.rm = T) ,CountC = n_distinct(ifelse(CityType == "C",CustomerID,NA),na.rm = T),AvgTimeC = mean(ifelse(CityType == "C",CustomerID, NA), na.rm = T),TCount = n_distinct(CustomerID),TAvgTime = (mean(Amount)))
print(Local)

```

Next was just a little bit more complicated. I wanted to check the time that each user had lived in the city and how that was to effect the likelihood of them wanting to try VR. As you can see from the data frame or in the PowerBI report, you can clearly see that **people who have lived in the city for more than a year are more likely to try VR.** I know that this is not a report to speculate in, however, this probably is due to people who have just moved in are looking to settle so too busy, people who are 2+ years area already settled so not looking for any new experiences.
Each of the counts with letters represent different cities and from the data we can see that it doesn't matter which city you live in, you are still more likely to try VR if you have lived in that city for 1-2 years.

```{r TKids}
"TKids" <- `CustomerData` %>%
  group_by(`HaveChildren`) %>%
  drop_na(HaveChildren) %>%
  summarise(Count = n_distinct(CustomerID), AvgTime = (mean(Amount))) %>%
  mutate(CountPerc = (Count / sum(Count)*100)) %>%
  mutate(TimePerc = (AvgTime / sum(AvgTime)*100))
print(TKids)
```

Now here comes the simple question of how many people who used VR have kids. I calculated the percentage on the total sum of the other columns since it would give me a more accurate reading.It is clear that just **under 60% of people who used VR didn't have children and just over 40% did.**

```{r Kid Count}
"KidCount" <- `CustomerData` %>% #Tried with item ID, ended up with 3623 rows, so trying with category instead
  group_by(`ItemCategory1`) %>%
  summarise(WithKids = n_distinct(ifelse(HaveChildren == TRUE,CustomerID, NA),na.rm = T),WOKids = n_distinct(ifelse(HaveChildren == FALSE,CustomerID, NA),na.rm = T)) %>%
  mutate(WithKidsPerc = WithKids / sum(WithKids)*100) %>% 
  mutate(WOKidsPerc = WOKids / sum(WOKids)*100) %>% 
  mutate(KidsPercDif = WithKidsPerc-WOKidsPerc) %>%
  arrange(KidsPercDif)
print(KidCount)
```

This is something I didn't use in my final report since it is showing a **0.5% difference between people with and without children.** I considered this statistically insignificant and thought this data isn't useful.

```{r KidTime}
"KidTime" <- `CustomerData` %>% #I'm not comfortable with these variables. These numbers are too tight. Maybe the second category can give me some light
  group_by(`ItemCategory1`) %>%
  summarise(WithKids = mean(ifelse(HaveChildren == TRUE,Amount, NA),na.rm = T),WOKids = mean(ifelse(HaveChildren == FALSE,Amount, NA),na.rm = T)) %>%
  mutate(WithKidsPerc = WithKids / (WithKids+WOKids)*100) %>% 
  mutate(WOKidsPerc = WOKids / (WithKids+WOKids)*100) %>%
  mutate(KidsPercDif = WithKidsPerc-WOKidsPerc) %>% #Something has gone wrong with this difference, however I think I will leave it since I can clearly see the data anyway
  arrange(KidsPercDif)
print(KidTime)
```
```{r KidTime2}
"KidTime2" <- `CustomerData` %>% 
  group_by(`ItemCategory2`) %>%
  summarise(WithKids = mean(ifelse(HaveChildren == TRUE,Amount, NA),na.rm = T),WOKids = mean(ifelse(HaveChildren == FALSE,Amount, NA),na.rm = T)) %>%
  mutate(WithKidsPerc = WithKids / (WithKids+WOKids)*100) %>% 
  mutate(WOKidsPerc = WOKids / (WithKids+WOKids)*100) %>%
  mutate(KidsPercDif = WithKidsPerc-WOKidsPerc) %>%
  arrange(KidsPercDif)
print(KidTime2)
```
```{r KidTime3}
"KidTime3" <-`CustomerData` %>% 
  group_by(`ItemCategory3`) %>%
  summarise(WithKids = mean(ifelse(HaveChildren == TRUE,Amount, NA),na.rm = T),WOKids = mean(ifelse(HaveChildren == FALSE,Amount, NA),na.rm = T)) %>%
  mutate(WithKidsPerc = WithKids / (WithKids+WOKids)*100) %>% 
  mutate(WOKidsPerc = WOKids / (WithKids+WOKids)*100) %>%
  mutate(KidsPercDif = WithKidsPerc-WOKidsPerc) %>%
  arrange(KidsPercDif)
print(KidTime3)
```
All these dataframes are looking at the different product catagories and explores the likelihood of people looking at these products categories when they have kids or not. It turns out that there wasn't a single catagory in any of Category types that really stood out as having one group or the other looking at those items more. **The most the difference even got was 2% in KidTime2 which I didn't consider enough to chase.**

```{r KidTimeArt}
"KidTimeArt" <-`CustomerData` %>% #Tried with item ID, ended up with 3623 rows, so trying with category instead, could put this in a line graph, see what we have
  group_by(`ItemID`) %>%
  summarise(WithKids = mean(ifelse(HaveChildren == TRUE,(Amount), NA),na.rm = T),WOKids = mean(ifelse(HaveChildren == FALSE,(Amount), NA),na.rm = T)) %>%
  mutate(WithKidsPerc = (WithKids / (WithKids+WOKids)*100)) %>% 
  mutate(WOKidsPerc = (WOKids / (WithKids+WOKids)*100)) %>% 
  mutate(KidsPercDif = WithKidsPerc-WOKidsPerc)%>% 
  mutate(Dif = WithKids-WOKids) %>%
  arrange(desc(KidsPercDif)) #I want to put this in Ascending to check the Null's
  #arrange(Dif) #I had to scroll down for the Nulls, however the Null's were so small, I thought I should look at the dataframe overall
#Success, the item with ID P00131842 was looked at 14159 more miliseconds by people with kids. I should take the everything where people with kids looked at the item 2000 miliseconds longer than people without kids and add the ones which people with kids looked at. Take that data and be able to assess if the others have kids or not.
print(KidTimeArt)
```

I instead looked at each individual article. **I found there were quite a few articles where people with kids would look at those longer than people without.** This could well predict whether people do have kids or not if we don't have that data.

```{r KidAgeCount}
"KidAgeCount" <-`CustomerData` %>% #The older people get, the more likely they are to have kids
  group_by(`Age`) %>%
  summarise(CountKids = n_distinct(ifelse(HaveChildren == TRUE,CustomerID, NA),na.rm = T),CountWOKids = n_distinct(ifelse(HaveChildren == FALSE,CustomerID,NA),na.rm = T)) %>%
  mutate(WithKidsPerc = (CountKids / (CountKids+CountWOKids))*100) %>% 
  mutate(WOKidsPerc = (CountWOKids / (CountKids+CountWOKids))*100) %>%
  mutate(dif = WithKidsPerc - WOKidsPerc)
print(KidAgeCount)
```
I then looked at people of differenct age groups and the liklihood of them having kids. It was immediately clear **The older the user gets, the more likely they are to have kids. Dropping off when we looked at people over the age of 55.** You can clearly see this in the report.

```{r KidCityYearCount}
"KidCityYearsCount" <-`CustomerData` %>% #There doesn't seem to be a difference when looking at years in the city
  group_by(`YearsInCity`) %>%
  summarise(CountKids = (n_distinct(ifelse(HaveChildren == TRUE,CustomerID, NA),na.rm = T)),CountWOKids = (n_distinct(ifelse(HaveChildren == FALSE,CustomerID,NA),na.rm = T))) %>%
  mutate(WithKidsPerc = (CountKids / sum(CountKids))*100) %>% 
  mutate(WOKidsPerc = (CountWOKids / sum(CountWOKids))*100) %>%
  mutate(dif = WithKidsPerc - WOKidsPerc)
print(KidCityYearsCount)
```

The last dataframe was looking deeper into this idea of the users having children and whether the amount of time the user has lived in the city makes a difference as to whether they had children or not. The result was **a very small difference between years in the city and whether or not the people were more likely to have kids**. I was going this way because I was thinking whether users who have just moved to the city were more likely to be travellers who never settled. But I couldn't find any data to back that theory up.

## Share
I exported all the DataFrames as CSV files to be pulled into PowerBI to throw together the report that shows all the visuals. I realised that this is probably the most efficent way of doing things since writing out code to show the visuals is very time consuming. 

```{r CSV, echo=TRUE}
write.csv(Age,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/Age.csv",row.names = FALSE)
write.csv(AgeTime,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/AgeTime.csv",row.names = FALSE)
write.csv(Sex,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/Sex.csv",row.names = FALSE)
write.csv(Prof,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/Prof.csv",row.names = FALSE)
write.csv(City,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/City.csv",row.names = FALSE)
write.csv(Local,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/Local.csv",row.names = FALSE)
write.csv(TKids,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/TKids.csv",row.names = FALSE)
write.csv(KidTimeArt,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/KidTimeArt.csv",row.names = FALSE)
write.csv(KidAgeCount,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/KidAgeCount.csv",row.names = FALSE)
write.csv(CustomerData,"C:/Users/chris/Documents/R/IkeaInterview220922/Ikea Interview/CSV/CustomerData.csv",row.names = FALSE)

```

### Answering Question 1 - Who is the main target group? Which segments do you identify?:
The Segments I identify are:

1. Men used VR more often and spent more time looking at the products

2. Aged 26-35, the younger also enjoyed VR and so did the older equally. Then it wavered off after the age of 50.

3. The professions that enjoyed VR most included 4,0,7 and 1.

4. The city type that enjoyed VR the most was type C, more of them using it and looking at products longer. Followed by B, then at the bottom was A

5. The majority of people trying out VR don't have kids

6. There are specific products that people with kids prefer to look at but the list is too long to list here

7. People between the ages of 51 to 55 appear to spend more time on average looking at each product in VR

### Answering Question 3 - The team wants to develop new features that is personalised for each target market. Which target market should they focus on first?
The right target group to create VR products for are males aged 26-35 living in cities catagorised as "C" having lived there for over 1 year but below 2 years, doesn't have children and profession falls into catagory 4.

### Answering Question 4 - Some users recorded whether they had children or not and others did not. The team is wanting to increase children product sales. They want to know which characteristics a user has that shows that they have children.
I looked into a variety of different tells that could tell us if someone had kids or not. The two segments that really stood out to me are as follows:

1. There were certain products users with kids would spend longer looking at. The list is too long to list here.

2. People were most likely to have kids if they were between the ages of 51-55 and the chance of people having kids goes down with age until its extremely unlikely that people have kids if they are a teenager. 


## Act

Question 2 asked me what kind of data would I want to improve my analysis and back-up the insights I mentioned and why? There are several bits of data I would like:

1. I would like to know the **time and date** when the users were trying out the VR headset to pull in other factors that could contribute to why certain groups were more likely to try VR then others

2. I would also love to have the data **revealing the professions each of the numbers represent** to know more certainly about whether they are working in the shop or surrounding shops and other factors.

3. I would like to know **what each of the City Types represent**, to understand whether they were multiple cities or just one. Whether the type was judged on density of the city, location or something else. All of this data would help build a better profile on the customer which is using the VR. Knowing the types of connections that city has and other variables that may not have been considered when assigning this city type.

The dataset was quite small with only a couple thousand participants. However, if I was working for this company, I would hope that these experiments would continue before they release the final product. I still feel that there is some more data needed to get a clear picture as to who our target market should be. This includes time and date when the VR was tried and more details on parts assigned codes instead of named. But overall, I do feel that the suggestions I included would help any team trying to improve their product.

Thankyou for reading.

