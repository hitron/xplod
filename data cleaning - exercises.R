# EXERCISES DATA SCIENCE BOOTCAMP
# ADAPTED FROM DATACAMP.COM

# DATA CLEANING

# Following is a preview of the vector x:
x <- c("A", NA, "B", "C", NA)

# What will be the outcome?
any(is.na(x)) #think before you run the code!

# LOAD LIBRARIES
# Don't forget to install them first!
library(dplyr) # alternatively, you can also do library(tidyverse) to load the complete set of packages at once!
library(hflights)
data("hflights")
hflights <- as.tbl(hflights)


# Choosing is not losing! The select verb------------------------
# LOAD LIBRARIES
# Don't forget to install them first!
library(dplyr) # alternatively, you can also do library(tidyverse) to load the complete set of packages at once!
library(hflights)
data("hflights")
hflights <- as.tbl(hflights)

# To answer the simple question whether flight delays tend to shrink or grow during a flight, we can safely discard a lot of the variables of each flight. To select only the ones that matter, we can use select().
#
head(hflights)
# As an example, take the following call, that selects the variables var1 and var2 from the data frame df.
#
#select(df, var1, var2)
select(hflights, ArrDelay, DepDelay)
# You can also use : to select a range of variables and - to exclude some variables, similar to indexing a data.frame with square brackets. You can use both variable's names as well as integer indexes. This call selects the four first variables except for the second one of a data frame df:
#
select(df, 1:4, -2)

# select() does not change the data frame it is called on; you have to explicitly assign the result of select() to a variable to store the result.
#
# INSTRUCTIONS
# Use select() to print out a tbl that contains only the columns ActualElapsedTime, AirTime, ArrDelay and DepDelay of hflights.
selection1<- select(hflights, ActualElapsedTime, AirTime, ArrDelay, DepDelay)
print(selection1)

# Print out the columns Origin up to Cancelled of hflights
selection2<- select(hflights, 14:19)
print(head(selection2))


# Find the most concise way to select: columns Year up to and including DayOfWeek, columns ArrDelay up to and including Diverted. You can examine the order of the variables in hflights with names(hflights) in the console.
selection3<-select(hflights,-(5:11))
print(head(selection3))

# Mutating is creating----
# mutate() is the second of five data manipulation functions. mutate() creates new columns which are added to a copy of the dataset.
# 
# Take this example that adds a new column, z, which is the element-wise sum of the columns x and y, to the data frame df:
#   
#   mutate(df, z = x + y)
# 
# INSTRUCTIONS
# Create a new data frame, g1, which is the data frame hflights with an additional column: ActualGroundTime, the difference between ActualElapsedTime and AirTime. 
g1 <- mutate(hflights, ActualGroundTime=ActualElapsedTime-AirTime)
print(head(g1[21:22]))
# Extend g1 further, by adding an additional column GroundTime. This column is the sum of the TaxiIn and TaxiOut columns. Store the resulting data frame in g2. Check in the console that the GroundTime and ActualGroundTime columns are equal.
g2 <- mutate(g1, GroundTime=TaxiIn+TaxiOut)
print(head(g2[21:23])) 
# Add a new variable to g2 named AverageSpeed that denotes the average speed that each plane flew in miles per hour. Save the resulting dataset as g3. Use the following formula: Distance / AirTime * 60.
g3 <- mutate(g2, AverageSpeed=60*Distance/AirTime)
# Print out g3
print(head(g3[23:24]))  


# Logical operators-------------------------------------
# R comes with a set of logical operators that you can use inside filter():

# x < y, TRUE if x is less than y
# x <= y, TRUE if x is less than or equal to y
# x == y, TRUE if x equals y
# x != y, TRUE if x does not equal y
# x >= y, TRUE if x is greater than or equal to y
# x > y, TRUE if x is greater than y
# x %in% c(a, b, c), TRUE if x is in the vector c(a, b, c)
# The following example filters df such that only the observations for which a is positive, are kept:
#   
#   filter(df, a > 0)

# INSTRUCTIONS
# Print out all flights in hflights that traveled 3000 or more miles.
(filter(hflights,Distance>3000))
# Print out all flights in hflights flown by JetBlue or Delta.
(filter(hflights,UniqueCarrier %in% c('BG','DL')))

# Extract from hflights all flights where taxiing took longer than the actual flight. Avoid the use of mutate() and do the math directly in the logical expression of filter().
(filter(hflights, (TaxiIn+TaxiOut)>AirTime))


# Blend together what you've learned!--------------------------------------
# So far, you have learned three data manipulation functions in the dplyr package. Time for a summarizing exercise. You will generate a new dataset from the hflights dataset that contains some useful information on flights that had JFK airport as their destination. You will need select(), mutate() and filter().
# 
# INSTRUCTIONS
# First, use filter() to select the flights that had JFK as their destination and save this result to c1.
c1<-filter(hflights, Dest=='JFK')

# Second, add a new column named Date to c1: paste() together the Year, Month and DayofMonth variables, separate them by a "-" by using the sep attribute of paste(). Save the resulting data frame as c2.
(c2<-mutate(c1,Date=paste(Year,Month,DayofMonth,sep="-")))
# Finally, select some columns to provide an overview: Date, DepTime, ArrTime and TailNum, in this order. Do not assign the resulting data frame to a variable; just print it to the console.
(select(c2,Date, DepTime, ArrTime , TailNum))

# With select(), mutate() and filter(), you can already reveal interesting information from a dataset. Through a combination of these expressions or by the use of a one-liner, try to answer the following question:

# How many weekend flights flew a distance of more than 1000 miles but had a total taxiing time below 15 minutes?
nrow(filter(hflights,Distance>1000 & TaxiOut+TaxiIn<15 & DayOfWeek %in% c(6,7)))


# Arranging your data------------------------------------
# arrange() can be used to rearrange rows according to any type of data. If you pass arrange() a character variable, for example, R will rearrange the rows in alphabetical order according to values of the variable. If you pass a factor variable, R will rearrange the rows according to the order of the levels in your factor (running levels() on the variable reveals this order).
# 
# dtc has already been defined below. It's up to you to write some arrange() expressions to display its contents appropriately!
# 
# INSTRUCTIONS
# Definition of dtc
dtc <- filter(hflights, Cancelled == 1, !is.na(DepDelay))
# Arrange dtc, by departure delays so that the shortest departure delay is at the top of the data set.
(arrange(dtc,DepDelay))

# Arrange dtc so that flights that were cancelled for the same reason appear next to each other.
(arrange(dtc,CancellationCode))

# Arrange dtc so that flights by the same carrier appear next to each other. Within each carrier, flights that have smaller departure delays appear before flights that have higher departure delays. Do this in a one-liner.
(arrange(dtc,UniqueCarrier,DepDelay))


# The syntax of summarise----------------------------------------------
# summarise(), the last of the 5 verbs, follows the same syntax as mutate(), but the resulting dataset consists of a single row instead of an entire new column in the case of mutate().
# 
# In contrast to the four other data manipulation functions, summarise() does not return an altered copy of the dataset it is summarizing; instead, it builds a new dataset that contains only the summarising statistics (min, max, mean, sd, var, median...).
# 
# INSTRUCTIONS
# Use summarise() to print out a summary of hflights containing two variables: min_dist, the shortest distance flown, and max_dist, the longest distance flown.

# Print out a summary of hflights with a single variable, max_div: the longest Distance for diverted flights. You will need one of the four other verbs to do this!
summarise(hflights)



# Overview of syntax: the pipe operator (%>%)-----------------------------
# As an example of the %>%, have a look at the following two commands that are completely equivalent:
#   
mean(c(1, 2, 3, NA), na.rm = TRUE)
c(1, 2, 3, NA) %>% mean(na.rm = TRUE)
# The %>% operator allows you to extract the first argument of a function from the arguments list and put it in front of it. You can read it as '..., then...'
# 
# INSTRUCTIONS
# Use dplyr functions and the pipe operator to transform the following English sentences into R code:
#   
#   Take the hflights data set and then ...
# Add a variable named diff that is the result of subtracting TaxiIn from TaxiOut, and then ...
# Pick all of the rows whose diff value does not equal NA, and then ...
# Summarise the data set with a value named avg that is the mean diff value
hflights %>% 
  mutate(diff=TaxiIn-TaxiOut) %>%
  filter(!is.na(diff)) %>% 
  summarise(avg = mean(diff))



# Unite and conquer using group_by-----------------------------------
# group_by() lets you define groups within your data set. Its influence becomes clear when calling summarise() on a grouped dataset: summarising statistics are calculated for the different groups separately.
# 
# In this exercise, you are going to create an ordered per-carrier summary of hflights by combining group_by(), summarise() and arrange().
# 
# INSTRUCTIONS
# Use group_by() to group hflights by UniqueCarrier.
# summarise() the grouped tbl with two summary variables:
#   p_canc, the percentage of cancelled flights
# avg_delay, the average arrival delay of flights whose delay does not equal NA.
# Finally, order the carriers in the summary from low to high by their average arrival delay. Use percentage of flights cancelled to break any ties. Tip: you can use n() to get the number of observations in the current group.
hflights %>%
  group_by(UniqueCarrier) %>%
  summarise(p_canc = sum(Cancelled/n())*100,
            avg_delay = mean(ArrDelay, na.rm = T)) %>%
  arrange(avg_delay, p_canc)


# Optional: Combine group_by with mutate--------------------------------
# 
# You can also combine group_by() with mutate(). When you mutate grouped data, mutate() will calculate the new variables independently for each group. This is particularly useful when mutate() uses the rank() function, that calculates within-group rankings. rank() takes a group of values and calculates the rank of each value within the group, e.g.
# 
rank(c(21, 22, 24, 23))
# has output

# [1] 1 2 4 3
# As with arrange(), rank() ranks values from the smallest to the largest.
# 
# Instructions
# filter() the hflights tbl to only keep observations for which ArrDelay is not NA and positive.
# Use group_by() on the result to group by UniqueCarrier.
# Next, use summarise() to calculate the average ArrDelay per carrier. Call this summary variable avg.
# Feed the result into a mutate() call: create a new variable, rank, calculated as rank(avg).
# Finally, arrange by this new rank variable

hflights %>%
  filter(!is.na(ArrDelay) & ArrDelay > 0) %>%
  group_by(UniqueCarrier) %>%
  summarise(avg = mean(ArrDelay)) %>%
  mutate(rank = rank(avg)) %>%
  arrange(rank)




# If you want to practice more, go to https://www.r-exercises.com/2017/01/12/lets-get-started-with-dplyr/











