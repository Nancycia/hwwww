#Name "Lintong Li"
## The purpose of this R script is to get you started on the
## midterm project. 

library(tidyverse)
library(magrittr)
library(readxl)


## Start by reading the data
strawb <- read_xlsx("C:/Users/Cici/Desktop/22fallhw/615hw/615hw/strawberries-2022oct30-a.xlsx", col_names = T)

## Get the column names and index them
cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]

## Explore data by viewing it in R.  
## Double click the strawb data frame to lauch the view() function.
## The data frame has 1008 rows, so you can't get very far by
## simply scrolling around.  But, you can generate some initial
## questions to help you explore using R functions from the
## tidyverse.  
##
## It looks like some of the columns may be blank or may contain 
## a single unique value.  These columns can be eliminated without 
## losing any information.

## Start by examining the content of the columns

## Column 1 contains two unique values.  
## Retain column 1 -- those values might be needed.
unique(strawb[1])

## Column 2 -- contains the years included in this dataset.
## Keep column 2, of course.
unique(strawb[2])

## Column 3 -- contains the time periods covered by in the dataset.
## There's only one -- years.  No info here.  Drop it
unique(strawb[3])

## you don't have to do this one column at a time.
## Note that the cells of columns that are empty contain NA, so
## the number of unique values in these columns is 1, just 
## like column_3.

## Set T as an indicator
T <- NULL

## Collect number of unique rows in each column
for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}

## Use T to select columns to drop -- 
drop_cols <- cnames[which(T == 1)]

## Now, drop the columns with only one unique value.
strawb %<>% select(!all_of(drop_cols))

## Let's arrange the data frame by year and state.
strawb %<>% arrange(Year, State)


## Look at the strawb data frame again. You can see that the 
## columns need work. The State ANSI column contains a unique
## code for each state. If you need to access US Census data for
## the states, this code will come in handy.

colnames(strawb)

## now look at the `Data Item` column

temp1 <- strawb %>% select(`Data Item`) %>% 
         distinct()

## Look at temp1!  There's a lot going on there.
## In fact, it's at least three columns packed into one.
## Use separate() to split it up

## When you run this code you can see that there are 
## some rows where `Data Item` has 4 comma-separated 
## data items.  Look at the warning on the Console 
## after 

strawb2 <- strawb %>% separate(col=`Data Item`,
                into = c("Strawberries", "items", "units"),
                sep = ",",
                fill = "right")

## try 4 columns

strawb3 <- strawb %>% separate(col=`Data Item`,
            into = c("Strawberries", "type", "items", "units"),
                               sep = ",",
                               fill = "right")

## That worked. Clean up the dat.

rm(strawb2, strawb3)

strawb %<>% separate(col=`Data Item`,
                    into = c("Strawberries", "type", "items", "units"),
                    sep = ",",
                    fill = "right")


## now explore the new columns

## we know that "THIRAM" is a chemical in the data, so
## test for it to check out the way code
r_thiram <- grep("THIRAM", strawb$`Domain Category`)
r_thiram_1 <- grep("Thiram", 
                   strawb$`Domain Category`, 
                   ignore.case = T)

## Chemicals mentioned in 
## the "Shoppers Guide to Pesticides in Produce"
## Carbendazim, Bifenthrin, methyl bromide, 1,3-dichloropropene,
## chloropicrin, Telone

df_carbendazim <- grep("carbendazim", 
                       strawb$`Domain Category`, ignore.case = T)

## Bifenthrin found 27
df_Bifenthrin <- grep("Bifenthrin", 
                       strawb$`Domain Category`, ignore.case = T)

## methyl bromide found 3
df_methyl_bromide <- grep("methyl bromide", 
                      strawb$`Domain Category`, ignore.case = T)

## 1,3-dichloropropene empty
df_1_3_dichloropropene <- grep("1,3-dichloropropene", 
                          strawb$`Domain Category`, 
                          ignore.case = T)

## chloropicrin found 18
df_chloropicrin <- grep("chloropicrin", 
                               strawb$`Domain Category`, 
                               ignore.case = T)

## Telone empty
df_Telone <- grep("Telone", 
                        strawb$`Domain Category`, 
                        ignore.case = T)




#1
# 1 CWT = 0.016 USD
dollars <- 285*0.016
#$4.56
# 1 CWT = 0.056 tons
tons <- 285*0.056
#15.96
# 1 CWT = 100 lbs
lbs <- 285*100
#28500
# 1 CWT = 50.80234544 K
k <- 285*50.80234544
#14478.6684504
# 1 CWT = 1 600 OZ
oz <- 285*1600
#456000

#2
strawb_organic <- strawb %>% filter(Domain == "ORGANIC STATUS")
strawb_organic_ca <- strawb_organic %>% filter(State == "CALIFORNIA"& Value != "(NA)" & Value != "(D)" & Year==2016)

# Compute the size
n <- length(strawb_organic_ca$Value)
# Find the mean
mean <- strawb_organic_ca$Value[1]
# Find the standard error
se <- 13.7
alpha = 0.05
freedom = n - 1
t_score = qt(p=alpha/2, df=freedom,lower.tail=F)
# Find the confidence interval
lower_bound <- mean - t_score * se
upper_bound <- mean + t_score * se
print(c(lower_bound,upper_bound))
#(231304921,231304991)


#3
strawb_norganic <- strawb %>% filter(Domain != "ORGANIC STATUS")
strawb_norganic_ca <- strawb_norganic %>% filter(State == "CALIFORNIA"& Value != "(NA)" & Value != "(D)" & Year==2016)
# Compute the size
n <- length(strawb_norganic_ca$Value)
# Calculate the mean 
strawb_norganic_ca$Value <- as.numeric(strawb_norganic_ca$Value)
mean_value <- mean(strawb_norganic_ca$Value)
# Find the standard deviation
std <- sd(strawb_norganic_ca$Value)
# Find the standard error
se <- std / sqrt(n)
alpha = 0.05
freedom = n - 1
t_score = qt(p=alpha/2, df=freedom,lower.tail=F)
# Calculating lower bound and upper bound
lower_bound <- mean_value - t_score * se
upper_bound <- mean_value + t_score * se

# Print the confidence interval
print(c(lower_bound,upper_bound))
#(-10194.61,196458.73)

#4
strawb4 <- strawb %>% separate(col=`Domain`,
                               into = c("chemical", "detail"),
                               sep = ",",
                               fill = "right")
chemical <- strawb4 %>% filter(chemical %in% c("CHEMICAL", "FERTILIZER"))
chemical_differ <- unique(chemical["Domain Category"])
ttl <- grep("TOTAL", 
                strawb4$`Domain Category`, 
                        ignore.case = T)
count(chemical_differ)-length(ttl)
#175-36=139

#5
chemical_ca <- chemical %>% filter(State == "CALIFORNIA")
chemical_ca_differ <- unique(chemical_ca ["Domain Category"])
ttl1 <- grep("TOTAL", 
           chemical_ca$`Domain Category`, 
           ignore.case = T)
chemical_fl <- chemical %>% filter(State == "FLORIDA")
chemical_fl_differ <- unique(chemical_fl ["Domain Category"])
ttl2 <- grep("TOTAL", 
             chemical_fl$`Domain Category`, 
             ignore.case = T)
ca <- count(chemical_ca_differ)-length(ttl1)
fl <- count(chemical_fl_differ)-length(ttl2)
ca-fl
#126-103=23
