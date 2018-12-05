#loading libraries for future use

library(dplyr)
library(tidyr)

#################### CHECKPOINT 1 #############################################

#Downloading company data from the link 'https://cdn.upgrad.com/UpGrad/temp/d934844e-5182-4b58-b896-4ba2a499aa57/companies.txt'.  

company_data_url <- "https://cdn.upgrad.com/UpGrad/temp/d934844e-5182-4b58-b896-4ba2a499aa57/companies.txt"
download.file(company_data_url, "companies.txt")

# Reading from txt file 'Company_data.txt'

companies  <- read.delim("companies.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
View(companies)

#  Reading from csv file 'rounds2.csv'

rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE)
View(rounds2)

# converting the permalink in rounds2 table to lower case as there are multiple same values in 
# different case due to which inconsistent answers are coming.

rounds2$company_permalink <- tolower(rounds2$company_permalink)
companies$permalink <- tolower(companies$permalink)

# How many unique companies are present in rounds2?	

nrow(distinct(rounds2, company_permalink))  # will give us the exact number of rows 

# How many unique companies are present in companies?	

nrow(distinct(companies, permalink))

# Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame?

master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink", all = TRUE)

View(master_frame)


# length(unique(master_frame$company_permalink)) : To find unique companies in master_frame

#################### CHECKPOINT 2 #############################################

# Average funding amount of venture type:	

venture_funding_type <- subset(master_frame, master_frame$funding_round_type=="venture")
View(venture_funding_type)
nrow(venture_funding_type)

## Average replacing the NA values with 0 
venture_funding_type_mean_1 <- mean(replace(venture_funding_type$raised_amount_usd, is.na(venture_funding_type$raised_amount_usd), 0))
venture_funding_type_mean_1

## Average keeping the na.rm = true and not replacing it with any value
venture_funding_type_mean_2 <- mean(venture_funding_type$raised_amount_usd, na.rm = TRUE)
venture_funding_type_mean_2

write.csv(venture_funding_type, file = "venture_funding_type.csv")

#  Average funding amount of angel type:

angel_funding_type <- subset(master_frame, master_frame$funding_round_type=="angel")
View(angel_funding_type)
nrow(angel_funding_type)

## Average replacing the NA values with 0
angel_funding_type_mean_1 <- mean(replace(angel_funding_type$raised_amount_usd, is.na(angel_funding_type$raised_amount_usd), 0))
angel_funding_type_mean_1

## Average keeping the na.rm = true and not replacing it with any value
angel_funding_type_mean_2 <- mean(angel_funding_type$raised_amount_usd, na.rm = TRUE)
angel_funding_type_mean_2


# Average funding amount of seed type:

seed_funding_type <- subset(master_frame, master_frame$funding_round_type == "seed")
nrow(seed_funding_type)

## Average replacing the NA values with 0
seed_funding_type_mean_1 <- mean(replace(seed_funding_type$raised_amount_usd, is.na(seed_funding_type$raised_amount_usd), 0))
seed_funding_type_mean_1

## Average keeping the na.rm = true and not replacing it with any value 
seed_funding_type_mean_2 <- mean(seed_funding_type$raised_amount_usd, na.rm = TRUE)
seed_funding_type_mean_2

#  Average funding amount of private equity type:
private_equity_funding_type <- subset(master_frame, master_frame$funding_round_type == "private_equity")

## Average replacing the NA values with 0:
private_equity_funding_type_mean_1 <- mean(replace(private_equity_funding_type$raised_amount_usd, is.na(private_equity_funding_type$raised_amount_usd), 0))
private_equity_funding_type_mean_1

## Average keeping the na.rm = true and not replacing it with any value:
private_equity_funding_type_mean_2 <- mean(private_equity_funding_type$raised_amount_usd, na.rm = TRUE)
private_equity_funding_type_mean_2

# basis the analsis done on the average for all the venture types    

###################### Checkpoint 3: Country Analysis ############################

# Spark Funds wants to see the top nine countries which have received the highest total 
# funding (across ALL sectors for the chosen investment type)

?summarise

venture_funding_type <- subset(master_frame, master_frame$funding_round_type=="venture")
venture_funding_type <- select(venture_funding_type, country_code, raised_amount_usd)

venture_funding_type <- group_by(venture_funding_type, country_code)

venture_funding_type <- aggregate(venture_funding_type$raised_amount_usd, by = list(country_code = venture_funding_type$country_code), FUN = sum, na.rm = TRUE)

venture_funding_type <- aggregate(x~country_code, venture_funding_type, sum) 

venture_funding_type <- arrange(venture_funding_type, desc(x))

View(venture_funding_type)

# For the chosen investment type, make a data frame named top9 with the top nine countries 

top9 <- slice(venture_funding_type,1:9)
top9[-3,1]


####################### Checkpoint 4: Sector Analysis 1 #######################

# reading mapping.csv file: 

sector_mapping <- read.csv("mapping.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# data in wide format. Will use gather function to convert from wide to long data format: 

mapping_long <- gather(sector_mapping, main_sector, primary_sector, -category_list)

mapping_long <- mapping_long[!(mapping_long$primary_sector == 0), ] #removed values having '0'
mapping_long <- mapping_long[ ,-3]                                  # Removed last column as it was not needed

View(mapping_long)

mapping_long <- mapping_long[!(mapping_long$main_sector == "Blanks"), ]  #Removed values having sector as 'Blanks'

# Separated the primary sector for all the companies in the new column 'primary_sector':

master_frame_separated <- separate(master_frame, category_list, into = c("primary_sector"), sep = "\\|")

#merged the mapping file into the 'master_frame_separated' file which now includes all the companies with there main sectors:

sector_analysis_merged_data <- merge(master_frame_separated, mapping_long, by.x = "primary_sector", by.y = "category_list", all = TRUE)

View(sector_analysis_merged_data)

sector_analysis_merged_data <- sector_analysis_merged_data[!(is.na(sector_analysis_merged_data$main_sector)), ]

