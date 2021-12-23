# Load packages
if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("ggcorrplot")) install.packages("ggcorrplot"); library("ggcorrplot")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("haven")) install.packages("haven"); library("haven")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("stringr")) install.packages("stringr"); library("stringr")
if(!require("maps")) install.packages("maps"); library("maps")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("shinydashboard")) install.packages("shinydashboard"); library("shinydashboard")
if(!require("rsconnect")) install.packages("rsconnect"); library("rsconnect")
if(!require("DT")) install.packages("DT"); library("DT")
if (!require("rfm")) install.packages("rfm"); library("DT")
if (!require("Hmisc")) install.packages('Hmisc'); library("Hmisc")
if (!require("lubridate")) install.packages("lubridate"); library("lubridate")
if (!require("formattable")) install.packages("formattable"); library("formattable")
if (!require("plotly")) install.packages("plotly"); library("plotly")
if (!require("readxl")) install.packages("readxl"); library("readxl");
if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse");
if (!require("mapproj")) install.packages("mapproj"); library("mapproj");
if (!require("rworldmap")) install.packages("rworldmap"); library("rworldmap");
if(!require("maptools")) install.packages("maptools"); library("maptools")
if(!require("sqldf")) install.packages("sqldf"); library("sqldf")
if(!require("maptools")) install.packages("maptools"); library("maptools")
if(!require("fBasics")) install.packages("fBasics"); library("fBasics")
if(!require("rworldmap")) install.packages("rworldmap"); library("rworldmap")


options(scipen = 20)

getwd()
#setwd("C:\\Users\\irana\\OneDrive - IESEG\\Documents\\GitHub\\marketing_datamart\\gambeling_analysis_app")
#setwd("/Users/inder/Dropbox/My Mac (Inders-MacBook-Pro.local)/Documents/GitHub/marketing_datamart/gambeling_analysis")


countries_apx <- read_excel("data//appendice.xlsx", sheet = "country_nm")
product_apx <- read_excel("data//appendice.xlsx", sheet = "prod")
language_apx <- read_excel("data//appendice.xlsx", sheet = "lang")
app_nm_apx <- read_excel("data//appendice.xlsx", sheet = "app_nm")

data = load('data//DataGroupAssignment.Rdata')

data

#check tibble feature types
str(Demographics)

#convert registration date to date time 
Demographics$RegDate <- as.Date(Demographics$RegDate)

str(Demographics$RegDate)

#convert first pay, first act, first sports book play, first casino play,
# first games play, and  first poker play dates to datetime.
Demographics$FirstPay <- as.Date(Demographics$FirstPay, "%Y%m%d")

Demographics$FirstAct <- as.Date(Demographics$FirstAct, "%Y%m%d")
Demographics$FirstSp <- as.Date(Demographics$FirstSp, "%Y%m%d")
Demographics$FirstCa <- as.Date(Demographics$FirstCa, "%Y%m%d")
Demographics$FirstGa <- as.Date(Demographics$FirstGa, "%Y%m%d")
Demographics$FirstPo <- as.Date(Demographics$FirstPo, "%Y%m%d")

str(Demographics)

#create a cutoff date object
october <- as.Date("20051002","%Y%m%d")

#calculate LOR (Length of Relation) based on time since 
#last transaction date in the dataset and the registration date of the 
#gambler
Demographics$LOR <- october - Demographics$RegDate

#calculate play to act interval (PTAI)
#based on time from FirstPay (first betting deposit date)
#and first active date
Demographics$PTAI <- Demographics$FirstAct - Demographics$FirstPay

#calculate registeration to first pay interval (RTFP)
#based on time from gambler registration date to first bet deposit
Demographics$RTFP <- Demographics$FirstPay - Demographics$RegDate

#calculate dummy variable for each first play category (sports book, casino play,
#first games play, first poker play) which indicates whether the indiviual
#has ever partook in these play categories
Demographics$SpPlayed <- ifelse(is.na(Demographics$FirstSp),0,1)
Demographics$CaPlayed <- ifelse(is.na(Demographics$FirstCa),0,1)
Demographics$GaPlayed <- ifelse(is.na(Demographics$FirstGa),0,1)
Demographics$PoPlayed <- ifelse(is.na(Demographics$FirstPo),0,1)

sapply(Demographics,function(x) sum(is.na(x)))

table(Demographics$Gender)

Demographics$Gender <- ifelse(is.na(Demographics$Gender),1, Demographics$Gender)

sum(is.na(Demographics$Gender))

head(Demographics)


UserDailyAggregation$Date <- as.Date(UserDailyAggregation$Date,"%Y%m%d")

sum(is.na(UserDailyAggregation))

head(UserDailyAggregation)



#Filter out transactions before first paydate
Demographics_paydate <- Demographics %>%
  select(UserID,FirstPay)

uda_filtered <- left_join(UserDailyAggregation,Demographics_paydate,by = 'UserID')
uda_filtered <- left_join(uda_filtered, product_apx, by="ProductID")


uda_filtered <- uda_filtered %>%
  dplyr::filter(Date >= FirstPay) %>%
  select(-FirstPay)


UserDailyAggregation_1 <- uda_filtered %>% 
  group_by(UserID) %>% 
  summarise(first_txn_dt=min(Date),
            last_txn_dt=max(Date), 
            txn_cnt=n(), 
            total_stakes=round(sum(Stakes)), 
            total_wins=round(sum(Winnings)), 
            total_bets=round(sum(Bets)), 
            avg_stakes=round(mean(Stakes)), 
            avg_wins=round(mean(Winnings)), 
            avg_bets=round(mean(Bets)))

head(UserDailyAggregation_1)

p_id <- unique(UserDailyAggregation[c("ProductID")])
p_id[order(p_id$ProductID),]

# Making a new dataframe with number of transactions per UserID in the given period
UserDailyAggregation_2 <- uda_filtered %>% 
  group_by(UserID) %>% 
  summarise(procuct_1_cnt=length(ProductID[ProductID == 1]),
            product_2_cnt=length(ProductID[ProductID == 2]),
            product_4_cnt=length(ProductID[ProductID == 4]),
            product_5_cnt=length(ProductID[ProductID == 5]),
            product_6_cnt=length(ProductID[ProductID == 6]),
            product_7_cnt=length(ProductID[ProductID == 7]),
            product_8_cnt=length(ProductID[ProductID == 8])
  )

head(UserDailyAggregation_2)

sapply(UserDailyAggregation_1,function(x) sum(is.na(x)))

head(UserDailyAggregation)

uda_cleaned <- merge(x = UserDailyAggregation_1, y = UserDailyAggregation_2, by = "UserID", all.x = TRUE)

head(uda_cleaned)

names(uda_cleaned)


#########
#Calculate Recency, Frequency and Monetary Value
#Get max date

#calculate recency and frequency
UserDailyAggregation_rfm <- uda_filtered %>% 
  group_by(UserID) %>% 
  summarise(recency = as.numeric(october - max(Date)),
            frequency = n(),
            monetary_value = sum(Winnings))

#Fill na with 0s
UserDailyAggregation_rfm[is.na(UserDailyAggregation_rfm)] = 0

#calculate RFM Score
UserDailyAggregation_rfm <- UserDailyAggregation_rfm %>%
  mutate(rfm_value = recency + frequency + monetary_value)


uda_cleaned <- left_join(uda_cleaned,UserDailyAggregation_rfm,by='UserID')

UserDailyAggregation_rfm <- UserDailyAggregation_rfm %>%
  mutate(recency_score = as.numeric(cut2(-recency, g=4)),
         frequency_score = as.numeric(cut2(frequency, g=4)),
         value_score = as.numeric(cut2(monetary_value, g=4)))
UserDailyAggregation_rfm <- UserDailyAggregation_rfm %>%
  mutate(recency_score = recency_score *0.2,
         frequency_score = frequency_score *0.2,
         value_score = value_score *0.6)

UserDailyAggregation_rfm$weighted_score <- rowMeans(UserDailyAggregation_rfm[,c("recency_score","frequency_score","value_score")])

########


uda_cleaned$profit <- uda_cleaned$total_stakes - uda_cleaned$total_wins


###########################

PokerChipConversions_buy <- PokerChipConversions %>% 
  group_by(UserID) %>% 
  dplyr::filter(TransType == 124) %>% 
  summarise(total_buy = sum(TransAmount),
            avg_buy = mean(TransAmount),
            min_buy = min(TransAmount),
            max_buy = max(TransAmount))


PokerChipConversions_sell <- PokerChipConversions %>% 
  group_by(UserID) %>% 
  dplyr::filter(TransType == 24) %>% 
  summarise(total_sell = sum(TransAmount),
            avg_sell = mean(TransAmount),
            min_sell = min(TransAmount),
            max_sell = max(TransAmount))

PokerChipConversions_2 <- full_join(x=PokerChipConversions_buy,y=PokerChipConversions_sell, by='UserID')

###########################

PokerChipConversions$Date <- as.POSIXlt(strptime(PokerChipConversions$TransDateTime, format = "%Y-%m-%d %H:%M:%S",tz="UTC"))
PokerChipConversions$Date <- as.Date(PokerChipConversions$Date)
PokerChipConversions_3 <- PokerChipConversions %>%
  group_by(UserID,Date) %>%
  dplyr::filter(TransType == 24) %>%
  dplyr::summarise(total_sell = sum(TransAmount))

uda_poker_table <- full_join(PokerChipConversions_3,uda_filtered,by=c('UserID','Date'),all.y=TRUE)

uda_poker_table <- uda_poker_table[ , -which(names(uda_poker_table) %in% c("Product Description"))]

head(uda_poker_table)

uda_poker_table[is.na(uda_poker_table)] = 0

uda_poker_table$total_amount = uda_poker_table$Winnings + uda_poker_table$total_sell
UserDailyAggregation_rfm_c <- rfm_table_order(data = uda_poker_table,customer_id = UserID,order_date = Date,revenue = total_amount,analysis_date = october )
UserDailyAggregation_rfm_2 <- UserDailyAggregation_rfm_c$rfm
names(UserDailyAggregation_rfm_2)[1] <- 'UserID'

###########################

###########################merge tables#######################

base_table <- merge(x= Demographics,y=uda_cleaned,by='UserID',all.x=TRUE)
base_table <- left_join(base_table,PokerChipConversions_2,by='UserID')
base_table <- left_join(base_table,UserDailyAggregation_rfm_2,by='UserID')

base_table <- base_table %>%
  mutate(w_recency_score = recency_score *0.2,
         w_frequency_score = frequency_score *0.2,
         w_monetary_score = monetary_score *0.6)
base_table$weighted_rfm <- rowMeans(base_table[,c("w_recency_score","w_frequency_score","w_monetary_score")])
base_table$Loyality <- ifelse(base_table$weighted_rfm > 1.6, 'Loyalists',
                              ifelse((base_table$weighted_rfm <= 1.6) & (base_table$weighted_rfm > 1.3),'Potential Loyalists',
                                     ifelse((base_table$weighted_rfm <= 1.3) & (base_table$weighted_rfm > 0.7),'Regular Customers',
                                            ifelse((base_table$weighted_rfm <= 0.7) & (base_table$weighted_rfm > 0.4),'Hesitant Customers','Needs Attention'
                                            ))))

###########################

head(base_table)
colnames(base_table)

sapply(base_table,function(x) sum(is.na(x)))

# creat function impute NA values with 0 in columns which are numeric
numeric_col_na_to_0 <- function(x) { replace(x, is.na(x), 0) }

base_table$txn_cnt <- numeric_col_na_to_0(base_table$txn_cnt)
base_table$total_stakes <- numeric_col_na_to_0(base_table$total_stakes)
base_table$total_wins <- numeric_col_na_to_0(base_table$total_wins)
base_table$total_bets <- numeric_col_na_to_0(base_table$total_bets)
base_table$avg_stakes <- numeric_col_na_to_0(base_table$avg_stakes)
base_table$avg_wins <- numeric_col_na_to_0(base_table$avg_wins)
base_table$avg_bets <- numeric_col_na_to_0(base_table$avg_bets)
base_table$procuct_1_cnt <- numeric_col_na_to_0(base_table$procuct_1_cnt)
base_table$product_2_cnt <- numeric_col_na_to_0(base_table$product_2_cnt)
base_table$product_4_cnt <- numeric_col_na_to_0(base_table$product_4_cnt)
base_table$product_5_cnt <- numeric_col_na_to_0(base_table$product_5_cnt)
base_table$product_6_cnt <- numeric_col_na_to_0(base_table$product_6_cnt)
base_table$product_7_cnt <- numeric_col_na_to_0(base_table$product_7_cnt)
base_table$product_8_cnt <- numeric_col_na_to_0(base_table$product_8_cnt)
base_table$recency <- numeric_col_na_to_0(base_table$recency)
base_table$frequency <- numeric_col_na_to_0(base_table$frequency)
base_table$monetary_value <- numeric_col_na_to_0(base_table$monetary_value)
base_table$rfm_value <- numeric_col_na_to_0(base_table$rfm_value)
base_table$recency_score <- numeric_col_na_to_0(base_table$recency_score)
base_table$frequency_score <- numeric_col_na_to_0(base_table$frequency_score)
base_table$monetary_score <- numeric_col_na_to_0(base_table$monetary_score)
base_table$rfm_score <- numeric_col_na_to_0(base_table$rfm_score)
base_table$profit <- numeric_col_na_to_0(base_table$profit)
base_table$total_buy <- numeric_col_na_to_0(base_table$total_buy)
base_table$avg_buy <- numeric_col_na_to_0(base_table$avg_buy)
base_table$min_buy <- numeric_col_na_to_0(base_table$min_buy)
base_table$max_buy <- numeric_col_na_to_0(base_table$max_buy)
base_table$total_sell <- numeric_col_na_to_0(base_table$total_sell)
base_table$avg_sell <- numeric_col_na_to_0(base_table$avg_sell)
base_table$min_sell <- numeric_col_na_to_0(base_table$min_sell)
base_table$max_sell <- numeric_col_na_to_0(base_table$max_sell)
base_table$recency_days <- numeric_col_na_to_0(base_table$recency_days)
base_table$transaction_count <- numeric_col_na_to_0(base_table$transaction_count)
base_table$amount <- numeric_col_na_to_0(base_table$amount)
base_table$w_recency_score <- numeric_col_na_to_0(base_table$w_recency_score)
base_table$w_frequency_score <- numeric_col_na_to_0(base_table$w_frequency_score)
base_table$w_monetary_score <- numeric_col_na_to_0(base_table$w_monetary_score)
base_table$weighted_rfm <- numeric_col_na_to_0(base_table$weighted_rfm)

sapply(base_table,function(x) sum(is.na(x)))

final_base_table <- left_join(base_table, countries_apx, by="Country")
final_base_table <- left_join(final_base_table, language_apx, by="Language")
final_base_table <- left_join(final_base_table, app_nm_apx, by="ApplicationID")

names(final_base_table)[names(final_base_table) == 'Country Name'] <- 'Country_Name'

glimpse(final_base_table)

#final_base_table %>% replace(is.na(.), 0)

save(final_base_table, file = "data//base_table.RData")
ls()

###########################
remove_list <- c("base_table", "numeric_col_na_to_0", "Demographics_paydate", "PokerChipConversions", "p_id", 
                 "october", "numeric_col_na_to_0", "PokerChipConversions_2", "PokerChipConversions_3", "PokerChipConversions_buy"
                 , "PokerChipConversions_sell", "UserDailyAggregation", "UserDailyAggregation_1", "UserDailyAggregation_2"
                 , "UserDailyAggregation_rfm", "UserDailyAggregation_rfm_2", "UserDailyAggregation_rfm_c" ,"remove_list",
                 "uda_cleaned", "uda_filtered", "uda_poker_table", "Demographics", "data")
#remove_list
rm(list = remove_list)

###########################
#create plot that visualizes monetary value (gambler' winnings), frequency, and recency
scatter_plot_monetary_value <- ggplot(final_base_table, aes(x=frequency,y=monetary_value,color=recency)) + 
  ggtitle('Gambler total winnings against frequency') + 
  xlab('Frequency (days)') + 
  ylab('Total winnings (Euros)') + 
  #create annotation for most extreme outlier, displaying their ID
  annotate(geom='text',x = 175, y= 1070000, label='Gambler ID with highest winnings',color='blue')

#code adapted and inspired from this forum thread: https://stackoverflow.com/questions/1923273/counting-the-number-of-elements-with-the-values-of-x-in-a-vector
scatter_plot_monetary_value_1 <- lapply(final_base_table[1], function(data) scatter_plot_monetary_value +
                                          geom_jitter(alpha=0.5) + 
                                          theme_light(base_size=11) + 
                                          theme(plot.title = element_text(hjust = 0.5)) + 
                                          geom_text(aes(label= ifelse(final_base_table$monetary_value > quantile(final_base_table$monetary_value, 0.9999999),as.character(final_base_table$UserID),'')),hjust=0,vjust=0))

scatter_plot_monetary_value_1
###########################

###########################

#base_table_p <- subset(base_table, Country %in% input$Country)

prod_counts = c(sum(final_base_table$procuct_1_cnt),sum(final_base_table$product_2_cnt),sum(final_base_table$product_4_cnt),sum(final_base_table$product_5_cnt),sum(final_base_table$product_6_cnt),sum(final_base_table$product_7_cnt),sum(final_base_table$product_8_cnt))

df_product_counts <- data.frame(
  products = as.factor(c('Prod_1','Prod_2','Prod_4','Prod_5','Prod_6','Prod_7','Prod_8')),
  counts = prod_counts
)

###########################

#create a barchart displaying most popular products excluding poker 
bar_plots_products <- ggplot(df_product_counts,aes(x=products,y=counts, fill=products)) +
  geom_col(position='dodge') +
  ggtitle('Product count (usage) against product type') + 
  theme_light(base_size=11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Count (times played)') + 
  scale_x_discrete('Product type',labels=c('Sports book fixed-odd','Sports book live-action','Casino BossMedia','Supertoto','Games VS','Games bwin','Casino Chartwell')) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_discrete(labels=c('Sports book fixed-odd','Sports book live-action','Casino BossMedia','Supertoto','Games VS','Games bwin','Casino Chartwell'))

bar_plots_products
###########################
#head(base_table)

###########################
df_donut <- sqldf("select [Country_Name] as country, count(UserID) as count from final_base_table group by 1 order by 2 desc limit 11")
write.csv(df_donut,"data//df_donut.csv", row.names = FALSE)
df_donut
df_donut <- read.csv("data//df_donut.csv")

plot_ly(labels = df_donut$country, values = df_donut$count)  %>% 
  plotly::add_pie(hole = 0.5) %>% 
  layout(title = "Top User Distribution by Country",  showlegend = T,
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

##ggplot
ggplot(df_donut, aes(x="", y=count, fill=country)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)  +
  theme_void() +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5),  color = "white") 
  
#names(final_base_table)
###########################
df_country <- final_base_table %>% 
  group_by(Country_Name, Gender) %>% 
  summarise(usr_cnt=length(Gender)
  )

df_country <- pivot_wider(df_country,names_from = Gender, values_from = usr_cnt )
df_country <- df_country %>% replace(is.na(.), 0)
names(df_country)[names(df_country) == 1] <- 'Male'
names(df_country)[names(df_country) == '0'] <- 'Female'

df_country$total <- df_country$Male + df_country$Female
df_country <- df_country[order(df_country$total, decreasing = TRUE),]
df_country <- head(df_country, 11)
write.csv(df_country,"data//df_country_de.csv", row.names = FALSE)
df_country <- df_country[(2:11),]
write.csv(df_country,"data//df_country.csv", row.names = FALSE)

fig_cntry <- plot_ly(
  df_country,
  x = ~reorder(Country_Name, Male),
  y = ~Male,
  type = "bar",
  name = 'Male',
  textposition = 'auto')  %>%
  add_trace(y = ~Female, name = 'Females')  %>% 
  layout(yaxis = list(title = 'Count of Users'), barmode = 'stack')

fig_cntry


#########for germany##################
#########avg bets##################
df_country_de <- final_base_table 
df_country_de <- filter(df_country_de, Country_Name == "Germany")
df_country_de$month <- factor(strftime(df_country_de$FirstPay, '%b'), levels = month.abb)
df_country_de <- df_country_de %>% 
  group_by(month, Gender) %>% 
  summarise(avg_bets=mean(avg_bets)
  )


df_country_de <- pivot_wider(df_country_de,names_from = Gender, values_from = avg_bets )
df_country_de <- df_country_de %>% replace(is.na(.), 0)
names(df_country_de)[names(df_country_de) == 1] <- 'Male'
names(df_country_de)[names(df_country_de) == '0'] <- 'Female'
df_country_de$total <- df_country_de$Male + df_country_de$Female
df_country_de <- subset(df_country_de, total > 0)
df_country_de



fig_cntry <- plot_ly(
df_country_de,
x = ~month,
y = ~Male,
type = "bar",
name = 'Male',
textposition = 'auto')  %>%
add_trace(y = ~Female, name = 'Females')  %>% 
layout(yaxis = list(title = 'Avg Bets'), barmode = 'stack')
fig_cntry

##########################################################

df_country_de <- final_base_table 
df_country_de <- filter(df_country_de, Country_Name == "Germany")
df_country_de$month <- factor(strftime(df_country_de$FirstPay, '%b'), levels = month.abb)
df_country_de <- df_country_de %>% 
  group_by(month, Gender) %>% 
  summarise(avg_bets=mean(avg_buy)
  )


df_country_de <- pivot_wider(df_country_de,names_from = Gender, values_from = avg_bets )
df_country_de <- df_country_de %>% replace(is.na(.), 0)
names(df_country_de)[names(df_country_de) == 1] <- 'Male'
names(df_country_de)[names(df_country_de) == '0'] <- 'Female'
df_country_de$total <- df_country_de$Male + df_country_de$Female
df_country_de <- subset(df_country_de, total > 0)
df_country_de



fig_cntry <- plot_ly(
  df_country_de,
  x = ~month,
  y = ~Male,
  type = "bar",
  name = 'Male',
  textposition = 'auto')  %>%
  add_trace(y = ~Female, name = 'Females')  %>% 
  layout(yaxis = list(title = avg_bets), barmode = 'stack')
fig_cntry

#############################
df_country_de <- final_base_table 
df_country_de <- filter(df_country_de, Country_Name == "Germany")
df_country_de$month <- factor(strftime(df_country_de$FirstPay, '%b'), levels = month.abb)
df_country_de <- df_country_de %>% 
  group_by(Gender) %>% 
  summarise(count_users=n())
  


df_country_de$Gender[df_country_de$Gender == 0] <- "Female"
df_country_de$Gender[df_country_de$Gender == 1] <- "Male"
df_country_de
# Compute percentages
df_country_de$fraction = df_country_de$count_users / sum(df_country_de$count_users)
# Compute the cumulative percentages (top of each rectangle)
df_country_de$ymax = cumsum(df_country_de$fraction)
# Compute the bottom of each rectangle
df_country_de$ymin = c(0, head(df_country_de$ymax, n=-1))

#Compute label position
df_country_de$labelPosition <- (df_country_de$ymax + df_country_de$ymin) / 2

# Compute a good label
df_country_de$label <- paste0(df_country_de$Gender, "\n", paste0((round(df_country_de$fraction,3)*100),"%"))
df_country_de
ggplot(df_country_de, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Gender)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=3) +
  coord_polar(theta="y") +
  theme_void() +
  theme(legend.position = "none") +
  xlim(c(2, 4))



###########################

rgn_plt <- final_base_table %>% 
                      count(Country) %>% 
                      rename(region = Country)
rgn_plt$Category <- ifelse(rgn_plt$n <= 100, 1, ifelse(rgn_plt$n > 1000 & rgn_plt$n<20000, 2, ifelse(rgn_plt$n > 20000, 4, 3)))

rgn_plt <- rename(rgn_plt, Country = region)

ctry_map_df <- read_excel("data//appendice.xlsx", sheet = "country_nm")

# Get the world map
worldMap <- getMap()

# Member States of the European Union
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")
# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME%in%europeanUnion)

# Extract longitude and latitude border's coordinates of members states of E.U. 
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})
europeCoords <- do.call("rbind", europeCoords)

world_map <- map_data("world")
rgn_plt_nm <- left_join(rgn_plt, ctry_map_df, by  = "Country")
rgn_plt_2 <- inner_join(rgn_plt_nm, europeCoords, by = c("Country Name"="region"))

# Plot map
map_plot <- ggplot() + geom_polygon(data = rgn_plt_2, aes(x = long, y = lat, group = Country, fill = Category),
      colour = "black", size = 0.1) +
      coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

map_plot <- map_plot + scale_fill_gradient(name = "Users Distribution", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50")
map_plot <- map_plot + theme(
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))

map_plot
###########################


###########################

data <- final_base_table
data(wrld_simpl)
myCountries = wrld_simpl@data$NAME %in% data$Country_Name
plot(wrld_simpl, col = c(gray(.45), "blue")[myCountries+1])



#head(base_table$Country)  

#data <- subset(base_table, Gender ==1)
data(wrld_simpl)
myCountries = wrld_simpl@data$NAME %in% data$Country
plot(wrld_simpl, col = c(gray(.80), "red")[myCountries+1])

hist(final_base_table$txn_cnt)

plot(final_base_table$txn_cnt, final_base_table$amount)

head(final_base_table)

save(final_base_table,file='/Users/dimitrikestenbaum/Desktop/marketing_datamart/gambeling_analysis/base_table.Rdata')



##final_base_table
#x <- "Country_Name"
#y <- "txn_cnt"
#data_temp <- final_base_table %>% 
#  group_by_(x) %>% 
#  summarise_(y=sum(y))
#data_temp
base_cluster <- final_base_table
nums <- unlist(lapply(base_cluster, is.numeric))  
base_cluster <- base_cluster[ , nums]

#############Run Clustering to identify segments#########
# Compute a correlation matrix

df <- base_cluster
corr <- round(cor(df), 1)

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(df)

# Visualize the lower triangle of the correlation matrix
# Barring the no significant coefficient
corr.plot <- ggcorrplot(
  corr, hc.order = TRUE, type = "lower", outline.col = "white",
  p.mat = p.mat
)
corr.plot

##########Remove corelated features#################
data <- base_cluster
tmp <- cor(data)
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0


data.new <- 
  data[, !apply(tmp, 2, function(x) any(abs(x) > 0.99, na.rm = TRUE))]
head(data.new)


#############replot corelarion############
corr <- round(cor(data.new), 1)

p.mat <- cor_pmat(data.new)

corr.plot <- ggcorrplot(
  corr, hc.order = TRUE, type = "lower", outline.col = "white",
  p.mat = p.mat
)
corr.plot


#########Scale Data###########
scaled_data <- scale(data.new)

head(scaled_data)
summary(scaled_data)
sd(scaled_data)
scaled_data <- subset(scaled_data, select=-c(UserID))
head(scaled_data)

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 6.
k.max <- 6
data <- scaled_data
scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

scaled_data <- as.data.frame(scaled_data)
typeof(scaled_data)
typeof(kmeans)

#########Run clustering############
kmeans <- kmeans(scaled_data, 5, nstart=1,iter.max = 20 )


#########merge clusters with DF############
scaled_data_final <- final_base_table %>%
  mutate(
    cluster = kmeans$cluster
    )
#head(scaled_data_final)

###########Plot CLusters###########
scaled_data_final <- scaled_data_final %>%
  group_by(cluster)  %>%
  summarise(count = n())
  

plot_ly(
  x = scaled_data_final$cluster,
  y = scaled_data_final$count,
  name = "Clusters",
  type = "bar"
)

################Plot Loyalty##################
loyal <- final_base_table %>%
  group_by(Loyality)  %>%
  summarise(count = n())

plot_ly(
  x = loyal$Loyality,
  y = loyal$count,
  name = "Clusters",
  type = "bar"
)

#################################

#############Plot Languages#####################
lang <- final_base_table %>%
  group_by(`Language Description`)  %>%
  summarise(count = n())

plot_ly(
  x = lang$`Language Description`,
  y = lang$count,
  name = "Languages",
  type = "bar"
)

#############Plot by application#####################
applications_plt <- final_base_table %>%
  group_by(`Application Description`)  %>%
  summarise(count_users = n())

plot_ly(
  applications_plt,
  x = ~reorder(`Application Description`, count_users),
  y = ~count_users,
  name = "Languages",
  type = "bar"
)

##########top 5 apps###########

applications_plt <- final_base_table %>%
  group_by(`Application Description`)  %>%
  summarise(count_users = n())

applications_plt_subst <-  head(arrange(applications_plt, desc(count_users)), n = 5)

fig <- plot_ly(applications_plt_subst, labels = ~`Application Description`, values = ~count_users, type = 'pie')
fig <- fig %>% layout(title = 'Top 5 performing applications',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig

##########bottom 5 apps###########

applications_plt <- final_base_table %>%
  group_by(`Application Description`)  %>%
  summarise(count_users = n())
applications_plt
applications_plt_subst <-  head(arrange(applications_plt, count_users), n = 5)

fig <- plot_ly(applications_plt_subst, labels = ~`Application Description`, values = ~count_users, type = 'pie')
fig <- fig %>% layout(title = 'Least 5 performing applications',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig


##########################some statastics###############
stats_data <- final_base_table %>% dplyr::select("txn_cnt", "total_stakes", "total_wins", 
                                                 "total_bets", "monetary_value"
                                                 , "profit","transaction_count")

df1 <- round(basicStats(stats_data)[c("Mean", "Stdev", "Median", "Minimum", "Maximum", "nobs"),],2)
Sum_stats <- c("Mean","Stdev", "Median", "Minimum", "Maximum","nobs")
Sum_stats
head(final_base_table)
#############Profits#####################
profits <- final_base_table %>%
  group_by(Country_Name)  %>%
  summarise(profit = sum(profit))


plot_ly(
  profits,
  x = ~reorder(Country_Name , profit),
  y = ~profit,
  name = "profit",
  type = "bar"
)

profits

profits_plot_2 <- final_base_table
profits_plot_2$month <- factor(strftime(profits_plot_2$FirstPay, '%b'), levels = month.abb)


profits_plot_2 <- profits_plot_2 %>%
  group_by(month)  %>%
  summarise(profit = sum(profit))


plot_ly(
  profits_plot_2,
  x = ~reorder(month , profit),
  y = ~profit,
  name = "profit",
  type = "bar"
)

