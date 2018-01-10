ru_path <- "D:/Pulkit/ru-waterfall/Data"
# read_path <- ""
save_path <- "D:/Pulkit/ru-waterfall/backups/production"

library(reshape2)
library(plyr)
library(dplyr)
library(scales)
library(lubridate)
library(magrittr)
# library(tidyverse)

#### Defining date variables ####
today<-Sys.Date()-1
wk_end_date <-today-wday(today)
wk_start_date <-wk_end_date-6
leadtime_start_date<-wk_start_date-70 # 10 weeks
low_sales_end_date <- as.numeric(format(wk_end_date, format="%Y%m%d")) 
low_sales_start_date <- as.numeric(format(wk_end_date-28, format="%Y%m%d"))
temp_week<-week(wk_end_date)


#### Reading Files ####
list.files(ru_path)
sales_raw <- read.csv(paste0(ru_path,"/fsn_fc_pincode_day_sales.csv")
                     ,nrows = 10000
                     )
lzn_mapping <- read.csv(paste0(ru_path,"/lzn_mapping.csv"))
vendor_site <- read.csv(paste0(ru_path,"/vendor_site.csv"))
lead_time <- read.csv(paste0(ru_path,"/fsn_fc_lead_time.csv"))
cat_lead_time <- read.csv(paste0(ru_path,"/cat_lead_time.csv"))
# min_sale_threshold <- read.csv(paste0(ru_path,"/min_sales_Depth.csv"))
exclusion_list <- read.csv(paste0(ru_path,"/exclusion_list.csv"))
preferred_warehouse <- read.table(paste0(ru_path,"/preferred_warehouse.tsv"),header = T)
# fsn_placement <- read.csv(paste0(ru_path,"/fsn_placement.csv"),nrows=2000)
fsn_fc_l0 <- read.csv(paste0(ru_path,"/fsn_fc_l0.csv"),nrows=100000) # nulls till 50L
explain_ff <- read.csv(paste0(ru_path,"/explain_ff.csv"),nrows = 5000000)
vendor_adherence <- read.csv(paste0(ru_path,"/fsn_fc_vendor_adherence.csv"),nrows=100000)
promotions <- read.csv(paste0(ru_path,"/fsn_pincode_promotion_sales.csv"),nrows=10000)

#### Basic Data Cleaning ####
sales_raw_2 <- sales_raw %>% filter(week==49)
sales <- sales_raw_2 %>% filter(fsn!="") %>%
  rename(dest_pincode = destination_pincode) %>%
  mutate(ru_loss = sales-ru_sales,
         dest_pincode = as.factor(dest_pincode))

lzn <- lzn_mapping %>% filter(lzn_value!="" & lzn_value!='0' & lzn_value!='NULL') %>%
  rename(fc=src_fc_code) %>%
  mutate(dest_pincode=as.factor(dest_pincode),src_pincode=as.factor(src_pincode))

lz <- lzn %>% filter(lzn_value %in% c("L1","L2","Z1","Z2"))

vendor_site_2 <- vendor_site %>% group_by(fsn,fc) %>% summarize(vendor_count=n_distinct(vsid)) %>% as.data.frame()

# explain_ff <- explain_ff %>% rename(dest_pincode = destination_pincode)
vendor_adherence %<>% mutate(poi_created_day = as.Date(as.character(poi_created_day), format = "%Y%m%d"),
        category="Book") # to be removed

cat_lead_time %<>% rename(cat_lead_time = lead_time)
lead_time %<>% 
  mutate(category="Book") %>% # to be removed 
  rename(fc=warehouse) %>%
  left_join(cat_lead_time, by = 'category') %>%
  mutate(policy_lt=ifelse(lead_time<1,cat_lead_time,lead_time),
         reco_week=week(wk_start_date-policy_lt)) %>% #check if any max treatement needs to be done?
  select(-bu,-super_cat) 


preferred_fc <- preferred_warehouse %>% mutate(pref_fc=substr(preferred_fc_non_large,1,regexpr(',',preferred_fc_non_large)-1),
                                               pincode=as.factor(as.character(pincode))) %>%
  select(pincode, pref_fc) %>% rename(dest_pincode=pincode)

names(promotions) <- c("fsn", "dest_pincode", "day", "week_num", "month", "year", "offer_sales") # to be removed after fact promoted to production
promotions_fsn_dp <- promotions %>% filter(week_num==temp_week) %>% group_by(fsn, dest_pincode) %>%
  summarise(offer_sales = sum(offer_sales))
