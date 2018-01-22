read_path <- "D:/Pulkit/ru-waterfall/Data"
# read_path <- "./mnt/rp/ru_waterfall/input"

# <<<<<<< HEAD
save_path <- "D:/Pulkit/ru-waterfall"
# =======
save_path <- "D:/Pulkit/ru-waterfall/backups/production"
# >>>>>>> 620a303d04cf883be29b16a255ee51d5d41044c7

# save_path <- "./mnt/rp/ru_waterfall/output"
# random changes


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
ru_week<-week(wk_end_date)


#### Reading Files ####
# list.files(read_path)
sales_raw <- read.csv(paste0(read_path,"/fsn_fc_pincode_sales.csv")
                     # ,nrows = 1000000
                     )
lzn_mapping <- read.csv(paste0(read_path,"/lzn_mapping.csv"))
vendor_site <- read.csv(paste0(read_path,"/fsn_fc_vendor.csv"))
lead_time <- read.csv(paste0(read_path,"/fsn_fc_lead_time.csv"))
cat_lead_time <- read.csv(paste0(read_path,"/category_lead_time.csv"))
min_sale_threshold <- read.csv(paste0(read_path,"/cat_min_sales_depth.csv"))
exclusion_list <- read.csv(paste0(read_path,"/fsn_exclusion_list.csv"))
preferred_warehouse <- read.table(paste0(read_path,"/preferred_warehouse.tsv"),header = T)
# fsn_fc_placement <- read.csv(paste0(read_path,"/fsn_fc_placement.csv"),nrows=2000)
fsn_fc_l0 <- read.csv(paste0(read_path,"/fsn_fc_req_l0.csv"),nrows=100000) # nulls till 50L
explain_ff <- read.csv(paste0(read_path,"/fsn_fc_explain_ff.csv"),nrows = 5000000)
vendor_adherence <- read.csv(paste0(read_path,"/fsn_fc_vendor_adherence.csv"))
offer_sales <- read.csv(paste0(read_path,"/fsn_pincode_offer_sales.csv"))

#### Basic Data Cleaning ####
sales_raw %<>% mutate(day=as.Date(as.character(day)))
sales_raw_2 <- sales_raw %>% filter(week==ru_week)
sales <- sales_raw_2 %>% filter(fsn!="") %>%
  rename(dest_pincode = destination_pincode) %>%
  mutate(nat_sales = sales-ru_sales,
         dest_pincode = as.factor(dest_pincode))

lzn <- lzn_mapping %>% filter(lzn_value!="" & lzn_value!='0' & lzn_value!='NULL') %>%
  rename(fc=src_fc_code) %>%
  mutate(dest_pincode=as.factor(dest_pincode),src_pincode=as.factor(src_pincode))

lz <- lzn %>% filter(lzn_value %in% c("L1","L2","Z1","Z2"))

vendor_site_2 <- vendor_site %>% group_by(fsn,fc) %>% summarize(vendor_count=n_distinct(vsid)) %>% as.data.frame()

fsn_category <- sales[!duplicated(sales[,c(3,7)]),c(3,7)] # Fsn - category list for week's sales

vendor_adherence %<>% mutate(poi_created_day = as.Date(as.character(poi_created_day), format = "%Y%m%d")) %>%
  right_join(fsn_category, by="fsn") # join to be removed, right join to only capture 

cat_lead_time %<>% rename(cat_lead_time = lead_time)
lead_time %<>% 
  right_join(fsn_category, by="fsn") %>% # join to be removed 
  rename(fc=warehouse) %>%
  left_join(cat_lead_time, by = 'category') %>%
  mutate(policy_lt=ifelse(lead_time<1,cat_lead_time,lead_time),
         reco_week=week(wk_start_date-policy_lt)) %>% #check if any max treatement needs to be done?
  select(-bu,-super_cat) 


preferred_warehouse %<>% mutate(pincode=as.factor(as.character(pincode))) %>%
  select(pincode, preferred_fc_non_large) %>% rename(dest_pincode=pincode, preferred_fc=preferred_fc_non_large)

preferred_fc <- preferred_warehouse %>% mutate(pref_fc=substr(preferred_fc,1,regexpr(',',preferred_fc)-1)) %>%
  select(dest_pincode, pref_fc)

offer_sales_fsn_dp <- offer_sales %>% filter(week_num==ru_week) %>% group_by(fsn, dest_pincode) %>%
  summarise(offer_sales = sum(offer_sales)) %>%
  mutate(dest_pincode=as.factor(as.character(dest_pincode)))

source("fsn_fc_l0_aggregation.R")