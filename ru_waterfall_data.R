ru_path <- "D:/Pulkit/ru-waterfall/Data"
# read_path <- ""
save_path <- "D:/Pulkit/ru-waterfall/backups/production"

library(reshape2)
library(plyr)
library(dplyr)
library(RODBC)
library(scales)
library(lubridate)

#Defining date variables
today<-Sys.Date()-1
wk_end_date <-today-wday(today)
wk_start_date <-wk_end_date-6
leadtime_start_date<-wk_start_date-70 # 10 weeks
low_sales_end_date <- as.numeric(format(wk_end_date, format="%Y%m%d")) 
low_sales_start_date <- as.numeric(format(wk_end_date-28, format="%Y%m%d"))
temp_week<-week(wk_end_date)


#### Reading Files ####
# list.files(ru_path)
sales_raw<- read.csv(paste0(ru_path,"/fsn_fc_pincode_day_sales.csv"))
lzn_mapping <- read.csv(paste0(ru_path,"/lzn_mapping.csv")) 
vendor_site <- read.csv(paste0(ru_path,"/vendor_site.csv"))
# lead_time <- read.csv(paste0(ru_path,"/fsn_fc_lead_time.csv"),nrows=2000)
# cat_lead_time <- read.csv(paste0(ru_path,"/cat_lead_time.csv"))
# min_sale_threshold <- read.csv(paste0(ru_path,"/min_sales_Depth.csv"))
# exclusion_list <- read.csv(paste0(ru_path,"/exclusion_list.csv"))
# preferred_warehouse <- read.table(paste0(ru_path,"/preferred_warehouse.tsv"),header = T)
# fsn_placement <- read.csv(paste0(ru_path,"/fsn_placement.csv"),nrows=2000)
# fsn_fc_l0 <- read.csv(paste0(ru_path,"/fsn_fc_l0.csv"),nrows=2000)
# explain_ff <- read.csv(paste0(ru_path,"/explain_ff.csv"),nrows = 1000)
# vendor_adherence <- read.csv(paste0(ru_path,"/fsn_fc_vendor_adherence.csv"),nrows=10000)
# promotions <- read.csv(paste0(ru_path,"/s1_offers.csv"))

# Basic Data Cleaning 
sales_raw_2 <- sales_raw %>% filter(week==51)
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
#
# vendor_adherence <- vendor_adherence %>%
#   mutate(poi_created_day = as.Date(as.character(poi_created_day), format = "%Y%m%d"),
#         category="Book")
#
# cat_lead_time <- cat_lead_time %>% rename(cat_lead_time = lead_time)
