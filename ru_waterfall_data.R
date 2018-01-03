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
sales<- read.csv(paste0(ru_path,"/fsn_fc_pincode_day_sales.csv"),nrows=2000)
lzn_mapping <- read.csv(paste0(ru_path,"/lzn_mapping.csv"))
vendor_site <- read.csv(paste0(ru_path,"/vendor_site.csv"))
lead_time <- read.csv(paste0(ru_path,"/fsn_fc_lead_time.csv"),nrows=2000)
cat_lead_time <- read.csv(paste0(ru_path,"/cat_lead_time.csv"))
min_sale_threshold <- read.csv(paste0(ru_path,"/min_sales_Depth.csv"))
exclusion_list <- read.csv(paste0(ru_path,"/exclusion_list.csv"))
preferred_warehouse <- read.table(paste0(ru_path,"/preferred_warehouse.tsv"),header = T)
fsn_placement <- read.csv(paste0(ru_path,"/fsn_placement.csv"),nrows=2000)
fsn_fc_l0 <- read.csv(paste0(ru_path,"/fsn_fc_l0.csv"),nrows=2000)
explain_ff <- read.csv(paste0(ru_path,"/explain_ff.csv"),nrows=2000)
vendor_adherence <- read.csv(paste0(ru_path,"/fsn_fc_vendor_adherence.csv"),nrows=2000)

# Basic Data Cleaning 
sales <- sales %>% filter(fsn!="") %>% mutate(nat_sales = sales-ru_sales) %>%
  rename(dest_pincode = destination_pincode)

lzn_mapping <- lzn_mapping %>% filter(lzn_value!="" & lzn_value!='0') %>%
  rename(fc=src_fc_code) %>% 
  mutate(dest_pincode=as.factor(dest_pincode),src_pincode=as.factor(src_pincode))
  
lzn_mapping_2 <- subset(lzn_mapping, dest_pincode %in% sales$dest_pincode)

dest_fsn <- sales %>% select(fsn,dest_pincode,brand,bu,category,vertical) %>% distinct %>%
  mutate(dest_pincode=as.factor(dest_pincode))
fc<-data.frame(unique(lzn_mapping_2$fc))
dest_fsn_fc <- merge(dest_fsn,fc,by=NULL) 
names(dest_fsn_fc)[ncol(dest_fsn_fc)]<- "fc"

vendor_site_2 <- vendor_site %>% group_by(fsn,fc) %>% summarize(vendors=n_distinct(vsid)) %>% as.data.frame()

