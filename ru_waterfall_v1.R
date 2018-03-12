#### Paths ####
# read_path <- "D:/Pulkit/ru-waterfall/Data"
read_path <- "/mnt/rp/ru_waterfall/input"

# save_path <- "D:/Pulkit/ru-waterfall/Output"
save_path <- "/mnt/rp/ru_waterfall/output"

# Loading Libraries

library(reshape2)
library(plyr)
library(dplyr)
library(scales)
library(lubridate)
library(magrittr)
library(data.table)
# library(bit)
# library(bit64)
# library(tidyverse)

#### Defining date variables ####
today<-Sys.Date()-1
wk_end_date <-today-wday(today)
wk_start_date <-wk_end_date-6
ru_week<-week(wk_end_date)

# ru_week <- 4
# wk_start_date <- as.Date("2018-01-07")+14
# wk_end_date <- wk_start_date+6

leadtime_start_date<-wk_start_date-70 # 10 weeks
low_sales_end_date <- as.numeric(format(wk_end_date, format="%Y%m%d"))  # Check using below or not
low_sales_start_date <- as.numeric(format(wk_end_date-28, format="%Y%m%d"))


#### Reading Files ####
# list.files(read_path)
sales_raw <- read.csv(paste0(read_path,"/fsn_fc_pincode_sales.csv")
                      # ,nrows = 1000000
                      )
lzn_mapping <- fread(paste0(read_path,"/lzn_mapping.csv"))
vendor_site <- fread(paste0(read_path,"/fsn_fc_vendor.csv"))
lead_time <- read.csv(paste0(read_path,"/fsn_fc_lead_time.csv"))
cat_lead_time <- read.csv(paste0(read_path,"/category_lead_time.csv"))
min_sale_threshold <- fread(paste0(read_path,"/cat_min_sales_depth.csv"))
exclusion_list <- fread(paste0(read_path,"/fsn_exclusion_list.csv"))
preferred_warehouse <- read.table(paste0(read_path,"/preferred_warehouse.tsv"),header = T)
# fsn_fc_placement <- fread(paste0(read_path,"/fsn_fc_placement.csv"),nrows=2000)
fsn_fc_l0 <- read.csv(paste0(read_path,"/fsn_fc_req_l0.csv")
                      ,stringsAsFactors = F
                      # ,nrows = 1000
                      ) # nulls till 50L
explain_ff <- fread(paste0(read_path,"/fsn_fc_explain_ff.csv")
                       # ,nrows = 5000000
                       )
vendor_adherence <- fread(paste0(read_path,"/fsn_fc_vendor_adherence.csv"))
offer_sales <- fread(paste0(read_path,"/fsn_pincode_offer_sales.csv"))


# # Temp lzn mapping from sale
# lzn_sales <- sales_raw %>% select(fc,source_pincode, destination_pincode,lzn_value) %>% distinct() %>%
#   filter(lzn_value %in% c('L1','L2','Z1','Z2','N1','N2')) %>% 
#   rename(src_pincode=source_pincode
#          ,dest_pincode=destination_pincode)
# 
# lzn_mapping <- lzn_sales
# temp_validation#
# lzn_temp <- lzn_mapping %>% group_by(dest_pincode,fc,src_pincode) %>% summarise(lzn_count =n_distinct(lzn_value))
# lzn_temp_2 <- lzn_temp %>% filter(lzn_count>1) %>% left_join(lzn_mapping, by=c("dest_pincode","fc","src_pincode"))

#### Basic Data Cleaning ####
sales_raw %<>% mutate(day=as.Date(as.character(day)))
sales_raw_2 <- sales_raw %>% filter(week==ru_week)
sales <- sales_raw_2 %>% filter(fsn!="") %>%
  rename(dest_pincode = destination_pincode) %>%
  mutate(unmapped_sales = sales-ru_sales-nat_sales,
         dest_pincode = as.factor(dest_pincode)
         ,source_pincode = as.factor(source_pincode))

sum(sales$unmapped_sales)

# temp_fc_pincode_sales <- sales_raw %>% group_by(fc,source_pincode,week) %>% summarise(sales=sum(sales))
# temp_sales <- sales %>% filter(source_pincode==900064)
# write.csv(temp_fc_pincode_sales,"temp_lzn.csv")

# temp_fc_sales <- sales %>% group_by(fc) %>% summarise(sales=sum(sales), nat_sales=sum(nat_sales))
# write.csv(temp_fc_sales, "fc_outbound.csv")
# 
# temp_fc <- fread("fc_list.csv")
# lzn_mapping_1 <- left_join(temp_fc, lzn_mapping, by=c("fc"="src_fc_code")) 
# table(lzn_mapping_1$fc)


# Continue
lzn <- lzn_mapping %>% filter(lzn_value!="" & lzn_value!='0' & lzn_value!='NULL') %>%
  rename(fc=src_fc_code) %>%
  mutate(dest_pincode=as.factor(dest_pincode),src_pincode=as.factor(src_pincode))


lz <- lzn %>% filter(lzn_value %in% c("L1","L2","Z1","Z2")) 

# temp_lzn_fc <- temp_lzn %>% group_by(src_pincode) %>% summarise(wh_count=n_distinct(warehouse))
# temp_lzn <- lzn %>% rename(lzn=lzn_value, warehouse=fc) %>% select(-refresh_id)
# temp_sales_lzn <- sales %>% left_join(temp_lzn, by=c("source_pincode"="src_pincode","dest_pincode")) %>%
#   mutate(fc=as.character(fc),warehouse=as.character(warehouse))
# temp_nomatch <- temp_sales_lzn %>% filter(fc!=warehouse & lzn_value!=lzn) %>% select(fc,warehouse,source_pincode,dest_pincode,lzn_value,lzn,sales)
# 
# table(temp_sales_lzn$lzn)
# table(temp_sales_lzn$lzn_value)


vendor_site_2 <- vendor_site %>% group_by(fsn,fc) %>% summarize(vendor_count=n_distinct(vsid)) %>% as.data.frame()

fsn_category <- sales[!duplicated(sales[,c(3,7)]),c(3,7)] # Fsn - category list for week's sales

vendor_adherence %<>% mutate(poi_created_day = as.Date(as.character(poi_created_day), format = "%Y%m%d")) %>%
  right_join(fsn_category, by="fsn") # join to be removed, right join to only capture 

cat_lead_time %<>% rename(cat_lead_time = lead_time)
lead_time1  <- lead_time %>% 
  right_join(fsn_category, by="fsn") %>% # join to be removed 
  rename(fc=warehouse) %>%
  left_join(cat_lead_time, by = 'category') %>%
  mutate(lead_time = as.numeric(levels(lead_time))[lead_time],
         policy_lt=ifelse(lead_time<1,cat_lead_time,lead_time),
         reco_week=week(wk_start_date-policy_lt)) %>% #check if any max treatement needs to be done?
  select(-bu,-super_cat) 

preferred_warehouse %<>% mutate(pincode=as.factor(as.character(pincode))) %>%
  select(pincode, preferred_fc_non_large) %>% rename(dest_pincode=pincode, preferred_fc=preferred_fc_non_large)

preferred_fc <- preferred_warehouse %>% mutate(pref_fc=substr(preferred_fc,1,regexpr(',',preferred_fc)-1)) %>%
  select(dest_pincode, pref_fc)

offer_sales_fsn_dp <- offer_sales %>% filter(week_num==ru_week) %>% group_by(fsn, dest_pincode) %>%
  summarise(offer_sales = sum(offer_sales)) %>%
  mutate(dest_pincode=as.factor(as.character(dest_pincode)))

fsn_fc_l1 <- fsn_fc_l0 %>% rename(fc=warehouse, reco_week=week) %>% 
  mutate_at(vars(c(starts_with("forecast"), ends_with("qty"))),as.numeric)
forecast_cols <- c("system_qty", "max_doh_qty", "ipc_reviewed_qty", "cdo_reviewed_qty", "po_qty",
                   "po_received_qty", "po_rejected_qty", "po_cancel_qty", "forecast_n", "forecast_n1", 
                   "forecast_n2", "forecast_n3", "forecast_n4", "forecast_n5", "forecast_n6", 
                   "forecast_n7")
fsn_fc_l1[forecast_cols][is.na(fsn_fc_l1[forecast_cols])] <- 0

# temp_fsn_fc_agg <- fsn_fc_l1[,5:20] %>%
#   summarise_all(funs(sum))

#### Unmapped Sales ####
unmapped_sales <- sales %>% 
  # filter(unmapped_sales>0) %>%
  group_by(fsn,dest_pincode) %>%
  mutate(unmapped_sales=sum(sales)-sum(ru_sales)-sum(nat_sales))
sum(unmapped_sales$unmapped_sales)

#### Placement Issues - Placement NOT Possible ####
#### 1.Serviceability Loss ####
# Creating Serviceability flag - to separate Large FCs as source
distinct_dp <- lzn %>% select(dest_pincode) %>% distinct 
dp_serv <- lz %>%
  group_by(dest_pincode) %>% summarise(lz_fc_count = n_distinct(fc)) %>% 
  right_join(distinct_dp, by='dest_pincode') %>%
  mutate(lz_fc_count = ifelse(is.na(lz_fc_count),0,lz_fc_count),
         is_serviceable = ifelse(lz_fc_count==0,0,1)) 
table(dp_serv$is_serviceable)

#### 2.Vendor Availability Loss ####
fsn_dp <- sales %>% select(fsn,dest_pincode,brand,bu,category,vertical) %>% distinct %>%
  mutate(dest_pincode=as.factor(dest_pincode)) # fsn-dp for sales dest pincodes

lzn_sales_dp <- subset(lzn, dest_pincode %in% sales$dest_pincode) # fc-dp mapping for sales dest pincodes
fc <- data.frame(unique(lzn_sales_dp$fc)) # FCs used for fulfilling to sales dest pincodes

fsn_dp_fc <- merge(fsn_dp,fc,by=NULL)  
names(fsn_dp_fc)[ncol(fsn_dp_fc)] <- "fc"

fsn_dp_fc_lz <- inner_join(fsn_dp_fc,lz,by=c("dest_pincode","fc")) # inner join to keeping only lz FCs to sales dest pincodes & sales fsns

# Validation
# fsn_dp_fc_temp <- fsn_dp_fc %>% select(fsn, dest_pincode, fc) %>% distinct()
# lz_temp <- lz %>% select(dest_pincode,fc) %>% distinct()
# 
# fsn_dp_fc_lz2 <- left_join(fsn_dp_fc,lz,by=c("dest_pincode","fc"))
# fsn_dp_fc_lz3 <- subset(fsn_dp_fc_lz2,!is.na(lzn_value))
# 
# 
# temp_dp_fc <- sales %>% select(dest_pincode, fc) %>% distinct()
# temp_dp_fc_lzn <- lzn %>% select(dest_pincode, fc) %>% distinct() %>% mutate(lzn_flag=1)
# 
# temp_lzn_mismatch <- left_join(temp_dp_fc,temp_dp_fc_lzn,by=c("dest_pincode","fc")) %>% filter(is.na(lzn_flag))
# temp_lzn_mismatch_fc <- as.data.frame(unique(temp_lzn_mismatch$fc))
# 
# lzn_mapping$dest_pincode <- as.character(lzn_mapping$dest_pincode)
# temp_lzn <- lzn_mapping %>% group_by(dest_pincode) %>% summarise(fc_count=n_distinct(src_fc_code))
# temp_lzn <- right_join(lzn_mapping, temp_lzn_mismatch, by=c("dest_pincode","src_fc_code"="fc")) 
# 
# sales_dp_fc <- sales %>% group_by(dest_pincode,fc,bu) %>% 
#   summarise(sales=sum(sales),
#             ru_sales=sum(ru_sales),
#             nat_sales=sum(nat_sales))
# 
# temp_missing <- left_join(temp_lzn_mismatch,sales_dp_fc,by=c("dest_pincode","fc"))  %>% filter(bu!="Large")
# write.csv(temp_missing, "lzn_issue.csv")
# 
# vendor_site_3 <- vendor_site_2 %>% select(fsn,fc) %>% distinct()
# fsn_dp_fc_lz_temp <- fsn_dp_fc_lz %>% select(fsn, dest_pincode, fc) %>% distinct()

# Continue
fsn_dp_fc_vendor <- fsn_dp_fc_lz %>% select(fsn,dest_pincode,fc) %>% distinct() %>%
  left_join(vendor_site_2,by=c("fsn","fc")) # joining vendor info

fsn_dp_vendor <- fsn_dp_fc_vendor %>% group_by(fsn,dest_pincode) %>%
  summarise(vendor_count=sum(vendor_count,na.rm=T)) %>%
  mutate(is_vendor_avl=ifelse(vendor_count==0,0,1)) # Creating vendor available flag

table(fsn_dp_vendor$is_vendor_avl)
# fsn_dp_vendor_issue <- fsn_dp_vendor %>% filter(is_vendor_avl==0) %>% select(-vendor_count)


#### 3.Low Sales Depth ####
low_sales <- sales_raw %>% filter(day < wk_start_date & day>=wk_start_date-28) %>% # not using week filter due to change of year
  group_by(fsn, category) %>% 
  summarise(sales=sum(sales)) %>% 
  left_join(min_sale_threshold, by = "category") %>%
  mutate(min_sales_depth=ifelse(is.na(min_sales_depth),0,min_sales_depth)) %>%
  filter(sales<=min_sales_depth)

#### 4.Exclusion Loss ####
exclusion_list %<>% mutate(skip_date=as.Date(skip_upto), is_excluded=ifelse(skip_date>=wk_start_date,1,0)) %>%
  filter(is_excluded==1)

#### Aggregating placement losses ####
# Joining serviceability flag & vendor flag into sales file
sales_fsn_dp <- sales %>% group_by(fsn,dest_pincode) %>%
  summarise(sales=sum(sales)
            ,ru_sales=sum(ru_sales)
            ,nat_sales=sum(nat_sales)
            # ,rru_sales=sum(rru_sales)
  )

# temp_sales <- sales_fsn_dp %>% select(fsn, dest_pincode) %>% distinct()

# Creating low sale flag
low_sale_fsn <- sales_fsn_dp %>% select(fsn) %>% distinct() %>%
  inner_join(low_sales[,c("fsn")], by="fsn") %>% # to fix low_sale_depth issue
  mutate(low_sale_flag=1)

sales_fsn_dp_no_plc <- sales_fsn_dp %>% 
  left_join(dp_serv[c('dest_pincode','is_serviceable')], by="dest_pincode") %>%
  left_join(subset(fsn_dp_vendor,select=-vendor_count), by=c("fsn","dest_pincode")) %>%
  left_join(low_sale_fsn, by="fsn") %>%
  left_join(exclusion_list[,c("fsn","is_excluded")], by="fsn")

placement_cols <- c("is_serviceable","is_vendor_avl","low_sale_flag","is_excluded")    
sales_fsn_dp_no_plc[placement_cols][is.na(sales_fsn_dp_no_plc[placement_cols])] <- 0

table(sales_fsn_dp_no_plc$is_serviceable)
table(sales_fsn_dp_no_plc$is_vendor_avl)

sales_fsn_dp_no_plc_2 <- sales_fsn_dp_no_plc %>%  
  mutate(serviceability_loss = ifelse(is_serviceable==0,nat_sales,0)
         ,vendor_loss = ifelse(is_serviceable==1 & is_vendor_avl==0,nat_sales,0)
         ,temp_var = nat_sales-serviceability_loss-vendor_loss
         ,low_sale_depth_loss = ifelse(low_sale_flag==1, temp_var,0)
         ,exclusion_loss = ifelse(is_excluded==1, temp_var-low_sale_depth_loss,0)
         ,placement_loss = serviceability_loss+vendor_loss+low_sale_depth_loss + exclusion_loss
         ,temp_var = nat_sales - placement_loss
  )

# Validation
temp_plc <- sales_fsn_dp_no_plc_2 %>% filter(low_sale_depth_loss>0 & exclusion_loss>0)
sum(sales_fsn_dp_no_plc_2$serviceability_loss)
sum(sales_fsn_dp_no_plc_2$vendor_loss)
sum(sales_fsn_dp_no_plc_2$low_sale_depth_loss)
sum(sales_fsn_dp_no_plc_2$exclusion_loss)
sum(sales_fsn_dp_no_plc_2$placement_loss)

sum(sales_fsn_dp_no_plc_2$serviceability_loss)/sum(sales$sales)*100
sum(sales_fsn_dp_no_plc_2$vendor_loss)/sum(sales$sales)*100
sum(sales_fsn_dp_no_plc_2$low_sale_depth_loss)/sum(sales$sales)*100
sum(sales_fsn_dp_no_plc_2$exclusion_loss)/sum(sales$sales)*100
sum(sales_fsn_dp_no_plc_2$placement_loss)/sum(sales$sales)*100

sales_fsn_dp_no_plc_3 <- sales_fsn_dp_no_plc_2 %>% filter(placement_loss>0)
sum(sales_fsn_dp_no_plc_3$placement_loss)
sum(sales_fsn_dp_no_plc_3$nat_sales)
sum(sales_fsn_dp_no_plc_2$temp_var)

# Other sales aggregations

# sales_vertical_dp <- sales %>% group_by(vertical,dest_pincode) %>%
#   summarise(sales=sum(sales),
#             ru_sales=sum(ru_sales),
#             nat_sales=sum(nat_sales),
#             rru_sales=sum(rru_sales))

sales_fsn_dp_fc <- sales %>% group_by(fsn,dest_pincode,fc) %>%
  summarise(sales=sum(sales)
            ,ru_sales=sum(ru_sales)
            ,nat_sales=sum(nat_sales)
            # ,rru_sales=sum(rru_sales)
  )

sum(sales_fsn_dp_fc$nat_sales)



#### Placement Possible ####
#### 5.FE Loss ####
## Identifying the national fiu ids for alpha sellers (ru leakage) ###
explain_ff %<>% rename(dest_pincode = destination_pincode) %>% mutate(date= as.Date(as.character(day), format = "%Y%m%d"),
                                     week= week(date)) %>%  
  filter(week==ru_week)

nat_fiu <- explain_ff %>%
  filter(lzn_value %in% c("N1","N2") & is_accepted=="true") 

lz_issue <- explain_ff %>%   # Identifying the FE issue
  filter(fiu_id %in% nat_fiu$fiu_id & (lzn_value %in% c("L1","L2","Z1","Z2")) 
         & effective_inventory>0)

fe_issue <- lz_issue %>% group_by(fsn,dest_pincode) %>% 
  summarize(fe_issue=n_distinct(fiu_id)) %>% 
  mutate(dest_pincode=as.factor(as.character(dest_pincode))) %>%
  as.data.frame()
sum(fe_issue$fe_issue)

#### 6.Forecast Error Loss ####
reco_leadtime <- inner_join(fsn_fc_l1,lead_time1, by=c("fsn","fc","reco_week")) %>%
  select(-reco_week) %>% 
  mutate(policy_lt_week = ceiling(policy_lt/7),
         forecast_qty = ifelse(policy_lt_week==1,forecast_n,
                               ifelse(policy_lt_week==2,forecast_n1,
                                      ifelse(policy_lt_week==3,forecast_n2,
                                             ifelse(policy_lt_week==4,forecast_n3,
                                                    ifelse(policy_lt_week==5,forecast_n4,
                                                           ifelse(policy_lt_week==6,forecast_n5,
                                                                  ifelse(policy_lt_week==7,forecast_n6,
                                                                         ifelse(policy_lt_week==8,forecast_n7,
                                                                                sum(forecast_n,forecast_n1,forecast_n2,forecast_n3,forecast_n4,forecast_n5,forecast_n6,forecast_n7)/8)))))))))

sales_fsn_pref_fc <- right_join(preferred_fc,sales_fsn_dp,by='dest_pincode') %>%
  group_by(fsn,pref_fc) %>% summarise(sales=sum(sales))  %>% as.data.frame()

forecast_issue <- left_join(sales_fsn_pref_fc,reco_leadtime[c("fsn","fc","forecast_qty")], by=c("fsn","pref_fc"="fc")) %>%
  mutate(forecast_qty=ifelse(is.na(forecast_qty),0,forecast_qty),
         forecast_issue=pmax(sales-forecast_qty,0)) %>%
  rename(fc=pref_fc)

forecast_flag = ifelse(sum(sales$sales)-sum(forecast_issue$sales)==0,"forecast_loss = Ok",
                       sum(sales$sales)-sum(forecast_issue$sales))


# temp_forecast <- sales_fsn_pref_fc %>% select(fsn, pref_fc) %>% distinct()

#### 7-9.N-DOH, IPC Override, CDO Override Loss ####
override_loss <- reco_leadtime[c("fsn","fc","system_qty","max_doh_qty","ipc_reviewed_qty","cdo_reviewed_qty","po_qty")] %>%
  mutate(ndoh_loss1=pmax(system_qty-max_doh_qty,0),
         ipc_override_loss1=pmax(max_doh_qty-ipc_reviewed_qty,0),
         cdo_override_loss1=pmax(ipc_reviewed_qty-cdo_reviewed_qty,0),
         po_loss1=pmax(cdo_reviewed_qty-po_qty,0))

# temp_override_agg <- override_loss[,3:11] %>% 
#   summarise_all(funs(sum))
# 
# temp_override_sample <- override_loss %>% filter(ipc_override_loss1>0 | cdo_override_loss1 >0 | po_loss1 >0)

#### 10.Vendor Adherence Loss ####
vendor_adh <- vendor_adherence %>% left_join(lead_time1[c("fsn","fc","policy_lt")], by = c("fsn", "fc")) %>%
  left_join(cat_lead_time[c("category","cat_lead_time")], by = "category") %>%
  mutate(policy_lt = ifelse(is.na(policy_lt),cat_lead_time,policy_lt))

vendor_adh_2 <- vendor_adh %>% 
  filter((po_status == "approved" & (poi_created_day <= (wk_end_date-7)-policy_lt)) | 
           # Open till end of previous week
           (po_status == "completed" & (poi_created_day + 2*policy_lt >= wk_start_date))) %>%
  # Closed, but not enough time to replan & make up for it
  mutate(vendor_adherence_loss = pmax(as.numeric(poi_ordered_qty)-as.numeric(poi_received_qty),0)) %>%
  group_by(fsn,fc) %>%
  summarise(vendor_adherence_loss1 = sum(vendor_adherence_loss))

# min(vendor_adh_2$vendor_adherence_loss1)

#### 11.Promotions Loss ####
# Only to be joined with main sales file 

#### 12.Joins & final aggregation ####
sales_fsn_dp_fc_plc <- anti_join(sales_fsn_dp_fc[c("fsn","dest_pincode","fc","sales","ru_sales","nat_sales")],
                                 sales_fsn_dp_no_plc_3,by=c("fsn","dest_pincode")) %>% # removing fsn-dp where no placement possible
  data.frame()

sum(sales_fsn_dp_fc_plc$nat_sales)+
sum(sales_fsn_dp_no_plc_3$nat_sales)
sum(sales_fsn_dp_fc_plc$nat_sales)/sum(sales$sales)*100

# Computing FE loss
sales_fsn_dp_plc_1 <- sales_fsn_dp_fc_plc %>% group_by(fsn,dest_pincode) %>%
  summarise(nat_sales=sum(nat_sales))%>%
  left_join(fe_issue, by=c("fsn","dest_pincode")) %>% 
  mutate(fe_loss = pmin(ifelse(is.na(fe_issue),0,fe_issue),nat_sales)
         ,fsn_dp_loss = pmax(nat_sales-fe_loss,0)
         # ,fe_flag = ifelse(nat_sales<fe_loss,1,0)
  ) 

# # Checking group by
# sum(sales_fsn_dp_plc_1$nat_sales)-
# sum(sales_fsn_dp_fc_plc$nat_sales)
# 
# sum(sales_fsn_dp_plc_1$fsn_dp_loss)
# sum(sales_fsn_dp_plc_1$fe_loss)
# temp_sum <- sales_fsn_dp_plc_1[,3:6] %>% 
#   summarise_all(funs(sum))

# Continue
fsn_dp_fc_ratio <- fsn_dp_fc_vendor %>% filter(vendor_count>0) # Filtering fsn-dp-fc where placement is possible  
fsn_dp_ratio <- fsn_dp_fc_ratio %>% group_by(fsn,dest_pincode) %>% summarise(fc_count = n_distinct(fc))
max(fsn_dp_ratio$fc_count)
n_distinct(lzn$fc) # 59 FCs in lzn file
# fc_list <- as.data.frame(unique(sales$fc))
# names(fc_list) <- "fc"
# write.csv(fc_list,"fc_list.csv")

# fsn_dp_fc_ratio_temp <- fsn_dp_fc_ratio %>% select(fsn,fc,dest_pincode) %>% distinct()
# fsn_dp_fc_vendor_temp <- fsn_dp_fc_vendor %>% select(fsn,fc,dest_pincode) %>% distinct()

fsn_dp_fc_ratio_2 <- left_join(fsn_dp_fc_ratio,fsn_dp_ratio, by=c("fsn","dest_pincode")) %>%
  mutate(ratio=1/fc_count) %>% select(fsn,dest_pincode,fc,ratio)
min(fsn_dp_fc_ratio_2$ratio)

# sales_fsn_dp_fc_plc_2 <- left_join(fsn_dp_fc_ratio_2,sales_fsn_dp_fc_plc,by=c("fsn","dest_pincode","fc"))
# sum(sales_fsn_dp_fc_plc_2$sales,na.rm = T)
# sum(sales_fsn_dp_fc_plc$sales) # Issue in ratio join
# #Note : DO not sum the sales_sum_6 to get DP - FSN level sales

# Temp
# temp_dd <- sales_fsn_dp_fc_plc_2 %>% group_by(fsn,dest_pincode) %>%
#   summarise(ratio_sum=sum(ratio))
# table(temp_dd$ratio_sum)
# 
# temp_da <- sales_fsn_dp_fc_plc_2 %>% filter(is.na(ratio))
# sum(temp_da$nat_sales)
# unique(temp_da$fc)

# continue
# sales_fsn_dp_fc_plc_3 <- left_join(sales_fsn_dp_fc_plc_2,sales_fsn_dp_plc_1[c("fsn","dest_pincode","fsn_dp_loss")], by=c("fsn","dest_pincode")) %>%
#   mutate(fsn_dp_fc_loss=ratio*fsn_dp_loss)
# sum(sales_fsn_dp_fc_plc_3$fsn_dp_fc_loss,na.rm = T)
# sum(sales_fsn_dp_plc_1$fsn_dp_loss) # Main Issue in fsn dp loss 

sales_fsn_dp_fc_plc_2 <- left_join(fsn_dp_fc_ratio_2,sales_fsn_dp_plc_1[c("fsn","dest_pincode","fsn_dp_loss")], by=c("fsn","dest_pincode")) %>%
  mutate(fsn_dp_fc_loss=ratio*fsn_dp_loss)
sum(sales_fsn_dp_fc_plc_2$fsn_dp_fc_loss,na.rm = T)
sum(sales_fsn_dp_plc_1$fsn_dp_loss) # Main Issue in fsn dp loss 

# temp_fsn_fc_plc_3 <- sales_fsn_dp_fc_plc_3 %>% group_by(fsn,dest_pincode,ratio) %>%
#   summarise(loss=sum(fsn_dp_fc_loss, na.rm=T)) %>%
#   left_join(sales_fsn_dp_plc_1[c("fsn","dest_pincode","fsn_dp_loss")], by=c("fsn","dest_pincode"))
# 
# temp_fsn_fc_plc_31 <- temp_fsn_fc_plc_3 %>% 
#   mutate(fsn_dp_loss=ifelse(is.na(fsn_dp_loss),0,fsn_dp_loss)
#          ,diff = fsn_dp_loss-loss
#   )
# 
# temp_fsn_fc_plc_311 <- temp_fsn_fc_plc_31 %>% filter(diff>0)


sales_fsn_dp_fc_plc_collated <- join_all(list(sales_fsn_dp_fc_plc_2,forecast_issue[c("fsn","fc","forecast_issue")]
                                              ,override_loss[c("fsn","fc","ndoh_loss1","ipc_override_loss1","cdo_override_loss1","po_loss1")]
                                              ,vendor_adh_2[c("fsn","fc","vendor_adherence_loss1")]
                                              ),by=c("fsn","fc"),type='left')

qty_cols <- c(
  # "sales","ru_sales","nat_sales",
  "fsn_dp_loss", "fsn_dp_fc_loss", "forecast_issue", "ndoh_loss1", "ipc_override_loss1", "cdo_override_loss1", "po_loss1", "vendor_adherence_loss1")
sales_fsn_dp_fc_plc_collated[qty_cols][is.na(sales_fsn_dp_fc_plc_collated[qty_cols])] <- 0

sum(sales_fsn_dp_fc_plc_collated$fsn_dp_fc_loss)

sales_fsn_dp_fc_plc_collated2 <- sales_fsn_dp_fc_plc_collated %>%
  mutate(forecast_loss = pmin(fsn_dp_fc_loss,forecast_issue),
                                         temp_var = pmax(fsn_dp_fc_loss-forecast_loss,0),
                                         ndoh_loss = pmin(temp_var,ndoh_loss1),
                                         temp_var2 = pmax(temp_var-ndoh_loss1,0),
                                         ipc_override_loss = pmin(temp_var2,ipc_override_loss1),
                                         temp_var3 = pmax(temp_var2-ipc_override_loss1,0),
                                         cdo_override_loss = pmin(temp_var3,cdo_override_loss1),
                                         temp_var4 = pmax(temp_var3-cdo_override_loss1,0),
                                         po_loss = pmin(temp_var4,po_loss1), # check if this needs to exist
                                         temp_var5 = pmax(temp_var4-po_loss1,0),
                                         vendor_adherence_loss = pmin(temp_var5,vendor_adherence_loss1),
                                         temp_var6 = pmax(temp_var5-vendor_adherence_loss1,0))


loss_cols <- c("forecast_loss", "ndoh_loss", "ipc_override_loss", "cdo_override_loss", "po_loss", "vendor_adherence_loss","temp_var6")
sum(sales_fsn_dp_fc_plc_collated2[loss_cols])
sum(sales_fsn_dp_fc_plc_2$fsn_dp_fc_loss,na.rm = T) # Attributed correctly
sum(sales_fsn_dp_fc_plc_collated2$temp_var6)

# ru_agg2 <- sales_fsn_dp_fc_plc_collated2[,6:24] %>% 
#   summarise_all(funs(sum))
# max(sales_fsn_dp_fc_plc_collated2$forecast_loss)

# write.csv(ru_agg2,"join_test.csv")

# # Checking for zero override losses
# temp_ndoh <- sales_fsn_dp_fc_plc_collated2 %>% filter(fsn_dp_fc_loss>0
#                                                       # , forecast_loss==0
#                                                       , ndoh_loss1>0) 
# sum(temp_ndoh$ndoh_loss)
# temp_override <- sales_fsn_dp_fc_plc_collated2 %>% filter(
#   # fsn_dp_fc_loss>0,
#                                                           # , forecast_loss==0
#                                                            ipc_override_loss1>0 | cdo_override_loss1 >0 | po_loss1 >0)  # Still coming zero before 
# sum(temp_override$fsn_dp_fc_loss)
# sum(sales_fsn_dp_fc_plc_collated$vendor_adherence_loss1)
# 
# temp_vendor_adh <- sales_fsn_dp_fc_plc_collated2 %>% filter(fsn_dp_fc_loss>0
#                                                             # , forecast_loss==0
#                                                             , vendor_adherence_loss1>0)
# sum(temp_vendor_adh$vendor_adherence_loss)

# continue
no_placement_loss <- sales_fsn_dp_no_plc_3 %>% 
  select(fsn,dest_pincode,serviceability_loss,vendor_loss,low_sale_depth_loss,exclusion_loss)
sum(no_placement_loss$exclusion_loss)

placement_loss_1 <- sales_fsn_dp_plc_1 %>% select(fsn,dest_pincode,fe_loss
                                                  # ,exclusion_loss
                                                  )

placement_loss_2 <- sales_fsn_dp_fc_plc_collated2%>% group_by(fsn,dest_pincode) %>%
  summarise(forecast_loss = sum(forecast_loss, na.rm=T),
            ndoh_loss = sum(ndoh_loss, na.rm=T),
            ipc_override_loss = sum(ipc_override_loss, na.rm=T),
            cdo_override_loss = sum(cdo_override_loss, na.rm=T),
            po_loss = sum(po_loss, na.rm=T),
            vendor_adherence_loss = sum(vendor_adherence_loss, na.rm=T),
            residual = sum(temp_var6, na.rm=T))


ru_loss_binded <- bind_rows(
                            # unmapped_sales,
                            no_placement_loss, placement_loss_1, placement_loss_2)
ru_loss_fsn_dp <- ru_loss_binded %>% group_by(fsn,dest_pincode) %>% 
  summarise(
    # unmapped_sales=sum(unmapped_sales, na.rm=T),
            serviceability_loss=sum(serviceability_loss, na.rm=T)
            ,vendor_loss=sum(vendor_loss, na.rm=T)
            ,low_sale_depth_loss=sum(low_sale_depth_loss, na.rm=T)
            ,exclusion_loss=sum(exclusion_loss, na.rm=T)
            ,fe_loss=sum(fe_loss, na.rm=T)
            ,forecast_loss = sum(forecast_loss, na.rm=T)
            ,ndoh_loss = sum(ndoh_loss, na.rm=T)
            ,ipc_override_loss = sum(ipc_override_loss, na.rm=T)
            ,cdo_override_loss = sum(cdo_override_loss, na.rm=T)
            ,po_loss = sum(po_loss, na.rm=T)
            ,vendor_adherence_loss = sum(vendor_adherence_loss, na.rm=T)
            ,residual = sum(residual, na.rm=T))

# sum(ru_loss_fsn_dp$unmapped_sales,na.rm = T)

# Joining Promotions Loss
ru_loss_fsn_dp2 <- ru_loss_fsn_dp %>%
  left_join(offer_sales_fsn_dp, by=c("fsn","dest_pincode")) %>%
  mutate(offer_sales=ifelse(is.na(offer_sales),0,offer_sales)
         ,promotions_loss=pmax(residual-offer_sales,0)
         ,residual = pmax(residual-promotions_loss,0)
  )

ru_waterfall <- left_join(sales_fsn_dp,ru_loss_fsn_dp2, by=c("fsn","dest_pincode")) %>%
  mutate(ru_computed_week=ru_week, unmapped_sales=sales-ru_sales-nat_sales) %>%
  select("fsn", "dest_pincode", "ru_computed_week", "sales", "ru_sales", "nat_sales"
         , "unmapped_sales"
         , "serviceability_loss", "vendor_loss","low_sale_depth_loss","exclusion_loss", "fe_loss", 
         "forecast_loss", "ndoh_loss", "ipc_override_loss", "cdo_override_loss"
         , "po_loss"
         , "vendor_adherence_loss", "promotions_loss", "residual")
  
## With unmapped sales - uncomment while including unmapped sales
ru_waterfall[,4:20][is.na(ru_waterfall[,4:20])] <- 0
ru_agg <- ru_waterfall[,4:20] %>%
  summarise_all(funs(sum)) %>% mutate(ru_computed_week = ru_week)

# # Removing unmapped sales temporarily - comment next two lines later
# ru_waterfall[,4:19][is.na(ru_waterfall[,4:19])] <- 0
# ru_agg <- ru_waterfall[,4:19] %>% 
#   summarise_all(funs(sum)) %>% mutate(ru_computed_week = ru_computed_week)
# # Continue


write.csv(ru_agg,paste0(save_path,"/ru_agg_wk",ru_week,".csv"),row.names = F)

# # Final Diff
# sum(ru_waterfall$nat_sales)-
#   sum(ru_waterfall[6:18]) 
# 
# # Placement issues aggregated correctly
# sum(ru_waterfall[,6:7])-
#   sum(sales_fsn_dp_no_plc_2$serviceability_loss)-
#   sum(sales_fsn_dp_no_plc_2$vendor_loss)
# 
# # Non placement issues also aggregated correctly - power -8 diff
# sum(ru_waterfall[,9:16])-
#   sum(sales_fsn_dp_fc_plc_collated[loss_cols])
# sum(sales$nat_sales)
# sum(sales_fsn_dp$nat_sales)
# 

#### Fetching Keys & exporting waterfall ####
key_fsn <- sales %>% select(fsn,product_id_key) %>% distinct()
key_dp <- sales %>% select(dest_pincode, destination_pincode_key) %>% distinct()
# key_dp_temp <- sales %>% group_by(dest_pincode) %>% summarise(key_count=n_distinct(destination_pincode_key)) %>% arrange(-key_count)
ru_waterfall_output <- ru_waterfall %>% left_join(key_fsn, by="fsn") %>% left_join(key_dp, by="dest_pincode") %>% 
  left_join(preferred_warehouse, by="dest_pincode")

# ru_waterfall_output <- ru_waterfall
# sum(is.na(ru_waterfall_output$product_id_key))
# temp_key <- sales[is.na(sales$product_id_key),]
# sum(is.na(ru_waterfall_output$destination_pincode_key))

temp_ru <- head(ru_waterfall_output,100)
# write.table(ru_waterfall_output,paste0(save_path,"/ru_waterfall.tsv"),row.names = F,sep="\t")
# write.table(ru_waterfall_output,paste0(save_path,"/ru_waterfall_wk",ru_week,".tsv"),row.names = F,sep="\t")
# fwrite(ru_waterfall_output,paste0(save_path,"/ru_waterfall_wk",ru_week,".tsv"),sep="\t") # For testing & debugging, weekname in output filename

fwrite(ru_waterfall_output,paste0(save_path,"/ru_waterfall_output.tsv"),sep="\t")

# save.image(paste0(save_path,"/ru_waterfall_wk",ru_week,".RData"))
# write.csv(head(ru_waterfall_output,100),paste0(save_path,"/ru_waterfall.tsv"),row.names = F,sep="\t")
