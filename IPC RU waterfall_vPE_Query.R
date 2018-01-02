# PLEASE CHANGE THE PATH OF FILES IN LINES 18,19 AND 20
setwd("D:/Pulkit/ru-waterfall")
# setwd("X:/QV App/Waterfall Codes")
save_path <- "D:/Pulkit/ru-waterfall/backups/ru_waterfall_reduced_wk"
# save_path <- "X:/QV App/Explain_fulfill/backups/ru_waterfall_forecast_wk"
read_path <- "//172.20.188.71/Output2"

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
lifestyle_end_date <- as.numeric(format(wk_end_date, format="%Y%m%d"))
lifestyle_start_date <- as.numeric(format(wk_end_date-28, format="%Y%m%d"))
temp_week<-week(wk_end_date)

########## Reading files ##########
detach(package:lubridate)

# Reading the files - change the file paths
sales<- read.csv(paste0("\\\\172.20.188.71\\QV App\\RRU Inputs\\WK",temp_week,"\\sales.csv"))
# inventory <- read.csv(paste0("\\\\172.20.188.71\\QV App\\RRU Inputs\\WK",temp_week,"\\inventory.csv"))
# sales_day <- read.csv(paste0("\\\\172.20.188.71\\QV App\\RRU Inputs\\WK",temp_week,"\\sales_day.csv"))

#Reading the static files
lzn <- read.csv("\\\\172.20.188.71\\Asan\\Overall Mapping.csv")
tax <- read.csv("\\\\172.20.188.71\\Output2\\Tax based output.csv")
# vendor<- read.csv("\\\\172.20.188.71\\Output2\\Vendor_availability.csv")
vendor <- readRDS("\\\\172.20.188.71\\Output2\\Vendor_availability.rds")
U2S <-read.csv("\\\\172.20.188.71\\Output2\\U2S.csv")
leadtime<-read.csv("\\\\172.20.188.71\\Output2\\LeadTime.csv")   ####### CHANGE TO NW LOCATION
cat_leadtime<-read.csv("\\\\172.20.188.71\\ipc\\Inventory Policy\\Inputs\\Lead_time_floor.csv")
min_sale_depth <- read.csv("\\\\172.20.188.71\\Output2\\min_sales_depth.csv")
exclusion_list<- read.csv("\\\\172.20.188.71\\ipc\\Instock Report\\Input\\Exclusion_list.csv")
preferred_wh <- read.csv("\\\\172.20.188.71\\Asan\\preferred_wh_mapping.csv")
# preferred_wh <- read.csv(paste0(read_path,"/preferred_wh_mapping.csv"))
# read_path <- "//172.20.188.71/Output2"
read_path <- "G:/Pulkit/Explain_Fulfill"
lzn_static <- read.csv(paste0(read_path,"/lzn_files/lzn_static.csv"))
lzn_map <- read.csv(paste0(read_path,"/lzn_files/source_id-pincode.csv"))
lzn_tier <- read.csv(paste0(read_path,"/lzn_files/sla_tier.csv"))
lzn_temp <- left_join(lzn_static,lzn_map,by=c('source_pincode'))
lzn_temp$dest_pincode <- as.factor(lzn_temp$dest_pincode)
lzn_tier <- lzn_tier %>% select(warehouse,dest_pincode,tier,tier_status) 
lzn_static$dest_pincode <- as.factor(lzn_static$dest_pincode)
lzn_tier$dest_pincode <- as.factor(lzn_tier$dest_pincode)


########## Basic Data Cleaning & flag creation ##########

# Basic Data cleaning & creating flags for the FC unavailability
sales <- sales %>% 
  filter(fsn!="") %>% 
  rename(sales=units_count) %>%
  mutate(nat_sales = sales-ru_sales)

# names(sales_day)[c(12,15)]<- c("date","sales")
# sales_day <- sales_day %>%
#   mutate(nat_sales = sales-ru_sales) %>%
#   mutate(date=as.Date(sales_day$date,origin="1899-12-30"))
# 
# inventory$inv_date <-  as.Date(inventory$inv_date,origin="1899-12-30")
names(cat_leadtime)[1]='category'

# FC unavailability -- can be improved
lz1<-names(lzn)[substr(names(lzn),1,2) %in% c('L1','L2','Z1','Z2')]
cols=c('Destination_Pincode',lz1)
lzn <- lzn[,which(names(lzn)%in% cols)]	
lzn_1 <- lzn[!(duplicated(lzn)),]
lzn_1$flag <-apply(lzn_1, 1,function(x) sum(x=="0" |x==""))
lzn_1$fc_available_flag <- ifelse(lzn_1$flag==29,0,1)# change this number for RU water or the number of FCs increase

# lzn file cleaning
lzn_2<-melt(subset(lzn_1,select= -c(flag,fc_available_flag)), id=c("Destination_Pincode"))
lzn_3 <- lzn_2 %>%
  filter(value!="" & value!='0') %>%
  rename(lzn=variable, FC=value) %>%
  mutate(Destination_Pincode=as.factor(Destination_Pincode))

# creating combination of dest_pincode & FSN -- go thru line by line
dest_fsn <- sales %>% select(fsn,dest_pincode,brand,bu,category,vertical) %>% distinct
fc<-data.frame(unique(lzn_3$FC)) 
dest_fsn_fc <- merge(dest_fsn,fc,by=NULL) 
names(dest_fsn_fc)[ncol(dest_fsn_fc)]<- "FC"
lzn_4 <- subset(lzn_3, Destination_Pincode %in% sales$dest_pincode)

#Cleaning tax, vendor and U2S files -- go thru line by line
tax_1<- subset(tax,select=-c(X)  ,FSN %in% sales$fsn)
tax_1$flag<-apply(tax_1[,-1],1,function(x) sum(x))
tax_2<- subset(tax_1,flag !=13, select=-flag) ## Total no. of FCs
tax_trn<- melt(tax_2, id=c("FSN"))
names(tax_trn)[2:3]<- c("FC","tax_flg")
#tax_trn$FC <- substr(tax_trn$variable,3,length(tax_trn$variable))
tax_trn$tax_flg=1
tax_trn_1 <- subset(tax_trn, tax_flg!=1 & FSN %in% sales$fsn)
names(vendor)[names(vendor)=='Brand']<-'brand'
vendor_1<-melt(vendor, id=c("brand","Vertical","Category"))
names(vendor_1)[4:5]<- c('FC','vendor_flag')
vendor_1 <- vendor_1 %>% mutate(brand=toupper(brand))
u2s_1<-melt(U2S, id=c('brand','vertical','super_category','category'))
names(u2s_1)[ncol(u2s_1)]<- "u2s_flag"
names(u2s_1)[ncol(u2s_1)-1]<- "FC"
#1

#### FF Loss Assignment ####
explain_query <-  paste0(
  "select 
  explain.fiu_id as fiu_id,
  explain.warehouse as source_id,
  explain.lpe_tier as lpe_tier,
  explain.effective_inventory as effective_inventory,
  explain.accepted as accepted,
  fu.fulfill_item_unit_destination_pincode as dest_pincode,
  p.product_id as fsn
  from  bigfoot_external_neo.scp_fulfillment__explain_fulfill_hive_fact as explain
  
  left join bigfoot_external_neo.scp_fulfillment__fulfillment_unit_hive_fact as fu
  ON fu.fulfill_item_unit_id = explain.fiu_id
  
  left join bigfoot_external_neo.sp_product__product_categorization_hive_dim as p
  on explain.product_id_key = p.product_categorization_hive_dim_key
  
  left join bigfoot_external_neo.sp_seller__seller_hive_dim as s
  on explain.seller_id_key=s.seller_hive_dim_key
  
  where fulfill_item_unit_status_modified not in ('warehouse_cancellation_requested','customer_cancelled','procurement_cancellation_requested','seller_cancelled')
  and s.is_first_party_seller = 1
  and p.analytic_business_unit <> 'Large'
  and explain.unit_reserve_actual_date_key>=", 
  as.numeric(format(wk_start_date,"%Y%m%d")),
  " and explain.unit_reserve_actual_date_key <=", 
  as.numeric(format(wk_end_date,"%Y%m%d")))

hive_connect <- odbcConnect("Hive_DB")
explain_ff_data <- sqlQuery(hive_connect,explain_query)
colnames(explain_ff_data)<-gsub(".*\\.(.*)", "\\1", colnames(explain_ff_data))
explain_ff_data$dest_pincode <- as.factor(explain_ff_data$dest_pincode)

explain_data <- explain_ff_data %>% 
  left_join(lzn_temp, by=c("dest_pincode"="dest_pincode","source_id"="source_id")) %>%
  left_join(lzn_tier,by=c("dest_pincode"="dest_pincode","source_id"="warehouse",
                          "lpe_tier"="tier")) %>%
  mutate(tier_status=ifelse(is.na(tier_status),"INACTIVE","ACTIVE")) #removing NAs from tier_status


#### Low Sales data for 4 weeks ####

#min_sale <- max(min_sale_depth$min_sales_depth)

odbcDataSources(type = c("all", "user", "system"))
hive_connect <- odbcConnect("Hive_DB")
ls_4wk_sale<-sqlQuery(hive_connect,'SET mapred.job.queue.name=adhoc')

query<-paste0("
              SELECT 
              count(distinct ff.fulfill_item_unit_id) as sales,
              p.product_id as fsn,
              p.analytic_vertical as vertical,
              p.analytic_sub_category as sub_category,
              p.analytic_category as category,
              p.analytic_super_category as super_category,
              p.analytic_business_unit as bu,
              p.title as product_title,
              p.brand as brand
              from bigfoot_external_neo.scp_fulfillment__fulfillment_unit_hive_fact ff
              left join bigfoot_external_neo.sp_product__product_categorization_hive_dim p
              on ff.product_id_key = p.product_categorization_hive_dim_key
              left join bigfoot_external_neo.scp_oms__date_dim_fact d
              on ff.fulfill_item_unit_reserve_actual_date_key = d.date_dim_key
              left outer join bigfoot_external_neo.sp_seller__seller_hive_dim seller
              on seller.seller_hive_dim_key = ff.seller_id_key
              where fulfill_item_unit_status_modified not in ('warehouse_cancellation_requested','customer_cancelled','procurement_cancellation_requested','seller_cancelled')
              and is_first_party_seller=1
              and fulfill_item_unit_reserve_actual_date_key > " , lifestyle_start_date,
              " and fulfill_item_unit_reserve_actual_date_key <=", lifestyle_end_date,
              " 
              group by
              p.product_id,
              p.analytic_vertical,
              p.analytic_sub_category,
              p.analytic_category,
              p.analytic_super_category,
              p.analytic_business_unit,
              p.brand,
              p.title
              having count(distinct ff.fulfill_item_unit_id) < 30")

ls_4wk_sale<-sqlQuery(hive_connect, query)

ls_4wk_sale_1 <- left_join(ls_4wk_sale,min_sale_depth,by="category",all.x=TRUE)
low_sale<- subset(ls_4wk_sale_1,sales < min_sales_depth)


#Reading & Cleaning Reco file for National DOH Loss and PO Adherence
temp_date<-wk_start_date - 7+as.POSIXlt(wk_start_date - 5)$wday
reco_wk1<-read.csv(paste0("\\\\172.20.188.71\\QV App\\Projections For Waterfall\\System_generated_auto_projections\\Auto_Reco_File_",temp_date,".csv"))
reco_wk2<-read.csv(paste0("\\\\172.20.188.71\\QV App\\Projections For Waterfall\\System_generated_auto_projections\\Auto_Reco_File_",temp_date-7,".csv"))
reco_wk3<-read.csv(paste0("\\\\172.20.188.71\\QV App\\Projections For Waterfall\\System_generated_auto_projections\\Auto_Reco_File_",temp_date-14,".csv"))
reco_wk4<-read.csv(paste0("\\\\172.20.188.71\\QV App\\Projections For Waterfall\\System_generated_auto_projections\\Auto_Reco_File_",temp_date-21,".csv"))
reco_wk5<-read.csv(paste0("\\\\172.20.188.71\\QV App\\Projections For Waterfall\\System_generated_auto_projections\\Auto_Reco_File_",temp_date-28,".csv"))
reco_wk6<-read.csv(paste0("\\\\172.20.188.71\\QV App\\Projections For Waterfall\\System_generated_auto_projections\\Auto_Reco_File_",temp_date-35,".csv"))
reco_wk7<-read.csv(paste0("\\\\172.20.188.71\\QV App\\Projections For Waterfall\\System_generated_auto_projections\\Auto_Reco_File_",temp_date-42,".csv"))
reco_wk8<-read.csv(paste0("\\\\172.20.188.71\\QV App\\Projections For Waterfall\\System_generated_auto_projections\\Auto_Reco_File_",temp_date-49,".csv"))
reco_wk9<-read.csv(paste0("\\\\172.20.188.71\\QV App\\Projections For Waterfall\\System_generated_auto_projections\\Auto_Reco_File_",temp_date-56,".csv"))
reco_wk10<-read.csv(paste0("\\\\172.20.188.71\\QV App\\Projections For Waterfall\\System_generated_auto_projections\\Auto_Reco_File_",temp_date-63,".csv"))

reco_file<-rbind(reco_wk1,reco_wk2,reco_wk3,reco_wk4,reco_wk5,reco_wk6,reco_wk7,reco_wk8,reco_wk9,reco_wk10)
rm(reco_wk1,reco_wk2,reco_wk3,reco_wk4,reco_wk5,reco_wk6,reco_wk7,reco_wk8,reco_wk9,reco_wk10)
save.image(paste0(save_path,temp_week,"_withReco.RData"))
#reco_file_ndoh1<-reco_wk1
names(reco_file)[1]='reco_date'
reco_file$reco_date<- with(reco_file,as.POSIXct(reco_date,format='%d/%m/%Y'))
reco_file$weeknum<- with(reco_file,week(reco_date))
reco_file$FSN<- as.factor(reco_file$FSN)

# PO Adherence New LOGIC
leadtime_1<-melt(leadtime, id=c("fsn"))
names(leadtime_1)[names(leadtime_1)=='value']='policy_lead_time'
leadtime_1$FC <- substr(leadtime_1$variable,15,length(leadtime_1$variable))
#4

####### Forecast Qty & NDOH #########
preferred_wh <- read.csv("preferred_wh_mapping.csv")

#5
# sales_forecast_2 <- right_join(preferred_wh,sales_fsn_dp,by='dest_pincode') %>%
#   group_by(fsn,preferred_wh) %>% summarise(sales=sum(sales)) %>% as.data.frame()


# # Validating new sales_forecast
# # summary(sales_forecast)
# check_var1 <- sum(sales_forecast$sales,na.rm=T)
# check_var2 <- sum(sales_forecast_2$sales,na.rm=T)
# check_var3 <- sum(sales$sales)
# check_var3-check_var2 # Sales summarized properly
# check_var2-check_var1 # Missing in pref warehouse
# n_distinct(sales$dest_pincode)
# n_distinct(preferred_wh$dest_pincode)

# sales_forecast<- sales %>% group_by(fsn,source_id) %>% summarise(sales=sum(sales))


# forecast_error<- merge(sales_forecast,reco_leadtime_1[c(-3,-4,-6)], by.x = c("fsn","source_id"), by.y= c("fsn", "FC"), all.x=TRUE)

########## Leadtime, PE, Promo #####
po_query<- "select internal_id, po.status ,origin_warehouse_id,
po.created_at,po.updated_at,
expiry_date  ,fsn,	quantity ,received_quantity,
cancel_quantity from purchase_orders as po
inner join purchase_order_items as poi 
on poi.purchase_order_id = po.id and 
date(po.created_at) >=  '2016-11-01'"
proc_connect<-  odbcConnect("Proc_b2b_flo")
po_adh <- sqlQuery(proc_connect,po_query) # needed only for 10 weeks


po_adh$created_at <- with(po_adh,as.Date(po_adh$created_at))
po_adh$weeknum<-week(po_adh$created_at)
names(po_adh)[3]='FC'
#6
#LEADTIME ERROR
odbcDataSources(type = c("all", "user", "system"))
hive_connect <- odbcConnect("Hive_DB")
po_query_1<-sqlQuery(hive_connect,'SET mapred.job.queue.name=adhoc')
po_query<-  "select  po_oroginal_warehouse_id,
poi_updated_date_key ,poi_initiated_date_key,fsn,poi_status,
sum(poi_orderd_quantity) as poi_ordered_quantity ,
sum(poi_received_quantity) as poi_received_quantity
from  bigfoot_external_neo.retail_procurement__fki_po_poi_l0_fact as po
where po_created_date_key >=  20161101 
group by po_oroginal_warehouse_id, poi_updated_date_key,poi_initiated_date_key,fsn,poi_status"
po <- sqlQuery(hive_connect, po_query)

po$lead_time <- with (po,  poi_updated_date_key - poi_initiated_date_key)

#7
# OFFERS AND PROMOTIONS
hive_connect <- odbcConnect("Hive_DB")
offer_query<-paste0("select order_item_product_id as fsn, sum(gross_units_per_offer) as offer_sales
                    from bigfoot_external_neo.cp_santa__offer_sales_cancellations_hive_fact a
                    where offer_flag=1 and is_first_party_seller=1 and a.date_dim_key>=", as.numeric(format(wk_start_date,"%Y%m%d")),
                    " and a.date_dim_key <=", as.numeric(format(wk_end_date,"%Y%m%d")) ,"
                    group by
                    order_item_product_id")

offer_sales<-sqlQuery(hive_connect, offer_query)
names(offer_sales)[c(1,2)]= c('fsn','offer_sales')

#8
save.image(paste0(save_path,temp_week,"_all_data.RData"))