#### Placement Issues - Placement NOT Possible ####

#### 1.Serviceability Loss ####
# Creating Serviceability flag - to separate Large FCs as source
distinct_dp <- lzn %>% select(dest_pincode) %>% distinct 
serv_dp <- lz %>%
  group_by(dest_pincode) %>% summarise(lz_fc_count = n_distinct(fc)) %>% 
  right_join(distinct_dp, by='dest_pincode') %>%
  mutate(lz_fc_count = ifelse(is.na(lz_fc_count),0,lz_fc_count),
         is_serviceable_flag = ifelse(lz_fc_count==0,0,1))
# table(serv_dp$is_serviceable_flag)

# Joining serviceability flag into sales file
sales_fsn_dp <- sales %>% group_by(fsn,dest_pincode) %>%
  summarise(sales=sum(sales),
            ru_sales=sum(ru_sales),
            ru_loss=sum(ru_loss),
            rru_sales=sum(rru_sales))

sales_fsn_dp_serv <- sales_fsn_dp %>% left_join(serv_dp[c('dest_pincode','is_serviceable_flag')], by="dest_pincode") 
# table(sales_fsn_dp_serv$is_serviceable_flag)


#### 2.Vendor Availability Loss ####
fsn_dp <- sales %>% select(fsn,dest_pincode,brand,bu,category,vertical) %>% distinct %>%
  mutate(dest_pincode=as.factor(dest_pincode)) # fsn-dp for sales dest pincodes

lzn_sales_dp <- subset(lzn, dest_pincode %in% sales$dest_pincode) # fc-dp mapping for sales dest pincodes
fc <- data.frame(unique(lzn_sales_dp$fc)) # FCs used for fulfilling to sales dest pincodes

fsn_dp_fc <- merge(fsn_dp,fc,by=NULL)  
names(fsn_dp_fc)[ncol(fsn_dp_fc)] <- "fc"

fsn_dp_fc_lz <- left_join(fsn_dp_fc,lz,by=c("dest_pincode","fc")) %>% 
  filter(lzn_value %in% c("L1","L2","Z1","Z2")) # only keeping lz FCs to sales dest pincodes

fsn_dp_fc_vendor <- fsn_dp_fc_lz %>% left_join(vendor_site_2,by=c("fsn","fc")) # joining vendor info

fsn_dp_vendor <- fsn_dp_fc_vendor %>% group_by(fsn,dest_pincode) %>%
  summarise(vendor_count=sum(vendor_count,na.rm=T)) %>%
  mutate(vendor_avl_flag=ifelse(vendor_count==0,0,1)) # Creating vendor available flag

# table(fsn_dp_vendor$vendor_avl_flag)
# fsn_dp_vendor_issue <- fsn_dp_vendor %>% filter(vendor_avl_flag==0) %>% select(-vendor_count)

# sales_vendor <- sales %>% left_join(fsn_dp_vendor_issue, by=c("fsn","dest_pincode"))
# vendor_loss <- sales_vendor %>% filter(vendor_avl_flag == 0)
# sum(vendor_loss$sales)
# sum(sales$sales)
# sum(vendor_loss$ru_loss)/sum(sales$sales)

#### Placement Possible ####
#### 4.FE Loss ####
## Identifying the national fiu ids for alpha sellers (ru leakage) ###
nat_fiu <- explain_ff %>%
  filter(lzn_value %in% c("N1","N2") & is_accepted=="true") 

lz_issue <- explain_ff %>%   # Identifying the FE issue
  filter(fiu_id %in% nat_fiu$fiu_id & (lzn_value %in% c("L1","L2","Z1","Z2")) 
         & effective_inventory>0)

fe_issue <- lz_issue %>% group_by(fsn,dest_pincode) %>% 
  summarize(fe_loss=n_distinct(fiu_id)) %>% as.data.frame()


#### 9.Vendor Adherence Loss ####
vendor_adh <- vendor_adherence %>% left_join(lead_time, by = c("fsn", "fc"="warehouse")) %>%
  left_join(cat_lead_time[c("category","cat_lead_time")], by = "category") %>%
  mutate(policy_lead_time = ifelse(is.na(lead_time),cat_lead_time,lead_time))

vendor_adh_2 <- vendor_adh %>% 
  filter((po_status == "approved" & (poi_created_day <= (wk_end_date-7)-policy_lead_time)) | 
           # Open till end of previous week
         (po_status == "completed" & (poi_created_day + 2*policy_lead_time >= wk_start_date))) %>%
           # Closed, but not enough time to replan & make up for it
  mutate(vendor_adh_loss = poi_ordered_qty-poi_received_qty)

#### 10.Promotions Loss ####
# Only to with main sales file 


#### 11.Joins & final aggregation ####

