#### Placement Issues - Placement NOT Possible ####

#### 1.Serviceability Loss ####
# Creating Serviceability flag - to separate Large FCs as source
distinct_dp <- lzn %>% select(dest_pincode) %>% distinct 
serv_dp <- lz %>%
  group_by(dest_pincode) %>% summarise(lz_fc_count = n_distinct(fc)) %>% 
  right_join(distinct_dp, by='dest_pincode') %>%
  mutate(lz_fc_count = ifelse(is.na(lz_fc_count),0,lz_fc_count),
         is_serviceable_flag = ifelse(lz_fc_count==0,0,1))
table(serv_dp$is_serviceable_flag)

#### 2.Vendor Availability Loss ####
fsn_dp <- sales %>% select(fsn,dest_pincode,brand,bu,category,vertical) %>% distinct %>%
  mutate(dest_pincode=as.factor(dest_pincode)) # fsn-dp for sales dest pincodes

lzn_sales_dp <- subset(lzn, dest_pincode %in% sales$dest_pincode) # fc-dp mapping for sales dest pincodes
fc <- data.frame(unique(lzn_sales_dp$fc)) # FCs used for fulfilling to sales dest pincodes

fsn_dp_fc <- merge(fsn_dp,fc,by=NULL)  
names(fsn_dp_fc)[ncol(fsn_dp_fc)] <- "fc"

fsn_dp_fc_lz <- left_join(fsn_dp_fc,lz,by=c("dest_pincode","fc")) %>% 
  filter(lzn_value %in% c("L1","L2","Z1","Z2")) # only keeping lz FCs (removing NAs) to sales dest pincodes

fsn_dp_fc_vendor <- fsn_dp_fc_lz %>% left_join(vendor_site_2,by=c("fsn","fc")) # joining vendor info

fsn_dp_vendor <- fsn_dp_fc_vendor %>% group_by(fsn,dest_pincode) %>%
  summarise(vendor_count=sum(vendor_count,na.rm=T)) %>%
  mutate(vendor_avl_flag=ifelse(vendor_count==0,0,1)) # Creating vendor available flag

table(fsn_dp_vendor$vendor_avl_flag)
fsn_dp_vendor_issue <- fsn_dp_vendor %>% filter(vendor_avl_flag==0) %>% select(-vendor_count)

# sales_vendor <- sales %>% left_join(fsn_dp_vendor_issue, by=c("fsn","dest_pincode"))
# vendor_loss <- sales_vendor %>% filter(vendor_avl_flag == 0)
# sum(vendor_loss$sales)
# sum(sales$sales)
# sum(vendor_loss$ru_loss)/sum(sales$sales)

#### Calculating serviceability & vendor loss ####
# Joining serviceability flag & vendor flag into sales file
sales_fsn_dp <- sales %>% group_by(fsn,dest_pincode) %>%
  summarise(sales=sum(sales),
            ru_sales=sum(ru_sales),
            ru_loss=sum(ru_loss),
            rru_sales=sum(rru_sales))

sales_fsn_dp_noplacement <- sales_fsn_dp %>% 
  left_join(serv_dp[c('dest_pincode','is_serviceable_flag')], by="dest_pincode") %>%
  left_join(subset(fsn_dp_vendor,select=-vendor_count), by=c("fsn","dest_pincode"))

placement_cols <- c("is_serviceable_flag","vendor_avl_flag")    
sales_fsn_dp_noplacement[placement_cols][is.na(sales_fsn_dp_noplacement[placement_cols])] <- 0

table(sales_fsn_dp_noplacement$is_serviceable_flag)
table(sales_fsn_dp_noplacement$vendor_avl_flag)

sales_fsn_dp_noplacement_2 <- sales_fsn_dp_noplacement %>%  
  mutate(serviceability_loss=ifelse(is_serviceable_flag==0,ru_loss,0),
         vendor_loss=ifelse(is_serviceable_flag==1 & vendor_avl_flag==0,ru_loss,0)) %>%
  filter(is_serviceable_flag==0 | vendor_avl_flag==0)


#### 3.Low Sales Depth ####


#### Placement Possible ####
#### 4.FE Loss ####
## Identifying the national fiu ids for alpha sellers (ru leakage) ###
explain_ff %<>% rename(dest_pincode = destination_pincode)
nat_fiu <- explain_ff %>%
  filter(lzn_value %in% c("N1","N2") & is_accepted=="true") 

lz_issue <- explain_ff %>%   # Identifying the FE issue
  filter(fiu_id %in% nat_fiu$fiu_id & (lzn_value %in% c("L1","L2","Z1","Z2")) 
         & effective_inventory>0)

fe_issue <- lz_issue %>% group_by(fsn,dest_pincode) %>% 
  summarize(fe_loss=n_distinct(fiu_id)) %>% 
  mutate(dest_pincode=as.factor(as.character(dest_pincode))) %>%
  as.data.frame()

#### 5.Exclusion Loss ####
exclusion_list %<>% mutate(skip_date=as.Date(skip_upto), is_excluded=ifelse(skip_date>=wk_start_date,1,0)) %>%
  filter(is_excluded==1)

#### 6.Forecast Error Loss ####
reco_leadtime <- inner_join(fsn_fc_l1,lead_time, by=c("fsn","fc","reco_week")) %>%
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

sales_fsn_prefFc <- right_join(preferred_fc,sales_fsn_dp,by='dest_pincode') %>%
  group_by(fsn,pref_fc) %>% summarise(sales=sum(sales))  %>% as.data.frame()

forecast_error <- left_join(sales_fsn_prefFc,reco_leadtime[c("fsn","fc","forecast_qty")], by=c("fsn","pref_fc"="fc")) %>%
  mutate(forecast_qty=ifelse(is.na(forecast_qty),0,forecast_qty),
         forecast_error=pmax(sales-forecast_qty,0)) %>%
  rename(fc=pref_fc)

forecast_flag = ifelse(sum(sales$sales)==sum(forecast_error$sales),"forecast_error = Ok","forecast_error issue")
stop(forecast_flag)

#### 7-9.N-DOH, IPC Override, CDO Override Loss ####
override_loss <- reco_leadtime[c("fsn","fc","system_qty","max_doh_qty","ipc_reviewed_qty","cdo_reviewed_qty","po_qty")] %>%
  mutate(ndoh_loss=pmax(system_qty-max_doh_qty,0),
         ipc_override_loss=pmax(ipc_reviewed_qty-max_doh_qty,0),
         cdo_override_loss=pmax(cdo_reviewed_qty-ipc_reviewed_qty,0),
         po_loss=pmax(po_qty-cdo_reviewed_qty,0))

#### 10.Vendor Adherence Loss ####
vendor_adh <- vendor_adherence %>% left_join(lead_time[c("fsn","fc","policy_lt")], by = c("fsn", "fc")) %>%
  left_join(cat_lead_time[c("category","cat_lead_time")], by = "category") %>%
  mutate(policy_lt = ifelse(is.na(policy_lt),cat_lead_time,policy_lt))

vendor_adh_2 <- vendor_adh %>% 
  filter((po_status == "approved" & (poi_created_day <= (wk_end_date-7)-policy_lt)) | 
           # Open till end of previous week
         (po_status == "completed" & (poi_created_day + 2*policy_lt >= wk_start_date))) %>%
           # Closed, but not enough time to replan & make up for it
  mutate(vendor_adh_loss = poi_ordered_qty-poi_received_qty)

#### 11.Promotions Loss ####
# Only to be joined with main sales file 

#### 12.Joins & final aggregation ####
sales_fsn_dp_fc_placement <- anti_join(sales[c("fsn","dest_pincode","fc","sales","ru_sales","ru_loss")],
                                    sales_fsn_dp_noplacement_2,by=c("fsn","dest_pincode")) # removing fsn-dp where no placement possible

# Computing FE loss
sales_fsn_dp_fe <- sales_fsn_dp_fc_placement %>% group_by(fsn,dest_pincode) %>%
  summarise(ru_loss=sum(ru_loss)) %>%
  left_join(fe_issue, by=c("fsn","dest_pincode")) %>% 
  left_join(exclusion_list[c("fsn","is_excluded")],by="fsn") %>%
  mutate(fe_loss = ifelse(is.na(fe_loss),0,fe_loss),
         is_excluded = ifelse(is.na(is_excluded),0,is_excluded),
         exclusion_loss = ifelse(is_excluded==1,ru_loss-fe_loss,0),
         fsn_dp_loss = ru_loss-fe_loss-exclusion_loss)

fsn_dp_fc_ratio <- fsn_dp_fc_vendor %>% filter(is.na(vendor_count)) # Filtering fsn-dp-fc where vendor is present 
fsn_dp_ratio <- fsn_dp_fc_ratio %>% group_by(fsn,dest_pincode) %>% summarise(fc_count = n_distinct(fc))
fsn_dp_fc_ratio_2 <- left_join(fsn_dp_fc_ratio,fsn_dp_ratio, by=c("fsn","dest_pincode")) %>%
  mutate(ratio=1/fc_count) %>% select(fsn,dest_pincode,fc,ratio)


sales_fsn_dp_fc_placement_2 <- left_join(fsn_dp_fc_ratio_2,sales_fsn_dp_fe[c("fsn","dest_pincode","fsn_dp_loss")],by=c("fsn","dest_pincode")) %>%
  mutate(ru_loss_fc=ratio*fsn_dp_loss)

sales_fsn_dp_fc_collated <- join_all(list(sales_fsn_dp_fc_placement_2,forecast_error[c("fsn","fc","forecast_error")],
                                          override_loss[c("fsn","fc","ndoh_loss","ipc_override_loss","cdo_override_loss","po_loss")],
                                          vendor_adh_2[c("fsn","fc","vendor_adh_loss")]),by=c("fsn","fc"),type='left')

qty_cols <- c("fsn_dp_loss", "ru_loss_fc", "forecast_error", "ndoh_loss", "ipc_override_loss", "cdo_override_loss", "po_loss", "vendor_adh_loss")
sales_fsn_dp_fc_collated[qty_cols][is.na(sales_fsn_dp_fc_collated[qty_cols])] <- 0


sales_fsn_dp_fc_collated %<>% mutate(forecast_error  = pmin(ru_loss_fc,forecast_error),
                                     temp_var = pmax(ru_loss_fc-forecast_error,0),
                                     ndoh_loss = pmin(temp_var,ndoh_loss),
                                     temp_var = pmax(temp_var-ndoh_loss,0),
                                     ipc_override_loss = pmin(temp_var,ipc_override_loss),
                                     temp_var = pmax(temp_var-ipc_override_loss,0),
                                     cdo_override_loss = pmin(temp_var,cdo_override_loss),
                                     temp_var = pmax(temp_var-cdo_override_loss,0),
                                     po_loss = pmin(temp_var,po_loss), # check if this needs to exist
                                     temp_var = pmax(temp_var-po_loss,0),
                                     vendor_adh_loss = pmin(temp_var,vendor_adh_loss),
                                     temp_var = pmax(temp_var-vendor_adh_loss,0))

sum(sales_fsn_dp_fc_collated$ru_loss_fc)
loss_cols <- c("forecast_error", "ndoh_loss", "ipc_override_loss", "cdo_override_loss", "po_loss", "vendor_adh_loss","temp_var")
sum(sales_fsn_dp_fc_collated[loss_cols])
sum(sales_fsn_dp_fe$fsn_dp_loss)

sum(sales_fsn_dp_fc_placement_2$ru_loss_fc,na.rm=T)
temp_df <- sales_fsn_dp_fe %>% select(fsn,dest_pincode) %>% distinct()
temp_df2 <- fsn_dp_fc_ratio_2 %>% select(fsn,dest_pincode) %>% distinct()
temp_df3 <- full_join(temp_df2,temp_df)

no_placement_loss <- sales_fsn_dp_noplacement_2 %>% 
  select(fsn,dest_pincode,serviceability_loss,vendor_loss)

placement_loss_1 <- sales_fsn_dp_fe %>% select(fsn,dest_pincode,fe_loss,exclusion_loss)

placement_loss_2 <- sales_fsn_dp_fc_collated %>% group_by(fsn,dest_pincode) %>%
  summarise(forecast_error = sum(forecast_error, na.rm=T),
            ndoh_loss = sum(ndoh_loss, na.rm=T),
            ipc_override_loss = sum(ipc_override_loss, na.rm=T),
            cdo_override_loss = sum(cdo_override_loss, na.rm=T),
            po_loss = sum(po_loss, na.rm=T),
            vendor_adh_loss = sum(vendor_adh_loss, na.rm=T),
            residual = sum(temp_var, na.rm=T))

ru_waterfall <- bind_rows(no_placement_loss, placement_loss_1, placement_loss_2)
ru_waterfall_1 <- left_join(sales_fsn_dp,ru_waterfall, by=c("fsn","dest_pincode"))
ru_waterfall_1[,3:17][is.na(ru_waterfall_1[,3:17])] <- 0

sum(ru_waterfall_1$ru_loss)
sum(sales$ru_loss)
sum(sales_fsn_dp$ru_loss)
sum(ru_waterfall_1[7:17])
temp_df4 <- ru_waterfall %>% select(fsn, dest_pincode) %>% distinct()


ru_waterfall_2 <- ru_waterfall_1 %>% mutate(sum_ru_loss=serviceability_loss+vendor_loss+fe_loss+exclusion_loss+forecast_error+ndoh_loss+
                                              ipc_override_loss+cdo_override_loss+po_loss+vendor_adh_loss+residual,
                                            ru_issue_flag=ifelse(ru_loss==sum_ru_loss,0,1))
sum(ru_waterfall_2$sum_ru_loss)
table(ru_waterfall_2$ru_issue_flag)

