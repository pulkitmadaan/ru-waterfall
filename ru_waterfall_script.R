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

fsn_dp_fc_vendor <- fsn_dp_fc_lz %>% left_join(vendor_site_2,by=c("fsn","fc")) # joining vendor info

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
  filter(sales<min_sales_depth)

low_sale_fsn <- sales_fsn_dp %>% select(fsn) %>% distinct() %>%
  left_join(low_sales[,c("fsn")], by="fsn") %>% 
  mutate(low_sale_flag=1)

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
         ,placement_loss = serviceability_loss+vendor_loss+low_sale_depth_loss - exclusion_loss
         ,temp_var = nat_sales - placement_loss
  )

# Validation
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

temp_df <- sales_fsn_dp_no_plc_2 %>% filter(placement_loss>0)
sum(temp_df$placement_loss)
sum(temp_df$nat_sales)


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

sales_fsn_pref_fc <- right_join(preferred_fc,sales_fsn_dp,by='dest_pincode') %>%
  group_by(fsn,pref_fc) %>% summarise(sales=sum(sales))  %>% as.data.frame()

forecast_loss <- left_join(sales_fsn_pref_fc,reco_leadtime[c("fsn","fc","forecast_qty")], by=c("fsn","pref_fc"="fc")) %>%
  mutate(forecast_qty=ifelse(is.na(forecast_qty),0,forecast_qty),
         forecast_loss=pmax(sales-forecast_qty,0)) %>%
  rename(fc=pref_fc)

forecast_flag = ifelse(sum(sales$sales)==sum(forecast_loss$sales),"forecast_loss = Ok","forecast_loss issue")
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
  mutate(vendor_adherence_loss = poi_ordered_qty-poi_received_qty) %>%
  group_by(fsn,fc) %>%
  summarise(vendor_adherence_loss = sum(vendor_adherence_loss))

vendor_adh_3 <-  vendor_adh_2 %>% group_by(fsn,fc) %>%
  summarise(vendor_adherence_loss=sum(vendor_adherence_loss))
sum(vendor_adh_3$vendor_adherence_loss)

#### 11.Promotions Loss ####
# Only to be joined with main sales file 

#### 12.Joins & final aggregation ####
sales_fsn_dp_fc_plc <- anti_join(sales_fsn_dp_fc[c("fsn","dest_pincode","fc","sales","ru_sales","nat_sales")],
                                    sales_fsn_dp_no_plc_2,by=c("fsn","dest_pincode")) # removing fsn-dp where no placement possible

# Computing FE loss
sales_fsn_dp_plc_1 <- sales_fsn_dp_fc_plc %>% group_by(fsn,dest_pincode) %>%
  summarise(nat_sales=sum(nat_sales)) %>%
  left_join(fe_issue, by=c("fsn","dest_pincode")) %>% 
  left_join(exclusion_list[c("fsn","is_excluded")],by="fsn") %>%
  mutate(fe_loss = ifelse(is.na(fe_loss),0,fe_loss),
         is_excluded = ifelse(is.na(is_excluded),0,is_excluded),
         exclusion_loss = ifelse(is_excluded==1,nat_sales-fe_loss,0),
         fsn_dp_loss = nat_sales-fe_loss-exclusion_loss)

fsn_dp_fc_ratio <- fsn_dp_fc_vendor %>% filter(is.na(vendor_count)) # Filtering fsn-dp-fc where placement is possible  
fsn_dp_ratio <- fsn_dp_fc_ratio %>% group_by(fsn,dest_pincode) %>% summarise(fc_count = n_distinct(fc))
# max(fsn_dp_ratio$fc_count)
# n_distinct(lzn$fc) # 59 FCs in lzn file
fsn_dp_fc_ratio_2 <- left_join(fsn_dp_fc_ratio,fsn_dp_ratio, by=c("fsn","dest_pincode")) %>%
  mutate(ratio=1/fc_count) %>% select(fsn,dest_pincode,fc,ratio)

# sales_fsn_dp_fc_plc_2 <- left_join(fsn_dp_fc_ratio_2,sales_fsn_dp_plc_1[c("fsn","dest_pincode","fsn_dp_loss")],by=c("fsn","dest_pincode")) %>%
#   mutate(fsn_fc_loss=ratio*fsn_dp_loss)

sales_fsn_dp_fc_plc_2 <- left_join(fsn_dp_fc_ratio_2,sales_fsn_dp_fc_plc,by=c("fsn","dest_pincode","fc"))
sum(sales_fsn_dp_fc_plc_2$sales,na.rm = T)
sum(sales_fsn_dp_fc_plc$sales) # Issue in ratio join
#Note : DO not sum the sales_sum_6 to get DP - FSN level sales

# Testing
sales_fsn_dp_fc_plc_k <- full_join(fsn_dp_fc_ratio_2,sales_fsn_dp_fc_plc,by=c("fsn","dest_pincode","fc"))
sum(sales_fsn_dp_fc_plc_k$sales,na.rm = T)
sum(sales_fsn_dp_fc_plc$sales) # Issue in ratio join
#Note : DO not sum the sales_sum_6 to get DP - FSN level sales

temp_da <- sales_fsn_dp_fc_plc_k %>% filter(is.na(ratio))
sum(temp_da$nat_sales)
unique(temp_da$fc)

# continue
sales_fsn_dp_fc_plc_3 <- left_join(sales_fsn_dp_fc_plc_2,sales_fsn_dp_plc_1[c("fsn","dest_pincode","fsn_dp_loss")], by=c("fsn","dest_pincode")) %>%
  mutate(fsn_fc_loss=ratio*fsn_dp_loss)
sum(sales_fsn_dp_fc_plc_3$fsn_fc_loss,na.rm = T)
sum(sales_fsn_dp_plc_1$fsn_dp_loss) # Issue in fsn dp loss 

sales_fsn_dp_fc_plc_collated <- join_all(list(sales_fsn_dp_fc_plc_3,forecast_loss[c("fsn","fc","forecast_loss")]
                                          ,override_loss[c("fsn","fc","ndoh_loss","ipc_override_loss","cdo_override_loss","po_loss")]
                                          ,vendor_adh_2[c("fsn","fc","vendor_adherence_loss")]
                                          ),by=c("fsn","fc"),type='left')

qty_cols <- c("sales","ru_sales","nat_sales","fsn_dp_loss", "fsn_fc_loss", "forecast_loss", "ndoh_loss", "ipc_override_loss", "cdo_override_loss", "po_loss", "vendor_adherence_loss")
sales_fsn_dp_fc_collated[qty_cols][is.na(sales_fsn_dp_fc_collated[qty_cols])] <- 0


sales_fsn_dp_fc_collated %<>% mutate(forecast_loss  = pmin(fsn_fc_loss,forecast_loss),
                                     temp_var = pmax(fsn_fc_loss-forecast_loss,0),
                                     ndoh_loss = pmin(temp_var,ndoh_loss),
                                     temp_var = pmax(temp_var-ndoh_loss,0),
                                     ipc_override_loss = pmin(temp_var,ipc_override_loss),
                                     temp_var = pmax(temp_var-ipc_override_loss,0),
                                     cdo_override_loss = pmin(temp_var,cdo_override_loss),
                                     temp_var = pmax(temp_var-cdo_override_loss,0),
                                     po_loss = pmin(temp_var,po_loss), # check if this needs to exist
                                     temp_var = pmax(temp_var-po_loss,0),
                                     vendor_adherence_loss = pmin(temp_var,vendor_adherence_loss),
                                     temp_var = pmax(temp_var-vendor_adherence_loss,0))

sum(sales_fsn_dp_fc_collated$fsn_fc_loss)
loss_cols <- c("forecast_loss", "ndoh_loss", "ipc_override_loss", "cdo_override_loss", "po_loss", "vendor_adherence_loss","temp_var")
sum(sales_fsn_dp_fc_collated[loss_cols])
sum(sales_fsn_dp_fc_plc_3$fsn_fc_loss,na.rm = T)
sum(sales_fsn_dp_plc_1$fsn_dp_loss)

# Not sure why created these
# temp_df <- sales_fsn_dp_plc_1 %>% select(fsn,dest_pincode) %>% distinct()
# temp_df2 <- fsn_dp_fc_ratio_2 %>% select(fsn,dest_pincode) %>% distinct()  # equivalent to placement_loss_2 rows
# temp_df3 <- full_join(temp_df2,temp_df)

no_placement_loss <- sales_fsn_dp_no_plc_2 %>% 
  select(fsn,dest_pincode,serviceability_loss,vendor_loss)

placement_loss_1 <- sales_fsn_dp_plc_1 %>% select(fsn,dest_pincode,fe_loss,exclusion_loss)

placement_loss_2 <- sales_fsn_dp_fc_collated %>% group_by(fsn,dest_pincode) %>%
  summarise(forecast_loss = sum(forecast_loss, na.rm=T),
            ndoh_loss = sum(ndoh_loss, na.rm=T),
            ipc_override_loss = sum(ipc_override_loss, na.rm=T),
            cdo_override_loss = sum(cdo_override_loss, na.rm=T),
            po_loss = sum(po_loss, na.rm=T),
            vendor_adherence_loss = sum(vendor_adherence_loss, na.rm=T),
            residual = sum(temp_var, na.rm=T))

ru_loss_binded <- bind_rows(no_placement_loss, placement_loss_1, placement_loss_2)
ru_loss_fsn_dp <- ru_loss_binded %>% group_by(fsn,dest_pincode) %>% 
  summarise(serviceability_loss=sum(serviceability_loss, na.rm=T)
            ,vendor_loss=sum(vendor_loss, na.rm=T)
            ,fe_loss=sum(fe_loss, na.rm=T)
            ,exclusion_loss=sum(exclusion_loss)
            ,forecast_loss = sum(forecast_loss, na.rm=T),
            ndoh_loss = sum(ndoh_loss, na.rm=T),
            ipc_override_loss = sum(ipc_override_loss, na.rm=T),
            cdo_override_loss = sum(cdo_override_loss, na.rm=T),
            po_loss = sum(po_loss, na.rm=T),
            vendor_adherence_loss = sum(vendor_adherence_loss, na.rm=T),
            residual = sum(residual, na.rm=T))

ru_waterfall <- left_join(sales_fsn_dp,ru_loss_fsn_dp, by=c("fsn","dest_pincode"))
ru_waterfall[,3:17][is.na(ru_waterfall[,3:17])] <- 0

sum(ru_waterfall$nat_sales)
sum(sales$nat_sales)
sum(sales_fsn_dp$nat_sales)
sum(ru_waterfall[7:17]) 

ru_waterfall_2 <- ru_waterfall %>% mutate(sum_nat_sales=serviceability_loss+vendor_loss+fe_loss+exclusion_loss+forecast_loss+ndoh_loss+
                                              ipc_override_loss+cdo_override_loss+po_loss+vendor_adherence_loss+residual,
                                            ru_issue_flag=ifelse(nat_sales==sum_nat_sales,0,1))
sum(ru_waterfall_2$sum_nat_sales)
table(ru_waterfall_2$ru_issue_flag)

temp_df5 <- placement_loss_1 %>%  select(fsn, dest_pincode) %>% distinct()
temp_df5 <- tail(ru_waterfall)
#### Fetching Keys & exporting waterfall ####
key_fsn <- sales %>% select(fsn,product_id_key) %>% distinct()
key_dp <- sales %>% select(dest_pincode, destination_pincode_key) %>% distinct()
temp_df6 <- temp_df5 %>% left_join(key_fsn, by="fsn") %>% left_join(key_dp, by="dest_pincode") %>% 
  left_join(preferred_warehouse, by="dest_pincode")

# write.table(temp_df6,"sample_output.tsv",row.names = F,sep="\t")

