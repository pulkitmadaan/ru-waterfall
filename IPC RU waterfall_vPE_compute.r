#1
########## Placement Issues - Placement NOT Possible ##########
#Whatif I change any file

# Creating Master files -- For placement issues
master_temp <- merge(dest_fsn_fc,lzn_3,by.x=c('dest_pincode','FC'),by.y=c('Destination_Pincode','FC'),all.x=TRUE)
master_temp_1 <- subset(master_temp, !(is.na(lzn)))
master_temp_2 <- merge(master_temp_1,tax_trn_1,by.x=c('fsn','FC'),by.y=c('FSN','FC'),all.x=TRUE)
# master_temp_3<- merge(master_temp_2,vendor_1,by.x=c("brand","vertical","FC"),by.y=c("brand","Vertical","FC"),all.x=TRUE)
# master_temp_4 <-merge(master_temp_3,u2s_1,by=c("brand","vertical","FC"),all.x=TRUE)
master_temp_3 <-merge(master_temp_2,u2s_1,by=c("brand","vertical","FC"),all.x=TRUE)
master_temp_3 <- master_temp_3 %>% mutate(brand_upper=toupper(brand)) # To fix vendor issue
master_temp_4<- merge(master_temp_3,vendor_1,by.x=c("brand_upper","vertical","FC"),by.y=c("brand","Vertical","FC"),all.x=TRUE)
master_temp_4$tax_flg <- as.numeric(as.character(master_temp_4$tax_flg))
master_temp_4$vendor_flag <- as.numeric(as.character(master_temp_4$vendor_flag))
master_temp_4$u2s_flag <- as.numeric(as.character(master_temp_4$u2s_flag))
master_temp_4$tax_flg <-ifelse(is.na(master_temp_4$tax_flg),1,master_temp_4$tax_flg)
master_temp_4$vendor_flag <-ifelse(is.na(master_temp_4$vendor_flag),1,master_temp_4$vendor_flag)
master_temp_4$u2s_flag <-ifelse(is.na(master_temp_4$u2s_flag),1,master_temp_4$u2s_flag)
master_temp_4$placement_flag <- with( master_temp_4,as.numeric(tax_flg & vendor_flag & u2s_flag))

#Dividing the loss equally for DP where placement is possible
ratio <-subset(master_temp_4, u2s_flag==1 & tax_flg==1 & vendor_flag==1) # Selecting pincodes where placement is possible                                    
ratio_1 <- ratio%>%group_by(fsn,dest_pincode)%>%summarise(fc_count=n_distinct(FC)) # Counting unique FCs
ratio_2<-merge(ratio,ratio_1, by=c("fsn","dest_pincode"),all.x=TRUE) # Joining with main file
ratio_2$ratio <- with(ratio_2,1/fc_count) # Finding ratio
ratio_3<- ratio_2%>%distinct(fsn,FC,dest_pincode,ratio,brand) # Distinct combinations

# Defining the loss function -- Flag 0 means placement not possible (Placement issue)
loss_attribution<-function(tax_flg,vendor_flag,u2s_flag){
  if(tax_flg==0){loss="tax"}else{
    if(tax_flg==1 & u2s_flag==0){loss="u2s1"}else{
      if(tax_flg==1 & u2s_flag==1 & vendor_flag==0){loss="vendor"}else{loss="N"}
    }
  }
  return(loss)
}

# Attributing  the right loss function
final_loss_class <- function(x){
  y<- unique(x[nchar(x)==max(nchar(x))])
  return(y)}

#loss tagging for no placement pincodes
no_plc_pin <- anti_join(master_temp_4, ratio, by=c('dest_pincode','fsn')) # Ratio - Pincodes where placement is possible
no_plc_pin_1<- no_plc_pin %>% rowwise() %>% mutate(loss=loss_attribution(tax_flg,vendor_flag,u2s_flag))
no_plc_pin_2 <- aggregate(loss ~ dest_pincode + fsn,no_plc_pin_1,function(x)final_loss_class(x))
no_plc_pin_2[no_plc_pin_2$loss=="u2s1","loss"]="u2s"

save.image(paste0(save_path,temp_week,"_placementissues.RData"))

########## No Placement Issues - Placement Possible ##########
#Loss attribution logic for non placement pincode
# library(dplyr)
sales_fsn_dp <- sales %>% group_by(dest_pincode,fsn) %>%
  summarise(sales=sum(sales),
            ru_sales=sum(ru_sales),
            nat_sales=sum(nat_sales))
sales_sum_1<- merge(sales_fsn_dp,lzn_1[c("fc_available_flag","Destination_Pincode")],by.x="dest_pincode",
                    by.y="Destination_Pincode",all.x=TRUE) # FC available flag
sales_sum_2<- merge(sales_sum_1,no_plc_pin_2,by=c('dest_pincode','fsn'),all.x=TRUE)
sales_sum_3 <- subset(sales_sum_2, fc_available_flag==0 | !(is.na(loss)) ) # Filtering non placement pincode
sales_sum_3$servicibility_loss <- with(sales_sum_3,ifelse(fc_available_flag==0,nat_sales,0))
# sales_sum_3$temp <- with(sales_sum_3,pmax(nat_sales-servicibility_loss,0))
sales_sum_3$tax_loss <- with(sales_sum_3,ifelse(loss=="tax",nat_sales,0))
sales_sum_3$vendor_loss <- with(sales_sum_3,ifelse(loss=="vendor",nat_sales,0))
sales_sum_3$u2s_loss <- with(sales_sum_3,ifelse(loss=="u2s",nat_sales,0))
col=c('tax_loss','vendor_loss','servicibility_loss','u2s_loss')
sales_sum_3[, col][is.na(sales_sum_3[, col])] <- 0  # Removing nulls & substituting with 0 in placement losses
# sales_sum_3$temp <- with(sales_sum_3,pmax(nat_sales-tax_loss-vendor_loss-servicibility_loss-u2s_loss,0))

#Loss ratio logic for pincode where placement is possible
sales_sum_4<- sales %>% group_by(dest_pincode,fsn,source_id,brand,bu,category,super_category,sub_category,vertical) %>%
  summarise(sales=sum(sales),
            ru_sales=sum(ru_sales),
            nat_sales=sum(nat_sales))
sales_sum_5 <- anti_join(sales_sum_4,sales_sum_3,by=c("fsn","dest_pincode")) # removing dest_pincode, fsn where placement is not possible
names(sales_sum_5)[names(sales_sum_5)=="source_id"]='FC'
#sales_sum_5_1 <- anti_join(sales_sum_5,subset(master_temp_4,placement_flag==0),by=c("fsn","dest_pincode","FC")) # removing placement constraint fc with DP where placement is possible
col=c('fsn','dest_pincode','FC','brand','ratio')
sales_sum_6<- merge(ratio_3[,col],sales_sum_5, by=c('fsn','dest_pincode','FC'),all.x=TRUE) 

#Note :ratio 2 should be on left as sales_sum_5 might have fc's where sales has not happened and we have to divide the loss for those fc's as well
sales_sum_7 <-sales_sum_5%>%group_by(dest_pincode,fsn)%>% 
  summarise(ru_loss=sum(nat_sales,na.rm=TRUE))
# save.image(paste0(save_path,temp_week,"_pre_pe.RData"))

#2
## Identifying the national fiu ids for alpha sellers (ru leakage) ###
nat_1 <- explain_data %>%
  filter(lzn %in% c("N1","N2") & accepted==1) 

lz_issue <- explain_data%>%   # Identifying the FF issue
  filter(fiu_id %in% nat_1$fiu_id & (lzn %in% c("L1","L2","Z1","Z2")) 
         & effective_inventory>0 & tier_status=="ACTIVE")

ff_issue <- lz_issue %>% group_by(fsn,dest_pincode) %>% summarize(ff_loss=n_distinct(fiu_id)) %>% as.data.frame() 
# write.csv(ff_issue, "ff_issue.csv")
# ff_issue <- read.csv('ff_issue.csv')
sales_sum_7$dest_pincode <- as.factor(sales_sum_7$dest_pincode)
sales_sum_6$dest_pincode <- as.factor(sales_sum_6$dest_pincode)
sales_sum_7_temp <- left_join(sales_sum_7,ff_issue,by=c('fsn','dest_pincode')) %>% 
  mutate(ff_loss=ifelse(is.na(ff_loss),0,ff_loss)) %>% 
  mutate(loss_at_dp_fsn_lvl=ru_loss-ff_loss) %>% 
  as.data.frame() 
save.image(paste0(save_path,temp_week,"_pe.RData"))

#Note : DO not sum the sales_sum_6 to get DP - FSN level sales

#### ####
sales_sum_8 <-merge(sales_sum_6,sales_sum_7_temp,by=c('fsn','dest_pincode'),all.x=TRUE)
sales_sum_8$loss_post_ratio <-with(sales_sum_8,ratio*loss_at_dp_fsn_lvl)

sales_sum_8 <-left_join(sales_sum_6,sales_sum_7_temp,by=c('fsn','dest_pincode')) %>%
  mutate(loss_post_ratio=ratio*loss_at_dp_fsn_lvl, ff_loss_post_ratio=ratio*ff_loss)
  
  
#3
#EXCLUSION LOSS

exclusion_list$exclusion_flag<-1

# save.image(paste0(save_path,temp_week,"_exclusion.RData"))

#4
# PO Adherence
sales_leadtime <- sales[!duplicated(sales[,c(3,6)]),c(3,6)] # Fsn - category list for week's sales
leadtime_1<- merge(leadtime_1, sales_leadtime, by='fsn', all.x= TRUE)
leadtime_1<-merge(leadtime_1,cat_leadtime[,1:2], by='category', all.x= TRUE)
leadtime_1$policy_lead_time = ifelse(is.na(leadtime_1$policy_lead_time),7,
                                     ifelse(leadtime_1$policy_lead_time==0,leadtime_1$Lead_time_floor,leadtime_1$policy_lead_time))
leadtime_1$reco_date<- with(leadtime_1,wk_start_date - policy_lead_time-as.POSIXlt(wk_start_date - policy_lead_time)$wday) # Meaning
leadtime_1$reco_week<- with(leadtime_1,week(reco_date))

reco_leadtime<-merge(leadtime_1, reco_file, by.x=c("fsn","FC","reco_week"), by.y=c("FSN","FC","weeknum"))
save.image(paste0(save_path,temp_week,"_pre_forecast.RData"))

#5
# Forecast Loss
sales_forecast <- left_join(preferred_wh,sales_fsn_dp,by='dest_pincode') %>%
  group_by(fsn,preferred_wh) %>% summarise(sales=sum(sales)) %>% rename(source_id=preferred_wh) %>% as.data.frame()
  
reco_leadtime$forecast_qty<- with(reco_leadtime,ifelse(ceiling(reco_leadtime$policy_lead_time/7)==1,Forecast_week_1,
                                                       ifelse(ceiling(reco_leadtime$policy_lead_time/7)==2,Forecast_week_2,
                                                              ifelse(ceiling(reco_leadtime$policy_lead_time/7)==3,Forecast_week_3,
                                                                     ifelse(ceiling(reco_leadtime$policy_lead_time/7)==4,Forecast_week_4,
                                                                            (Forecast_week_1+Forecast_week_2+Forecast_week_3+Forecast_week_4)/4)))))

reco_leadtime_1<- reco_leadtime%>%group_by(fsn,FC,reco_week)%>% 
  summarise(total_qty_recomended=sum(Qty.Recommended),total_forecast_qty=sum(forecast_qty),projected_qty = sum(Qty.Generated)) %>%
  as.data.frame()#####v7

  forecast_error <- left_join(sales_forecast,reco_leadtime_1[c(-3,-4,-6)], by = c("fsn"="fsn","source_id"="FC"))
forecast_error$total_forecast_qty <- with(forecast_error, ifelse(is.na(total_forecast_qty),0,total_forecast_qty))
forecast_error$forecast_diff<- with(forecast_error, pmax(sales-total_forecast_qty,0))
names(forecast_error)[names(forecast_error)=="source_id"]= "FC"
forecast_error_2 <- forecast_error # For Debugging & validation
forecast_error<-forecast_error[c(1,2,5)]

# save.image(paste0(save_path,temp_week,"_forecast.RData"))
# NDOH
reco_leadtime_1$total_qty_recomended <- with(reco_leadtime_1, ifelse(is.na(total_qty_recomended),0,total_qty_recomended))
reco_leadtime_1$projected_qty <- with(reco_leadtime_1, ifelse(is.na(projected_qty),0,projected_qty))
# reco_file_ndoh<- reco_leadtime_1[,-5] %>% mutate(national_doh = projected_qty-total_qty_recomended)
reco_file_ndoh<- reco_leadtime_1[,-5] %>% mutate(national_doh = pmax(projected_qty-total_qty_recomended,0)) # Fix for negative NDOH
reco_file_ndoh <- reco_file_ndoh[c(1,2,6)]


#6
po_adh_1 <- po_adh %>%
  filter(status !='cancelled') %>%
  mutate(po_raised_qty = quantity-cancel_quantity)

po_adh_2 <- po_adh_1 %>% group_by(fsn,FC,weeknum)%>%
  summarise(total_po_raised_qty = sum(po_raised_qty))

po_reco_leadtime <- merge(po_adh_2,reco_leadtime_1,by.x=c("FC","fsn","weeknum"),by.y=c("FC","fsn","reco_week"), all.y = TRUE)
po_adh_final<-po_reco_leadtime %>% group_by(fsn,FC) %>% summarise(total_po_raised_qty = sum(total_po_raised_qty, na.rm= TRUE), final_qty_recommended=sum(total_qty_recomended, na.rm= TRUE))

save.image(paste0(save_path,temp_week,"_po_adh.RData"))

#7
# Leadtime
po_1 <- merge(po,leadtime_1,by.x=c("fsn","po_oroginal_warehouse_id"),by.y=c("fsn","FC"),all.x=TRUE)
po_2<- subset(po_1, poi_status %in% c("approved","completed"))
po_2$poi_initiated_date<- as.Date(as.character(po_2$poi_initiated_date_key),format="%Y%m%d")
po_3 <- po_2 %>%
  mutate(policy_lead_time = ifelse(is.na(policy_lead_time),7,policy_lead_time))%>%
  filter( (poi_status=='approved' & (poi_initiated_date + policy_lead_time >=leadtime_start_date & 
                                       poi_initiated_date + policy_lead_time<=wk_end_date)) | 
            (poi_status=='completed' & (poi_initiated_date + policy_lead_time >=wk_start_date & 
                                          poi_initiated_date + policy_lead_time<=wk_end_date)) )%>%
  dplyr::rename(FC=po_oroginal_warehouse_id)
po_4 <- po_3 %>% group_by(fsn,FC)%>%
  summarise(po_received_quantity = sum(poi_received_quantity),
            po_ordered_quantity = sum(poi_ordered_quantity))
po_4$po_received_quantity <- with(po_4, ifelse(is.na(po_received_quantity),0,po_received_quantity))
po_4$po_ordered_quantity <- with(po_4, ifelse(is.na(po_ordered_quantity),0,po_ordered_quantity))

#8
#### Addding & attributing the loss for DP where placement is possible ########
# library(plyr)
sales_sum_81<- join_all(list(sales_sum_8,exclusion_list,offer_sales), by='fsn', type='left') #CHANGE TO LEFT
sales_sum_9 <-join_all(list(sales_sum_81,po_4,po_adh_final,reco_file_ndoh,forecast_error), by=c('fsn','FC'), type='left') #CHANGE TO LEFT
# detach(package:plyr)
cols=c('sales','ru_sales','nat_sales','po_received_quantity','po_ordered_quantity','final_qty_recommended','total_po_raised_qty','ff_loss_post_ratio','national_doh','loss_post_ratio','exclusion_flag','offer_sales','forecast_diff')
sales_sum_9[cols][is.na(sales_sum_9[cols])] <- 0



# sales_sum_9$pe_loss <- with(sales_sum_9,pmin(loss_post_ratio,pe_excess_inv))
# sales_sum_9$temp <- with(sales_sum_9,pmax(loss_post_ratio-pe_loss,0))

sales_sum_9$forecast_error <- with(sales_sum_9,pmin(loss_post_ratio,forecast_diff))
sales_sum_9$temp <- with(sales_sum_9,pmax(loss_post_ratio-forecast_error,0))

sales_sum_9$ndoh_loss <- with(sales_sum_9,pmin(temp,national_doh))
sales_sum_9$temp <- with(sales_sum_9,pmax(temp-ndoh_loss,0))

sales_sum_9$exclusion_loss <- with(sales_sum_9,ifelse(sales_sum_9$exclusion_flag==1,temp,0))
sales_sum_9$temp <- with(sales_sum_9,pmax(temp-exclusion_loss,0))

sales_sum_9$adherence_loss <- with(sales_sum_9,pmin(temp,pmax(final_qty_recommended-total_po_raised_qty,0)))
sales_sum_9$temp <- with(sales_sum_9,pmax(temp-adherence_loss,0))

sales_sum_9$lead_time_loss <- with(sales_sum_9,pmin(temp,pmax(po_ordered_quantity-po_received_quantity,0)))
sales_sum_9$temp <- with(sales_sum_9,pmax(temp-lead_time_loss,0))

sales_sum_9$promotions_loss<-with(sales_sum_9,ifelse(sales_sum_9$offer_sales>0,pmin(temp,offer_sales),0))
sales_sum_9$residual <- with(sales_sum_9,pmax(temp-promotions_loss,0))
#Append the losses from the codes where placement is possible and not possible
save.image(paste0(save_path,temp_week,"_promo.RData"))

n_placement_loss <-sales_sum_3%>%group_by(fsn,dest_pincode)%>%
  summarise(servicibility_loss=sum(servicibility_loss),
    tax_loss=sum(tax_loss),
    vendor_loss=sum(vendor_loss),
    u2s_loss=sum(u2s_loss)) %>%
  mutate(dest_pincode=as.character(dest_pincode)) %>%
  as.data.frame()

y_placement_loss <- sales_sum_9%>%group_by(fsn,dest_pincode)%>%
  summarise(ndoh_loss=sum(ndoh_loss,na.rm=TRUE),
    ff_loss=sum(ff_loss_post_ratio,na.rm=TRUE),
    exclusion_loss=sum(exclusion_loss,na.rm=TRUE),
    adherence_loss=sum(adherence_loss,na.rm=TRUE),
    lead_time_loss=sum(lead_time_loss,na.rm=TRUE),
    forecast_error=sum(forecast_error,na.rm=TRUE),
    promotions_loss=sum(promotions_loss,na.rm=TRUE)) %>%
  mutate(dest_pincode=as.character(dest_pincode)) %>%
  as.data.frame()


ru_waterfall<- bind_rows(n_placement_loss,y_placement_loss)
ru_waterfall_1 <- merge(sales_fsn_dp,ru_waterfall,by=c('fsn','dest_pincode'),all.x=TRUE)
ru_waterfall_1[3:16][is.na(ru_waterfall_1[3:16])] <-0
cols<- c("fsn","bu","super_category","category","sub_category","vertical","brand")
prod_temp <- sales[!duplicated(sales[cols]),]
prod_temp_1<- subset(prod_temp,select=cols)
ru_waterfall_2 <- merge(ru_waterfall_1,prod_temp_1,by="fsn")
ru_waterfall_3 <- ru_waterfall_2%>% mutate( low_sale_depth_loss = ifelse(fsn %in% low_sale$fsn,nat_sales-servicibility_loss-tax_loss-u2s_loss,0))
cols <- c("vendor_loss","ndoh_loss","ff_loss","exclusion_loss","adherence_loss","lead_time_loss","forecast_error","promotions_loss")
ru_waterfall_3[,cols][ru_waterfall_3$low_sale_depth>0,]<-0
ru_waterfall_3$residual <- with(ru_waterfall_3,
                                 pmax(nat_sales-	servicibility_loss-	tax_loss-
                                        vendor_loss-	u2s_loss-low_sale_depth_loss- ndoh_loss-	ff_loss	-adherence_loss-	lead_time_loss-forecast_error-promotions_loss-exclusion_loss,0))
# names(ru_waterfall_3)[names(ru_waterfall_3)=='ru_sales']="ru_sales"
ru_waterfall_final<- ru_waterfall_3
ru_agg<-aggregate(cbind( ru_sales ,nat_sales, servicibility_loss,tax_loss,
                         u2s_loss,low_sale_depth_loss,vendor_loss,ndoh_loss,ff_loss,exclusion_loss,adherence_loss,lead_time_loss,forecast_error,promotions_loss,residual) ~ bu, ru_waterfall_3,sum)

save.image(paste0(save_path,temp_week,"_agg.RData"))

##### Mapping & File export #####
#IPC Mapping
#ru_agg_per<- as.data.frame(apply(ru_agg[,-1],1,function(x) round(x*100/sum(x[c(1,2)]),2)))
#colnames(ru_agg_per)<- ru_agg[,1]

# Title Mapping
atp<-readRDS("\\\\172.20.188.71\\IPC\\IPC Data Crons\\Historical Inventory Data\\Inventory_2017-03-15.rds")
atp<-atp[,c("product_fsn","product_title")]
colnames(atp)<-c("fsn","title")
atp<-unique(atp[,1:2])

ru_waterfall_final<-merge(ru_waterfall_final,atp,by="fsn",all.x = TRUE)

# Zone Mapping
destzone<-read.csv("\\\\172.20.188.71\\Users\\asan.kumar\\Desktop\\FC-Zone.csv")
destzone<-destzone[,c("Pincode","Zone")]
colnames(destzone)<-c("dest_pincode","Zone")
destzone<-unique(destzone[,1:2])

ru_waterfall_final<-merge(ru_waterfall_final,destzone,by="dest_pincode",all.x = TRUE)

#LZ Fcs -- replace it with preferred wh
lzfc<-read.csv("\\\\172.20.188.71\\IPC\\QV App\\Mapping - Concatenate.csv")
colnames(lzfc)<-c("dest_pincode","LZ-FC")
ru_waterfall_final<-merge(ru_waterfall_final,lzfc,by="dest_pincode",all.x = TRUE)

ru_waterfall_final$tax_loss<-NULL
ru_agg$tax_loss<-NULL

library(lubridate)
# path <-paste0("\\\\172.20.188.71\\QV App\\Waterfall Output\\RU\\ru_agg_",temp_week,".csv")
path <-paste0(save_path,"/ru_agg_new_wk",temp_week,".csv")
write.csv(ru_agg,path,row.names=FALSE)
# path <-paste("\\\\172.20.188.71\\QV App\\Waterfall Output\\RU\\ru_waterfall_",temp_week,".csv")
path <-paste0(save_path,"/ru_waterfall_new_wk",temp_week,".csv")
write.csv(rru_waterfall_final,path,row.names=FALSE)
detach(package:lubridate)


save.image(paste0(save_path,temp_week,"_final.RData"))