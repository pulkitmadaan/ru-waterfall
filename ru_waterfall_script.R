#### Placement Issues - Placement NOT Possible
# Creating Master files
master_temp <- left_join(dest_fsn_fc,lzn_mapping,by=c("dest_pincode","fc"))
master_temp_1 <- subset(master_temp, !(is.na(lzn_value))) 
nrow(master_temp) == nrow(master_temp_2) # Check if nas are passed in lzn_mapping despite removing thru filters
master_temp_2<- left_join(master_temp_1,vendor_site_2,by=c("fsn","fc")) %>% 
  mutate(vendor_avl_flag=ifelse(is.na(vendors),0,1)) # Creating vendor flag

#creation of vendor flag