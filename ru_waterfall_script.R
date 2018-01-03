#### Placement Issues - Placement NOT Possible
# Creating Master pincode files
master_temp <- left_join(dest_fsn_fc,lzn_mapping,by=c("dest_pincode","fc"))
master_temp_1 <- subset(master_temp, !(is.na(lzn_value))) 
nrow(master_temp) == nrow(master_temp_2) # Check if nas are passed in lzn_mapping despite removing thru filters
master_temp_2<- left_join(master_temp_1,vendor_site_2,by=c("fsn","fc")) %>% 
  mutate(vendor_avl_flag=ifelse(is.na(vendor_count),0,1)) # Creating vendor flag

# Creating Serviceability flag - to separate Large FCs as source
distinct_dp <- lzn_mapping %>% select(dest_pincode) %>% distinct 
serviceability <- lzn_mapping %>% filter(lzn_value %in% c('L1','L2','Z1','Z2')) %>%
  group_by(dest_pincode) %>% summarise(lz_fc_count = n_distinct(fc)) %>% 
  right_join(distinct_dp, by='dest_pincode') %>%
  mutate(lz_fc_count = ifelse(is.na(lz_fc_count),0,lz_fc_count),
         is_serviceable_flag = ifelse(lz_fc_count==0,0,1))

# L
sales_fsn_dp <- sales %>% group_by(dest_pincode,fsn) %>%
  summarise(sales=sum(sales),
            ru_sales=sum(ru_sales),
            ru_loss=sum(ru_loss),
            rru_sales=sum(rru_sales))