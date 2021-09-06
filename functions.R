#functions.R

#*************************************************************************************
#Import data
read_data<-function(data_folder){
  csv_files <- list.files(path=data_folder, pattern = "*.csv",full.names = T)
  csv_data<-csv_files %>% 
    map(read_csv)
  excel_files<- list.files(path=data_folder, pattern = "*.xlsx",full.names = T)
  excel_data<-excel_files %>% 
    map(readxl::read_excel)
  full_list<-csv_data %>% 
    append(excel_data)
  return(full_list)
}
#*************************************************************************************
sum.na <- function(df){
  if (all(is.na(df))){
    suma <- NA
  }  
  else {    
    suma <- sum(df, na.rm = T)
  }
  return(suma)
}
#*************************************************************************************
mean.na <- function(df){
  if (all(is.na(df))){
    mean.df <- NA
  }  
  else {    
    mean.df <- mean(df, na.rm = T)
  }
  return(mean.df)
}
#**************************************************************************************
#Clean data

wrangle_data<-function(data_list,meat_list){
  
  population_imports<-data_list[[4]]%>% 
    rename(iso3c='Country (ISO3 code)') %>% 
    select(iso3c,'[2015]','[2016]','[2017]') %>% 
    pivot_longer('[2015]':'[2017]', names_to = "year", values_to = "population") %>% 
    mutate(population=population*1000) #Due to unit in 1000s
  
  population_production<-data_list[[4]]%>% 
    rename(iso3c='Country (ISO3 code)') %>% 
    select(iso3c,'[2006]','[2007]','[2008]','[2009]','[2010]','[2011]','[2012]','[2013]','[2014]','[2015]','[2016]') %>% 
    pivot_longer('[2006]':'[2016]', names_to = "year", values_to = "population") %>% 
    mutate(population=population*1000) %>% 
    group_by(iso3c) %>% 
    summarise(av_population=mean(population))
  
  trade_data<-data_list[[3]] %>% 
    rename(iso3c='Country (ISO3 code)') %>% 
    rename(flow='Trade flow (Name)') %>% 
    rename(com_code='Commodity (Code)') %>% 
    select(iso3c,com_code,flow,'[2015]','[2016]','[2017]') %>% 
    pivot_longer('[2015]':'[2017]', names_to = "year", values_to = "tonnes") %>% 
    filter(str_detect(com_code, "^03")) %>% 
    filter(com_code!="0301.11") %>% 
    filter(com_code!="0301.19") %>% 
    filter(flow=="Import") %>% #|flow=="Export" no loger include Exports
    group_by(iso3c,flow,year) %>% 
    summarise(flow_sum_year = sum(tonnes)*1000)%>% #*1000 to get from tonnes to kg
    left_join(population_imports) %>% 
    mutate(yearly_import_kg_percap=flow_sum_year/population) %>% 
    ungroup() %>% 
    group_by(iso3c,flow) %>% 
    summarise(import_kg_percap_year=mean(yearly_import_kg_percap)) %>% 
    mutate_at(vars(import_kg_percap_year), ~replace(., is.nan(.), 0)) #Since all NaN have appeared in calculations of 0
  
  #pivot_wider(names_from = flow, values_from = flow_average) #To use if want to reinsert Exports. 
  
  #GHG_data<-data_list[[11]] %>% 
  #  left_join(data_list[[10]]) %>% 
  #  rename(prod_sum='prod_sum (tonnes)') %>% 
  #  rename(ghg_pertonne='GHG total (kg CO2-eq per tonne)') %>% 
  #  mutate(GHG_sum=prod_sum*ghg_pertonne) %>% 
  #  group_by(iso3c) %>% 
  #  summarise(total_tonnes=sum(prod_sum),total_GHG=sum(GHG_sum)) %>% 
  #  mutate(total_GHG_pertonne=total_GHG/total_tonnes)
  
  #OLD CODE - KEEP in case we want to go back to FAOSTAT redmeat
  #red_meat_data<-data_list[[10]] %>% 
  #  rename(iso3c='Area Code') %>% 
  #  filter(iso3c!=41) %>% #Remove China, mainland since no ISO3 code
  #  group_by(iso3c,Element,Year) %>% 
  #  summarise(total_meat=sum(Value)) %>% 
  #  ungroup() %>% 
  #  group_by(iso3c,Element) %>% 
  #  summarise(average_meat=mean(total_meat)) %>% 
  #  pivot_wider(names_from = Element, values_from = average_meat) %>% 
  #  rename(red_meat_kgcapyear='Food supply quantity (kg/capita/yr)') %>% 
  #  rename(red_meat_kcalcapyear='Food supply (kcal/capita/day)')
  
  red_meat<-meat_list[[1]] %>% 
    select("X1","X2","Bovine Meat","Mutton & Goat Meat","Pigmeat") %>% 
    rename(bovine_09="Bovine Meat") %>% 
    rename(mutton_09="Mutton & Goat Meat") %>% 
    rename(pig_09="Pigmeat") %>% 
    left_join(meat_list[[2]]) %>% 
    select("X1","X2",bovine_09,mutton_09,pig_09,"Bovine Meat","Mutton & Goat Meat","Pigmeat") %>% 
    rename(bovine_10="Bovine Meat") %>% 
    rename(mutton_10="Mutton & Goat Meat") %>% 
    rename(pig_10="Pigmeat") %>%
    left_join(meat_list[[3]]) %>% 
    select("X1","X2",bovine_09,mutton_09,pig_09, bovine_10,mutton_10,pig_10,"Bovine Meat","Mutton & Goat Meat","Pigmeat") %>% 
    rename(bovine_11="Bovine Meat") %>% 
    rename(mutton_11="Mutton & Goat Meat") %>% 
    rename(pig_11="Pigmeat") %>% 
    slice(-(1:2)) %>%
    rename(iso3c=X1) %>% 
    rename(country=X2) %>% 
    mutate(across(bovine_09:pig_11, na_if, "*")) %>% 
    mutate_at(vars(-iso3c, -country), as.numeric) %>% 
    rowwise() %>% 
    mutate(red_meat_09 = sum.na(c(bovine_09,mutton_09,pig_09))) %>% 
    mutate(red_meat_10 = sum.na(c(bovine_10,mutton_10,pig_10))) %>% 
    mutate(red_meat_11 = sum.na(c(bovine_11,mutton_11,pig_11))) %>% 
    mutate(red_meat_gcapday=mean.na(c(red_meat_09,red_meat_10,red_meat_11))) %>% 
    mutate(red_meat_gcapday=red_meat_gcapday*0.746) %>%  #Conversion of raw to cooked weight
    select(iso3c,red_meat_gcapday)
  
  ruminant_meat<-meat_list[[1]] %>% 
    select("X1","X2","Bovine Meat","Mutton & Goat Meat") %>% 
    rename(bovine_09="Bovine Meat") %>% 
    rename(mutton_09="Mutton & Goat Meat") %>% 
    left_join(meat_list[[2]]) %>% 
    select("X1","X2",bovine_09,mutton_09,"Bovine Meat","Mutton & Goat Meat") %>% 
    rename(bovine_10="Bovine Meat") %>% 
    rename(mutton_10="Mutton & Goat Meat") %>% 
    left_join(meat_list[[3]]) %>% 
    select("X1","X2",bovine_09,mutton_09,bovine_10,mutton_10,"Bovine Meat","Mutton & Goat Meat") %>% 
    rename(bovine_11="Bovine Meat") %>% 
    rename(mutton_11="Mutton & Goat Meat") %>% 
    slice(-(1:2)) %>%
    rename(iso3c=X1) %>% 
    rename(country=X2) %>% 
    mutate(across(bovine_09:mutton_11, na_if, "*")) %>% 
    mutate_at(vars(-iso3c, -country), as.numeric) %>% 
    rowwise() %>% 
    mutate(ruminant_meat_09 = sum.na(c(bovine_09,mutton_09))) %>% 
    mutate(ruminant_meat_10 = sum.na(c(bovine_10,mutton_10))) %>% 
    mutate(ruminant_meat_11 = sum.na(c(bovine_11,mutton_11))) %>% 
    mutate(ruminant_meat_gcapday=mean.na(c(ruminant_meat_09,ruminant_meat_10,ruminant_meat_11))) %>% 
    mutate(ruminant_meat_gcapday=ruminant_meat_gcapday*0.746) %>%  #Conversion of raw to cooked weight
    select(iso3c,ruminant_meat_gcapday)
  
  dalys<-data_list[[8]] %>% 
    pivot_longer(!Category, names_to = "iso3c", values_to = "daly") %>% 
    pivot_wider(names_from = Category, values_from = daly) %>% 
    filter(iso3c!="AFG") %>%  #Since no data and is messing up code
    rename(diabetes = 'Diabetes mellitus') %>% 
    rename(cardiovascular = 'Cardiovascular diseases') %>% 
    rowwise() %>% 
    mutate(DALY_cardiovascular_cap = cardiovascular/population) %>% 
    select(iso3c,DALY_cardiovascular_cap)
  
  clean_data<-data_list[[1]] %>% 
    full_join(data_list[[2]]) %>% 
    select(!'X1') %>% 
    full_join(data_list[[5]]) %>%
    select(!'X1') %>% 
    full_join(data_list[[6]]) %>% 
    select(!'X1') %>%
    full_join(data_list[[7]]) %>%
    full_join(data_list[[9]]) %>%
    full_join(population_production) %>% 
    full_join(dalys) %>% 
    mutate(mean_total_production=as.numeric(mean_total_production)) %>%
    mutate(prod_kgpercap_year=(mean_total_production/av_population)*1000) %>% 
    full_join(trade_data) %>% 
    #full_join(GHG_data) %>% 
    full_join(red_meat) %>%
    full_join(ruminant_meat) %>% 
    filter(!is.na(iso3c)) %>% 
    select(iso3c,ssp585_2050,SEV_omega3,aq_reliance_ratio,
           SEV_vitB12,prod_kgpercap_year,import_kg_percap_year,red_meat_gcapday,ruminant_meat_gcapday,DALY_cardiovascular_cap,
           export_percgdp,totjobs_percap) %>% 
    mutate(aq_reliance_ratio=as.numeric(aq_reliance_ratio)) %>% 
    mutate(iso3c=as.factor(iso3c)) %>% 
    mutate(num.col = rowSums(!is.na(.))) %>% 
    mutate(data_coverage = num.col-1) %>% 
    mutate(data_coverage_percent = round((data_coverage/11)*100)) %>% 
    select(!num.col) %>% 
    ungroup()
  
  return(clean_data)
}
#***************************************************************************************
#Saving file for app
save_file_and_return_path <- function(clean_data) {
  write.csv(clean_data, file=("./App-Directory/clean_data.csv"))
  
  return("./App-Directory/clean_data.csv")
}

#**************************************************************************************
#Policy recommendation assignment
assigning_recommendation<-function(clean_data){
  
  #Policy inclusion criteria
  #**************************
  #Policy 1 - (only full exists)
  #(SEV_omega3 OR SEV_vitB12)  AND (prod_kgpercap_year OR import_kg_percap_year)
  
  # Policy 2 (only full exists)
  # red_meat_gcapday
  
  #Policy 3 - full inclusion with AND, partial with OR
  #red_meat_gcapday AND daly_cardiovascular_cap
  
  #Policy 4 - only full 
  #mean_total_production OR total_GHG
  
  #Policy 5 - full inclusion 
  #(totjobs_percap OR exportpercgdp OR (prop_aquatic_omega3 OR prop_aquatic_B12)) AND ssp585_2050
  #Policy 5 - partial inclusion 
  #(totjobs_percap OR export_percgdp OR (prop_aquatic_omega3 OR prop_aquatic_B12))
  
  
  #Inclusion Cut-offs for each variable
  #************************************
  #SEV_omega3 H =>10
  #SEV_vitB12  H=>10
  #fish_relative_caloric_price =1.75
  
  #bf_availability = prod_kgpercap_year + import_kg_percap_year > 8 (50% of global without China)
  
  #red_meat_gcapday =>50 (with pigmeat) 
  #ruminant_meat_gcapday-> 7 (without pigmeat)
  #daly_cardiovascular_cap = 0.05
  #mean_total_production > 1 000 000
  #total_GHG >1500000000
  #totjobs_percap >0.01
  #export_percgdp > 0.03
  #prop_aquatic_omega3 > 0.2
  #prop_aquatic_B12 >0.2
  #ssp585_2050 > 50
  #aq_reliance_ratio >0.2
  
  #Example plotting code for deciding graph based cut-offs - change y variable
  #ggplot(clean_data) + 
  #geom_point(aes(reorder(iso3c, DALY_cardiovascular_cap
  #                       , mean), 
  #               y=DALY_cardiovascular_cap))+
  #  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #  xlab("Countries")
  
  
  
  policy_data<-clean_data %>% 
    mutate(bf_availability_kgcap_year=prod_kgpercap_year + import_kg_percap_year) %>% 
    mutate(SEV_omega3_inclusion = case_when((SEV_omega3 >= 10) ~ 1,
                                            (SEV_omega3 < 10) ~ 0,
                                            TRUE~NA_real_)) %>% 
    mutate(SEV_B12_inclusion = case_when((SEV_vitB12 >= 10) ~ 1,
                                         (SEV_vitB12 < 10) ~ 0,
                                         TRUE~NA_real_)) %>%
    mutate(availability_inclusion = case_when((bf_availability_kgcap_year >= 8) ~ 1, 
                                              (bf_availability_kgcap_year < 8) ~ 0,
                                              TRUE~NA_real_)) %>%
    mutate(red_meat_inclusion = case_when((red_meat_gcapday >= 50) ~ 1,
                                          (red_meat_gcapday < 50) ~ 0,
                                          TRUE~NA_real_)) %>%
    mutate(ruminant_meat_inclusion = case_when((ruminant_meat_gcapday >= 7) ~ 1,
                                               (ruminant_meat_gcapday < 7) ~ 0,
                                               TRUE~NA_real_)) %>%
    mutate(daly_inclusion = case_when((DALY_cardiovascular_cap>= 0.05) ~ 1, 
                                      (DALY_cardiovascular_cap< 0.05) ~ 0,
                                      TRUE~NA_real_)) %>%
    mutate(jobs_inclusion = case_when((totjobs_percap >= 0.01) ~ 1,
                                      (totjobs_percap < 0.01) ~ 0,
                                      TRUE~NA_real_)) %>%
    mutate(export_inclusion = case_when((export_percgdp >= 0.03) ~ 1, 
                                        (export_percgdp < 0.03) ~ 0, 
                                        TRUE~NA_real_)) %>%
    mutate(consump_inclusion = case_when((aq_reliance_ratio>= 0.2) ~ 1,
                                         (aq_reliance_ratio < 0.2) ~ 0,
                                         TRUE~NA_real_)) %>% 
    mutate(climate_inclusion = case_when((ssp585_2050 >= 50) ~ 1,
                                         (ssp585_2050 < 50) ~ 0,
                                         TRUE~NA_real_)) %>%
    rowwise() %>% 
    mutate(SEV_inclusion = case_when((SEV_B12_inclusion==1 & SEV_omega3_inclusion==1)~2,
                                     (SEV_B12_inclusion==1 | SEV_omega3_inclusion==1)~1,
                                     (SEV_B12_inclusion==0 & SEV_omega3_inclusion==0)~0,
                                     TRUE~NA_real_)) %>% 
    mutate(policy_1=case_when((SEV_inclusion>0 & availability_inclusion ==1) ~"highly_relevant",
                              (SEV_inclusion>0 & availability_inclusion !=1) ~"relevant",
                              (SEV_inclusion==0) ~ "less_applicable",
                              TRUE ~"missing_data")) %>% 
    mutate(policy_2=case_when((red_meat_inclusion==1 & daly_inclusion==1 & availability_inclusion==1) ~"highly_relevant",
                              (red_meat_inclusion==1 & daly_inclusion==1 & availability_inclusion!=1) ~"relevant",
                              (red_meat_inclusion==0 & daly_inclusion==0) ~"less_applicable",
                              (red_meat_inclusion==1 & daly_inclusion==0) ~"less_applicable",
                              (red_meat_inclusion==0 & daly_inclusion==1) ~"less_applicable",
                              TRUE ~"missing_data")) %>% 
    mutate(policy_3=case_when((ruminant_meat_inclusion==1 & availability_inclusion==1) ~"highly_relevant",
                              (ruminant_meat_inclusion==1 & availability_inclusion!=1) ~"relevant",
                              (ruminant_meat_inclusion==0) ~"less_applicable",
                              TRUE~"missing_data")) %>% 
    mutate(policy_4=case_when(((jobs_inclusion==1  | export_inclusion==1 | consump_inclusion==1)& climate_inclusion ==1) ~"highly_relevant",
                              ((jobs_inclusion==1  | export_inclusion==1 | consump_inclusion==1)& climate_inclusion ==0) ~ "relevant",
                              (jobs_inclusion==0  & export_inclusion==0 & consump_inclusion==0) ~ "less_applicable",
                              TRUE ~"missing_data")) 
  
  
  return(policy_data)
}


#***************************************************************************************
#Plotting each inclusion variable
#Note - this is for further investigation of each variable, not included in paper. 
plotting_inclusions<-function(policy_data){
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  inclusion_world<-world %>% 
    select(iso_a3,geometry) %>% 
    rename(iso3c=iso_a3) %>% 
    left_join(policy_data) %>% 
    mutate_at(vars(SEV_omega3_inclusion:nutrient_inclusion), as.factor) 
  
  cols <- c("0" = "red", "1" = "green", "NA" = "grey")
  
  
  #Variable maps - Change names
  plot<-ggplot(data = inclusion_world) +
    geom_sf(aes(fill = climate_inclusion))+
    scale_fill_manual(values=cols,name = "Climate Hazard inclusion", labels = c("Low - not included",
                                                                                "High - included",
                                                                                "Missing data"))+
    theme(legend.position="top")
  
  
  return(plot)
  
}


#**************************************************************************************
#Summarising policy recommendations

summarising_policy<-function(policy_recommendation_data){
  
  policy_summary<-policy_recommendation_data %>%
    mutate(country_name=countrycode(iso3c,origin = "iso3c",destination = "country.name")) %>% 
    select(iso3c,country_name,policy_1,policy_2,policy_3,policy_4) %>% 
    unique()
  
  return(policy_summary)
}
#***************************************************************************************
#Calculating policy salience
calc_policy_salience<-function(policy_summary){
  
  salience_data<-policy_summary %>%  
    mutate(overal_relevant_policy1=as.factor(case_when((policy_1=="highly_relevant"|policy_1=="relevant")~"relevant",
                                                       TRUE~policy_1))) %>% 
    mutate(overal_relevant_policy2=as.factor(case_when((policy_2=="highly_relevant"|policy_2=="relevant")~"relevant",
                                                       TRUE~policy_2))) %>% 
    mutate(overal_relevant_policy3=as.factor(case_when((policy_3=="highly_relevant"|policy_3=="relevant")~"relevant",
                                                       TRUE~policy_3))) %>% 
    mutate(overal_relevant_policy4=as.factor(case_when((policy_4=="highly_relevant"|policy_4=="relevant")~"relevant",
                                                       TRUE~policy_4))) %>% 
    mutate(policy1_policy1=case_when(overal_relevant_policy1=="relevant"~1,
                                     TRUE~0)) %>% 
    mutate(policy1_policy2=case_when((overal_relevant_policy1=="relevant" & overal_relevant_policy2=="relevant")~1, 
                                     TRUE~0)) %>% 
    mutate(policy1_policy3=case_when((overal_relevant_policy1=="relevant" & overal_relevant_policy3=="relevant")~1, 
                                     TRUE~0)) %>% 
    mutate(policy1_policy4=case_when((overal_relevant_policy1=="relevant" & overal_relevant_policy4=="relevant")~1, 
                                     TRUE~0)) %>% 
    mutate(policy2_policy2=case_when(overal_relevant_policy2=="relevant"~1,
                                     TRUE~0)) %>% 
    mutate(policy2_policy1=case_when((overal_relevant_policy2=="relevant" & overal_relevant_policy1=="relevant")~1, 
                                     TRUE~0)) %>% 
    mutate(policy2_policy3=case_when((overal_relevant_policy2=="relevant" & overal_relevant_policy3=="relevant")~1, 
                                     TRUE~0)) %>% 
    mutate(policy2_policy4=case_when((overal_relevant_policy2=="relevant" & overal_relevant_policy4=="relevant")~1, 
                                     TRUE~0)) %>% 
    mutate(policy3_policy3=case_when(overal_relevant_policy3=="relevant"~1,
                                     TRUE~0)) %>% 
    mutate(policy3_policy1=case_when((overal_relevant_policy3=="relevant" & overal_relevant_policy1=="relevant")~1, 
                                     TRUE~0)) %>% 
    mutate(policy3_policy2=case_when((overal_relevant_policy3=="relevant" & overal_relevant_policy2=="relevant")~1, 
                                     TRUE~0)) %>% 
    mutate(policy3_policy4=case_when((overal_relevant_policy3=="relevant" & overal_relevant_policy4=="relevant")~1, 
                                     TRUE~0)) %>% 
    mutate(policy4_policy4=case_when(overal_relevant_policy4=="relevant"~1,
                                     TRUE~0)) %>% 
    mutate(policy4_policy1=case_when((overal_relevant_policy4=="relevant" & overal_relevant_policy1=="relevant")~1, 
                                     TRUE~0)) %>% 
    mutate(policy4_policy2=case_when((overal_relevant_policy4=="relevant" & overal_relevant_policy2=="relevant")~1, 
                                     TRUE~0)) %>% 
    mutate(policy4_policy3=case_when((overal_relevant_policy4=="relevant" & overal_relevant_policy3=="relevant")~1, 
                                     TRUE~0))
  
  
  salience_sum<-salience_data %>% 
    select(policy1_policy1:policy4_policy3) %>%
    ungroup() %>% 
    summarise_all(list(sum)) 
  
  policy1_salience<-salience_sum %>% 
    select(policy1_policy1:policy1_policy4) %>% 
    pivot_longer(policy1_policy1:policy1_policy4,names_to = "type",names_prefix = "policy1_",values_to="total") %>% 
    mutate(policy1=round(100*(total/max(total)))) %>% 
    rename(policy1_num=total)
  
  policy2_salience<-salience_sum %>% 
    select(policy2_policy2:policy2_policy4) %>% 
    pivot_longer(policy2_policy2:policy2_policy4,names_to = "type",names_prefix = "policy2_",values_to="total") %>% 
    mutate(policy2=round(100*(total/max(total))))%>% 
    rename(policy2_num=total) 
  
  policy3_salience<-salience_sum %>% 
    select(policy3_policy3:policy3_policy4) %>% 
    pivot_longer(policy3_policy3:policy3_policy4,names_to = "type",names_prefix = "policy3_",values_to="total") %>% 
    mutate(policy3=round(100*(total/max(total))))%>% 
    rename(policy3_num=total) 
  
  policy4_salience<-salience_sum %>% 
    select(policy4_policy4:policy4_policy3) %>% 
    pivot_longer(policy4_policy4:policy4_policy3,names_to = "type",names_prefix = "policy4_",values_to="total") %>% 
    mutate(policy4=round(100*(total/max(total))))%>% 
    rename(policy4_num=total) 
  
  salience_percentage<-policy1_salience %>% 
    left_join(policy2_salience) %>% 
    left_join(policy3_salience) %>% 
    left_join(policy4_salience)
  
  #writexl::write_xlsx(salience_percentage,"salience_percentage.xlsx")
  
  plot_data<-salience_percentage%>%
    mutate(type=case_when(type=="policy1"~"policy A",
                          type=="policy2"~"policy B",
                          type=="policy3"~"policy C",
                          type=="policy4"~"policy D")) %>% 
    mutate(type=as.factor(type)) %>%
    mutate(type = fct_rev(type)) %>%
    mutate(text_n="(n = ") %>% 
    mutate(percent="%") %>%
    mutate(end_bracket =")") %>% 
    unite("policy1_text",c("policy1", "percent","text_n","policy1_num"),sep=" ", remove = F) %>% 
    unite("policy2_text",c("policy2", "percent","text_n","policy2_num"),sep=" ", remove = F) %>% 
    unite("policy3_text",c("policy3", "percent","text_n","policy3_num"),sep=" ", remove = F) %>% 
    unite("policy4_text",c("policy4", "percent","text_n","policy4_num"),sep=" ", remove = F) %>%
    unite("policy1_text2",c("policy1_text","end_bracket"),sep="",remove=F) %>% 
    unite("policy2_text2",c("policy2_text","end_bracket"),sep="",remove=F) %>% 
    unite("policy3_text2",c("policy3_text","end_bracket"),sep="",remove=F) %>% 
    unite("policy4_text2",c("policy4_text","end_bracket"),sep="",remove=F) %>% 
    unite("policy1_full",c("policy1_text2","policy1"),sep="_",remove=T) %>% 
    unite("policy2_full",c("policy2_text2","policy2"),sep="_",remove=T) %>% 
    unite("policy3_full",c("policy3_text2","policy3"),sep="_",remove=T) %>% 
    unite("policy4_full",c("policy4_text2","policy4"),sep="_",remove=T) %>% 
    select(!c(policy1_num,policy2_num,policy3_num,policy4_num,text_n,percent,end_bracket,policy1_text,policy2_text,policy3_text,policy4_text)) %>% 
    pivot_longer(
      cols = policy1_full:policy4_full,
      names_to = c("policy", "remove"), 
      names_pattern = "(.*)_(.*)",
      values_to = "full") %>% 
    separate(full,c("text","percentage"),remove=F,sep="_") %>% 
    mutate(percentage=as.numeric(percentage)) %>% 
    mutate(policy=case_when(policy=="policy1"~"policy A",
                            policy=="policy2"~"policy B",
                            policy=="policy3"~"policy C",
                            policy=="policy4"~"policy D")) %>% 
    mutate(policy=as.factor(policy))
  
  
  salience_plot<-ggplot(plot_data, aes(policy,type)) +
    geom_raster(aes(fill=percentage))+
    coord_fixed(ratio = 0.5)+
    scale_fill_gradient(low="white",high="grey", limits=c(0,100))+
    geom_text(aes(label=text),size=6)+
    scale_x_discrete(position = "top", name="")+
    ylab("")+
    labs(fill = "Overlap (%)")
  
  #ggsave(salience_plot,file="./Plots/final_plots/salience_manual.svg")
  
  return(salience_percentage)
}

#***************************************************************************************
#Plotting policy recommendation on global maps

map_plot<-function(policy_summary){
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  BF_world<-world %>% 
    select(iso_a3,geometry) %>% 
    rename(iso3c=iso_a3) %>% 
    left_join(policy_summary) %>% 
    mutate_at(vars(policy_1,policy_2,policy_3,policy_4), as.factor) %>% 
    mutate(policy_1 = fct_relevel(policy_1, "highly_relevant", "relevant","less_applicable","missing_data")) %>% 
    mutate(policy_2 = fct_relevel(policy_2, "highly_relevant","relevant","less_applicable","missing_data")) %>% 
    mutate(policy_3 = fct_relevel(policy_3, "highly_relevant", "relevant","less_applicable","missing_data")) %>% 
    mutate(policy_4 = fct_relevel(policy_4, "highly_relevant", "relevant","less_applicable","missing_data")) %>% 
    filter(!is.na(policy_1))
  
  cols <- c("highly_relevant" = "#012998", "relevant" = "#205BFE","less_applicable" = "#AEC3FF", "missing_data" = "grey")
  
  map1<-ggplot(data = BF_world) +
    geom_sf(aes(fill = policy_1))+
    scale_fill_manual(values=cols,labels = c("Highly relevant", "Relevant", "Less relevant","Missing data"))+
    theme(legend.position="top")+
    labs(fill="") #For facet
  #labs(fill = "Improving nutrition")
  
  map2<-ggplot(data = BF_world) +
    geom_sf(aes(fill = policy_2))+
    scale_fill_manual(values=cols,labels = c("Highly relevant", "Relevant", "Less relevant","Missing data"))+
    theme(legend.position="top")+
    labs(fill="") #For facet
  #labs(fill = "Reducing the burden of cardiovascular disease")
  
  map3<-ggplot(data = BF_world) +
    geom_sf(aes(fill = policy_3))+
    scale_fill_manual(values=cols,labels = c("Highly relevant", "Relevant", "Less relevant","Missing data"))+
    theme(legend.position="top")+
    labs(fill="") #For facet
  #labs(fill = "Reducing environmental footprints of food consumption and production")
  
  map4<-ggplot(data = BF_world) +
    geom_sf(aes(fill = policy_4))+
    scale_fill_manual(values=cols, labels = c("Highly relevant", "Relevant", "Less relevant","Missing data"))+
    theme(legend.position="top")+
    labs(fill="") #For facet
  #labs(fill = "Safeguard food system contributions to climate change")
  
  facet_labels<-c("A: Reducing deficiencies", "B: Reducing cardiovascular disease","C: Reducing environmental footprints","D: Safeguard food system contributions")
  
  facet_map<-plot_grid(map1, map2,map3,map4,ncol=2,labels=facet_labels,hjust = 0, label_x = 0.01)
  
  return(facet_map)
  
}
