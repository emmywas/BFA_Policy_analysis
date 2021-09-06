#targets script

library(targets)
source("./functions.R") # defines functions

tar_option_set(packages = c("tidyverse", "countrycode","rnaturalearth","rnaturalearthdata",
                            "cowplot", "cluster"))

list(
  
  tar_target(
    data_list,
    read_data("./data/data")
  ),
  
  tar_target(
    meat_list,
    read_data("./data/meat_data")
  ),
  
  tar_target(
    clean_data,
    wrangle_data(data_list,meat_list)
  ),
  
  tar_target(
    app_file,
    save_file_and_return_path(clean_data),
    format = "file"
  ),
  
  tar_target(
    policy_data,
    assigning_recommendation(clean_data)
  ),
  
  tar_target(
    policy_summary,
    summarising_policy(policy_data)
  ), 
  
  tar_target(
    salience_percentage,
    calc_policy_salience(policy_summary)
  ),
  
  tar_target(
    map_plots,
    map_plot(policy_summary)
  )
)



#***************************************
#to run: tar_make()
#to check: tar_visnetwork()
#to load: tar_load(target_name)
#to delete cache of specific target: tar_delete("target_name")


