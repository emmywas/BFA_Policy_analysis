#Shiny app - Interactive tool

#**************************************************************
#Load packages

library(tidyverse)
library(shiny)
library(shinydashboard)
library(countrycode)
library("rnaturalearth")
library("rnaturalearthdata")
library(rgeos)
library(scales)
library(writexl)

#**************************************************************
#Load and set up data ----
app_data<-read_csv("clean_data.csv")
hazard_by_system_data<-read_csv("hazard_by_system.csv") 

hazard_clean<-hazard_by_system_data %>% 
  select(-country_name_en) %>% 
  pivot_longer(cols = fisheries_marine:postprod,names_to = "System",values_to = "hazard_score") %>% 
  mutate(country_name=countrycode(iso3c,origin = "iso3c",destination = "country.name")) %>% 
  mutate(slider_value=0)

not_current_iso<-c("ANT", "CSK", "SCG", "SUN", "YUG")

data_clean<- app_data%>%
  #select(-X1) %>% 
  filter(data_coverage!=0) %>% #Remove all countries which we have no data for
  filter(!iso3c %in% not_current_iso) %>% 
  mutate(country_name=countrycode(iso3c,origin = "iso3c",destination = "country.name")) %>% 
  mutate(slider_value=0) %>% 
  mutate(bf_availability_kgcap_year=prod_kgpercap_year + import_kg_percap_year)

country_choices<-unique(data_clean$country_name)

policy_choices<-c("Reducing blue food sensitive deficiencies: B12",
                   "Reducing blue food sensitive deficiencies: omega-3",
                  "Reducing cardiovascular disease",
                  "Reducing environmental footprints",
                  "Safeguarding food system contributions")

policy0_variables<-c("country_name","slider_value","SEV_vitB12","import_kg_percap_year","prod_kgpercap_year","policy_0")
policy1_variables<-c("country_name","slider_value","SEV_omega3","import_kg_percap_year","prod_kgpercap_year","policy_1")
policy2_variables<-c("country_name","slider_value","import_kg_percap_year","prod_kgpercap_year","red_meat_gcapday","DALY_cardiovascular_cap","policy_2")
policy3_variables<-c("country_name","slider_value","import_kg_percap_year","prod_kgpercap_year","ruminant_meat_gcapday","policy_3")
policy4_variables<-c("country_name","slider_value","ssp585_2050","totjobs_percap","export_percgdp","aq_reliance_ratio","policy_4")

range_SEVomega3<-range(data_clean$SEV_omega3,na.rm = T)
range_SEVB12<-range(data_clean$SEV_vitB12,na.rm = T)
range_import<-range(data_clean$import_kg_percap_year,na.rm = T)
range_prodkg<-range(data_clean$prod_kgpercap_year,na.rm = T)
range_red_meat<-range(data_clean$red_meat_gcapday,na.rm = T)
range_ruminant_meat<-range(data_clean$ruminant_meat_gcapday,na.rm = T)
range_daly<-range(data_clean$DALY_cardiovascular_cap,na.rm = T)
range_climate<-range(data_clean$ssp585_2050,na.rm = T)
range_jobs<-range(data_clean$totjobs_percap,na.rm = T)
range_export<-range(data_clean$export_percgdp,na.rm = T)
range_reliance<-range(data_clean$aq_reliance_ratio,na.rm = T)


world <- ne_countries(scale = "medium", returnclass = "sf")

cols <- c("highly_relevant" = "#012998", "relevant" = "#205BFE","less_relevant" = "#AEC3FF", "missing_data" = "grey")

# Deactivate s2
sf::sf_use_s2(FALSE)

#**************************************************************



############
#App Code ----
############

#UI CODE#
ui <- navbarPage(title = "Interactive Data Tool", fluid = TRUE,
                 
                 #------------------------------------------------------
                 tabPanel(title="Article Info",
                          box(title = "Article Info", status = "primary",width=12,
                              fluidRow(column(width = 8, "This is the interactive database tool for the paper:", 
                                              br(), 
                                              "Crona et al, 2022 - unpublished",
                                              br(),
                                              "This article is under XXX licence, make sure to cite paper when using the data",
                                              br(),
                                              "Full data can be downloaded", tags$a(href=" https://doi.org/10.7910/DVN/ILA0XI", "here") ,
                                              br(),
                                              br(),
                                              strong("Why should decision-makers engage with this tool?"),
                                              br(),
                                              "Users of the application can:",
                                              br(),
                                              "Explore the relevance to any particular nation of four policy ambitions addressing the diverse roles blue foods can (and do) in the food system",
                                              br(),
                                              "Explore how different cut-off values affect how relevant a policy ambition is for one or several nations",
                                              br(),
                                              br(),
                                              strong("Why is this important?"),
                                              br(),
                                              "It allows decision-makers to identify nations dealing with similar issues. This can provide a basis for coalitions to tackle these or exchange best practices. ",
                                              br(),
                                              "Countries where missing data precludes analysis can focus attention on collection of key statistics that will improve understanding of the role of blue foods can play in a nation."
                              )))),
                 #------------------------------------------------------
                 tabPanel("Policy Overview", 
                          tags$iframe(style="height:1000px; width:100%", 
                                      src="Supplementary_Table_S2.pdf")),
                 
                 #------------------------------------------------------
                 tabPanel("Variable Distribution", 
                          tags$iframe(style="height:1000px; width:100%", 
                                      src="Supplementary_Figure_S6.pdf")),
                 
                 #--------------------------------------------------
                 tabPanel(title="Policy maps",
                          titlePanel("Policy objectives maps"),
                          br(),
                          "Here is an interactive view of the geographical distribution of the policy objectives.",
                          br(),
                          "The sliders are set at the cut-off values informing the analysis in the paper. You can select your own cut-off value and see how the maps change. To return to the default values, press the button at the bottom of the sidebar panel.",
                          br(),
                          "Below the maps is a table that provides the same information in table format (useful for smaller countries). Use the download button to download the dataset with the selected cut-offs.",
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("SEV_omega_slider","Cut-off value for SEV Omega 3:",
                                          min=round(min(data_clean$SEV_omega3,na.rm=T)),max=round(max(data_clean$SEV_omega3, na.rm=T)),
                                          value=10),
                              sliderInput("SEV_B12_slider","Cut-off value for SEV B12:",
                                          min=round(min(data_clean$SEV_vitB12,na.rm=T)),max=round(max(data_clean$SEV_vitB12, na.rm=T)),
                                          value=10),
                              sliderInput("availability_slider","Cut-off value for BF availability (kg/cap/year):",
                                          min=round(min(data_clean$bf_availability_kgcap_year,na.rm=T)),max=round(max(data_clean$bf_availability_kgcap_year, na.rm=T)),
                                          value=8),
                              sliderInput("red_meat_slider","Cut-off value for red meat consumption (g/cap/day):",
                                          min=round(min(data_clean$red_meat_gcapday,na.rm=T)),max=round(max(data_clean$red_meat_gcapday, na.rm=T)),
                                          value=50),
                              sliderInput("ruminant_meat_slider","Cut-off value for ruminant meat consumption (g/cap/day):",
                                          min=round(min(data_clean$ruminant_meat_gcapday,na.rm=T)),max=round(max(data_clean$ruminant_meat_gcapday, na.rm=T)),
                                          value=7),
                              sliderInput("daly_slider","Cut-off value for DALYs:",
                                          min=round(min(data_clean$DALY_cardiovascular_cap,na.rm=T),digits=2),max=round(max(data_clean$DALY_cardiovascular_cap, na.rm=T),digits= 2),
                                          value=0.05),
                              sliderInput("jobs_slider","Cut-off value for BF employment:",
                                          min=round(min(data_clean$totjobs_percap,na.rm=T),digits=2),max=round(max(data_clean$totjobs_percap, na.rm=T),digits=2),
                                          value=0.01),
                              sliderInput("export_slider","Cut-off value for exports (perc of GDP):",
                                          min=round(min(data_clean$export_percgdp,na.rm=T),digits=2),max=round(max(data_clean$export_percgdp, na.rm=T),digits = 2),
                                          value=0.03),
                              sliderInput("reliance_slider","Cut-off value for BF reliance ratio :",
                                          min=round(min(data_clean$aq_reliance_ratio,na.rm=T)),max=round(max(data_clean$aq_reliance_ratio, na.rm=T)),
                                          value=0.2),
                              sliderInput("climate_slider","Cut-off value for Climate Hazard Score :",
                                          min=round(min(data_clean$ssp585_2050,na.rm=T)),max=round(max(data_clean$ssp585_2050, na.rm=T)),
                                          value=50),
                              #actionButton("update","Update"),
                              actionButton("reset", "Reset to default"),
                              downloadButton('downloadData', 'Download Data'),
                              width = 3
                            ),
                            mainPanel(
                              fluidRow(
                                column(6,plotOutput("policy0_map")),
                                column(6,plotOutput("policy1_map"))),
                              fluidRow(
                                column(6,div(style = "padding: 0px 0px; margin-top:-2em", plotOutput("policy2_map"))),
                                column(6,div(style = "padding: 0px 0px; margin-top:-2em", plotOutput("policy3_map")))),
                              fluidRow(
                                column(6,div(style = "padding: 0px 0px; margin-top:-2em", plotOutput("policy4_map")))),
                              fluidRow(
                                dataTableOutput("table")
                              )
                            )
                          )
                          
                 ),
                 #--------------------------------------------------
                 tabPanel(title = "Country breakdown",
                          titlePanel("Country Data"),
                          br(),
                          "Here you can see the data that underlies each policy recommendation.To view a specific country and policy, select them in the drop down lists.",
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("country", "Select Country:", 
                                          choices=country_choices),
                              selectInput("policy", "Select Policy Recommendation:", 
                                          choices=policy_choices),
                              width = 3
                            ),
                            mainPanel(
                              fluidRow(
                                column(6,uiOutput("policy_description")),
                                column(6,uiOutput("country_info"))),
                              fluidRow(
                                column(6,plotOutput("first_plot", height = "180px"))),
                              fluidRow(
                                column(6,plotOutput("second_plot", height = "100px"))),
                              fluidRow(
                                column(6,plotOutput("third_plot", height = "100px"))),
                              fluidRow(
                                column(6,plotOutput("fourth_plot", height = "100px")))
                            )
                          ))
)


#***************************************************

#SERVER CODE#
server <- function(input, output,session) {
  
  observeEvent(input$reset,{
    updateSliderInput(session,'SEV_omega_slider',value = 10)
    updateSliderInput(session,'SEV_B12_slider',value = 10)
    updateSliderInput(session,'availability_slider',value = 8)
    updateSliderInput(session,'red_meat_slider',value = 50)
    updateSliderInput(session,'ruminant_meat_slider',value = 7)
    updateSliderInput(session,'daly_slider',value = 0.05)
    updateSliderInput(session,'jobs_slider',value = 0.01)
    updateSliderInput(session,'export_slider',value = 0.03)
    updateSliderInput(session,'reliance_slider',value = 0.2)
    updateSliderInput(session,'climate_slider',value = 50)
  })
  
  
  #*************
  
  #**************
  #Reactive cut-offs
  
  reactive_data<-reactive({
    
    data_clean %>% 
      mutate(SEV_omega3_inclusion = case_when((SEV_omega3 >= input$SEV_omega_slider) ~ 1,
                                              (SEV_omega3 < input$SEV_omega_slider) ~ 0,
                                              TRUE~NA_real_)) %>% 
      mutate(SEV_B12_inclusion = case_when((SEV_vitB12 >= input$SEV_B12_slider) ~ 1,
                                           (SEV_vitB12 < input$SEV_B12_slider) ~ 0,
                                           TRUE~NA_real_)) %>%
      mutate(availability_inclusion = case_when((bf_availability_kgcap_year >= input$availability_slider) ~ 1, 
                                                (bf_availability_kgcap_year < input$availability_slider) ~ 0,
                                                TRUE~NA_real_)) %>%
      mutate(red_meat_inclusion = case_when((red_meat_gcapday >= input$red_meat_slider) ~ 1,
                                            (red_meat_gcapday < input$red_meat_slider) ~ 0,
                                            TRUE~NA_real_)) %>%
      mutate(ruminant_meat_inclusion = case_when((ruminant_meat_gcapday >= input$ruminant_meat_slider) ~ 1,
                                                 (ruminant_meat_gcapday < input$ruminant_meat_slider) ~ 0,
                                                 TRUE~NA_real_)) %>%
      mutate(daly_inclusion = case_when((DALY_cardiovascular_cap>= input$daly_slider) ~ 1, 
                                        (DALY_cardiovascular_cap< input$daly_slider) ~ 0,
                                        TRUE~NA_real_)) %>%
      mutate(jobs_inclusion = case_when((totjobs_percap >= input$jobs_slider) ~ 1,
                                        (totjobs_percap < input$jobs_slider) ~ 0,
                                        TRUE~NA_real_)) %>%
      mutate(export_inclusion = case_when((export_percgdp >= input$export_slider) ~ 1, 
                                          (export_percgdp < input$export_slider) ~ 0, 
                                          TRUE~NA_real_)) %>%
      mutate(consump_inclusion = case_when((aq_reliance_ratio>= input$reliance_slider) ~ 1,
                                           (aq_reliance_ratio < input$reliance_slider) ~ 0,
                                           TRUE~NA_real_)) %>% 
      mutate(climate_inclusion = case_when((ssp585_2050 >= input$climate_slider) ~ 1,
                                           (ssp585_2050 < input$climate_slider) ~ 0,
                                           TRUE~NA_real_)) %>%
      rowwise() %>% 
      #mutate(SEV_inclusion = case_when((SEV_B12_inclusion==1 & SEV_omega3_inclusion==1)~2,
      #                                 (SEV_B12_inclusion==1 | SEV_omega3_inclusion==1)~1,
      #                                 (SEV_B12_inclusion==0 & SEV_omega3_inclusion==0)~0,
      #                                 TRUE~NA_real_)) %>% 
      #mutate(policy_1=case_when((SEV_inclusion>0 & availability_inclusion ==1) ~"highly_relevant",
      #                          (SEV_inclusion>0 & availability_inclusion !=1) ~"relevant",
      #                          (SEV_inclusion==0) ~ "less_relevant",
      #                          TRUE ~"missing_data")) %>% 
      mutate(policy_0=case_when((SEV_B12_inclusion==1 & availability_inclusion ==1) ~"highly_relevant",
                                (SEV_B12_inclusion==1 & availability_inclusion !=1) ~"relevant",
                                (SEV_B12_inclusion==0 ) ~ "less_relevant",
                                TRUE ~"missing_data")) %>% 
      mutate(policy_1=case_when((SEV_omega3_inclusion==1 & availability_inclusion ==1) ~"highly_relevant",
                                (SEV_omega3_inclusion==1 & availability_inclusion !=1) ~"relevant",
                                (SEV_omega3_inclusion==0) ~ "less_relevant",
                                TRUE ~"missing_data")) %>% 
      mutate(policy_2=case_when((red_meat_inclusion==1 & daly_inclusion==1 & availability_inclusion==1) ~"highly_relevant",
                                (red_meat_inclusion==1 & daly_inclusion==1 & availability_inclusion!=1) ~"relevant",
                                (red_meat_inclusion==0 & daly_inclusion==0) ~"less_relevant",
                                (red_meat_inclusion==1 & daly_inclusion==0) ~"less_relevant",
                                (red_meat_inclusion==0 & daly_inclusion==1) ~"less_relevant",
                                TRUE ~"missing_data")) %>% 
      mutate(policy_3=case_when((ruminant_meat_inclusion==1 & availability_inclusion==1) ~"highly_relevant",
                                (ruminant_meat_inclusion==1 & availability_inclusion!=1) ~"relevant",
                                (ruminant_meat_inclusion==0) ~"less_relevant",
                                TRUE~"missing_data")) %>% 
      mutate(policy_4=case_when(((jobs_inclusion==1  | export_inclusion==1 | consump_inclusion==1)& climate_inclusion ==1) ~"highly_relevant",
                                ((jobs_inclusion==1  | export_inclusion==1 | consump_inclusion==1)& climate_inclusion ==0) ~ "relevant",
                                (jobs_inclusion==0  & export_inclusion==0 & consump_inclusion==0) ~ "less_relevant",
                                TRUE ~"missing_data"))
    
  })
  
  summary_table<-reactive({reactive_data() %>% 
    mutate(country_name=countrycode(iso3c,origin = "iso3c",destination = "country.name")) %>% 
    select(country_name,policy_0:policy_4) %>% 
    rename("Reducing deficiencies: B12"="policy_0") %>% 
    rename("Reducing deficiencies: omega-3"="policy_1") %>% 
    rename("Reducing cardiovascular disease "="policy_2") %>% 
    rename("Reducing environmental footprints"="policy_3") %>% 
    rename("Safeguarding food system contributions"="policy_4")
  })
  
  output$table<-renderDataTable(summary_table())
  
  #output$table<-renderDataTable(reactive_data() %>% 
  #                                mutate(country_name=countrycode(iso3c,origin = "iso3c",destination = "country.name")) %>% 
  #                                select(country_name,policy_0:policy_4) %>% 
  #                                rename("Reducing deficiencies: B12"="policy_0") %>% 
  #                                rename("Reducing deficiencies: omega-3"="policy_1") %>% 
  #                                rename("Reducing cardiovascular disease "="policy_2") %>% 
  #                                rename("Reducing environmental footprints"="policy_3") %>% 
  #                                rename("Safeguarding food system contributions"="policy_4") 
  #)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("policy_recommendations_selected_cutoffs", ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(summary_table(), file)
    }
  )
  
  #**************
  #Maps based on reactive data
  BF_world<-reactive({
    world %>% 
      select(iso_a3,geometry) %>% 
      rename(iso3c=iso_a3) %>% 
      left_join(reactive_data()) %>% 
      mutate_at(vars(policy_0,policy_1,policy_2,policy_3,policy_4), as.factor) %>% 
      mutate(policy_0 = fct_relevel(policy_0, "highly_relevant", "relevant","less_relevant","missing_data")) %>%
      mutate(policy_1 = fct_relevel(policy_1, "highly_relevant", "relevant","less_relevant","missing_data")) %>% 
      mutate(policy_2 = fct_relevel(policy_2, "highly_relevant","relevant","less_relevant","missing_data")) %>% 
      mutate(policy_3 = fct_relevel(policy_3, "highly_relevant", "relevant","less_relevant","missing_data")) %>% 
      mutate(policy_4 = fct_relevel(policy_4, "highly_relevant", "relevant","less_relevant","missing_data")) %>% 
      filter(!is.na(policy_1))
  })
  

  output$policy0_map<-renderPlot({
    ggplot(data = BF_world()) +
      geom_sf(aes(fill = policy_0))+
      scale_fill_manual(values=cols,labels = c("Highly relevant", "Relevant", "Less relevant","Missing data"))+
      theme(legend.position="top")+
      labs(title = "Reducing blue food sensitive deficiencies: B12", fill=NULL)
  })
  
  
  output$policy1_map<-renderPlot({
    ggplot(data = BF_world()) +
      geom_sf(aes(fill = policy_1))+
      scale_fill_manual(values=cols,labels = c("Highly relevant", "Relevant", "Less relevant","Missing data"))+
      theme(legend.position="top")+
      labs(title = "Reducing blue food sensitive deficiencies: omega-3", fill=NULL)
  })
  
  
  output$policy2_map<-renderPlot({
    ggplot(data = BF_world()) +
      geom_sf(aes(fill = policy_2))+
      scale_fill_manual(values=cols,labels = c("Highly relevant", "Relevant", "Less relevant","Missing data"))+
      theme(legend.position="top")+
      labs(title = "Reducing cardiovascular disease risk", fill=NULL)
  })
  
  
  output$policy3_map<-renderPlot({
    ggplot(data = BF_world()) +
      geom_sf(aes(fill = policy_3))+
      scale_fill_manual(values=cols,labels = c("Highly relevant", "Relevant", "Less relevant","Missing data"))+
      theme(legend.position="top")+
      labs(title = "Reducing env. footprints of food consumption and production",fill=NULL)
  })
  
  
  output$policy4_map<-renderPlot({
    ggplot(data = BF_world()) +
      geom_sf(aes(fill = policy_4))+
      scale_fill_manual(values=cols,labels = c("Highly relevant", "Relevant", "Less relevant","Missing data"))+
      theme(legend.position="top")+
      labs(title = "Safeguarding food system contributions under climate change",fill=NULL)
  })
  
  #***************************************   
  #Reactive data for country breakdown
  filtered_data<-reactive({
    
    if(input$policy == "Reducing blue food sensitive deficiencies: B12"){select_variables=policy0_variables}
    else if (input$policy == "Reducing blue food sensitive deficiencies: omega-3"){select_variables=policy1_variables}
    else if (input$policy == "Reducing cardiovascular disease"){select_variables=policy2_variables}
    else if (input$policy == "Reducing environmental footprints"){select_variables=policy3_variables} 
    else if (input$policy == "Safeguarding food system contributions"){select_variables=policy4_variables} 
    
    reactive_data() %>%
      filter(country_name==input$country) %>% 
      select(any_of(select_variables)) %>% 
      rename_at(vars(starts_with("policy")), funs(paste0("policy")))
  })
  
  hazard_filtered<-reactive({
    hazard_clean %>% 
      filter(country_name==input$country)
  })
  
  #**************
  #Reactive plots  
  SEV_omega_plot <- reactive({
    ggplot(filtered_data(), aes(x=SEV_omega3, y=slider_value))+
      geom_point(size=5)+
      ylim(c(-2,2))+
      theme_classic()+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 10,col="red")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y =element_blank())+
      scale_x_continuous(name="Summary exposure values (SEV) Omega 3",limits = range_SEVomega3)
  })
  
  SEV_B12_plot <- reactive({
    ggplot(filtered_data(), aes(x=SEV_vitB12, y=slider_value))+
      geom_point(size=5)+
      ylim(c(-2,2))+
      theme_classic()+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 10,col="red")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y =element_blank())+
      scale_x_continuous(name="Summary exposure values (SEV) vitamin B12",limits = range_SEVB12)
  })
  
  
  import_plot <- reactive({
    ggplot(filtered_data(), aes(x=import_kg_percap_year, y=slider_value))+
      geom_point(size=5)+
      ylim(c(-2,2))+
      theme_classic()+
      geom_hline(yintercept = 0)+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y =element_blank())+
      geom_vline(xintercept = 8,col="red",linetype="dashed")+
      scale_x_continuous(name="Domestic imports (kg/capita/year)",limits = range_import)
  })
  
  prodkg_plot <- reactive({
    ggplot(filtered_data(), aes(x=prod_kgpercap_year, y=slider_value))+
      geom_point(size=5)+
      ylim(c(-2,2))+
      theme_classic()+
      geom_hline(yintercept = 0)+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y =element_blank())+
      geom_vline(xintercept = 8,col="red",linetype="dashed")+
      scale_x_continuous(name="Domestic production (kg/capita/year",limits = range_prodkg)
  })
  
  red_meat_plot <- reactive({
    ggplot(filtered_data(), aes(x=red_meat_gcapday, y=slider_value))+
      geom_point(size=5)+
      ylim(c(-2,2))+
      theme_classic()+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 50,col="red")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y =element_blank())+
      scale_x_continuous(name="Red meat consumption (g/capita/day)",limits = range_red_meat)
  })
  
  ruminant_meat_plot <- reactive({
    ggplot(filtered_data(), aes(x=ruminant_meat_gcapday, y=slider_value))+
      geom_point(size=5)+
      ylim(c(-2,2))+
      theme_classic()+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 7,col="red")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y =element_blank())+
      scale_x_continuous(name="Ruminant meat consumption (g/capita/day)",limits = range_ruminant_meat)
  })
  
  daly_plot <- reactive({
    ggplot(filtered_data(), aes(x=DALY_cardiovascular_cap, y=slider_value))+
      geom_point(size=5)+
      ylim(c(-2,2))+
      theme_classic()+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0.05,col="red")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y =element_blank())+
      scale_x_continuous(name="Disability adjusted life years per capita", limits = range_daly)
  })
  
  climate_plot <- reactive({
    ggplot(filtered_data(), aes(x=ssp585_2050, y=slider_value))+
      geom_point(size=5)+
      geom_point(data=hazard_filtered(),aes(x=hazard_score,y=slider_value,col=System),size=4)+
      ylim(c(-2,2))+
      theme_classic()+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 50,col="red")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y =element_blank(),
            legend.position = "top")+
      scale_x_continuous(name="Climate Hazard Score", limits = range_climate)
  })
  
  jobs_plot <- reactive({
    ggplot(filtered_data(), aes(x=totjobs_percap, y=slider_value))+
      geom_point(size=5)+
      ylim(c(-2,2))+
      theme_classic()+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0.01,col="red")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y =element_blank())+
      scale_x_continuous(name="Percent of workforce employed in Blue Food sector",limits = range_jobs)
  })
  
  
  export_plot <- reactive({
    ggplot(filtered_data(), aes(x=export_percgdp, y=slider_value))+
      geom_point(size=5)+
      ylim(c(-2,2))+
      theme_classic()+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0.03,col="red")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y =element_blank())+
      scale_x_continuous(name="Export as percentage of GDP",limits = range_export)
  })
  
  
  reliance_plot <- reactive({
    ggplot(filtered_data(), aes(x=aq_reliance_ratio, y=slider_value))+
      geom_point(size=5)+
      ylim(c(-2,2))+
      theme_classic()+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0.2,col="red")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y =element_blank())+
      scale_x_continuous(name="Blue Foods reliance ratio", limits = range_reliance)
  })
  
  #************  
  firstplot_Input <- reactive({
    switch(input$policy,
           "Reducing blue food sensitive deficiencies: B12" = SEV_B12_plot(),
           "Reducing blue food sensitive deficiencies: omega-3" = SEV_omega_plot(),
           "Reducing cardiovascular disease" = red_meat_plot(),
           "Reducing environmental footprints" = ruminant_meat_plot(),
           "Safeguarding food system contributions" = climate_plot()
    )
  })  
  
  #************  
  secondplot_Input <- reactive({
    switch(input$policy,
           "Reducing blue food sensitive deficiencies: B12" = prodkg_plot(),
           "Reducing blue food sensitive deficiencies: omega-3" = prodkg_plot(),
           "Reducing cardiovascular disease" = daly_plot(),
           "Reducing environmental footprints" = prodkg_plot(),
           "Safeguarding food system contributions" = jobs_plot()
    )
  })   
  
  #************  
  thirdplot_Input <- reactive({
    switch(input$policy,
           "Reducing blue food sensitive deficiencies: B12" = import_plot(),
           "Reducing blue food sensitive deficiencies: omega-3" = import_plot(),
           "Reducing cardiovascular disease" = prodkg_plot(),
           "Reducing environmental footprints" = import_plot(),
           "Safeguarding food system contributions" = export_plot()
    )
  })  
  
  #************  
  fourthplot_Input <- reactive({
    switch(input$policy,
           "Reducing cardiovascular disease" = import_plot(),
           "Safeguarding food system contributions" = reliance_plot()
    )
  })   
  
  #***********  
  output$first_plot<-renderPlot({
    firstplot_Input()
  })
  
  #***********  
  output$second_plot<-renderPlot({
    secondplot_Input()
  })  
  
  #***********  
  output$third_plot<-renderPlot({
    thirdplot_Input()
  })  
  
  #***********  
  output$fourth_plot<-renderPlot({
    fourthplot_Input()
  })  
  
  #***********************      
  output$policy_description <- renderUI({
    
    p0_text<-"Policy recommendation: Reducing blue food sensitive deficiencies: vitamin B12"
    p1_text<-"Policy recommendation: Reducing blue food sensitive deficiencies: omega-3"
    p2_text<-"Policy recommendation: Reducing cardiovascular disease risk"
    p3_text<- "Policy recommendation: Reducing environmental footprints of food consumption and production "
    p4_text1<-"Policy recommendation: Safeguarding food system contributions under climate change" 
    p4_text2<-"Note that policy relevance is set by the aggregate score of climate hazard (black), althought the hazard score for each subsystem is shown."
    p4_text3<-"Colors indicate the climate hazard score for different production  subsystems of the country in focus."
    
    
    if (input$policy == "Reducing blue food sensitive deficiencies: B12"){p(p0_text,style="height:225px;
                    padding:25px;
                    background-color:papayawhip;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(input$policy == "Reducing blue food sensitive deficiencies: omega-3"){p(p1_text,style="height:225px;
                    padding:25px;
                    background-color:papayawhip;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    else if (input$policy == "Reducing cardiovascular disease"){p(p2_text,style="height:225px;
                    padding:25px;
                    background-color:papayawhip;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    else if (input$policy == "Reducing environmental footprints") { p(p3_text,style="height:225px;
                    padding:25px;
                    background-color:papayawhip;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    else if (input$policy == "Safeguarding food system contributions") { p(p4_text1,br(),br(),p4_text2,br(),br(),p4_text3,
                                                                           style="height:225px;
                    padding:25px;
                    background-color:papayawhip;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    
    
  })
  
  #***********************      
  output$country_info <- renderUI({
    
    general_text<-paste(filtered_data()$country_name,"has a status of",filtered_data()$policy)
    
    policy0_high<-"The selected country has status “Highly Relevant”. This means the country has a deficiency in vitamin B12 and blue foods are available based on an assessment domestic production and imports. Blue foods can therefore feasibly help reduce this deficiencies. "
    policy1_high<-"The selected country has status “Highly Relevant”. This means the country has a deficiency in omega-3 and blue foods are available based on an assessment domestic production and imports. Blue foods can therefore feasibly help reduce this deficiencies. "
    policy2_high<-"The selected country has status “Highly Relevant”. This means the country has both a high consumption of red meat and high risk of cardiovascular disease, and blue foods are available based on an assessment domestic production and imports. Blue foods can therefore feasibly help reduce cardiovascular disease risk. "
    policy3_high<-"The selected country has status “Highly Relevant”. This means the country has a high consumption of ruminant meat and blue foods are available based on an assessment domestic production and imports. Blue foods can therefore feasibly help reduce the environmental footprint of the consumption."
    policy4_high1<-"The selected  country has status “Highly Relevant”. This means the blue foods sector is important for employment, export revenue or national nutrition. The country also faces a high climate hazard. "
    policy4_high2<-"These conditions suggest policies geared towards safeguarding the contribution of Blue Foods is urgent and important."
    
    policy0_relevant<-"The selected country has status “Relevant”. This means the country has a deficiency in vitamin B12 but Blue Foods are not currently produced or imported to any great extent. By sourcing more Blue Foods deficiencies can feasibly be reduced. "
    policy1_relevant<-"The selected country has status “Relevant”. This means the country has a deficiency in omega-3 but Blue Foods are not currently produced or imported to any great extent. By sourcing more Blue Foods deficiencies can feasibly be reduced. "
    policy2_relevant<-"The selected country has status “Relevant”. This means the country has both a high consumption of red meat and high risk of cardiovascular disease but Blue Foods are not currently produced or imported to any great extent. By sourcing more Blue Foods disease risk can feasibly be reduced."
    policy3_relevant<-"The selected country has status “Relevant”. This means the country has a high consumption of ruminant meat but Blue Foods are not currently produced or imported to any great extent. By sourcing more Blue Foods the dietary environmental footprint can feasibly be reduced."
    policy4_relevant1<-"The selected country has status “Relevant”. This means the blue foods sector is important for employment, export revenue or national nutrition, but the country does not face a high climate hazard. "
    policy4_relevant2<-"These conditions suggest policies geared towards safeguarding the contribution of Blue Foods are important but climate change is not the most urgent issue to tackle in relation to BF."
    
    policy0_less<-"The selected country has status “Less Relevant”. This means the country does not have a deficiency in vitamin B12. "
    policy1_less<-"The selected country has status “Less Relevant”. This means the country does not have a  deficiency in omega-3. "
    policy2_less<-"The selected country has status “Less Relevant”. This means the country does not have high consumption of red meat or does not suffer from high levels of cardiovascular disease (see visualization). "
    policy3_less<-"The selected country you have has status “Less Relevant”. This means the country does not have high consumption of ruminant meat."
    policy4_less<-"The selected country you have has status “Less Relevant”. This means the blue foods sector is not important for employment, export revenue, or high national nutrition (i.e., all variables below threshold, see visualization). "
    
    
    missing_data<-"We are sorry, but the country you have selected has too much missing data. The tool is not able to provide a recommendation in relation to this policy because the analysis is constrained by missing data in some variable(s) (those lacking a black circle marker in the visualization below)"
    
    
    
    if(filtered_data()$policy=="highly_relevant"& input$policy=="Reducing blue food sensitive deficiencies: omega-3")
    {p(general_text,br(),br(),policy1_high,
       style="height:225px;
                    padding:25px;
                    background-color:#4D74D3;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="highly_relevant"& input$policy=="Reducing blue food sensitive deficiencies: B12")
    {p(general_text,br(),br(),policy0_high,
       style="height:225px;
                    padding:25px;
                    background-color:#4D74D3;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="highly_relevant"& input$policy=="Reducing cardiovascular disease")
    {p(general_text,br(),br(),policy2_high,
       style="height:225px;
                    padding:25px;
                    background-color:#4D74D3;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="highly_relevant"& input$policy=="Reducing environmental footprints")
    {p(general_text,br(),br(),policy3_high,
       style="height:225px;
                    padding:25px;
                    background-color:#4D74D3;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="highly_relevant"& input$policy=="Safeguarding food system contributions")
    {p(general_text,br(),br(),policy4_high1,br(),policy4_high2,
       style="height:225px;
                    padding:25px;
                    background-color:#4D74D3;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="relevant"& input$policy=="Reducing blue food sensitive deficiencies: B12")
    {p(general_text,br(),br(),policy0_relevant,
       style="height:225px;
                    padding:25px;
                    background-color:#82A3FC;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="relevant"& input$policy=="Reducing blue food sensitive deficiencies: omega-3")
    {p(general_text,br(),br(),policy1_relevant,
       style="height:225px;
                    padding:25px;
                    background-color:#82A3FC;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="relevant"& input$policy=="Reducing cardiovascular disease")
    {p(general_text,br(),br(),policy2_relevant,
       style="height:225px;
                    padding:25px;
                    background-color:#82A3FC;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="relevant"& input$policy=="Reducing environmental footprints")
    {p(general_text,br(),br(),policy3_relevant,
       style="height:225px;
                    padding:25px;
                    background-color:#82A3FC;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="relevant"& input$policy=="Safeguarding food system contributions")
    {p(general_text,br(),br(),policy4_relevant1,br(),policy4_relevant2,
       style="height:225px;
                    padding:25px;
                    background-color:#4D74D3;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="less_relevant"& input$policy=="Reducing blue food sensitive deficiencies: B12")
    {p(general_text,br(),br(),policy0_less,
       style="height:225px;
                    padding:25px;
                    background-color:#E5ECFF;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="less_relevant"& input$policy=="Reducing blue food sensitive deficiencies: omega-3")
    {p(general_text,br(),br(),policy1_less,
       style="height:225px;
                    padding:25px;
                    background-color:#E5ECFF;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="less_relevant"& input$policy=="Reducing cardiovascular disease")
    {p(general_text,br(),br(),policy2_less,
       style="height:225px;
                    padding:25px;
                    background-color:#E5ECFF;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="less_relevant"& input$policy=="Reducing environmental footprints")
    {p(general_text,br(),br(),policy3_less,
       style="height:225px;
                    padding:25px;
                    background-color:#E5ECFF;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    
    else if(filtered_data()$policy=="less_relevant"& input$policy=="Safeguarding food system contributions")
    {p(general_text,br(),br(),policy4_less,
       style="height:225px;
                    padding:25px;
                    background-color:#E5ECFF;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    
    
    else if(filtered_data()$policy=="missing_data"){p(general_text,br(),br(),missing_data,
                                                      style="height:225px;
                    padding:25px;
                    background-color:gainsboro;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
  })
  #***********************      
  
}

shinyApp(server = server, ui = ui)