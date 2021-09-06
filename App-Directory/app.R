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

#**************************************************************
#Load and set up data ----
app_data<-read_csv("clean_data.csv")

not_current_iso<-c("ANT", "CSK", "SCG", "SUN", "YUG")

data_clean<- app_data%>%
  select(-X1) %>% 
  filter(data_coverage!=0) %>% #Remove all countries which we have no data for
  filter(!iso3c %in% not_current_iso) %>% 
  mutate(country_name=countrycode(iso3c,origin = "iso3c",destination = "country.name")) %>% 
  mutate(slider_value=0) %>% 
  mutate(bf_availability_kgcap_year=prod_kgpercap_year + import_kg_percap_year)

country_choices<-unique(data_clean$country_name)

policy_choices<-c("Policy 1","Policy 2","Policy 3","Policy 4")

policy1_variables<-c("country_name","slider_value","SEV_omega3","SEV_vitB12","import_kg_percap_year","prod_kgpercap_year","policy_1")
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

cols <- c("highly_relevant" = "#012998", "relevant" = "#205BFE","less_applicable" = "#AEC3FF", "missing_data" = "grey")

#**************************************************************



############
#App Code ----
############

#UI CODE#
ui <- navbarPage(title = "Interactive Data Tool",
                 
                 #------------------------------------------------------
                 tabPanel(title="Article Info",
                          box(title = "Article Info", status = "primary",width=12,
                              fluidRow(column(width = 8, "This is the interactive database tool for the paper:", 
                                              br(), 
                                              "XXX [for double blind review] et al, 2021 - unpublished",
                                              br(),
                                              "This article is under XXX licence, make sure to cite paper when using the data",
                                              br(),
                                              "Full data can be downloaded at XXXX",
                                              br(),
                                              br(),
                                              strong("Why should decision-makers engage with this tool?"),
                                              br(),
                                              "Users of the application can:",
                                              br(),
                                              "Explore the salience to any particular nation of five policy ambitions addressing the diverse roles blue foods can (and do) in the food system",
                                              br(),
                                              "Explore how different cut-off values affect how salient a policy ambition is for one or several nations",
                                              br(),
                                              br(),
                                              strong("Why is this important?"),
                                              br(),
                                              "It allows decision-makers to identify nations dealing with similar issues. This can provide a basis for coalitions to tackle these or exchange best practices. ",
                                              br(),
                                              "Countries where missing data precludes analysis can focus attention on collection of key statistics that will improve understanding of the role of blue foods can play in a nation."
                              )))),
                 #--------------------------------------------------
                 tabPanel(title="Policy maps",
                          titlePanel("Policy objectives maps"),
                          br(),
                          "Here is an interactive view of the geographical distribution of the policy objectives.",
                          br(),
                          "The sliders are set at the cut-off values from the paper. Select your own to see how the maps change. To return to the default values, press the button at the bottom of the sidebar panel.",
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
                              width = 3
                            ),
                            mainPanel(
                              fluidRow(
                                column(6,plotOutput("policy1_map")),
                                column(6,plotOutput("policy2_map"))),
                              fluidRow(
                                column(6,plotOutput("policy3_map")),
                                column(6,plotOutput("policy4_map")))
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
                              width = 2
                            ),
                            mainPanel(
                              fluidRow(
                                column(6,uiOutput("policy_description")),
                                column(6,uiOutput("country_info"))),
                              fluidRow(
                                column(6,plotOutput("first_plot", height = "100px")),
                                column(4,uiOutput("firstplot_description"))),
                              fluidRow(
                                column(6,plotOutput("second_plot", height = "100px")),
                                column(4,uiOutput("secondplot_description"))),
                              fluidRow(
                                column(6,plotOutput("third_plot", height = "100px")),
                                column(4,uiOutput("thirdplot_description"))),
                              fluidRow(
                                column(6,plotOutput("fourth_plot", height = "100px")),
                                column(4,uiOutput("fourthplot_description")))
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
    
  })
  #**************
  #Maps based on reactive data
  BF_world<-reactive({
    world %>% 
      select(iso_a3,geometry) %>% 
      rename(iso3c=iso_a3) %>% 
      left_join(reactive_data()) %>% 
      mutate_at(vars(policy_1,policy_2,policy_3,policy_4), as.factor) %>% 
      mutate(policy_1 = fct_relevel(policy_1, "highly_relevant", "relevant","less_applicable","missing_data")) %>% 
      mutate(policy_2 = fct_relevel(policy_2, "highly_relevant","relevant","less_applicable","missing_data")) %>% 
      mutate(policy_3 = fct_relevel(policy_3, "highly_relevant", "relevant","less_applicable","missing_data")) %>% 
      mutate(policy_4 = fct_relevel(policy_4, "highly_relevant", "relevant","less_applicable","missing_data")) %>% 
      filter(!is.na(policy_1))
  })
  
  output$policy1_map<-renderPlot({
    ggplot(data = BF_world()) +
      geom_sf(aes(fill = policy_1))+
      scale_fill_manual(values=cols,labels = c("Highly relevant", "Relevant", "Less relevant","Missing data"))+
      theme(legend.position="top")+
      labs(fill = "Improving nutrition")
  })
  
  
  output$policy2_map<-renderPlot({
    ggplot(data = BF_world()) +
      geom_sf(aes(fill = policy_2))+
      scale_fill_manual(values=cols,labels = c("Highly relevant", "Relevant", "Less relevant","Missing data"))+
      theme(legend.position="top")+
      labs(fill = "Reducing the burden of cardiovascular disease")
  })
  
  
  output$policy3_map<-renderPlot({
    ggplot(data = BF_world()) +
      geom_sf(aes(fill = policy_3))+
      scale_fill_manual(values=cols,labels = c("Highly relevant", "Relevant", "Less relevant","Missing data"))+
      theme(legend.position="top")+
      labs(fill = "Reducing env. footprints of food consumption and production")
  })
  
  
  output$policy4_map<-renderPlot({
    ggplot(data = BF_world()) +
      geom_sf(aes(fill = policy_4))+
      scale_fill_manual(values=cols,labels = c("Highly relevant", "Relevant", "Less relevant","Missing data"))+
      theme(legend.position="top")+
      labs(fill = "Safeguard food system contributions to climate change")
  })
  
  #***************************************   
  #Reactive data for country breakdown
  filtered_data<-reactive({
    
    if(input$policy == "Policy 1"){select_variables=policy1_variables} 
    else if (input$policy == "Policy 2"){select_variables=policy2_variables}
    else if (input$policy == "Policy 3"){select_variables=policy3_variables} 
    else if (input$policy == "Policy 4"){select_variables=policy4_variables} 
    
    reactive_data() %>% 
      filter(country_name==input$country) %>% 
      select(any_of(select_variables)) %>% 
      rename_at(vars(starts_with("policy")), funs(paste0("policy")))
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
      geom_vline(xintercept = 0.07,col="red")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y =element_blank())+
      scale_x_continuous(name="Disability adjusted life years per capita", limits = range_daly)
  })
  
  climate_plot <- reactive({
    ggplot(filtered_data(), aes(x=ssp585_2050, y=slider_value))+
      geom_point(size=5)+
      ylim(c(-2,2))+
      theme_classic()+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 50,col="red")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y =element_blank())+
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
           "Policy 1" = SEV_omega_plot(),
           "Policy 2" = red_meat_plot(),
           "Policy 3" = ruminant_meat_plot(),
           "Policy 4" = climate_plot()
    )
  })  
  
  #************  
  secondplot_Input <- reactive({
    switch(input$policy,
           "Policy 1" = SEV_B12_plot(),
           "Policy 2" = daly_plot(),
           "Policy 3" = prodkg_plot(),
           "Policy 4" = jobs_plot()
    )
  })   
  
  #************  
  thirdplot_Input <- reactive({
    switch(input$policy,
           "Policy 1" = prodkg_plot(),
           "Policy 2" = prodkg_plot(),
           "Policy 3" = import_plot(),
           "Policy 4" = export_plot()
    )
  })  
  
  #************  
  fourthplot_Input <- reactive({
    switch(input$policy,
           "Policy 1" = import_plot(),
           "Policy 2" = import_plot(),
           "Policy 4" = reliance_plot()
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
    
    if(input$policy == "Policy 1"){message="Policy recommedation: Improving nutrition"}
    else if (input$policy == "Policy 2"){message="Policy recommendation: Reducing the burden of cardiovascular disease"}
    else if (input$policy == "Policy 3") {message="Policy recommendation: Reducing environmental footprints of food consumption and production "}
    else if (input$policy == "Policy 4") {message="Policy recommendation: Safeguard food system contributions to climate change"}
    
    
    p(message,style="height:100px;
                    padding:25px;
                    background-color:papayawhip;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")
    
    
  })
  
  #***********************      
  output$country_info <- renderUI({
    
    text<-paste(filtered_data()$country_name,"has a status of",filtered_data()$policy)
    text2<-"We are sorry, but the country you have selected has too much missing data to be able to give a recommendation for this policy."
    text3<-"Variables that are not plotted (lack a circle marker in the plots) are required to provide a recommendation."
    
    if(filtered_data()$policy=="highly_relevant"){p(text,style="height:100px;
                    padding:25px;
                    background-color:#4D74D3;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="relevant"){p(text,style="height:100px;
                    padding:25px;
                    background-color:#82A3FC;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="less_applicable"){p(text,style="height:100px;
                    padding:25px;
                    background-color:#E5ECFF;
                    border-left:8px solid teal;
                    border-top: 1px solid black;
                    border-right:1px solid black;
                    border-bottom: 1px solid black;
                    color:black;
                    text-align:center")}
    
    else if(filtered_data()$policy=="missing_data"){p(text,br(),br(),text2,br(),br(),text3,
                                                      style="height:170px;
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
  #output$firstplot_description <- renderUI({
  
  
  #if(input$policy == "Policy 1"){message="There is missing data in this variable"}
  #else if (input$policy == "Policy 2"){message="This is a message"}
  #else if (input$policy == "Policy 3") {message="This is a message"}
  #else if (input$policy == "Policy 4") {message="This is a message"}
  #else if (input$policy == "Policy 5") {message="This is a message"}
  
  # p(message,style="height:80px;
  #                padding:15px;
  #                background-color:gainsboro;
  #                border-left:1px solid black;
  #                border-top: 1px solid black;
  #                border-right:1px solid black;
  #                border-bottom: 1px solid black;
  #                color:black;
  #                text-align:center")
  
  
  #})  
  
}

shinyApp(server = server, ui = ui)