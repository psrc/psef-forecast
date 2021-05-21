
# Packages ----------------------------------------------------------------

# Packages for Dashboard
library(shiny)
library(shinydashboard)

# Packages for Data Wrangling
library(tidyverse)
library(readxl)
library(tidyr)
library(lubridate)

# Packages for Chart Creation
library(ggplot2)
library(scales)
library(plotly)

# Packages for Maps
library(sf)
library(leaflet)

psrc.colors <- list("Employment (thous.)" = "#8CC63E",
                    "Unemployment rate (%)" = "#00A7A0",
                    "Average apartment rent ($)" = "#C0E095",
                    "Average home price (thous. $)" = "#F05A28",
                    "Housing permits (thous.)" = "#F7A489",
                    "Single-family" = "#E2F1CF",
                    "Multi-family" = "#8CC63E",
                    "Other taxable sales" = "#FBD6C9",
                    "Retail trade" = "#F05A28",
                    "Taxable retail sales (bils. $)" = "#8CC63E")

pre.covid.peak <- yq("2020:Q1")
current.quarter <- yq("2020:Q4")

pre.gr.peak <- yq("2008:Q1")

pre.dc.peak <- yq("2000:Q4")

# Shiny Server Working Directory
wrkdir <- "/home/shiny/apps/psef-forecast/data"

# Functions ---------------------------------------------------------------

create.bar.charts <- function(d, metric, s.yr, lbls, suff="", fact=1, deci = 0, w.pre="") {
    
    t <- d %>%
        filter(Metric %in% metric, Year >= s.yr) %>%
        mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>%
        select(Metric,Estimate,DataDate)
    
    g <-  ggplotly(ggplot(data = t,
                      aes(x = `DataDate`, 
                          y = Estimate, 
                          fill = Metric,
                          text = paste0("Q",quarter(DataDate)," of ",year(DataDate),": ",w.pre, prettyNum(round(Estimate*fact, deci), big.mark = ","),suff))) +
                   geom_col(
                       alpha = 1.0,
                       position='stack') +
                   labs(x = NULL, y = NULL) +
                   scale_fill_manual(values = psrc.colors) +
                   scale_color_manual(values = psrc.colors) +
                   scale_y_continuous(labels = lbls) +
                   theme(plot.title = element_text(size = 10, face = 'bold'),
                         axis.line = element_blank(),
                         panel.background = element_blank(),
                         panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         legend.position = "none",
                         legend.title = element_blank()),
               tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25), hovermode = "x")
 
    return(g)   
}

# Population and Employment Data -----------------------------------------------------
col.thousands <- c("Employment (thous.)", "Goods producing", "Natural resources", "Construction", "Manufacturing", "Aerospace",
                   "Other durable goods", "Nondurable goods", "Services producing", "Wholesale and retail trade", "Transportation and public utilities",
                   "Information", "Financial activities", "Professional and business services", "Other services", "Government", "State and local", "Federal",
                   "Housing permits (thous.)", "Population (thous.)", "Net migration (thous.)")

col.shares <- c("Unemployment rate (%)", "Employment", "Personal income (cur. $)", "Consumer price index", "Housing permits", "Population")

col.billions <- c("Personal income (bils. $12)","Personal income (bils. $)","Wage and salary disbursements", "Other income")

col.asis <- c("Per capita personal income ($)", "Consumer price index (82-84=1.000)")

region.forecast.data  <- read_excel(file.path(wrkdir,"Quarterly-Forecast-CV03-2021.xls"), sheet="Region", skip=9) %>%
    drop_na() %>%
    rename(Metric=1) %>%
    select(-2) %>%
    pivot_longer(!Metric, names_to = "quarter") %>%
    separate(quarter, c("Year","Quarter"), "\\.") %>%
    mutate(Quarter = gsub("0999999999999", "1", Quarter), Quarter = gsub("4000000000001", "4", Quarter)) %>%
    mutate(across(c('Year','Quarter'), as.numeric)) %>%
    mutate(Estimate = case_when(
        Metric %in% col.thousands ~ value*1000,
        Metric %in% col.shares ~ value/100,
        Metric %in% col.billions ~ value*1000000000,
        Metric %in% col.asis ~ value)) %>%
    select(-value)

pre.covid.jobs <- region.forecast.data %>% filter(Metric == "Employment (thous.)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==pre.covid.peak) %>% select(Estimate) %>% pull() %>% round(-1)

current.jobs <- region.forecast.data %>% filter(Metric == "Employment (thous.)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==current.quarter) %>% select(Estimate) %>% pull() %>% round(-1)

covid.return.to.pk.jobs <- region.forecast.data %>% filter(Metric == "Employment (thous.)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>%
    filter(DataDate>=current.quarter & Estimate >= pre.covid.jobs) %>% slice(1:1) %>% select(DataDate) %>% pull()

covid.yrs.to.recover <- round(int_length(interval(pre.covid.peak, covid.return.to.pk.jobs)) / 31536000,1)

pre.gr.jobs <- region.forecast.data %>% filter(Metric == "Employment (thous.)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==pre.gr.peak) %>% select(Estimate) %>% pull() %>% round(-1)

gr.return.to.pk.jobs <- region.forecast.data %>% filter(Metric == "Employment (thous.)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>%
    filter(DataDate>=pre.gr.peak & Estimate >= pre.gr.jobs) %>% slice(1:1) %>% select(DataDate) %>% pull()

gr.yrs.to.recover <- round(int_length(interval(pre.gr.peak, gr.return.to.pk.jobs)) / 31536000,1)

pre.dc.jobs <- region.forecast.data %>% filter(Metric == "Employment (thous.)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==pre.dc.peak) %>% select(Estimate) %>% pull() %>% round(-1)

dc.return.to.pk.jobs <- region.forecast.data %>% filter(Metric == "Employment (thous.)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>%
    filter(DataDate>=pre.dc.peak & Estimate >= pre.dc.jobs) %>% slice(1:1) %>% select(DataDate) %>% pull()

dc.yrs.to.recover <- round(int_length(interval(pre.dc.peak, dc.return.to.pk.jobs)) / 31536000,1)

# Housing Data -----------------------------------------------------
col.thousands <- c("Housing permits (thous.)", "Single-family", "Multi-family", "Average home price (thous. $)", "Active home listings (thous.)", "Home sales (thous.)")
col.shares <- c("Apartment vacancy rate (%)")
col.asis <- c("Average apartment rent ($)")

region.housing.data  <- read_excel(file.path(wrkdir,"Quarterly-Forecast-CV03-2021.xls"), sheet="Housing", skip=9) %>%
    drop_na() %>%
    rename(Metric=1) %>%
    slice(-(4:6)) %>%
    slice(-(9:11)) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(!Metric, names_to = "quarter") %>%
    separate(quarter, c("Year","Quarter"), "\\.") %>%
    mutate(Quarter = gsub("0999999999999", "1", Quarter), Quarter = gsub("4000000000001", "4", Quarter), value = gsub("NA", "0", value)) %>%
    mutate(across(c('Year','Quarter','value'), as.numeric)) %>%
    mutate(Estimate = case_when(
        Metric %in% col.thousands ~ value*1000,
        Metric %in% col.shares ~ value/100,
        Metric %in% col.asis ~ value)) %>%
    select(-value)

# Retail Sales Data --------------------------------------------------------
col.thousands <- c("Housing permits (thous.)", "Single-family", "Multi-family", "Average home price (thous. $)", "Active home listings (thous.)", "Home sales (thous.)")
col.shares <- c("Apartment vacancy rate (%)")
col.asis <- c("Average apartment rent ($)")

region.sales.data  <- read_excel(file.path(wrkdir,"Quarterly-Forecast-CV03-2021.xls"), sheet="Retail Sales", skip=9) %>%
    drop_na() %>%
    rename(Metric=1) %>%
    slice(-(14:15)) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(!Metric, names_to = "quarter") %>%
    separate(quarter, c("Year","Quarter"), "\\.") %>%
    mutate(Quarter = gsub("0999999999999", "1", Quarter), Quarter = gsub("4000000000001", "4", Quarter), value = gsub("NA", "0", value)) %>%
    mutate(across(c('Year','Quarter','value'), as.numeric)) %>%
    mutate(Estimate = value*1000000000) %>%
    select(-value)

pre.covid.sales <- region.sales.data %>% filter(Metric == "Taxable retail sales (bils. $)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==pre.covid.peak) %>% select(Estimate) %>% pull() %>% round(-1)

current.sales <- region.sales.data %>% filter(Metric == "Taxable retail sales (bils. $)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==current.quarter) %>% select(Estimate) %>% pull() %>% round(-1)

covid.return.to.pk.sales <- region.sales.data %>% filter(Metric == "Taxable retail sales (bils. $)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>%
    filter(DataDate>pre.covid.peak & Estimate >= pre.covid.sales) %>% slice(1:1) %>% select(DataDate) %>% pull()

covid.yrs.to.recover.sales <- round(int_length(interval(pre.covid.peak, covid.return.to.pk.sales)) / 31536000,1)

pre.covid.building.materials <- region.sales.data %>% filter(Metric == "Building materials") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==pre.covid.peak) %>% select(Estimate) %>% pull() %>% round(-1)

current.sales.building.materials <- region.sales.data %>% filter(Metric == "Building materials") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==current.quarter) %>% select(Estimate) %>% pull() %>% round(-1)

pre.covid.furn.elect <- region.sales.data %>% filter(Metric == "Furniture and electronics") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==pre.covid.peak) %>% select(Estimate) %>% pull() %>% round(-1)

current.sales.furn.elect <- region.sales.data %>% filter(Metric == "Furniture and electronics") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==current.quarter) %>% select(Estimate) %>% pull() %>% round(-1)

pre.covid.food.drink <- region.sales.data %>% filter(Metric == "Food services and drinking") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==pre.covid.peak) %>% select(Estimate) %>% pull() %>% round(-1)

current.sales.food.drink <- region.sales.data %>% filter(Metric == "Food services and drinking") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==current.quarter) %>% select(Estimate) %>% pull() %>% round(-1)

pre.covid.clothes <- region.sales.data %>% filter(Metric == "Clothing and accessories") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==pre.covid.peak) %>% select(Estimate) %>% pull() %>% round(-1)

current.sales.clothes <- region.sales.data %>% filter(Metric == "Clothing and accessories") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==current.quarter) %>% select(Estimate) %>% pull() %>% round(-1)

pre.covid.gas <- region.sales.data %>% filter(Metric == "Gasoline stations") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==pre.covid.peak) %>% select(Estimate) %>% pull() %>% round(-1)

current.sales.gas <- region.sales.data %>% filter(Metric == "Gasoline stations") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==current.quarter) %>% select(Estimate) %>% pull() %>% round(-1)

pre.covid.generalmerch <- region.sales.data %>% filter(Metric == "General merchandise") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==pre.covid.peak) %>% select(Estimate) %>% pull() %>% round(-1)

current.sales.generalmerch <- region.sales.data %>% filter(Metric == "General merchandise") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
    filter(DataDate==current.quarter) %>% select(Estimate) %>% pull() %>% round(-1)

# User Interface for Dashboard --------------------------------------------
ui <- dashboardPage(skin = "purple", title = "Economic Forecast Data",
                    dashboardHeader(title = "March 2021 PSEF Forecast",
                                    titleWidth = '20%'),
                    
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
                            menuItem("Employment", tabName = "jobs", icon = icon("briefcase")),
                            menuItem("Housing", tabName = "housing", icon = icon("house-user")),
                            menuItem("Retail Sales", tabName = "sale", icon = icon("shopping-cart"))
                        )
                    ),
                    
                    dashboardBody(
                        tabItems(
                            tabItem(tabName = "dashboard",
                                    
                                    h2("COVID-19 Economic Impacts"),
                                    hr(),
                                    
                                    fluidRow(
                                        infoBoxOutput("preempBox"),
                                        infoBoxOutput("nowempBox"),
                                        infoBoxOutput("retempBox")
                                    ),
                                    hr(),
                                    
                                    h2("Great Recession Economic Impacts"),
                                    hr(),
                                    
                                    fluidRow(
                                        infoBoxOutput("pregrBox"),
                                        infoBoxOutput("retgrBox")
                                    ),
                                    hr(),
                                    
                                    h2("Dot-Com Economic Impacts"),
                                    hr(),
                                    
                                    fluidRow(
                                        infoBoxOutput("predcBox"),
                                        infoBoxOutput("retdcBox")
                                    ),
                                    hr(),
                                    
                            ), # end of overview panel
                            
                            tabItem(tabName = "jobs",
                                    
                                    box(title = "Regional Employment by Quarter", solidHeader = TRUE, status = "success",
                                        column(width = 12, plotlyOutput("jobs.chart"))),
                                    box(title = "Regional Unemployment Rate by Quarter", solidHeader = TRUE, status = "primary",
                                        column(width = 12, plotlyOutput("unemployment.chart")))
                                    
                            ), # end of Jobs Panel
                            
                            tabItem(tabName = "housing",
                                    
                                    fluidRow(
                                    
                                        box(title = "Average Home Price by Quarter", solidHeader = TRUE, status = "danger", plotlyOutput("homecost.chart")),
                                        box(title = "Average Monthly Rent by Quarter", solidHeader = TRUE, status = "success", plotlyOutput("rentalcost.chart"))),
                                    
                                    fluidRow(
                                        box(title = "Housing Permits by Quarter", solidHeader = TRUE, status = "warning", plotlyOutput("allpermits.chart")),
                                        box(title = "Housing Permit Types by Quarter", solidHeader = TRUE, status = "success", plotlyOutput("typpermits.chart")))
                                    
                            ), # end of Housing Panel
                            
                            
                            tabItem(tabName = "sale",
                                    
                                    h2("COVID-19 Economic Impacts on Taxable Retail Sales"),
                                    hr(),
                                    
                                    fluidRow(
                                        infoBoxOutput("presaleBox"),
                                        infoBoxOutput("nowsaleBox"),
                                        infoBoxOutput("retsaleBox"),
                                        infoBoxOutput("buildmatBox"),
                                        infoBoxOutput("furnelecBox"),
                                        infoBoxOutput("fooddrinkBox"),
                                        infoBoxOutput("clothesBox"),
                                        infoBoxOutput("gasBox"),
                                        infoBoxOutput("genmerchBox")
                                    ),
                                    hr(),
                                    
                                    box(title = "Regional Total Taxable Sales by Quarter", solidHeader = TRUE, status = "success", plotlyOutput("allsales.chart")),
                                    box(title = "Regional Taxable Sales by Type by Quarter", solidHeader = TRUE, status = "warning", plotlyOutput("taxsales.chart")),
                                    
                                    hr(),
                                    
                                    fluidRow(),
                                    
                            ) # end of Jobs Panel
                            
                        ) # end of tab items for main body
                        
                    ) #end of dashboard body
)


# Server Logic ------------------------------------------------------------
server <- function(input, output) {
    
    output$preempBox <- renderInfoBox({
        
        infoBox(
            "Pre-Covid Jobs:", HTML(paste0(prettyNum(pre.covid.jobs, big.mark = ","), br(), "(Q", quarter(pre.covid.peak)," of ",year(pre.covid.peak),")")), icon = icon("briefcase"),
            color = "blue"
        )
    })
    
    output$nowempBox <- renderInfoBox({
        
        infoBox(
            "Current Jobs:", HTML(paste0(prettyNum(current.jobs, big.mark = ","), br(), "(Q", quarter(current.quarter)," of ",year(current.quarter), ")")), icon = icon("briefcase"),
            color = "orange"
        )
    })
    
    output$retempBox <- renderInfoBox({
        
        infoBox(
            "Estimated Recovery Date:", HTML(paste0("Q",quarter(covid.return.to.pk.jobs)," of ", year(covid.return.to.pk.jobs),br()," (",covid.yrs.to.recover, " years to recover)")), icon = icon("briefcase"),
            color = "purple"
        )
    })
    
    output$pregrBox <- renderInfoBox({
        
        infoBox(
            "Pre-Great Recession Jobs:", HTML(paste0(prettyNum(pre.gr.jobs, big.mark = ","), br(), "(Q", quarter(pre.gr.peak)," of ", year(pre.gr.peak),")")), icon = icon("briefcase"),
            color = "blue"
        )
    })
    
    output$retgrBox <- renderInfoBox({
        
        infoBox(
            "Recovery Date:", HTML(paste0("Q",quarter(gr.return.to.pk.jobs)," of ", year(gr.return.to.pk.jobs),br()," (",gr.yrs.to.recover, " years to recover)")), icon = icon("briefcase"),
            color = "purple"
        )
    })

    output$predcBox <- renderInfoBox({
        
        infoBox(
            "Pre-DotCom Recession Jobs:", HTML(paste0(prettyNum(pre.dc.jobs, big.mark = ","), br(), "(Q", quarter(pre.dc.peak)," of ", year(pre.dc.peak),")")), icon = icon("briefcase"),
            color = "blue"
        )
    })
    
    output$retdcBox <- renderInfoBox({
        
        infoBox(
            "Recovery Date:", HTML(paste0("Q",quarter(dc.return.to.pk.jobs)," of ", year(dc.return.to.pk.jobs),br()," (",dc.yrs.to.recover, " years to recover)")), icon = icon("briefcase"),
            color = "purple"
        )
    })
    
    
    output$presaleBox <- renderInfoBox({
        
        infoBox(
            "Pre-Covid Taxable Sales:", HTML(paste0("$",prettyNum(pre.covid.sales, big.mark = ","), br(), "(Q", quarter(pre.covid.peak)," of ",year(pre.covid.peak),")")), icon = icon("shopping-cart"),
            color = "blue"
        )
    })
    
    output$nowsaleBox <- renderInfoBox({
        
        infoBox(
            "Current Taxable Sales:", HTML(paste0("$",prettyNum(current.sales, big.mark = ","), br(), "(Q", quarter(current.quarter)," of ",year(current.quarter), ")")), icon = icon("shopping-cart"),
            color = "orange"
        )
    })
    
    output$retsaleBox <- renderInfoBox({
        
        infoBox(
            "Estimated Recovery Date:", HTML(paste0("Q",quarter(covid.return.to.pk.sales)," of ", year(covid.return.to.pk.sales),br()," (",covid.yrs.to.recover.sales, " years to recover)")), icon = icon("shopping-cart"),
            color = "purple"
        )
    })
    
    
    output$buildmatBox <- renderInfoBox({
        
        infoBox(
            "Building Materials Sales:", HTML(paste0("$",prettyNum(current.sales.building.materials, big.mark = ","), br(), "(",round((current.sales.building.materials / pre.covid.building.materials)*100,0), "% of pre-covid sales)")), icon = icon("tools"),
            color = "purple"
        )
    })

    output$furnelecBox <- renderInfoBox({
        
        infoBox(
            "Furniture/Electronic Sales:", HTML(paste0("$",prettyNum(current.sales.furn.elect, big.mark = ","), br(), "(",round((current.sales.furn.elect / pre.covid.furn.elect)*100,0), "% of pre-covid sales)")), icon = icon("couch"),
            color = "olive"
        )
    })

    output$fooddrinkBox <- renderInfoBox({
        
        infoBox(
            "Food & Drink Sales:", HTML(paste0("$",prettyNum(current.sales.food.drink, big.mark = ","), br(), "(",round((current.sales.food.drink / pre.covid.food.drink)*100,0), "% of pre-covid sales)")), icon = icon("cocktail"),
            color = "maroon"
        )
    })
    
    output$clothesBox <- renderInfoBox({
        
        infoBox(
            "Clothing & Accessories Sales:", HTML(paste0("$",prettyNum(current.sales.clothes, big.mark = ","), br(), "(",round((current.sales.clothes / pre.covid.clothes)*100,0), "% of pre-covid sales)")), icon = icon("tshirt"),
            color = "aqua"
        )
    })

    output$gasBox <- renderInfoBox({
        
        infoBox(
            "Gasoline Sales:", HTML(paste0("$",prettyNum(current.sales.gas, big.mark = ","), br(), "(",round((current.sales.gas / pre.covid.gas)*100,0), "% of pre-covid sales)")), icon = icon("gas-pump"),
            color = "light-blue"
        )
    })

    output$genmerchBox <- renderInfoBox({
        
        infoBox(
            "General Merchandise Sales:", HTML(paste0("$",prettyNum(current.sales.generalmerch, big.mark = ","), br(), "(",round((current.sales.generalmerch / pre.covid.generalmerch)*100,0), "% of pre-covid sales)")), icon = icon("cart-arrow-down"),
            color = "green"
        )
    })
    
    
    output$jobs.chart <- renderPlotly({create.bar.charts(d=region.forecast.data, metric=c("Employment (thous.)"), s.yr=2000, lbls=scales::comma)})
    output$unemployment.chart <- renderPlotly({create.bar.charts(d=region.forecast.data, metric=c("Unemployment rate (%)"), s.yr=2000, lbls=scales::percent, suff="%", fact=100, deci=1)})
    output$rentalcost.chart <- renderPlotly({create.bar.charts(d=region.housing.data, metric=c("Average apartment rent ($)"), s.yr=2000, lbls=scales::dollar, w.pre="$")})
    output$homecost.chart <- renderPlotly({create.bar.charts(d=region.housing.data, metric=c("Average home price (thous. $)"), s.yr=2000, lbls=scales::dollar, w.pre="$")})
    output$allpermits.chart <- renderPlotly({create.bar.charts(d=region.housing.data, metric=c("Housing permits (thous.)"), s.yr=2000, lbls=scales::comma)})
    output$typpermits.chart <- renderPlotly({create.bar.charts(d=region.housing.data, metric=c("Single-family", "Multi-family"), s.yr=2000, lbls=scales::comma)})
    output$allsales.chart <- renderPlotly({create.bar.charts(d=region.sales.data, metric=c("Taxable retail sales (bils. $)"), s.yr=2000, lbls=scales::dollar, w.pre="$")})
    output$taxsales.chart <- renderPlotly({create.bar.charts(d=region.sales.data, metric=c("Retail trade", "Other taxable sales"), s.yr=2000, lbls=scales::dollar, w.pre="$")})

    
}

# Run the application 
shinyApp(ui = ui, server = server)
