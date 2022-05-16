#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#import packages
#install the packages if you haven't, using -> install.packages("__the package name_")
library(readr)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggrepel)
library(ggpubr)
library(gridExtra)
library(maps)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
options(warn=-1)


#read csv files

#   If there is a problem with importing csv files, do:
#   set and get the working directory
#   setwd("___path of this R script in your file explorer___")
#   getwd()

quarter_revenue <- read.csv(file="Apple Revenue by Quarter.csv",header=TRUE) #fields: year, quarters, revenue
region_revenue <- read.csv(file="Apple Revenue by Region.csv",header=TRUE) #fields: region, year, revenue
product_revenue_sales <- read.csv(file="Apple Revenue Sales by Product.csv",header=TRUE) #fields: product, year, revenue, sales
iphone_price <- read.csv(file="Apple iPhone Price Evolution.csv",header=TRUE) #fields: model, year, start price USD, end price USD, start price MYR, end price MYR
global_market <- read.csv(file="Apple Global Market Share 2021.csv",header=TRUE) #fields: brands, date, share
apple_stores <- read.csv(file="Apple Stores Worldwide.csv",header=TRUE) #fields: region, number of stores


#define the color palettes for plotting 
col_set1<-c ("#854D27","#DD7230","#E99E47","#F4C95D","#DECED6","#896f95","#89a25b","#699Fa1","#75C8AE","#a5d6d9","#E7E393")
col_set2<-c("#F4A127","#E5771E", "#FFECB4", "#5A3D2B","#75C8AE") #brown, orange, retro neon green
col_set3<- c("#699Fa1","#a5d6d9","#c3e5e7","#df7027","#dd8627") #3 blue and 2 orange
background <- c("#F2F1EF") #color for background panel of plots


# Define UI for application
ui <- fluidPage(
  

    ################# HTML Styling ##################
    
    setBackgroundColor("whiteghost"),
    
    #set colors of tabs
    tags$style(HTML("
    .tabbable > .nav > li > a[data-value='Revenue by Year'] {background-color: #854D27;   color:white;}
    .tabbable > .nav > li > a[data-value='Revenue by Region'] {background-color: #854D27;  color:white}
    .tabbable > .nav > li > a[data-value='Revenue by Product'] {background-color: #854D27; color:white}
    .tabbable > .nav > li > a[data-value='iPhone Price Evolution'] {background-color: #854D27; color:white}
    .tabbable > .nav > li > a[data-value='Global Smartphone Market Share 2021'] {background-color: #854D27; color:white}
    .tabbable > .nav > li[class=active]    > a {background-color: darkgray; color:white}
    ")),
    
    tags$style("
    
    #title {
      font-size:50px;
      font-family: San Francisco;
      font-weight: bold;
      text-align: center;
    }
    
    h3,h4 {
      margin-left: 20px;
      margin-top: 20px;
      font-weight: bold;
      font-family: San Francisco;
    }
    
    p{
      font-size: 18px;
      font-family: San Francisco;
    }
    
    #Year_Revenue_By_Region_Title,
    #Monthly_Global_Market_Title{
      font-size: 25px;
      font-family: San Francisco;
      font-weight: bold;
      margin-left:20px; 
      margin-top: 20px;
    }
    
    #note_guide_year,
    #note_guide_region,
    #note_guide_product,
    #note_guide_iphone_price,
    #note_guide_global_market{
      font-size:15px;
      font-style: italic;
      font-weight: bold;
    }
    
    #year_quarter,#quarterbar_type,
    #year_piechart,#product_category,
    #global_market_month,#price_currency{
      font-size:18px;
    }
    
    #region_bartype{
      margin-left:30px;
      font-size:18px;
    }
    
    #most, #total{
      padding-top:30px;
      padding-bottom: 30px;
      border: 5px solid #854D27;
      background-color:#F2F1EF;
      text-align:center;
      font-size:   18px;
      font-weight: bold;
      width: 350px;
      border-radius: 25px;
    }   
    "),
    
    
    br(), #add space
  
    #Title
    titlePanel(title=div(id="title","Apple Statistics",
                         img(src="AppleLogo.png",height=80,width=80))),
    
    hr(), #add line
    br(),
    
    
    ##################### Main Tabs ####################
    tabsetPanel( 
      
      ###### TAB 1: UI Revenue by Year ######
      tabPanel("Revenue by Year",
               br(),
               tabsetPanel(
                 
                 tabPanel("Plot",
                          br(),
                          
                          sidebarPanel(
                            width=3,
                            tags$div(style="margin-top:10px;"),
                            
                            #display Yearly/Quarterly options for users to choose for the revenue plot
                            radioButtons(inputId = "year_quarter", label = "Select Graph Metrics", choices = c("Year", "Quarter","Merged"),
                                         selected="Year",inline=TRUE),
                            
                            tags$div(style="margin-top:20px;"),
                            
                            #if Quarterly plot is selected, this panel will be displayed to let user choose grouped or stacked bar chart
                            conditionalPanel(condition="input.year_quarter=='Quarter'",
                                             radioButtons(inputId="quarterbar_type",label="Select a type", choices=c("Stacked","Grouped"),
                                                          selected="Stacked")),
                            
                          ), 
                          
                          #plot year/quarter revenue graph
                          mainPanel(width=9,
                                    plotlyOutput("Year_Quarter_Plot"),
                                    br(),
                                    textOutput("note_guide_year"),
                                    #hr(),
                                    br()
                                    
                          )
                          
                        ),
                 
                 tabPanel("Summary",
                          br(),
                          fluidRow(
                            column(1),
                            column(10,
                                   h3("Key Statistics"), 
                                   br(),
                                   
                                   p("- As observed, Apple revenue experienced a significant increase starting year 2010."), 
                                   p("- From year 2010 to 2012, its revenue has risen from $65billion USD to $156.3billion USD."),
                                   p("- In year 2016, its revenue dropped from $233.6billion USD to $215.4billion USD."),
                                   p("- Its best performance was last year - 2021 - where it had obtained a revenue of $365.6billion USD."),
                                   p("- Based on the quarterly graph, Apple's performance in terms of revenue is often better during the first quarters."),
                                   br()),
                            column(1)
                          )
                        )
               )
               
          ),
      
      ###### TAB 2: UI Revenue by Region ######
      tabPanel("Revenue by Region",
               br(),
               tabsetPanel(
                 
                 tabPanel("Plot",
                          
                          fluidRow(
                            #plot a bar chart for Apple Revenue by Region
                            column(8,
                                   h3("Apple Revenue from 2015 to 2021 by Region"),
                                   plotlyOutput("Region_BarPlot"),
                                   br(),
                                   #display options to user for viewing a stacked or a grouped bar chart
                                   radioButtons(inputId = "region_bartype", label = "Select a type", choices = c("Stacked", "Grouped"),
                                                selected="Stacked",inline=TRUE),
                                   br(),
                                   textOutput("note_guide_region"),
                                   br()
                            ),
                            #plot a pie chart for the revenue contribution of each region for a selected year
                            column(4,
                                   h4("To view the revenue distribution for a particular year"),
                                   
                                   #display year options to users for selecting a particular year for the pie chart
                                   wellPanel(selectInput(inputId="year_piechart",label="Select a year",
                                                         choices=c("2015","2016","2017","2018","2019","2020","2021"),
                                                         selected="2015"),
                                             textOutput("Year_Revenue_By_Region_Title"), 
                                             plotlyOutput("Region_PieChart")) 
                            )
                         ),
                         hr(),br(),
                         fluidRow(
                           #plot a world map for Apple Store locations
                           column(8,
                                  h3("Apple Stores across the World"),
                                  plotOutput("Apple_Stores_World"),
                                  br()
                           ),
                           #display total number of Apple Stores and regions with the most Apple Stores
                           column(4,
                                  br(),br(),
                                  h4("Total number of Apple Stores"),
                                  div(id="total",textOutput("total_stores")),
                                  br(),br(),
                                  h4("Region with the most Apple Stores"),
                                  div(id="most",textOutput("most_stores"))
                           )
                         )),

                 tabPanel("Summary",
                          br(),
                          fluidRow(
                            column(1),
                            column(10,
                                   h3("Key Statistics"), 
                                   br(),
                                   
                                   h4("Revenue by Region"),
                                   p("- Overall, Apple’s revenues have increased in each region over the years."),
                                   p("- But in 2018, there was a significant drop in revenue in the Asia Pacific from $44.1billion to $17.4billion."),
                                   p("- Based on the plots, Americas has contributed the most to Apple's revenue from 2015 to 2021, with 40% and above."),
                                   p("- Its peak contribution was in year 2020, responsible for 45.4% of Apple's revenue, which is equivalent to $124.5billion USD."),
                                   p("- In 2021, its revenue contribution was 41.9%, which is equivalent to $153.3billion USD."), 
                                   p("- Whereas, Japan is amongst the least with an overall contribution of around 7% to 8%."),
                                   
                                   br(),
                                   hr(),
                                   
                                   h4("Apple Stores across the World"),
                                   p("- As observed, Apple's stores are mainly located at North/South Americas, Europe, Australia, and Asia."),
                                   p("- The Americas is home to over 200 Apple stores, which makes it the Top 1 region with the most Apple stores."),
                                   p("- Such a fact also explains why Americas is also the main revenue generator for Apple in terms of region."),
                                   br()
                                   
                            ),
                            column(1)
                          ))
               
               
               )
      ),
      
      ###### TAB 3: UI Revenue by Product ######
      tabPanel("Revenue by Product",
               br(),
               tabsetPanel(
                 
                 tabPanel("Plot",
                          h3("Sales and Revenue"),
                          br(),
                          
                          #plot a bar-line plot for Sales and Revenue of each Apple product by category
                          fluidRow(
                            column(1),
                            column(10,
                                   plotlyOutput("Produce_RevenueSales")),
                            column(1)
                          ),
                          
                          #display product category options for user to choose
                          fluidRow(
                            column(3),
                            column(6,
                                   br(),
                                   wellPanel(radioButtons(inputId = "product_category", label = "Select a Product Category", choices = c("iPhone", "iPad","Mac","Wearables & Home"),
                                                          selected="iPhone",inline=TRUE))),
                            column(3)
                            
                          ),
                          
                          #plot an area graph to observe the overall revenue contribution of each product category
                          fluidRow(
                            h3("Overview of Apple's Revenue Sources by Products"),br(),
                            column(7,
                                   plotlyOutput("Product_AreaGraph"),
                                   br()),
                            column(5,
                                   br(),
                                   p("The area graph shows the contribution of each Apple product to Apple's Revenue over the years."),
                                   p("The year range is selected for the reason that 2015 is when each category has had at least 1 product launched."),
                                   textOutput("note_guide_product"))
                          )),
                 
                 tabPanel("Summary",
                          br(),
                          fluidRow(
                            column(1),
                            column(10,
                                   h3("Key Statistics"), 
                                   br(),
                                   
                                   h4("Sales and Revenue Performance of each Product"),
                                   br(),
                                   h5("iPhone"),
                                   p("- iPhone experienced a significant growth from year 2007 to 2015, but the rate has slowed down since."),
                                   p("- iPhone sales and revenue underwent a drop for the period from year 2015 to 2017, and from year 2018 to 2020."),
                                   p("- In 2021, iPhone generated $191.9billion USD for Apple with 242 million units sold."),
                                   br(),
                                   
                                   h5("iPad"),
                                   p("- There was an increase of sales for iPad from year 2011 to 2013. However, its performance underwent a trough between year 2013 and 2021."),
                                   p("- In 2021, iPad generated $31.8billion USD for Apple with 57.8 million units sold."),
                                   br(),
                                   
                                   h5("Mac"),
                                   p("- The sales of Mac have been steady, despite a slight drop in year 2013, 2016, and 2019."),
                                   p("- In 2021, Mac generated $35.1billion USD for Apple with 25.7 million units sold."),
                                   br(),
                                   
                                   h5("Wearables & Home"),
                                   p("- Apple's Wearables and Home have experienced a significant growth of sales from year 2017 to 2020, which is from 50.2 million to 195.2 million units."),
                                   p("- In 2021, Wearables and Home generated $38.3billion USD for Apple with 175.8 million units sold."),
                                   br(),
                                   
                                   hr(),

                                   h4("Apple's Revenue Sources Overview"),
                                   br(),
                                   
                                   p("- As observed, iPhone has been the main revenue generator for Apple from year 2015 to 2021, with more than 60% of contribution."),
                                   p("- iPhone's best contribution was in year 2018, where it was responsible for 73.1% of the revenue."),
                                   p("- In 2021, iPhone has contributed 64.6% of Apple's Revenue."),
                                   p("- The contributions of iPad and Mac have been consistent, contributing around 8%-10% and 11%-13% of the revenue, respectively."),
                                   p("- Wearables and Home products are catching up. As observed, its revenue contribution increased significantly from 7.6% in 2018 to 12.9% in 2021."),
                                   
                                   br(),
                                   br()
                            ),
                            column(1)
                          )
                        )
                )
              ),
      
      ###### TAB 4: UI iPhone Price Evolution and Sales######
      tabPanel("iPhone Price Evolution",
               br(),
               
               tabsetPanel(
                 tabPanel("Plot",
                          h3("iPhone Price Evolution from 2007 to 2021"),
                          
                          fluidRow(
                            column(1),
                            column(8,
                            #display currency options to users
                            radioButtons(inputId = "price_currency", label="",choices = c("$USD (US Dollar)", "MYR (Malaysia Ringgit)"),
                                                selected="$USD (US Dollar)",inline=TRUE)),
                            column(4)),
                          
                          #plot a point range graph to display the price range of each iPhone based on the selected currency
                          fluidRow(
                            column(1),
                            column(10,plotlyOutput("iPhone_Price_RangePlot")),
                            column(1)
                            ),
                          
                          br(),
                          
                          fluidRow(
                            column(1),
                            column(10,
                                   textOutput("note_guide_iphone_price"),
                                   br(), br(),
                                   p("The first iPhone that was made available in Malaysia was 'iPhone 3G'; the MYR price range for 'iPhone' is a conversion from its retail price in $USD and not the official retail price in Malaysia.")),
                            column(1)
                          ),
                          
                          br(),
                          hr(),
                          
                          h3("iPhone Sales VS Price"),
                          
                          #plot a scatter plot to display the relationship between price and sales of iPhone
                          fluidRow(
                            column(7,plotlyOutput("iPhone_Price_Sales_ScatterPlot")),
                            column(5,
                                   p("The point graph shows the association between the price and the global sales of iPhones. A best fit line is drawn to show the overall trend."),
                                   p("The prices of iPhones released in the same year are used to obtain an average price for that particular year, 
                                      which is then plotted against the sales of that year.")
                            )),
                          br(),br()
                          ),
                 
                 tabPanel("Summary",
                          br(),
                          fluidRow(
                            column(1),
                            column(10,
                                   h3("Key Statistics"), 
                                   br(),
                                   
                                   p("- As observed, the price of iPhone has been rising in general, except for a few models such as 'iPhone 5c', 'iPhone SE 1st generation' and 'iPhone SE 2nd generation'."),
                                   p("- The difference between the start price and end price has also increased over the years."),
                                   p("- Based on the official retail price in $USD, the most expensive model is 'iPhone 13 Pro Max' - from $1099 USD to $1599 USD."),
                                   p("- Meanwhile, the most budget-friendly model is 'iPhone SE 1st Generation', from $399 USD to $499 USD."), 
                                   p("- Aside from the improved functionalities and features, such price change is also due to the increase in storage size."),
                                   p("- Despite the increasing price points, the sales generally have not dropped but rather, increased.")
                                   ),
                            column(1)
                          )
                        )
               )),
      
      
      ###### TAB 5: UI Global Market Share ######
      tabPanel("Global Smartphone Market Share 2021",
               br(),
               tabsetPanel(
                 
                 tabPanel("Plot",
                          h3("Global Smartphone Market Share 2021"),
                          
                          #plot a multi-line graph to display the market performance of different smartphone brands, including Apple
                          fluidRow(
                            column(1),
                            column(10,
                                   plotlyOutput("Global_Market_LinePlot")),
                            column(1)
                          ),
                          
                          br(),
                          
                          fluidRow(
                            column(1),
                            column(10,
                                   textOutput("note_guide_global_market"),
                                   br(),
                                   p("The line graph above shows the share(%) of famous smartphone brands such as Apple, Samsung, Huawei, etc. in the Global Smartphone Market for year 2021.")
                            ),
                            column(1) 
                          ),
                          
                          hr(),
                          
                          textOutput("Monthly_Global_Market_Title"),
                          
                          #plot a pie chart to display the market performance of brands for a selected month
                          fluidRow(
                            column(6,
                                   plotlyOutput("Monthly_Global_Market_PieChart")),
                            column(6,
                                   h4("To view the Market Share of a particular month"),
                                   
                                   #display month options for users to choose for the pie chart
                                   wellPanel(
                                     selectInput(inputId = "global_market_month", label = "Select a Month to view", 
                                                 choices = c("January", "February","March",
                                                             "April","May","June","July",
                                                             "August","September","October","November","December"),
                                                 selected="January")
                                   ),
                                   br())
                                 )
                          ), 
                 
                 tabPanel("Summary",
                          br(),
                          fluidRow(
                            column(1),
                            column(10,
                                   h3("Key Statistics"), 
                                   br(),
                                   
                                   p("- Apple and Samsung have been dominating the market in 2021."),
                                   p("- As observed, Apple's best competitor in 2021 was Samsung."),
                                   p("- From January to September 2021, Samsung had been taking the lead."),
                                   p("- It was only until September 2021, Apple surpassed Samsung in the market, which may be due to the release of iPhone 13 series."),
                                   p("- From September to December 2021, Apple's market share increased from 26.75% to 29.24%."),
                                   p("- Ignoring the Unknown brands and Others, LG is among the brand with the least market share.")
                                   ),
                            column(1))
               )
      ))
      
    )
      
) #end of UI



# Define server logic required to plot
server <- function(input, output) {

    # the short notes put under each plot as a user guidance for interacting with the plot
  
    note <- paste0("Note: Hover over the plot to view the data values. Access the functions at the upper right corner of the plot for zooming in/out, selecting, saving as images, etc.
                 Single click an item at the Legend to include/exclude it from the plot.
                 Double click to isolate it on the plot.")
    
    output$note_guide_year <- renderText({HTML(paste(note))})
    
    output$note_guide_region <- renderText({HTML(paste(note))})
    
    output$note_guide_product <- renderText({HTML(paste(note))})
    
    output$note_guide_iphone_price <- renderText({HTML(paste(note))})
    
    output$note_guide_global_market <- renderText({HTML(paste(note))})
    
    
    
    ###################  Plotting: TAB 1 Apple Revenue by Year/Quarter #######################
    
    #Apple Revenue by Year/Quarter - Line Graph/Bar Chart
    output$Year_Quarter_Plot <- renderPlotly({
      
      #calculate the total revenue for each year and put it into a new dataset
      yearly_revenue_data <- group_by(quarter_revenue,Year)%>%summarise(TotalRevenue=sum(Revenue))

      
      #if user selects "Year" graph
      if (input$year_quarter == "Year") {
        
        #plot x - year, y - Total Revenue
        plot<-ggplot(data=yearly_revenue_data, aes(x=Year,y=TotalRevenue)) + 
          geom_line(stat="identity", colour=col_set2[5], linetype="solid",size=1) +
          geom_point(aes(text=sprintf("Total Revenue: $%.2fbillion USD", TotalRevenue)),
                     colour=col_set3[5],size=2) +
          ggtitle("Apple Revenue from 2006 to 2021 by Years") +
          scale_x_continuous(breaks=seq(2006,2021,by=1)) +
          scale_y_continuous(breaks = seq(0,max(yearly_revenue_data$TotalRevenue),by=50))+
          xlab("Year") + ylab("Revenue ($billion USD)") +
          theme(panel.background = element_rect(fill = background, size = 0.5, linetype = "solid")) 
          
        ggplotly(plot, tooltip = c("Year","text"))
      
      } 
      
      #if user selects "Quarter" graph
      else if (input$year_quarter =="Quarter"){
        
        #if user selects "Grouped" bar chart
        if(input$quarterbar_type =="Grouped"){
          
          #plot x - year, y - Revenue, fill - Quarters
          plot<-ggplot(data=quarter_revenue, aes(x=Year,y=Revenue,fill=Quarters)) +
            geom_col(position=position_dodge(0.8),width=0.8) +  #set to dodge aka grouped
            ggtitle("Apple Revenue from 2006 to 2021 by Quarters") +
            scale_fill_manual(values=col_set2) +
            scale_x_continuous(breaks=seq(2006,2021,by=1)) +
            scale_y_continuous(breaks = seq(0,max(yearly_revenue_data$TotalRevenue),by=50))+
            xlab("Year") + ylab("Revenue ($billion USD)") +
            theme(panel.background = element_rect(fill = background, size = 0.5, linetype = "solid")) 
          
          ggplotly(plot)
          
        }
        
        #if user selects "Stacked" bar chart
        else{
          
          #plot x - year, y - Revenue, fill - Quarters
          plot<-ggplot(data=quarter_revenue, aes(x=Year,y=Revenue,fill=Quarters)) +
            geom_col(position="stack") +   #set to stack
            ggtitle("Apple Revenue from 2006 to 2021 by Quarters") +
            scale_fill_manual(values=col_set2) +
            scale_x_continuous(breaks=seq(2006,2021,by=1)) +
            scale_y_continuous(breaks = seq(0,max(yearly_revenue_data$TotalRevenue),by=50))+
            xlab("Year") + ylab("Revenue ($billion USD)") +
            theme(panel.background = element_rect(fill = background, size = 0.5, linetype = "solid")) 
          
          ggplotly(plot)
          
        }
        
      } 
      
      #if user selects "Merged" graph (Year+Quarter)
      else if (input$year_quarter == "Merged"){
        
        plot<-ggplot()+
          geom_col(data=quarter_revenue,aes(x=Year,y=Revenue,fill=Quarters), position="stack")+
          geom_line(data=yearly_revenue_data,aes(x=Year,y=TotalRevenue), stat="identity", size=1,colour=col_set2[5]) +
          ggtitle("Apple Yearly Revenue from 2006 to 2021 by Years and Quarters") +
          scale_fill_manual(values=col_set2) +
          scale_x_continuous(breaks=seq(2006,2021,by=1)) +
          scale_y_continuous(breaks = seq(0,max(yearly_revenue_data$TotalRevenue),by=50)) +
          xlab("Year") + ylab("Revenue ($billion USD)") +
          theme(panel.background = element_rect(fill = background, size = 0.5, linetype = "solid")) 
      
        ggplotly(plot)
        
      }
    

    })
    
    
    ############################## Plotting: TAB 2 Apple Stores by Region + Revenue by Region ###################################
    
    
    #Apple Revenue by Region over the years - Stacked Bar Chart
    output$Region_BarPlot <- renderPlotly({
      
      #if user chooses to view a stacked bar chart
      if (input$region_bartype == "Stacked"){
        
        region_revenue_plot <- ggplot(data=region_revenue,aes(x=Year,y=Revenue,fill=Region)) +
          geom_col(position="stack") +
          xlab("Year") + ylab("Revenue ($billion USD)") +
          scale_x_continuous(breaks = seq(2015, 2021, by = 1)) +
          scale_fill_manual(values=col_set2)+
          theme(panel.background = element_rect(fill = background, size = 0.5, linetype = "solid"))
      }
      
      #if user choose to view a grouped bar chart
      else if(input$region_bartype == "Grouped"){
        
        region_revenue_plot <- ggplot(data=region_revenue,aes(x=Year,y=Revenue,fill=Region)) +
          geom_col(position=position_dodge(0.8),width=0.8) +
          xlab("Year") + ylab("Revenue ($billion USD)") +
          scale_x_continuous(breaks = seq(2015, 2021, by = 1)) +
          scale_fill_manual(values=col_set2)+
          theme(panel.background = element_rect(fill = background, size = 0.5, linetype = "solid"))
      }
      
      #plot
      ggplotly(region_revenue_plot)
      
    })
    
    
    #Apple Revenue by Region for a particular year - Pie Chart
    output$Region_PieChart <- renderPlotly({
      
      #get chosen year by user
      chosen_year=input$year_piechart
      
      #extract data for the chosen year
      chosen_region_year<-filter(region_revenue,Year==chosen_year) %>% arrange(desc(Revenue)) %>% 
        mutate(revenue_contribution = Revenue / sum(Revenue)*100) #and then, calculate the revenue contribution (%) of each region for the chosen year
      
      #factorize / set levels to each year
      chosen_region_year$Region_levels<-factor(chosen_region_year$Region,
                                               levels=c("Americas","China","Europe","Japan","Rest of Asia Pacific"))
      
      #plot pie chart using plotly for interactivity 
      plot_ly(chosen_region_year, labels = ~Region_levels, values = ~revenue_contribution, 
              type = 'pie',
              textposition = 'inside',   #show revenue contribution inside each slice of the pie chart
              textinfo = 'percent',
              hoverinfo = 'text',
              text = ~paste(Region,": $",Revenue,"billion"),  #when hover over a slice, display the Region name and its revenue contribution in $billion USD
              marker = list(colors = col_set2)) %>%  #define color palette 
        layout(showlegend = TRUE) %>%
        layout(legend = list(orientation ="h")) #hide legend for pie chart
      
    })
    
    #display selected year for the pie chart above
    output$Year_Revenue_By_Region_Title <- renderText({
      
      HTML(paste("Year",input$year_piechart))
      
    })
    
    
    
    #Apple Stores Location Worldwide - World Map
    output$Apple_Stores_World <- renderPlot({
      
      #get a world map
      world_map <- map_data("world")
      
      #join world map and apple stores location together by common region
      apple_stores_world<-left_join(world_map,apple_stores,by="region")
      
      #plot map, fill colors varied according to the Number of Stores for each location
      apple_stores_map <- ggplot() +
        geom_map( 
          data =apple_stores_world, map = apple_stores_world,
          aes(x=long, y=lat, map_id = region, fill=Number_of_Stores),
          col="white"
        ) +
        scale_fill_gradient(high=col_set3[4],low='#FFD6AD') + 
        xlab("longtitude") + 
        ylab("latitude") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_line(color = 'white', linetype = 'dashed', size = 0.3), 
              panel.background = element_rect(fill = col_set3[1]))
      
      #plot
      plot(apple_stores_map)
      
    })
    
    
    #display total number of Apple stores
    output$total_stores <- renderText({
      
      #find the sum number of stores
      total <- sum(apple_stores$Number_of_Stores)
      
      HTML(paste(total))
      
    })
    
    #display region with the most Apple stores
    output$most_stores <- renderText({
      
      #subset data with the highest number of stores
      most_store <- subset(apple_stores,Number_of_Stores==max(Number_of_Stores))
      
      #change USA to Americas for name-consistency across the app
      most_store[most_store$region=="USA","region"] <- "Americas" 
      
      #display output
      HTML(paste(most_store$region[1]," - ",most_store$Number_of_Stores[1], " store(s)"))
      
    })
    
    
    
    
    ############################## Plotting: TAB 3 Sales and Revenue of Apple Product ###################################
    
    #Revenue and Sales of each Apple's product category - Bar-Line Graph (dual y-axis)
    output$Produce_RevenueSales <- renderPlotly({
      
      title_text <- list(
        size = 20,
        color = 'black')
      
      legend_text <- list(
        size=15
      )
      
      #get the chosen product category
      chosen_product_category<-input$product_category
      
      #extract data based on the chosen product category
      chosen_product_revenue_sales <- group_by(product_revenue_sales,Product) %>% filter(Product==chosen_product_category)##
      
      #find the year range of the product category 
      chosen_product_max_year <- summarize(chosen_product_revenue_sales,max_y=max(Year)) %>% select(max_y)
      chosen_product_min_year <- summarize(chosen_product_revenue_sales,min_y=min(Year)) %>% select(min_y)
      
      #plot the graph using plotly
      plot_ly(chosen_product_revenue_sales, x = ~Year, y = ~Revenue, type = "bar", name = "Revenue", color=I(col_set3[5])) %>% #plot bars
        add_lines(x = ~Year, y = ~Sales, mode = "lines+markers", yaxis = "y2", name = "Sales", color=I(col_set3[1])) %>% #plot lines
        layout(yaxis2 = list(overlaying = "y", side = "right", title="Sales (million)")) %>% 
        layout(yaxis = list(title="Revenue ($billion USD)")) %>%
        layout(xaxis = list(dtick=1,range=list(chosen_product_min_year,chosen_product_max_year))) %>%
        layout(legend = list(x = 100, y = 1.3,font=legend_text)) %>%
        layout(title=list(text=sprintf("%s",chosen_product_revenue_sales$Product[1]),
                          x=0.05,font=title_text))
      
    })
    
    
    #Revenue Contribution of each product category - Area Graph
    output$Product_AreaGraph <- renderPlotly({
      
      #set levels to Apple Products
      product_revenue_sales$Apple_product<-factor(product_revenue_sales$Product, levels=c("Wearables & Home","Mac","iPad","iPhone"))
      
      #extract data from 2015 to 2021 because 2015 is when each category has had at least 1 product launched
      product_revenue_20152021 <-subset(product_revenue_sales,Year>=2015)
      
      #calculate total revenue for each year
      product_revenue_20152021<-group_by(product_revenue_20152021,Year)%>%mutate(totalrev=sum(Revenue))
      
      #calculate revenue contribution (%) of each product category for each year
      product_revenue_20152021<-mutate(product_revenue_20152021,revenue_contribution=round(Revenue/totalrev*100,digits = 2))
      
      #plot, x - year, y - Revenue Contribution (%), fill - Apple product category
      product_revenue_20152021_area_plot <- ggplot(product_revenue_20152021, aes(x=Year, y=revenue_contribution, fill=Apple_product)) + 
        geom_area(alpha=0.9) +
        labs(x="Year",y="Revenue %") +
        scale_x_continuous(breaks = seq(2015, 2021, by = 1)) +
        scale_fill_manual("Categories",values=col_set2[2:6]) +
        theme(
          legend.text = element_text(size=10),
          panel.background = element_rect(fill = background, size = 0.5, linetype = "solid")
        ) 
      
      #plot
      ggplotly(product_revenue_20152021_area_plot)
      
    })
    
    
    ############################## Plotting: TAB 4 iPhone Price and Sales ###################################
    
    #iPhone Price Evolution - Point Range Graph
    output$iPhone_Price_RangePlot <- renderPlotly({
      
      #arrange iPhone by year in ascending order
      iphone_price<-iphone_price %>% arrange(Year)
      
      #set levels to Years and models 
      iphone_price$Year_levels <- factor(iphone_price$Year,levels=seq(2007,2021,by=1))
      iphone_price$Models_levels <- factor(iphone_price$Models,levels=iphone_price$Models)
      
      #find the middle price point for each iPhone model based on the start and end price, in USD and MYR currencies
      iphone_price<-iphone_price %>% group_by(Models) %>% mutate(Mid_Price_USD=median(c(Start_Price_USD,End_Price_USD)))
      iphone_price<-iphone_price %>% group_by(Models) %>% mutate(Mid_Price_MYR=median(c(Start_Price_MYR,End_Price_MYR)))
      
      #if user selects USD currency
      if(input$price_currency=="$USD (US Dollar)"){
        
        iphone_price_rangeplot <- ggplot(iphone_price,aes(x=Models_levels)) + 
          geom_pointrange(aes(y=Mid_Price_USD,ymin=Start_Price_USD, ymax=End_Price_USD, 
                              text=sprintf("Model: %s\nMiddle Price: $%dUSD",Models_levels,Mid_Price_USD)),
                          size=1,col=col_set1[1],alpha=0.8) +
          geom_point(aes(y=Start_Price_USD, col="Start Price", 
                         text=sprintf("Start Price: $%dUSD",Start_Price_USD))) + 
          geom_point(aes(y=End_Price_USD, col="End Price", 
                         text=sprintf("Start Price: $%dUSD",End_Price_USD)))+
          ylab("Price (USD)") + 
          xlab("") +
          scale_y_continuous(breaks=seq(0,max(iphone_price$End_Price_USD)+100,by=100)) +
          scale_color_manual("",values=c("Start Price" = col_set3[5],"End Price" = col_set3[1]))+ 
          theme(
            axis.text.y = element_text(size=8),
            axis.text.x = element_text(angle=45,hjust=1),
            panel.spacing.y=unit(2, "cm"),
            panel.background = element_rect(fill = background, size = 0.5, linetype = "solid")
          )
        
      } 
      
      #if user selects MYR currency
      else if(input$price_currency=="MYR (Malaysia Ringgit)"){
        
        iphone_price_rangeplot <- ggplot(iphone_price,aes(x=Models_levels)) + 
          geom_pointrange(aes(y=Mid_Price_MYR,ymin=Start_Price_MYR, ymax=End_Price_MYR, 
                              text=sprintf("Model: %s\nMiddle Price: RM%.2f",Models_levels,Mid_Price_MYR)),
                          size=1,col=col_set1[1],alpha=0.8) +
          geom_point(aes(y=Start_Price_MYR, col="Start Price", 
                         text=sprintf("Start Price: RM%.2f",Start_Price_MYR)),show.legend = FALSE) + 
          geom_point(aes(y=End_Price_MYR, col="End Price", 
                         text=sprintf("End Price: RM%.2f",End_Price_MYR)),show.legend = FALSE) +
          ylab("Price (MYR)") + 
          xlab("") +
          scale_y_continuous(breaks=seq(0,max(iphone_price$End_Price_MYR)+100,by=1000)) +
          scale_color_manual("",values=c("Start Price" = col_set3[5],"End Price" = col_set3[1]))+ 
          theme(
            axis.text.y = element_text(size=8),
            axis.text.x = element_text(angle=45,hjust=1),
            panel.spacing.y=unit(2, "cm"),
            panel.background = element_rect(fill = background, size = 0.5, linetype = "solid")
          )
        
      }
      
      #plot
      ggplotly(iphone_price_rangeplot, tooltip = c("text"))
      
      
    })
    
    
    #iPhone Sales VS Price - Scatter Plot
    output$iPhone_Price_Sales_ScatterPlot <- renderPlotly({
      
      #extract sales data of iPhone + corresponding Years of release
      iphone_sales_data <- product_revenue_sales %>% filter(Product=="iPhone") %>%  select(Year,Sales)
      
      #find the average price of iPhone released in the same year, for each year
      # 1. calculate the middle price point of each iPhone model based on its start and end price
      iphone_price<-iphone_price %>% group_by(Models) %>% mutate(Mid_Price_USD=median(c(Start_Price_USD,End_Price_USD)))
      # 2. calculate the average price for each year based on the middle price points of iPhone released in the same year
      iphone_price<-iphone_price%>%group_by(Year)%>%mutate(Avg_Price_USD=mean(Mid_Price_USD))
      
      #extract the Average Price of iPhones released in the same year + corresponding Years of release
      iphone_price_data <- iphone_price %>% select(Year,Avg_Price_USD)
      
      #join the average prices and sales data of iPhone together by their common Year 
      iphone_price_sales_join <- left_join(iphone_price_data, iphone_sales_data, by="Year")
      
      #since there are times when Apple released multiple iPhone models in a year, 
      #the joined dataset will have rows with the same Year and same data
      
      #So, data cleaning: select only the rows with distinct year 
      iphone_price_sales_join<- iphone_price_sales_join %>% distinct(Year, .keep_all = TRUE)
      
      
      #plot x - avg price, y - sales
      iphone_price_sales_plot<-ggplot(iphone_price_sales_join, aes(x=Avg_Price_USD, y=Sales)) + 
        geom_point(aes(text=sprintf("Year: %d\nAverage Price: $%.2fUSD\nSales: %.2f million",Year,Avg_Price_USD,Sales)),col=col_set3[5]) + 
        geom_smooth(method="lm",col=col_set3[1],se=FALSE,show.legend = FALSE) + #draw the best fit line 
        scale_x_continuous(breaks=seq(0,max(iphone_price_sales_join$Avg_Price_USD)+200,by=100)) +
        scale_y_continuous(breaks=seq(0,max(iphone_price_sales_join$Sales)+100,by=20))+
        xlab("Price ($USD)") +
        ylab("Sales (million)") +
        theme(
          panel.background = element_rect(fill = background, size = 0.5, linetype = "solid")
        )
      
      #plot
      ggplotly(iphone_price_sales_plot, tooltip = "text")
      
    })
    
    ############################## Plotting: TAB 5 Global Market Share 2021 ###################################
    
    #set levels to Date using Months as labels
    global_market$Months <- factor(global_market$Date,
                                   levels=global_market$Date[1:12],
                                   labels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
    
    
    #Global Market Share(%) of all involved Brands - Multi-line Graph
    output$Global_Market_LinePlot <- renderPlotly({
      
      #plot x - Months, y - Share(%), col - Brands, group - Brands (each brand will have 1 line)
      global_market_line_plot<-ggplot(global_market,aes(x=Months,y=Share,col=Brands,group=Brands)) + 
        geom_line() +
        geom_point(size=1,aes(text=sprintf("Brands: %s\nShare: %.2f\nMonths: %s",Brands,Share,Months))) + 
        scale_color_manual(values=col_set1) +
        ylab("Share %") + 
        xlab("") +
        theme(
          panel.background = element_rect(fill = background, size = 0.5, linetype = "solid")
        )
      
      #plot
      ggplotly(global_market_line_plot, tooltip = "text")
      
    })
    
    
    #Global Market Share(%) for a selected month - Pie Chart
    output$Monthly_Global_Market_PieChart <- renderPlotly({
      
      #get the chosen month by user and extract global market data based on the chosen month
      chosen_month_global_market <- subset(global_market,Months==input$global_market_month)
      
      #plot using plotly 
      plot_ly(chosen_month_global_market, labels = ~Brands, values = ~Share, 
              type = 'pie',
              textposition = 'inside', #inside each slice of the pie is a market share(%) value
              textinfo = 'percent',
              hoverinfo = 'text',
              text = ~paste(Brands,"-", Share, '%'), #when hover, it will display the Brand and its Share(%)
              marker = list(colors = col_set1)) #set color palette to the pie chart
    
    })
    
    #display the month chosen for the pie chart
    output$Monthly_Global_Market_Title <- renderText({
      
      HTML(paste("Global Market: ",input$global_market_month, " 2021"))
      
    })

} #end of server


# Run the application 
shinyApp(ui = ui, server = server)
