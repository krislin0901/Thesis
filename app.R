library(shiny)
library(dplyr)
library(plotly)
library(shinydashboard)
library(heatmaply)
library(shinyHeatmaply)
library(hrbrthemes)
library(dplyr)
library(grid)
library(gridExtra)
library(readxl)
library(shinyWidgets)

Bitcoin <- read.csv("CBBTCUSD.csv")
Gold <- read.csv("GOLDPMGBD228NLBM.csv")

df <- merge(Bitcoin, Gold, by = "DATE")
df$DATE <- as.Date(df$DATE, format = "%Y-%m-%d")

colnames(df)[1:3] <- c("Date","Bitcoin", "Gold")


df2 <- read.csv("df.wide2.csv")
df2$date<- as.Date(df2$date, format = "%Y-%m-%d")
is.na(df.wide) <- sapply(df.wide, is.infinite)
df.wide <- na.omit(df.wide)
colnames(df2) <- c("Date", "Bitcoin", "Inflation (CPI)", "US Dollar Index", "Unemployment Rate", "Fed Funds Rate", "Consumer Sentiment Index","Dow Jones Industrial Average", "Gold")
df2 <- df2 %>%
    select("Date", "Bitcoin", "Gold", "Inflation (CPI)", "Fed Funds Rate", "Dow Jones Industrial Average", "US Dollar Index","Consumer Sentiment Index", "Unemployment Rate")
df3 <- df2
table <- read_xlsx("DescriptiveStats.xlsx")
colnames(table)[1] <- ""

ui <- dashboardPage(skin = "blue",
      dashboardHeader(title = "Thesis Progress Report", titleWidth = 250),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
      titlePanel("Is Bitcoin the Real Digital Gold?"),
      
      fluidRow(
      box(
      title = "Introduction",
      solidHeader = TRUE,
      width = 12,
      status = "info",
      p("Gold has always been considered as one of the strongest hedges against inflation. The US dollar had been experiencing a downward movement and losing its dominance in the past few months with COVID-19 devastating the country’s economy. With Biden’s administration and a future COVID stimulus incoming, it is foreseeable that the downtrend of the US dollar 
      may be continuing in the future years. Bitcoin, which is often referred to as digital gold, is the hybrid of fiat and commodity currencies. It is the first and the largest digital currency in the world which uses blockchain technology to create a decentralized ledger that allows users to skip intermediaries. With the abovementioned characteristics and its recent 
      stability, Bitcoin along with other digital currencies has been largely considered as an alternative of gold as a strong hedge moving toward the future. In this project, a VAR model will be applied for both bitcoin & gold, then the results will be compared to see whether bitcoin has similar characteristics with gold and how they react to the variables."),
         )
      ),
      fluidRow(
          box(
              title = "FYI",
              solidHeader = TRUE, 
              width = 12,
              status = "info",
              h4("What is VAR?"),
              p("Vector autoregression (VAR) is a statistical model used to capture the relationship between multiple quantities as they change over time. VAR is a type of stochastic process model. VAR models generalize the single-variable (univariate) autoregressive model by allowing for multivariate time series."),
              h4("What is GARCH?"),
              p("Generalized AutoRegressive Conditional Heteroskedasticity (GARCH) is a statistical model used in analyzing time-series data where the variance error is believed to be serially autocorrelated. It can be used to capture volatility clustering"),
          )
      ),
      fluidRow(
          box(
              title = "Methodology",
              solidHeader = TRUE, 
              width = 12,
              status = "info",
              h4("1. Normality Test (Jarque-Bera Test)"),
              h4("2. Descriptive Statistics"),
              h4("3. Stationarity Test (ADF and KPSS test)"),
              h4("4: GARCH Test"),
              h4("5: Cointegration Test (Johansen or Granger Test)"),
              h4("6: VAR or VECM model"),
              h4("7: IRF and FEVD")
          )
      ),
      # Sidebar with a slider input for number of bins 
      fluidRow(
      box(
      title = "Asset Prices",
      solidHeader = TRUE, 
      width = 12,
      status = "info",
      sidebarLayout(
          sidebarPanel(
              selectInput("var1", "Select Variable", choices = c("Bitcoin", "Gold")),
              sliderInput("date", "Select Date Range:", 
                         min = as.Date("2015-02-01", "%Y-%m-%d"), 
                         max = as.Date("2021-04-01", "%Y-%m-%d"),
                         value = as.Date(c("2015-02-01","2021-04-01") , "%Y-%m-%d"),
                         timeFormat = "%Y-%m-%d")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotly::plotlyOutput("graph1", width = "auto")
        )
    )
    )
),
    
    fluidRow(
        box(
            title = "Scatter Plots Between Variables",
            solidHeader = TRUE, 
            width = 12,
            status = "info",
            sidebarLayout(
                sidebarPanel(
                    varSelectInput("varx", "Select Variable (X-Axis):", df2[,4:9])
                ),
                mainPanel(
                    tabsetPanel(
                    tabPanel("Bitcoin (Y-Axis)", plotly::plotlyOutput("graph3", width = "auto")),  
                    tabPanel("Gold (Y-Axis)", plotly::plotlyOutput("graph2", width = "auto"))
                ))
        )
    )
),
    fluidRow(
        box(
            title = "Variable Selection",
            solidHeader = TRUE, 
            width = 12,
            status = "info",
            h4("Inflation Rate:"),p("It can be seen that the results resemble our intuition that when the inflation rate is higher, it means higher opportunity cost to hold cash such as US dollar. On the other hand, Bullions, precious metals, such as gold’s value will rise higher when the inflation rate is high. On the other hand, it is indicated in Figure 2 that bitcoin has a slightly positive correlation with the inflation rate despite it is still not obvious. Inflation rate is undoubtedly included in our model as this is the variable that is usually considered having the strongest correlation with gold/bitcoin and also the variable we are most interested to see how it react to bitcoin’s price movement in our model."),
            h4("US Dollar Exchange Index:"),p("It can be seen that the results resemble our intuition as gold’s price usually acts negatively with the US dollar value. When the value of US Dollar goes up, the price of gold will reflect as it now takes less US dollar to buy the commodity; that is, the price of gold will decrease when dollar increases its value, and vice versa. From Figure 4, we can see that there a slight negative relationship between Bitcoin and US dollar in the past 7 years, but it would be interesting to see how Bitcoin respond to US Dollar Index in our model."),
            h4("Fed Funds Rate:"),p("It can be indicated that the relationship between gold and fed funds rate resembles the intuition as they have a negative relation. When the fed funds rate is high, it means there is a higher opportunity cost for holding non-interest bearing assets, such as gold. Rising interest rates will make bonds and other fixed-income investments more attractive, which money will flow into higher-yielding investments and out of gold when rates move higher. Therefore, the price of gold tends to move lower when the fed funds rate go higher, which is a negative correlation. From Figure 4, it is also shown that Bitcoin surprisingly has a strong negative correlation with fed funds rate. Thus, fed funds rate is also included in our model as it is usually considered as one of the vital variables that changes gold’s price which the negative relation is proved by the scatter plot."),
            h4("Consumer Sentiment Index:"),p("It can be indicated that the relationship between gold and consumer sentiment index resembles the intuition as they have a negative relation. When the consumer sentiment for the market is high, investors usually put their money into assets that are riskier that gold as they tend to believe that they can earn higher returns with lower risks in these financial instruments, and vice versa. Thus, the demand for gold will be lower when the consumer sentiment is high, which is a negative relation. From Figure 4, Bitcoin also shown negative correlation with consumer correlation."),
            h4("Dow Jones Industrial Average (DJIA):"),p("From multiple past studies such as what He et al. (2018) indicated, it is usually said that gold has a small correlation with the stock market such as the Dow Jones Industrial Average Index. On the other side, Xin Wang & Xi Chen & Peng Zhao,
(2020) indicated that Bitcoin has shown a strong relationship with the stock market, which S&P500 and DJIA especially has an advantageous effect on Bitcoin’s price movement. Thus, we visualized the scatter plot for gold/bitcoin and DJIA to see the relation between the two in the past 7 years and decide if we should include DJIA Index in our model. As Figure 2 and 3 both shown that gold/bitcoin has a positive relation with DJIA Index, we decided to include DJIA Index in our model."),
            h4("Unemployment Rate:")
            ),
           
),
fluidRow(
    box(
        title = "Descriptive Statisitics",
        solidHeader = TRUE, 
        width = 8,
        status = "info",
        tableOutput('table'),
    ),
    box("Data Source",
            width = 4,
            solidHeader = TRUE,
            background = "aqua",
            textOutput("References"))
    )
),

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$graph1 <- plotly::renderPlotly({
        df1 <- df %>%
            filter(Date >= input$date[1], Date <= input$date[2]) %>%
            select(Date, input$var1)
        df1 %>%
            ggplot(aes_string(x = "Date", y = input$var1)) +
            geom_line(aes(colour = "red")) +
            theme(plot.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  legend.position = "none",
                  axis.line = element_line(color="black", size = 1))
    }
            )
    output$graph3 <- plotly::renderPlotly({
        df3 %>%
            ggplot(aes_string(x = input$varx, y = "Bitcoin")) +
            geom_point(aes(colour = "red")) +
            theme(plot.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  legend.position = "none",
                  axis.line = element_line(color="black", size = 1))
    })
    output$graph2 <- plotly::renderPlotly({
        df2 %>%
            ggplot(aes_string(x = input$varx, y = "Gold")) +
            geom_point(aes(colour = "red"))+
            theme(plot.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  legend.position = "none", 
                  axis.line = element_line(color="black", size = 1))
    })
    output$table <- renderTable({
        table
    })
    output$References <- renderText({
        "FRED (Federal Reserve Bank of St. Louis) Retreived from FRED: https://fred.stlouisfed.org"
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
