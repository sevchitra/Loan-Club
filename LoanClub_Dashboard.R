library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)


options(scipen=999) 

loanClub_data <- read.csv("Data/LoanStats3a.csv",stringsAsFactors = FALSE)

loanClub_data <- loanClub_data %>% 
  select(loan_amnt,issue_d,int_rate,addr_state,loan_status)
cat_var <- names(loanClub_data)[which(sapply(loanClub_data, is.character))]

loanClub_data[,cat_var] <- lapply(loanClub_data[,cat_var] , factor)

loanClub_data$issue_month<-substr(loanClub_data$issue_d,1,3)
loanClub_data$issue_month<-as.factor(loanClub_data$issue_month)

loanClub_data$issue_year<-substr(loanClub_data$issue_d,5,8)
loanClub_data$issue_year <- paste(20,loanClub_data$issue_year, sep="") 

loanClub_data <- loanClub_data[!(is.na(loanClub_data$issue_year)),]

loanClub_data$int_rate <- gsub("%", "", loanClub_data$int_rate)
loanClub_data$int_rate <- as.numeric(loanClub_data$int_rate)

loanClub_data$int_rate_class<-cut(loanClub_data$int_rate, 4, labels = c("1","2", "3", "4"))
loanClub_data$int_rate_class <- as.factor(loanClub_data$int_rate_class)


loanClub_data$issue_year2 <- loanClub_data$issue_year

loanClub_data$issue_year2 = ifelse(loanClub_data$issue_year2 == "2007","1",loanClub_data$issue_year2)
loanClub_data$issue_year2 = ifelse(loanClub_data$issue_year2 == "2008","2",loanClub_data$issue_year2)
loanClub_data$issue_year2 = ifelse(loanClub_data$issue_year2 == "2009","3",loanClub_data$issue_year2)
loanClub_data$issue_year2 = ifelse(loanClub_data$issue_year2 == "2010","4",loanClub_data$issue_year2)
loanClub_data$issue_year2 = ifelse(loanClub_data$issue_year2 == "2011","5",loanClub_data$issue_year2)

loanClub_data$issue_year2 <- as.factor(loanClub_data$issue_year2)

data(state.regions)
loanClub_data <- merge(loanClub_data, state.regions, by.x = "addr_state", by.y = "abb")


header <- dashboardHeader(title = "Loan Club Analytics")

sidebar <- dashboardSidebar(disable = TRUE)

frow1 <- fluidRow(
  box(width = 2, status = "warning",
      checkboxGroupInput("year", "Year",
                         choices = c(
                           "2007" = 1,
                           "2008" = 2,
                           "2009" = 3,
                           "2010" = 4,
                           "2011" = 5
                         ),
                         selected = c(1, 2, 3, 4,5)
      )
  ), 
  
  box(width = 2, status = "warning",
      checkboxGroupInput("int_rate", "InterestRate",
                         choices = c(
                           "Low" = 1,
                           "Normal" = 2,
                           "High" = 3,
                           "Very High" = 4
                         ),
                         selected = c(1, 2, 3, 4)
      )
  ),
  box(width = 8, status = "warning",
      valueBoxOutput("loanCount")
      ,valueBoxOutput("loanAverage")
      ,valueBoxOutput("loanTotal")
  )
)



frow2 <- fluidRow(
  box(width = 6,
      title = "Loan Amount Dispersion"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput("loanAmountBar", height = "200px")
  ), 
  
  box(width = 6,
      title = "Top 10 States"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput("Top10States", height = "200px")
  )
)
frow3 <- fluidRow(
  
  
  box(width = 6,
      title = "Loan Concentration of States - Total Amount"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput("states_value", height = "300px")
  ), 
  
  box(width = 6,
      title = "Loan Concentration of States - Count"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput("states_volume", height = "300px")
  ) 
  
)

body <- dashboardBody(frow1, frow2, frow3)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  observe({
    
    
    data <- filter(loanClub_data, int_rate_class %in% input$int_rate & issue_year2 %in% input$year)
    
    output$loanCount <- renderValueBox({
      valueBox(
        formatC(length(data$loan_amnt), format="d", big.mark=',')
        ,"Count Of Loans"
      )
    })
    
    output$loanAverage <- renderValueBox({
      valueBox(
        formatC(mean(data$loan_amnt), format="d", big.mark=',')
        ,"Average Of Loans"
      )
    })
    
    output$loanTotal <- renderValueBox({
      valueBox(
        formatC(sum(data$loan_amnt), format="d", big.mark=',')
        ,"Total Amount  Of Loans"
      )
    })
    
    
    output$loanAmountBar <- renderPlot({
      
      ggplot(data=data, aes(x=loan_amnt)) + 
        geom_histogram(bins=20,aes(y=..density.., fill=..count..)) +
        scale_fill_gradient("Count", low="#abc6f2", high="#116af9") +
        stat_function(fun=dnorm,
                      color="#0b7c53",size=1,
                      args=list(mean=mean(data$loan_amnt), 
                                sd=sd(data$loan_amnt))) +
        geom_vline(aes(xintercept=mean(loan_amnt, na.rm=T)),   # Ignore NA values for mean
                   color="red", linetype="dashed", size=1) +
        geom_vline(aes(xintercept=median(loan_amnt, na.rm=T)),   # Ignore NA values for mean
                   color="grey", linetype="dashed", size=1) +
        xlab("Loan Amount") +
        ylab("Density")
      
    })
    output$Top10States <- renderPlot({
      
      data %>% filter(loan_status != 'Fully Paid') %>% 
        group_by(addr_state) %>% 
        summarize(TotalLoan = sum(loan_amnt)) %>% 
        arrange(desc(TotalLoan)) %>% 
        head(10) -> TopStates
      
      ggplot(data=TopStates,aes(x=reorder(addr_state,-TotalLoan),y=TotalLoan,label=TotalLoan)) + 
        geom_bar(stat='identity',fill = "#4286f4") +
        geom_text(vjust=-1,size=2.5) +
        xlab("States") +
        ylab("Total Loan Amount")
      
    })
    
    output$states_value <- renderPlot({
      
      state_by_value <-
        loanClub_data %>% group_by(region) %>%
        summarise(value = sum(loan_amnt, na.rm=TRUE))
      
      state_choropleth(state_by_value, title = "Value by State")
      
    })
    
    output$states_volume <- renderPlot({
      
      state_by_volume <-
        loanClub_data %>% group_by(region) %>%
        summarise(value = n())
      
      state_choropleth(state_by_volume, title = "Volume by State")
      
    })
    
  })
  
}
shinyApp(ui, server)

