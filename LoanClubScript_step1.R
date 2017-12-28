library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
options(scipen=999) 

loanClub_data <- read.csv("Data/LoanStats3a.csv",stringsAsFactors = FALSE)
dim(loanClub_data)

loanClub_data <- loanClub_data %>% 
  select(loan_amnt,issue_d,int_rate,addr_state,loan_status)

dim(loanClub_data)

cat_var <- names(loanClub_data)[which(sapply(loanClub_data, is.character))]

loanClub_data[,cat_var] <- lapply(loanClub_data[,cat_var] , factor)

head(loanClub_data$issue_d)

loanClub_data$issue_month<-substr(loanClub_data$issue_d,1,3)
loanClub_data$issue_month<-as.factor(loanClub_data$issue_month)

loanClub_data$issue_year<-substr(loanClub_data$issue_d,5,8)
loanClub_data$issue_year <- paste(20,loanClub_data$issue_year, sep="")


loanClub_data$issue_year2 <- loanClub_data$issue_year

loanClub_data$issue_year2 = ifelse(loanClub_data$issue_year2 == "2007","1",loanClub_data$issue_year2)
loanClub_data$issue_year2 = ifelse(loanClub_data$issue_year2 == "2008","2",loanClub_data$issue_year2)
loanClub_data$issue_year2 = ifelse(loanClub_data$issue_year2 == "2009","3",loanClub_data$issue_year2)
loanClub_data$issue_year2 = ifelse(loanClub_data$issue_year2 == "2010","4",loanClub_data$issue_year2)
loanClub_data$issue_year2 = ifelse(loanClub_data$issue_year2 == "2011","5",loanClub_data$issue_year2)

loanClub_data$issue_year2 <- as.factor(loanClub_data$issue_year2)


################## UI Code ############################


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
  
  
  box(width = 8, status = "warning",
      valueBoxOutput("loanCount")
      ,valueBoxOutput("loanAverage")
      ,valueBoxOutput("loanTotal")
  )
)



frow2 <- fluidRow(
  box(width = 12,
      title = "Loan Amount Dispersion"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput("loanAmountBar", height = "400px")
  ) 
  
)


body <- dashboardBody(frow1, frow2)

ui <- dashboardPage(header, sidebar, body)

################## Server Code ############################


server <- function(input, output, session) {
  
  observe({
    
    
    data <- filter(loanClub_data, issue_year2 %in% input$year)
    
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
    
    
  })
  
}
shinyApp(ui, server)

