library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readr)
library(DT)
library(VIM)
library(randomForest)
library(caret)

#RAW DATAAAA
dataset <- read.csv("in-vehicle-coupon-recommendation.csv", na.strings = c("", "NA"))

#Data Encoding
data <- read.csv("in-vehicle-coupon-recommendation.csv", na.strings = c("", "NA"))
encode_ordinal <- function(x, order = unique(x)) {
    x <- as.numeric(factor(x, levels = order))
    x
}
#table(data[["car"]], encode_ordinal(data[["car"]]))
data$CarryAway <- as.factor(encode_ordinal(data[["CarryAway"]]))
data$CoffeeHouse <- as.factor(encode_ordinal(data[["CoffeeHouse"]]))
data$Restaurant20To50 <- as.factor(encode_ordinal(data[["Restaurant20To50"]]))
data$RestaurantLessThan20 <- as.factor(encode_ordinal(data[["RestaurantLessThan20"]]))
data$Bar <- as.factor(encode_ordinal(data[["Bar"]]))
data$age <- as.factor(encode_ordinal(data[["age"]]))
data$destination <- as.factor(encode_ordinal(data[["destination"]]))
data$passanger <- as.factor(encode_ordinal(data[["passanger"]]))
data$weather <- as.factor(encode_ordinal(data[["weather"]]))
data$time <- as.factor(encode_ordinal(data[["time"]]))
data$coupon <- as.factor(encode_ordinal(data[["coupon"]]))
data$expiration <- as.factor(encode_ordinal(data[["expiration"]]))
data$gender <- as.factor(encode_ordinal(data[["gender"]]))
data$maritalStatus <- as.factor(encode_ordinal(data[["maritalStatus"]]))
data$has_children <- as.factor(encode_ordinal(data[["has_children"]]))
data$education <- as.factor(encode_ordinal(data[["education"]]))
data$income <- as.factor(encode_ordinal(data[["income"]]))
data$occupation <- as.factor(encode_ordinal(data[["occupation"]]))
data <- subset(data, select = -c(toCoupon_GEQ5min, car))
data$toCoupon_GEQ15min <- as.factor(data[["toCoupon_GEQ15min"]])
data$toCoupon_GEQ25min <- as.factor(encode_ordinal(data[["toCoupon_GEQ25min"]]))
data$Y <- as.factor(encode_ordinal(data[["Y"]]))
data$direction_same <- as.factor(encode_ordinal(data[["direction_same"]]))
data$direction_opp <- as.factor(encode_ordinal(data[["direction_opp"]]))
data$temperature <- as.factor(encode_ordinal(data[["temperature"]]))

#Data Encoding
data_encodee <- transform(
    data,
    CarryAway <- as.factor(encode_ordinal(data[["CarryAway"]])),
    CoffeeHouse <- as.factor(encode_ordinal(data[["CoffeeHouse"]])),
    Restaurant20To50 <- as.factor(encode_ordinal(data[["Restaurant20To50"]])),
    RestaurantLessThan20 <- as.factor(encode_ordinal(data[["RestaurantLessThan20"]])),
    Bar <- as.factor(encode_ordinal(data[["Bar"]])),
    age <- as.factor(encode_ordinal(data[["age"]])),
    destination <- as.factor(encode_ordinal(data[["destination"]])),
    passanger <- as.factor(encode_ordinal(data[["passanger"]])),
    weather <- as.factor(encode_ordinal(data[["weather"]])),
    time <- as.factor(encode_ordinal(data[["time"]])),
    coupon <- as.factor(encode_ordinal(data[["coupon"]])),
    expiration <- as.factor(encode_ordinal(data[["expiration"]])),
    gender <- as.factor(encode_ordinal(data[["gender"]])),
    maritalStatus <- as.factor(encode_ordinal(data[["maritalStatus"]])),
    has_children <- as.factor(encode_ordinal(data[["has_children"]])),
    education <- as.factor(encode_ordinal(data[["education"]])),
    income <- as.factor(encode_ordinal(data[["income"]])),
    occupation <- as.factor(encode_ordinal(data[["occupation"]])),
    toCoupon_GEQ15min <- as.factor(data[["toCoupon_GEQ15min"]]),
    toCoupon_GEQ25min <- as.factor(encode_ordinal(data[["toCoupon_GEQ25min"]])),
    Y <- as.factor(encode_ordinal(data[["Y"]])),
    direction_same <- as.factor(encode_ordinal(data[["direction_same"]])),
    direction_opp <- as.factor(encode_ordinal(data[["direction_opp"]])),
    temperature <- as.factor(encode_ordinal(data[["temperature"]]))
    
)

databersih <- read.csv("databersih.csv")
fix_data <- data_encodee
fix_data[["Bar"]] <- databersih[["Bar"]]
fix_data[["CoffeeHouse"]] <- databersih[["CoffeeHouse"]]
fix_data[["CarryAway"]] <- databersih[["CarryAway"]]
fix_data[["RestaurantLessThan20"]] <- databersih[["RestaurantLessThan20"]]
fix_data[["Restaurant20To50"]] <- databersih[["Restaurant20To50"]]

fix_data2 <- distinct(fix_data)

set.seed(100)
train <- sample(nrow(fix_data2), 0.75*nrow(fix_data2), replace = FALSE)
TrainSet <- fix_data[train,]
ValidSet <- fix_data[-train,]

model1 <- randomForest(Y~ ., data = TrainSet, importance = TRUE, mtry = 6, seed=1)

#Define UI
ui <- fluidPage(theme = shinytheme("united"),
                shinythemes::themeSelector(),
                navbarPage(
                    "Dashboard Kelompok I",
                    tabPanel("Dataset", 
                             fluidRow(
                                 column(4,
                                        selectInput("Y",
                                                    "Dependent Variables:",
                                                    c("All",
                                                      unique(as.character(dataset$Y))))
                                 )
                             ),
                             
                             fluidRow(h3("Dataset Information"),
                                      "This data was collected via a survey on Amazon Mechanical Turk. The survey describes different driving scenarios including 
                                    the destination, current time, weather, passenger, etc., and then ask the person whether he will accept the coupon if he is the driver"
                             ),
                             br(),
                             
                             
                             # Create a new row for the table.
                             DT::dataTableOutput("table")
                    ),
                    tabPanel("EDA",
                             sidebarLayout(
                                 sidebarPanel(
                                     selectInput(inputId = "var",
                                                 label = "variabel", 
                                                 choices = list("destination"='a', "passanger"='b', "weather"='c', 
                                                                "time"="d", "coupon"="e","expiration"="f", "gender"="g", 
                                                                "maritalStatus"= "h", "has_children"= "i", "education"="j",
                                                                "occupation"= "k","income"="l", "car"="m", "Bar"="n", "CoffeeHouse"="o",
                                                                "CarryAway"="p", "RestaurantLessThan20"="q", "Restaurant20To50"="r", "toCoupon_GEQ5min"="s",
                                                                "toCoupon_GEQ15min"="t", "toCoupon_GEQ25min"="u", "direction_same"="v", 
                                                                "direction_opp"="w", "Y"="x", "age"='y', "temperature"= "z")),
                                     
                                 ),
                                 
                                 mainPanel(tabsetPanel(type = "tabs",
                                                       tabPanel("Summary", verbatimTextOutput("summary"),
                                                                fluidRow(h2("Data Duplikat"),
                                                                         verbatimTextOutput("dup"))),
                                                       tabPanel("Missing Value Visualisation", plotOutput("aggr")),
                                                       tabPanel("Bar Chart", plotOutput("bar")),
                                                       tabPanel("Description", tableOutput("tab")),
                                 )),
                                 
                             )),
                    tabPanel("Pre-Processing",
                             sidebarLayout(
                                 sidebarPanel(),#sidebar panel
                                 
                                 mainPanel(tabsetPanel(type = "tabs",
                                                       tabPanel("encoding", verbatimTextOutput("encode")),
                                                       tabPanel("Missing Value", verbatimTextOutput("tab_miss"),
                                                                fluidRow(h2("Data Duplikat setelah Preprocessing"),
                                                                         verbatimTextOutput("dup2"))),
                                                       tabPanel("Check Missing Values", plotOutput("aggr2")),
                                                       tabPanel("Clean Data", verbatimTextOutput("tab2")),
                                 )),
                             )),
                    tabPanel("Model",
                             sidebarLayout(
                                 sidebarPanel(
                                     selectInput(inputId = "num", 
                                                 label = "Benchmarks",
                                                 choices = c("All", "1", "2")),
                                 ), #sidebar panel
                                 
                                 mainPanel(tabsetPanel(type = "tabs", 
                                                       tabPanel("Random Forest", verbatimTextOutput("ranfor")),
                                                       tabPanel("Confusion Matrix", verbatimTextOutput("cm")),
                                                       tabPanel("Importance", verbatimTextOutput("imp"),
                                                                br(),
                                                                plotOutput("impvis")),
                                 )),
                                 
                             ))
                    
                    
                )# navbarPage
)#Fluid Page

#Define server function
server <- function(input, output) {
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        if (input$Y != "All") {
            dataset <- dataset[dataset$Y == input$Y,]
        }
        dataset
    }))
    
    # Descriptive statistics
    output$summary <- renderPrint({
        summary(dataset)
    })
    # Duplicate
    output$dup <- renderPrint({
        sum(duplicated(dataset))
    })
    
    #aggregat plot
    output$aggr <- renderPlot({
        aggr_plot <- aggr(dataset, col=c('navyblue','red'), numbers=TRUE, 
                          sortVars=TRUE, labels=names(dataset), cex.axis=.7,
                          gap=3, ylab=c("Histogram of missing data","Pattern"))
    })
    
    #Piechart
    output$bar <- renderPlot({
        if(input$var=='a') { i<-1 }     
        if(input$var=='b') { i<-2 }     
        if(input$var=='c') { i<-3 }
        if(input$var=='d') { i<-5 }
        if(input$var=='e') { i<-6 }
        if(input$var=='f') { i<-7 }
        if(input$var=='g') { i<-8 }
        if(input$var=='h') { i<-10 }
        if(input$var=='i') { i<-11 }
        if(input$var=='j') { i<-12 }
        if(input$var=='k') { i<-13 }
        if(input$var=='l') { i<-14 }
        if(input$var=='m') { i<-15 }
        if(input$var=='n') { i<-16 }
        if(input$var=='o') { i<-17 }
        if(input$var=='p') { i<-18 }
        if(input$var=='q') { i<-19 }
        if(input$var=='r') { i<-20 }
        if(input$var=='s') { i<-21 }
        if(input$var=='t') { i<-22 }
        if(input$var=='u') { i<-23 }
        if(input$var=='v') { i<-24 }
        if(input$var=='w') { i<-25 }
        if(input$var=='x') { i<-26 }
        if(input$var=='y') { i<-9 }
        if(input$var=='z') { i<-4 }
        
        variabel<- data_encodee[, i] 
        ggplot(data=data_encodee, aes(x=variabel, y="", fill=variabel)) +
            geom_bar(stat="identity")
    })
    #Description Table
    output$tab <- renderTable({
        var_desc <- read.csv("var_desc.csv", encoding="UTF-8", sep=";")
        var_desc
    })
    #Encoding
    output$encode <- renderPrint({
        summary(data_encodee)
        
    })
    #Missing Value Table
    output$tab_miss <- renderPrint({
        new_dataset <- read.csv("new_dataset.csv")
        new_dataset <- transform(
            new_dataset,
            Bar=as.factor(Bar),
            CoffeeHouse=as.factor(CoffeeHouse),
            CarryAway=as.factor(CarryAway),
            RestaurantLessThan20=as.factor(RestaurantLessThan20),
            Restaurant20To50=as.factor(Restaurant20To50))
        
        summary(new_dataset)
    })
    
    output$dup2 <- renderPrint({
        sum(duplicated(fix_data2))
    })
    #Data Bersih
    databersih <- read.csv("databersih.csv")
    output$aggr2 <- renderPlot({
        aggr_plot <- aggr(databersih, col=c('navyblue','red'), numbers=TRUE, 
                          sortVars=TRUE, labels=names(databersih), cex.axis=.7,
                          gap=3, ylab=c("Histogram of missing data","Pattern"))
    })
    #Fix Data
    output$tab2 <- renderPrint({
        summary(fix_data)
        
    })  
    #Random Forest
    output$ranfor <- renderPrint({
        model1
    })
    #Confusion Matrix 
    output$cm <- renderPrint({
        predValid <- predict(model1, ValidSet, type = "class")
        confusionMatrix(predValid, ValidSet$Y)
    })
    #Importance 
    output$imp <- renderPrint({
        if(input$num=='All') {j <- NULL}
        if(input$num=='1') { j<-1 }     
        if(input$num=='2') { j<-2 }
        importance(model1, type = j)
    })
    
    output$impvis <- renderPlot({
        varImpPlot(model1)
    })
    
    # #hist
    # output$hist <- renderPlot({
    #   x    <- data[, 4]
    #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #   qplot(x, data = data, geom = "histogram", breaks =bins, fill = 'steelblue', xlab = "Age in year")
    # })
    # 
    # #boxplot
    # output$bar <- renderPlot({
    #   if(input$s=='a') { i<-1 }     
    #   if(input$s=='b') { i<-2 }     
    #   if(input$s=='c') { i<-3 }     
    #   if(input$s=='d') { i<-4 }     
    #   if(input$s=='e') { i<-5 }     
    #   
    #   variabel<- data[, i]     
    #   qplot(variabel, data = data, geom = ("boxplot"), fill="green", xlab = "value")}) 
}

# Create shiny Object
shinyApp(ui = ui, server = server)