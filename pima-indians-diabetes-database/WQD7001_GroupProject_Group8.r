#WQD7001
##Group Project
##Group 8

#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("here")
#install.packages("ggplot2")
#install.packages("rsconnect")
#install.packages("shinythemes")
#install.packages("fmsb")
#install.packages("dplyr")
#install.packages("randomForest")
#install.packages("ggridges")
#install.packages("e1071")
library(shiny)
library(shinydashboard)
library(here)
library(shiny)
library(ggplot2)
library(ggridges)
library(rsconnect)
library(shinythemes)
library(fmsb)
library(dplyr)
library(rsconnect)


diabetes <- read.csv("diabetes.csv")
diabetes[,2:6][diabetes[,2:6]==0] <- NA # replaces all zero values from column two to six with NA
diabetes <- na.omit(diabetes) # now we omit all NA values
diabetes$Outcome <- as.factor(diabetes$Outcome)
levels(diabetes$Outcome) <- c("No Diabetes","Diabetes")

#compute quantity for each group
diab.quantity <- diabetes %>%
  group_by(Outcome) %>% summarise(Count=n())

#compute the mean for each variable by each group
diab.mean <- diabetes %>%
  group_by(Outcome) %>% summarise_each(funs(mean),mean(diabetes[,1:8]))


#generic line initiating the SERVER
server <- shinyServer(function(input, output){
  
  output$table.1 <- renderTable(diab.quantity)
  
  output$table.2 <- renderTable({
    if(length(input$outcomeS)==1){
      if(input$outcomeS=='Diabetes'){
        diab.yes <- subset(diab.mean,Outcome=="Diabetes")
        diab.yes
      }
      else if(input$outcomeS=='No Diabetes'){
        diab.no <- subset(diab.mean,Outcome=="No Diabetes")
        diab.no
      }
    }
    else if(length(input$outcomeS)==2){
      diab.mean
    }
  })
  
  output$outcome1 <- renderPrint(input$outcomeS)
  output$outcome2 <- renderPrint(input$outcomeS)
  output$plot.1 <- renderPlot({
    if(length(input$outcomeS)==1){
      if(input$outcomeS=='Diabetes'){
        diab.yes <- subset(diab.mean,Outcome=="Diabetes")
        a<-diab.yes[2:9]
        diab.yes.mean<-rbind(c(10,200,100,50,300,50,1,80),rep(0,10),a)
        radarchart(diab.yes.mean,axistype=2,seg=5,pcol=rgb(0.8,0.2,0.5,0.9),pfcol=rgb(0.8,0.2,0.5,0.4),plwd=4,
                   cglcol="grey",cglty=1,axislabcol="grey",cglwd=0.8,vlcex=1.0)
      }
      else if(input$outcomeS=='No Diabetes'){
        diab.no <- subset(diab.mean,Outcome=="No Diabetes")
        b<-diab.no[2:9]
        diab.no.mean<-rbind(c(10,200,100,50,300,50,1,80),rep(0,10),b)
        radarchart(diab.no.mean,axistype=2,seg=5,pcol=rgb(0.2,0.5,0.5,0.9),pfcol=rgb(0.2,0.5,0.5,0.4),plwd=4,
                   cglcol="grey",cglty=1,axislabcol="grey",cglwd=0.8,vlcex=1.0)
      }
    }
    else if(length(input$outcomeS)==2){
      c<-diab.mean[2:9]
      diab.mean1<-rbind(c(10,200,100,50,300,50,1,80),rep(0,10),c)
      
      colors_border=c(rgb(0.2,0.5,0.5,0.9),rgb(0.8,0.2,0.5,0.9))
      colors_in=c(rgb(0.2,0.5,0.5,0.4),rgb(0.8,0.2,0.5,0.4))
      radarchart(diab.mean1,axistype=2,seg=5,pcol=colors_border,pfcol=colors_in,plwd=4,
                 cglcol="grey",cglty=1,axislabcol="grey",cglwd=0.8,vlcex=1.0)
      legend(2,1,legend=levels(as.factor(diab.mean$Outcome)),col=colors_border, 
             seg.len=2,border="transparent",pch=16,lty=1)
    }
  })
  
  output$outcome3 <- renderPrint(input$variable1)
  output$plot.2 <- renderPlot({
    ggplot(diabetes,aes(x=Outcome,y=diabetes[,input$variable1],col=Outcome,fill=Outcome)) +
      geom_violin(alpha = 0.8) +
      ylab(input$variable1) +
      scale_color_manual(values = c("red","blue")) +
      scale_fill_manual(values = c("red","blue"))
  })
  
  output$image <- renderImage({
    list(src = "BMI.png",height=250,width=370)
    }, deleteFile = FALSE)

  
  output$outcome4 <- renderPrint(input$variable2)
  output$plot.3 <- renderPlot({
    diabetes$bmiGroup<-findInterval(diabetes$BMI,c(0,18.5,24.9,29.9,34.9))
    diabetes<-diabetes %>% 
      rename(BMI_Range=bmiGroup) %>% 
      mutate(BMI_Range=as.factor(BMI_Range))
    levels(diabetes$BMI_Range)<-c("Underweight","Healthy","Overweight","Obese",
                                  "Extremely Obese")
    ggplot(diabetes,aes(x= diabetes[,input$variable2],y=BMI_Range,col=BMI_Range,fill=BMI_Range)) +
      geom_density_ridges() +
      xlab(input$variable2) +
      ylab("BMI Index") +
      theme_ridges() + 
      theme(legend.position = "none")
  })

  output$prediction<-renderPrint({
    # Splitting the dataset into the Training set and Test set
    library(caTools)
    set.seed(123)
    split = sample.split(diabetes$Outcome, SplitRatio = 0.80)
    training_set = subset(diabetes, split==TRUE)
    test_set = subset(diabetes, split==FALSE)
    
    # Fitting naive bayes to training set
    library(e1071)
    set.seed(123)
    classifier = naiveBayes(x = training_set[-9],
                              y = training_set$Outcome)
    
    
    # Predicting the Test set results
    prob_percent=predict(classifier,
                         newdata=data.frame("Pregnancies"=input$Pregnancies,"Glucose"=input$Glucose, 
                                            "BloodPressure"=input$BloodPressure,"SkinThickness"=input$SkinThickness,
                                            "Insulin"=input$Insulin,"BMI"=input$"BMI",
                                            "DiabetesPedigreeFunction"=input$DiabetesPedigreeFunction,
                                            "Age"=input$Age),type='raw')
    percent<-round(prob_percent[2]*100,3)
    
   if(percent>=50){
     cat(paste(" Your risk of getting diabetes is:", percent,"%","\n",
               "Kindly visit your nearest healthcare professional for personalized medical advice."))
     }
   else if(percent<50){
     cat(paste(" Great! Your risk of getting diabetes is:", percent,"%,lower than 50%.","\n",
               "Please continue your healthy lifestyle and do medical check up regularly"))
     }
  })
})

  
ui<-fluidPage(headerPanel('Pima Indian Diabetic Analysis and Prediction'),
              tabsetPanel(
                navbarMenu("Visualization",icon=icon("fas fa-chart-bar"),
                           tabPanel("Spider Chart",
                                    sidebarPanel(checkboxGroupInput('outcomeS','Please select the outcome',c('Diabetes','No Diabetes')),
                                                 submitButton(text="Submit",icon=icon("refresh"),width = NULL)),
                                    mainPanel(h5("Number of diabetes and no diabetes"),tableOutput("table.1"),
                                              h5("Spider Chart for"),verbatimTextOutput("outcome1"),plotOutput("plot.1"),
                                              h5("Mean of each variable for"),verbatimTextOutput("outcome2"),tableOutput("table.2"))),
                           tabPanel("Violin Plot",
                                    sidebarPanel(selectInput("variable1","Violin Plot",names(diabetes)[1:8],multiple=FALSE,selected="Insulin"),
                                                 submitButton(text="Submit",icon=icon("refresh"),width=NULL)),
                                    mainPanel(h5("Violin Plot of"),verbatimTextOutput("outcome3"),plotOutput("plot.2"))),
                           tabPanel("Ridgeline Chart",
                                    sidebarPanel(selectInput("variable2","Ridgeline Chart",names(diabetes)[1:8],multiple=FALSE,selected="Insulin"),
                                                 submitButton(text="Submit",icon=icon("refresh"),width=NULL),
                                                 imageOutput("image")),
                                    mainPanel(h5("Ridgeline Chart of"),verbatimTextOutput("outcome4"),plotOutput("plot.3")))),
               
                 tabPanel("Prediction",icon=icon("fas fa-ambulance"),
                         sidebarPanel(numericInput("Pregnancies","Please key in the value for number of times pregnant",value=0,min=0,max=50),
                                      numericInput("Glucose","Please key in the value for plasma glucose concentration(a 2 hours in an oral glucose tolerance test)",value=0,min=0,max=500),
                                      numericInput("BloodPressure","Please key in the value for diastolic blood pressure (mm Hg)",value=0,min=0,max=200),
                                      numericInput("SkinThickness","Please key in the value for triceps skin fold thickness (mm)",value=0,min=0,max=150),
                                      numericInput("Insulin","Please key in the value for 2-Hour serum insulin (mu U/ml)",value=0,min=0,max=1000),
                                      numericInput("BMI","Please key in the value for Body Mass Index",value=0,min=0,max=100),
                                      numericInput("DiabetesPedigreeFunction","Please key in the value for Diabetes Pedigree Function",value=0,min=0,max=5),
                                      numericInput("Age","Please key in the value for age",value=0,min=0,max=110),
                                      submitButton(text="Submit",icon=icon("refresh"),width=NULL)),
                         mainPanel(h3("Diabetes Disease Prediction"),verbatimTextOutput("prediction")))
                )
              )


shinyApp(ui=ui,server=server)
