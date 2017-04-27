rm(list = ls())
cat("\014")

library(shiny)
library(ggplot2)
library(ggvis)
library(dplyr)
library(tidyr)
library(scales)
if (!require(GGally)){
  install.packages("GGally", repos="http://cran.us.r-project.org")
}
library(GGally)



facebook <- read.csv("dataset_Facebook.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")
# facebook["like"] <- rescale(facebook["like"])
facebook$Post.Weekday <- factor(facebook$Post.Weekday, labels = c("Sunday", "Monday", "Tuesday", 
                                                              "Wednesday", "Thursday", 
                                                              "Friday", "Saturday"), ordered = TRUE)
facebook$Post.Month <- factor(facebook$Post.Month, labels = c("January", "February", "March", "April", "May",
                                                               "June", "July", "August", "September", "October", 
                                                               "November", "December"), ordered = TRUE)

ui <- shinyUI(fluidPage(
  navbarPage("Facebook Page Performance Visualization",
             ####### Heatmap #######
             tabPanel('Heatmap',
                      fluidPage(
                      fluidRow(column(2,
                        radioButtons("useraction", label = h3("User Actions"),
                                     choices = list("Comment" = "comment", "Like" = "like", "Share" = "share"), 
                                     selected = "comment"),
                        radioButtons("monthweekday", label = h3("Time Frame"),
                                     choices = list("Weekday" = "Post.Weekday", "Month" = "Post.Month"), 
                                     selected = "Post.Weekday")),
                      column(10,
                        plotOutput("heatmap"))))
                      
                      
             ),
             ####### Small Multiples #######
             tabPanel('Small Multiples',
                      fluidPage(
                        fluidRow(column(2,
                                        radioButtons("monthweekdayhour", label = h3("Time Frame"),
                                                     choices = list("Hour" = "Post.Hour", "Weekday" = "Post.Weekday", 
                                                                    "Month" = "Post.Month"
                                                                    ), 
                                                     selected = "Post.Hour"),
                                        radioButtons("impclick", label = h3("Impression and Click"),
                                                     choices = list("Impression" = "Lifetime.Post.Total.Impressions", 
                                                                    "Click" = "Lifetime.Post.Consumers"), 
                                                     selected = "Lifetime.Post.Total.Impressions")),
                                 column(10,
                                        plotOutput("smallmul", height="600px")))
                      )
             ),
             ####### PCP #######
             tabPanel('Parallel Coordinates Plot',
                      fluidPage(
                        fluidRow(column(2,
                                        radioButtons("weekday", label = h3("Weekdays"),
                                                     choices = list("Sunday" = "Sunday", "Monday" = "Monday",
                                                                    "Tuesday" = "Tuesday", "Wednesday" = "Wednesday",
                                                                    "Thursday" = "Thursday", "Friday" = "Friday",
                                                                    "Saturday" = "Saturday"), 
                                                     selected = "Sunday")
                                                     ),
                                 column(10,
                                        plotOutput("pcp", height="600px")))
                      )
  )
  )
)
)

server <- function(input, output) {

  selectcol <- reactive({facebook[input$useraction]})
  selectcol2 <- reactive({facebook[input$monthweekday]})
  selectcol3 <- reactive({facebook[input$monthweekdayhour]})
  selectcol4 <- reactive({facebook[input$impclick]})
  
  tempdf <- reactive({
    tempdf <- data.frame(selectcol2(), facebook[c("Post.Hour")], selectcol())
    names(tempdf) <- c("Post.Time", "Post.Hour", "interaction")
    tempdf <- tempdf[!is.na(tempdf$interaction), ]
    tempdf <- tempdf[tempdf$interaction < 10*mean(tempdf$interaction), ]
    tempdf %>% 
      group_by_(.dots=c("Post.Time","Post.Hour")) %>% 
      summarize(interaction=mean(interaction))
    tempdf
  })
  
  print(reactive({tempdf()}))
  
  tempdf2 <- reactive({
    tempdf2 <- data.frame(selectcol3(), facebook[c("Type", "Paid")], selectcol4())
    names(tempdf2) <- c("Post.Time", "Type", "Paid", "ImpClick")
    tempdf2 <- tempdf2[!is.na(tempdf2$Paid), ]
    tempdf2
    })
  
  tempdf3 <- reactive({
    tempdf3 <- facebook[c("Type", "like", "comment", "share", "Post.Weekday")]
    tempdf3 <- tempdf3[(!is.na(tempdf3$Type)  & !is.na(tempdf3$like) &
                          !is.na(tempdf3$comment) & !is.na(tempdf3$share) & 
                          !is.na(tempdf3$Post.Weekday) & !is.na(tempdf3$Post.Weekday)), ]
    tempdf3["Type"] <- as.factor(tempdf3[, "Type"])
    tempdf3 = tempdf3[tempdf3$Post.Weekday==input$weekday, ]
    tempdf3
  })
  
  output$heatmap <- renderPlot({
    ggplot(tempdf(), aes(Post.Hour, Post.Time)) +
      geom_tile(aes(fill = interaction)) +
      scale_fill_gradient(low = "floralwhite", high = "red", 
                          guide = guide_legend(title = input$useraction, ncol = 6, byrow = TRUE)) + 
      scale_x_continuous(expand = c(0, 0), breaks = seq(0, 24, 1)) + scale_y_discrete(expand = c(0, 0)) + 
      labs(x = "\nHour of Day", y = "Weekday\n") + 
      theme(panel.background = element_rect(fill = "floralwhite", colour = "white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "bottom")
  })
  
  output$smallmul <- renderPlot({
    ggplot(data=tempdf2(), aes(x=Type, y=ImpClick, fill=factor(Paid))) +
      geom_bar(stat="identity", position=position_dodge()) + 
      labs(fill = "Paid Facebook") +
      facet_wrap(~Post.Time) + scale_fill_discrete(labels = c("Yes", "No")) +
      ggtitle("Small Multiples in R") + scale_y_continuous(labels = comma) +
      theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
      theme(axis.text.x = element_text(angle=90))
  })
  
  output$pcp <- renderPlot({
    ggparcoord(data = tempdf3(),                 
               columns = c("comment", "like", "share"),                 
               groupColumn = "Type",
               order = c(2:4),                
               showPoints = FALSE,                
               alphaLines = 0.6,                
               shadeBox = NULL,                
               scale = "uniminmax"
    ) + theme(axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#bbbbbb"),
            panel.background = element_rect(fill = "white", color = "grey40", size=0.5),
            legend.position = "bottom"
            )
  })


}

shinyApp(ui = ui, server = server)
