library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(modelr)
library(gganimate)

exoplanetsTotApp <- read.csv("~/Downloads/planets_2020.06.10_11.33.06.csv", comment.char="#")

appSet <- exoplanetsTotApp %>%
  filter(!is.na(pl_bmassj)) %>%
  filter(!is.na(pl_radj)) %>%
  mutate(massT = ((pl_bmassj *  (1.898 * (10 ^ 27))) / (5.972 * (10 ^ 24))) ) %>%
  mutate(radT = ((pl_radj * 69911) / 6371)) %>%
  
  mutate(rapporto = (massT / radT) ) %>%
  mutate(massMerc = (massT * (5.972 * (10 ^ 24)) / (3.285 *(10 ^ 23))),
         massVen = (massT * (5.972 * (10 ^ 24)) / (4.867 *(10 ^ 24))),
         massMar = (massT * (5.972 * (10 ^ 24)) / (6.39 *(10 ^ 23))),
         massSat = (massT * (5.972 * (10 ^ 24)) / (5.683 *(10 ^ 26))),
         massUr = (massT * (5.972 * (10 ^ 24)) / (8.681 *(10 ^ 25))),
         massNett = (massT * (5.972 * (10 ^ 24)) / (1.024 *(10 ^ 26))),
         
         radMerc = (radT * 6371 / 2440),
         radVen = (radT * 6371 / 6052),
         radMar = (radT * 6371 / 3389.5),
         radSat = (radT * 6371 / 58232),
         radUr = (radT * 6371 / 25362),
         radNett = (radT * 6371 / 24622)
         ) %>%
mutate(rappMerc = (massMerc / radMerc)) %>%
  mutate(rappVen = (massVen / radVen)) %>%
  mutate(rappMar = (massMar / radMar)) %>%
  mutate(rappGio = (pl_bmassj / pl_radj)) %>%
  mutate(rappSat = (massSat /  radSat)) %>%
  mutate(rappUr = (massUr / radUr)) %>%
mutate(rappNett = (massNett / radNett)) 

# Define UI for application that draws a histogram
ui = fluidPage( 
  
  # Application title
  titlePanel("Esopianeti"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Input object: a slider called bins 


        radioButtons("pianeti", h3("Scelta pianeti:"),
                            choices = list("Mercurio" = 1, "Venere" = 2,
                                           "Terra" = 3, "Marte" = 4, 
                                           "Giove" = 5, "Saturno" = 6, 
                                           "Urano" = 7, "Nettuno" = 8), selected = 3),
        radioButtons("xaxis", "X",
                     choices = list("Massa Mercurio" = "massMerc", 
                                    "Massa Venere" = "massVen", 
                                    "Massa Terra" = "massT",
                                    "Massa Marte" = "massMar", 
                                    "Massa Giove" = "pl_bmassj", 
                                    "Massa Saturno" = "massSat",
                                    "Massa Urano" = "massUr", 
                                    "Massa Nettuno" = "massNett"), 
                     selected = "massT"),
        
        
        # y input
        radioButtons("yaxis", "Y",
                     choices = list("Raggio Mercurio" = "radMerc", 
                                    "Raggio Venere" = "radVen", 
                                    "Raggio Terra" = "radT",
                                    "Raggio Marte" = "radMar", 
                                    "Raggio Giove" = "pl_radj", 
                                    "Raggio Saturno" = "radSat",
                                    "Raggio Urano" = "radUr", 
                                    "Raggio Nettuno" = "radNett"), 
                     selected = "radT")
    ),
    
    # A reactive output object: a plot with name histPlot
    mainPanel(
      plotOutput("planetPlot")
    )
  )
)



server <- function(input, output) {
  output$value <- renderPrint({ input$pianeti })
  output$planetPlot <- renderPlot({
    
#    dataset = (ifelse((input$pianeti == "Terra"), filter(appSet, (rapporto>0.75) & (rapporto<1.25)),
 #              (ifelse((input$pianeti == "Mercurio"), filter(appSet, ( (((appSet$massMerc * (3.285 *(10 ^ 23) / 2440))/ appSet$radMerc) >0.75) & (((appSet$massMerc * (3.285 *(10 ^ 23) / 2440))  / appSet$radMerc) < 1.25) )),
  #              (ifelse((input$oianeti == "Venere"), filter(appSet,  (((appSet$massVen * (4.867 *(10 ^ 24) / 6052))/ appSet$radVen) >0.75) & (((appSet$massVen * (4.867 *(10 ^ 24) / 6052)) / appSet$radVen) <1.25) ),
   #              (ifelse((input$pianeti == "Marte"), filter(appSet,  (((appSet$massMar * (6.39 *(10 ^ 23) / 3389.5)) / appSet$radMar) >0.75) & (((appSet$massMar * (6.39 *(10 ^ 23) / 3389.5)) / appSet$radMar) <1.25) ),
    #              (ifelse((input$pianeti == "Giove"), filter(appSet,  (((pl_bmassj / pl_radj) / (1.898 *(10 ^ 27) / 69911)) >0.75) & ((((pl_bmassj / 69911) / (1.898 *(10 ^ 27) / 69911)) / pl_radj) <1.25) ),
     #              (ifelse((input$pianeti == "Saturno"), filter(appSet, (((appSet$massSat * (5.683 *(10 ^ 26) / 58232)) / appSet$radSat) >0.75) & (((appSet$massSat * (5.683 *(10 ^ 26) / 58232)) / appSet$radSat) <1.25) ),
      #              (ifelse((input$pianeti == "Urano"), filter(appSet, (((appSet$massUr * (8.681 *(10 ^ 25) / 25362)) / appSet$radUr) >0.75) & (((appSet$massUr (8.681 *(10 ^ 25) / 25362)) / appSet$radUr) <1.25) ),
       #              (ifelse((input$pianeti == "Nettuno"), filter(appSet, (((appSet$massNett * (1.024 *(10 ^ 26) / 24622)) / appSet$radNett) >0.75) & (((appSet$massNett * (1.024 *(10 ^ 26) / 24622)) / appSet$radNett) <1.25) )
        #                     )) )) )) )) )) )) )) )) 
    # generate dataset
    if (input$pianeti == 3) {
      dataset = filter(appSet, (rapporto>0.75) & (rapporto<1.25)) 
    } else if(input$pianeti == 1){
      dataset = appSet %>%
        filter((rappMerc >0.75) & (rappMerc < 1.25)) #1.35 x 10^20
    } else if(input$pianeti == 2){
      dataset = appSet %>% 
        filter((rappVen >0.75) & (rappVen < 1.25)) 
    } else if(input$pianeti == 4){
      dataset = appSet %>% 
        filter((rappMar >0.75) & (rappMar < 1.25)) 
    }  else if(input$pianeti == 5){
      dataset = appSet %>% 
        filter((rappGio >0.75) & (rappGio < 1.25)) 
    } else if(input$pianeti == 6){
      dataset = appSet %>% 
        filter((rappSat >0.75) & (rappSat < 1.25)) 
    } else if(input$pianeti == 7){
      dataset = appSet %>% 
        filter((rappUr >0.75) & (rappUr < 1.25)) 
    } else if(input$pianeti == 8){
      dataset = appSet %>% 
        filter((rappNett >0.75) & (rappNett < 1.25)) 
    }
    
    # draw the scatter plot
    ggplot(dataset, aes_string(input$xaxis, input$yaxis)) +
      geom_point(alpha = 0.5, show.legend = FALSE) +
      scale_size(range = c(2, 20)) +
      scale_x_log10() +
      scale_y_log10() +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

