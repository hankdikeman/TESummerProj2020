###
#  This is the transesterification app code, which implements the numeric integration strategy
#  developed by the biofuels team in Innovative Engineers UMN
###

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Webpage Title
    titlePanel("Transesterification Reaction Simulation Module"),
    # Division of layout into sidebar and main section
    sidebarLayout(
        sidebarPanel(
            # selection of mass or volume input
            radioButtons(
                        "inputType",
                        "Ingredient Entry Type",
                        choices = c("By Mass","By Volume"),
                        inline = TRUE
            ),
            # triglyceride amount added entry
            numericInput(
                        "tg_initial",
                        "Mass/Volume Triglyceride Added",
                        min = 0,
                        value = 1000
                         ),
            # alcohol amount added entry
            numericInput(
                        "alc_initial",
                        "Mass/Volume Alcohol Added",
                        min = 0,
                        value = 200
                        ),
            # sodium hydroxide added entry
            numericInput(
                        "oh_initial",
                        "Mass/Volume NaOH Added",
                        min = 0,
                        value = 2
                        ),
            # reaction temperature entry
            sliderInput(
                        "temp_initial",
                        "Reaction Temperature  (ºC)",
                        min = 20,
                        max = 70,
                        value = 25
            ),
            br(),
            # length of integration
            numericInput(
                "t_length",
                "Total Time of Integration (minutes)",
                min = 0,
                value = 1000
            ),
            # "go" button
            actionButton(
                inputId = "go",
                label = "Generate Reaction Simulation"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # addition of concentration or other plot
           plotOutput("histPlot"),
           # selection of displayed graph
           selectInput(
               "graph_select",
               "Select the graph you would like to view:",
               choices = c("graph1","graph2")
           ),
           # selection of timepoint of interest
           sliderInput(
               "timept_select",
               "Select the Timepoint of Displayed Concentrations",
               min = 0,
               max = 1000,
               value = 0,
               width = 800
           ),
           # table displaying species concentrations and title of table
           p(paste("Display of Species Concentrations At Specific Timepoint")),
           tableOutput("sim_tab")
        )
    )
)

# load functions and define server logic
server <- function(input, output) {
        # loads in all relevant helper functions
        source("./Functions/loadFunct.R")
        loadfunct()
        print("functions loaded")
        
        IC_df <- reactive({
            print(paste("IC generated",input$go))
            isolate({
                if (input$inputType == "By Mass"){
                    IC_df <- IC_vol(input$tg_initial, input$oh_initial, input$alc_initial)
                }
                else{
                    IC_df <- IC_wt(input$tg_initial, input$oh_initial, input$alc_initial)
                }
                print("used diff function")
                print(IC_df)
                IC_df
            })
        })
        
        # run simulation
        sim_df <- reactive({
            print(paste("reaction simulated",input$go))
            isolate({
                # calculate initial conditions
                if (input$inputType == "By Mass"){
                    IC_df <- IC_vol(input$tg_initial, input$oh_initial, input$alc_initial)
                }
                else{
                    IC_df <- IC_wt(input$tg_initial, input$oh_initial, input$alc_initial)
                }
                print(IC_df)
                # calculate k values
                k_df <- k_set((input$temp_initial+273))/60
                
                # numerical integration
                sim_conc <- RK4(k_df, IC_df, input$t_length, dt = 5, 1.065)
                sim_conc[,(ncol(sim_conc)+1)] <- seq(from = 0, to = input$t_length, length.out = nrow(sim_conc))
                colnames(sim_conc)[ncol(sim_conc)] <- "timept"
                # set concentration dataframe as output
                return(sim_conc)
            })
        })
        
        output$sim_tab <- renderTable({
            print(paste("table made",input$go))
            sim_df()
            })
        
        # generates plot upon trigger
        output$concPlot <- renderPlot({
                print(paste("plot made",input$go))
                ggplot(data = sim_df(), aes(timept)) + 
                    geom_line(aes(y = E/(3), color = "Ester")) + 
                    geom_line(aes(y = TG, color = "Triglyceride")) + 
                    geom_line(aes(y = DG, color = "Diglyceride")) + 
                    geom_line(aes(y = MG, color = "Monoglyceride")) + 
                    geom_line(aes(y = ROH/4.7, color = "Alcohol")) + 
                    geom_line(aes(y = OH/.12, color = "Hydroxide")) + 
                    geom_line(aes(y = G, color = "Glycerol")) + 
                    geom_line(aes(y = S, color = "Soap")) + 
                    labs(title = "Species Concentration as a Function of Time", subtitle = "Room temp") +
                    xlab("time (min)") + 
                    ylab("Normalized Species Concentration") +
                    scale_color_discrete(name = "Reaction Species")
            })
}

# Run the application 
shinyApp(ui = ui, server = server)
