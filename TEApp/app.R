###
#  This is the transesterification app code, which implements the numeric integration strategy
#  developed by the biofuels team in Innovative Engineers UMN
###

library(shiny)

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
                        value = 1
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
           tableOutput("sim_df")
        )
    )
)

# load functions and define server logic
server <- function(input, output) {
        # loads in all relevant helper functions
        source("./Functions/loadFunct.R")
        loadfunct()
        print("functions loaded")
        
        # run simulation
        output$sim_df <- renderTable({
            input$go
            isolate({
                # calculate initial conditions
                if (input$inputType == "By Mass"){
                    print(input$alc_initial)
                    IC_df <- IC_vol(input$tg_initial, input$oh_initial, input$alc_initial)
                    print("by mass")
                }
                else{
                    print(input$oh_initial)
                    IC_df <- IC_wt(input$tg_initial, input$oh_initial, input$alc_initial)
                }
                print(IC_df)
                # calculate k values
                k_df <- k_set((input$temp_initial+273))
                
                # numerical integration
                sim_conc <- RK4(k_df, IC_df, input$t_length, dt = 2, 1.065)
                
                return(sim_conc)
            })
        })
        
        # generates plot upon trigger
        output$histPlot <- renderPlot({
                # button trigger
                input$go
                # isolated histogram creation
                isolate({
                x    <- mtcars[, 3]
                bins <- seq(min(x), max(x), length.out = input$temp_initial + 1)
                return(hist(x, breaks = bins, col = 'darkgray', border = 'white'))
                })
            })
        
        # generate concentration dataframe upon button trigger
        # output$conc_df <- renderTable({
        #     input$go
        #     isolate({
        #         return(output$sim_df[1:5,])
        #     })
        # })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
