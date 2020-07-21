###
#  This is the transesterification app code, which implements the numeric integration strategy
#  developed by the biofuels team in Innovative Engineers UMN
###
#### Set-Up and Loading Packages ####
# packages loaded
library(shiny)
library(tidyverse)
library(shinycssloaders)
library(DT)
print("packages loaded")

# loads in all relevant helper functions
source("./Functions/loadFunct.R")
loadfunct()
print("functions loaded")

#### UI ####
ui <- fluidPage(#### Overall Style and Set-up ####
                # CSS style sheet import
                includeCSS("www/styles.css"),
                # Webpage Title
                wellPanel(h2(
                        strong("Transesterification Reaction Simulation Module"),
                        align = 'center'
                )),
                # Division of layout into sidebar and main section
                sidebarLayout(
                        #### Sidebar IC Entry Panel ####
                        sidebarPanel(
                                # title of sidebar
                                tags$h3(tags$strong("Initial Conditions Entry"), align = 'center'),
                                tags$br(),
                                tags$hr(),
                                # short blurb explanation
                                tags$p(
                                        "Please select your initial reactant amounts, temperatures, and desired time of simulation below:"
                                ),
                                # selection of mass or volume input
                                radioButtons(
                                        "inputType",
                                        "Ingredient Entry Type",
                                        choices = c("By Mass", "By Volume"),
                                        inline = TRUE
                                ),
                                # triglyceride amount added entry
                                tags$p("Mass/Volume Triglyceride Added"),
                                numericInput(
                                        "tg_initial",
                                        NULL,
                                        min = 0,
                                        value = 1000
                                ),
                                # alcohol amount added entry
                                tags$p("Mass/Volume Alcohol Added"),
                                numericInput(
                                        "alc_initial",
                                        NULL,
                                        min = 0,
                                        value = 200
                                ),
                                # sodium hydroxide added entry
                                tags$p("Mass/Volume NaOH Added"),
                                numericInput(
                                        "oh_initial",
                                        NULL,
                                        min = 0,
                                        value = 2
                                ),
                                tags$hr(),
                                # reaction temperature entry
                                tags$p("Reaction Temperature  (ºC)"),
                                sliderInput(
                                        "temp_initial",
                                        NULL,
                                        min = 20,
                                        max = 70,
                                        value = 25
                                ),
                                tags$hr(),
                                # length of integration
                                tags$p("Total Time of Integration (minutes)"),
                                numericInput(
                                        "t_length",
                                        NULL,
                                        min = 0,
                                        value = 150
                                ),
                                tags$p("Time step *[temporary]*"),
                                numericInput(
                                        "step_size",
                                        NULL,
                                        min = 0.0001,
                                        value = 5
                                ),
                                tags$hr(),
                                # time step of integration
                                # "go" button
                                actionButton(inputId = "go",
                                             label = "Generate Reaction Simulation")
                        ),
                        #### Main Results Panel ####
                        # Show a plot of the generated distribution
                        mainPanel(
                                conditionalPanel(
                                        # only shows results stuff once go button has been clicked
                                        condition = "input.go != 0",
                                        # results wellPanel
                                        wellPanel(
                                                # title of main panel
                                                tags$h3(strong("Simulation Results"), align = 'center'),
                                                tags$br(),
                                                # addition of concentration or other plot
                                                plotOutput("concPlot") %>% withSpinner(color = "#000000")),
                                        # selection of displayed graph
                                        tags$p("Select the graph you would like to view:"),
                                        selectInput(
                                                "graph_select",
                                                NULL,
                                                choices = c("graph1", "graph2")
                                        ),
                                        tags$hr(),
                                        fluidRow(
                                                # selection of timepoint of interest
                                                tags$p("Select the Timepoint of Displayed Concentrations"),
                                                sliderInput(
                                                        "timept_select",
                                                        NULL,
                                                        min = 0,
                                                        max = 150,
                                                        value = 0,
                                                        width = 800
                                                ),
                                                # table displaying species concentrations and title of table
                                                tags$p(
                                                        paste(
                                                                "Dimensionless Species Concentrations At Selected Timepoint"
                                                        ),
                                                        align = 'center'
                                                ),
                                                column(12,
                                                dataTableOutput("sim_tab")),
                                                plotOutput("yield_pie_plot")
                                        )
                                )
                        )
                ))

#### Server Backend ####
server <- function(input, output) {
        #### Initial Reactants Calculation ####
        IC_df <- eventReactive(input$go, {
            print(paste("IC generated", input$go))
            if (input$inputType == "By Mass") {
                IC_df <- IC_vol(input$tg_initial, input$oh_initial, input$alc_initial)
            }
            else{
                IC_df <- IC_wt(input$tg_initial, input$oh_initial, input$alc_initial)
            }
            print(IC_df)
            IC_df
        })
        
        #### RK4 Simulation ####
        sim_df <- eventReactive(input$go, {
            print(paste("reaction simulated",input$go))
            # calculate k values
            k_df <- k_set((input$temp_initial + 273)) / 60
            # numerical integration
            sim_conc <-
                RK4(k_df, IC_df(), input$t_length, input$step_size, 1.065)
            # setting timepoint variable, rearranging so it's first
            sim_conc[, (ncol(sim_conc) + 1)] <-
                seq(
                    from = 0,
                    to = input$t_length,
                    length.out = nrow(sim_conc)
                )
            colnames(sim_conc)[ncol(sim_conc)] <- "minutes"
            # set concentration dataframe as output
            return(sim_conc)
        })
        
        #### Determine Displayed Timepoint and Pie Chart ####
        observeEvent(c(input$go, input$timept_select), {
            # find concentrations at the selected timepoint value
            print(paste("timepoint changed", input$go))
            tp_df <- filter(sim_df(), minutes >= input$timept_select) %>%
                    select(minutes, E:OH)
            # generate pie plot at timepoint
            slices <- c(tp_df$E[1], (tp_df$TG[1] * 3 + tp_df$DG[1] * 2 + tp_df$MG[1]), tp_df$S[1])
            perc_conv <- c(format(round(slices[1]/3*100,1), nsmall = 1), format(round(slices[2]/3*100,1), nsmall = 1),format(round(slices[3]/3*100,1), nsmall = 1))
            output$yield_pie_plot <-
                renderPlot(pie(
                    slices,
                    labels = c(paste("Converted Fatty Acids",perc_conv[1],"%"), paste("Unconverted Fatty Acids",perc_conv[2],"%"), paste("Saponified Fatty Acids",perc_conv[3],"%")),
                    main = paste("Conversion Percentage at",input$timept_select,"minutes")
                ))
            # rounds concentration and time values to friendlier looking form
            tp_df[1,1] <- as.numeric(format(round(tp_df[1,1],1), nsmall = 3))
            for(colval in 2:ncol(tp_df)){
                    tp_df[1,colval] <- as.numeric(format(round(tp_df[1,colval],3), nsmall = 3))
            }
            colnames(tp_df) <- c("Time (min)", "Ester", "Triglyceride", "Diglyceride", "Monoglyceride", "Alcohol", "Glycerol", "Soap", "Hydroxide")
            # render data table of values
            output$sim_tab <- renderDataTable(tp_df[1, ])
        })
        #### Generate Concentration Plot ####
        # generates plot upon trigger
        observeEvent(input$go, ({
            output$concPlot <- renderPlot({
                isolate({
                print(paste("plot made", input$go))
                speciesPlot <-
                    ggplot(data = sim_df(), aes(minutes)) +
                    geom_line(aes(y = E / (3), color = "Ester")) +
                    geom_line(aes(y = TG, color = "Triglyceride")) +
                    geom_line(aes(y = DG, color = "Diglyceride")) +
                    geom_line(aes(y = MG, color = "Monoglyceride")) +
                    geom_line(aes(y = ROH / IC_df()[1, 5], color = "Alcohol")) +
                    geom_line(aes(y = OH / IC_df()[1, 8], color = "Hydroxide")) +
                    geom_line(aes(y = G, color = "Glycerol")) +
                    geom_line(aes(y = S, color = "Soap")) +
                    labs(title = "Species Concentration as a Function of Time", subtitle = paste("Temp = ", input$temp_initial, "ºC")) +
                    xlab("time (min)") +
                    ylab("Normalized Species Concentration") +
                    scale_color_discrete(name = "Reaction Species")
                return(speciesPlot)
                })
            })
        }))
}

# Run the application 
shinyApp(ui = ui, server = server)
