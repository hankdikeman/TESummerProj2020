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
library(shinyjs)
print("packages loaded")

# loads in all relevant helper functions
source("./Functions/loadFunct.R")
loadfunct()
print("functions loaded")

#### UI ####
ui <- fluidPage(
        # boot up shinyjs
        useShinyjs(),
        # Loading message
        div(
                id = "loading-content",
                br(),br(),br(),
                h2("Reaction Module is Starting Up...", align = 'center')
        ),
        
        # Divider and hidden function for loading in app content
        hidden(
                div(
                id = "app-content",
        #### Overall Style and Set-up ####
        # webpage tab information
        tags$head(tags$title("Transesterification Reaction Module")),
        # CSS style sheet import
        includeCSS("www/styles.css"),
        # Webpage Title
        tags$br(),
        wellPanel(h1(
                strong("Transesterification Reaction Simulation Module"),
                align = 'center'
        )),
        # Division of layout into sidebar and main section
        sidebarLayout(
                #### Sidebar IC Entry Panel ####
                sidebarPanel(
                        # title of sidebar
                        tags$h3(tags$strong("Initial Conditions Entry"), align = 'center'),
                        tags$hr(),
                        # short blurb explanation
                        tags$p("Please select your initial reactant amounts, temperatures, and desired time of simulation below:"),
                        # selection of mass or volume input
                        radioButtons(
                                "inputType",
                                "Ingredient Entry Type",
                                choices = c("By Mass", "By Volume"),
                                inline = TRUE
                        ),
                        # triglyceride amount added entry
                        tags$p("Mass/Volume Triglyceride Added"),
                        numericInput("tg_initial",
                                     NULL,
                                     min = 0,
                                     value = 1000,),
                        # alcohol amount added entry
                        tags$p("Mass/Volume Alcohol Added"),
                        numericInput("alc_initial",
                                     NULL,
                                     min = 0,
                                     value = 200),
                        # sodium hydroxide added entry
                        tags$p("Mass/Volume NaOH Added"),
                        numericInput("oh_initial",
                                     NULL,
                                     min = 0,
                                     value = 2),
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
                        numericInput("t_length",
                                     NULL,
                                     min = 0,
                                     value = 150),
                        tags$p("Time step *[temporary]*"),
                        numericInput("step_size",
                                     NULL,
                                     min = 0.0001,
                                     value = 5),
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
                                        plotOutput("dispPlot") %>% withSpinner(color = "#000000")
                                ),
                                # selection of displayed graph
                                fluidRow(
                                        column(
                                                width = 4,
                                                selectInput(
                                                        "graph_select",
                                                        label = "Select the graph you would like to view",
                                                        choices = c("All Concentrations", "Product Gen Rate vs Time")
                                                )
                                        ),
                                        # selection of displayed species in concentration graph
                                        column(
                                                width = 8,
                                                conditionalPanel(condition = "input.graph_select == 'All Concentrations'",
                                                                 wellPanel(
                                                                         tags$div(
                                                                                 checkboxGroupInput(
                                                                                         "species_sel",
                                                                                         label = "Select Species for Concentration Graph",
                                                                                         inline = TRUE,
                                                                                         choiceNames = c("E", "TG", "DG", "MG", "ROH", "G", "S", "OH"),
                                                                                         choiceValues = c(1, 2, 3, 4, 5, 6, 7, 8),
                                                                                         selected = c(1, 2, 3, 4, 5, 6, 7, 8)
                                                                                 ),
                                                                                 align = 'center'
                                                                         )
                                                                 ),
                                                )
                                        )
                                ),
                                tags$hr(),
                                wellPanel(
                                        fluidRow(# selection of timepoint of interest
                                                column(
                                                        width = 12,
                                                        tags$p("Select the Timepoint of Displayed Concentrations (minutes)", align = 'center'),
                                                        sliderInput(
                                                                "timept_select",
                                                                NULL,
                                                                min = 0,
                                                                max = 150,
                                                                value = 0,
                                                                width = "100%"
                                                        )
                                                )
                                        ),
                                        hr(),
                                        # table displaying species concentrations and title of table
                                        tags$p(paste("Species Concentrations (mol/L)"), align = 'center'),
                                        dataTableOutput("sim_tab"),
                                        hr(),
                                        plotOutput("yield_pie_plot")
                                        
                                )
                        )
                )
        )
))
)

#### Server Backend ####
server <- function(input, output, session) {
        #### Initial Reactants Calculation ####
        sim_temp <- eventReactive(input$go,input$temp_initial)
        
        IC_df <- eventReactive(input$go, {
            print(paste("IC generated", input$go))
            if (input$inputType == "By Mass") {
                IC_df <- IC_wt(input$tg_initial, input$oh_initial, input$alc_initial)
            }
            else{
                IC_df <- IC_vol(input$tg_initial, input$oh_initial, input$alc_initial)
            }
            print(IC_df)
        })
        
        ### Scale factor generation function 
        scl_fctr <- eventReactive(input$go, {
                if (input$inputType == "By Mass") {
                    scl_fctr <- scale_factor_wt(input$tg_initial, input$oh_initial, input$alc_initial)
                }
                else{
                    scl_fctr <- scale_factor_vol(input$tg_initial, input$oh_initial, input$alc_initial)
                }
                print(scl_fctr)
        })
        
        #### RK4 Simulation ####
        sim_df <- eventReactive(input$go, {
            print(paste("reaction simulated",input$go))
        
            # calculate k values
            k_df <- k_set((input$temp_initial + 273)) / 60
            # numerical integration
            sim_conc <-
                RK4(k_df, IC_df(), input$t_length, input$step_size, scl_fctr())
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
            
            # Re-dimensionalize concentration values in data table to molarity
            tp_df[1,2:9] <- tp_df[1,2:9]*scl_fctr()

            # rounds concentration and time values to friendlier looking form
            tp_df[1,1] <- as.numeric(format(round(tp_df[1,1],1), nsmall = 3))
            for(colval in 2:ncol(tp_df)){
                    tp_df[1,colval] <- as.numeric(format(round(tp_df[1,colval],3), nsmall = 3))
            }
            tab_df <- tp_df[1,2:ncol(tp_df)]
            rownames(tab_df) <- c(paste(tp_df[1,1], "min"))
            colnames(tab_df) <- c("Ester", "TriG", "DiG", "MonoG", "Alcohol", "Glycerol", "Soap", "Hydroxide")
            
            # render data table of values
            output$sim_tab <- renderDataTable(tab_df[1,])
        })
        
        # Change slider length to match time input
        observeEvent(input$go, {
                slider_length <- input$t_length
                updateSliderInput(
                        session,
                        "timept_select",
                        value = 0,
                        min = 0,
                        max = slider_length)
        })

        #### Generate All Graphs ####
        # generates graphs upon simulation trigger
        
        #### Selection of Displayed Plot ####
        observeEvent(c(input$graph_select,input$disp_species), ({
                output$dispPlot <- renderPlot({
                        switch(
                                input$graph_select,
                                "All Concentrations" = totalConcPlot(sim_df(), sim_temp(), IC_df(),input$species_sel),
                                "Product Gen Rate vs Time" = progConcBar(sim_df(), sim_temp())
                        )
                }) 
        }))
        
        # hide loading screen, show rest of content
        Sys.sleep(0.5)
        hide(id = "loading-content", anim = TRUE, animType = "slide")    
        show("app-content")
}

# Run the application 
shinyApp(ui = ui, server = server)
