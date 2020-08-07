###
#  This is the transesterification webpage code, which implements the numeric integration strategy
#  developed by the biofuels team in Innovative Engineers UMN
###
#### Set-Up and Loading Packages ####
# packages loaded
library(shiny)
library(tidyverse)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
# change plot font size
theme_set(
        theme_minimal(base_size = 17)
)
print("packages loaded, theme set")

# loads in all relevant helper functions
source("./Functions/loadFunct.R")
loadfunct()
print("functions loaded")

#### UI ####
ui <- fluidPage(
        # boot up shinyjs
        useShinyjs(),
        # boot up alert system
        useSweetAlert(),
        setBackgroundImage(src = "DarkTurbineBackground.jpg"),
        # Loading message
        div(
                id = "loading-content",
                br(),br(),br(),
                tags$div(
                        tags$img(src = "IELogo2020.png", width = "45%"),
                        align = 'center'
                ),
                br(),br(),br(),
                h2("Reaction Module is Starting Up...", align = 'center', style = "font-size: 400%;"),
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
                        tags$h2(tags$strong("Initial Conditions Entry"), align = 'center'),
                        # short blurb explanation
                        tags$p("Please enter your initial reactant amounts, temperatures, and desired length of simulation below:", align = 'center'),
                        tags$hr(),
                        # selection of mass or volume input
                        radioGroupButtons(
                                "inputType",
                                "Ingredient Entry Type",
                                choices = c("By Mass", "By Volume"),
                                justified = TRUE
                        ),
                        # triglyceride amount added entry
                        textOutput("TG_Label"),
                        numericInput("tg_initial",
                                     NULL,
                                     min = 0,
                                     value = 1000,),
                        # alcohol amount added entry
                        textOutput("Alc_Label"),
                        numericInput("alc_initial",
                                     NULL,
                                     min = 0,
                                     value = 200),
                        # sodium hydroxide added entry
                        textOutput("OH_Label"),
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
                                     value = 90),
                        tags$p("Please select the desired simulation precision (minutes)"),
                        # label above precision slider, showing effects on precision
                        fluidRow(column(6,
                                        tags$p("More Precise",
                                               align = 'left', style = "font-size: 85%;")),
                                 column(6,
                                        tags$p("Less Precise",
                                               align = 'right', style = "font-size: 85%;"))
                        ), 
                        sliderTextInput(
                                "precision_sel",
                                NULL,
                                choices = c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10),
                                selected = 5,
                                grid = T
                        ), 
                        
                        tags$hr(),
                        # time step of integration
                        # "go" button
                        tags$div(
                                actionBttn(inputId = "go",
                                        label = "Generate Reaction Simulation",
                                        style = "jelly",
                                        color = "primary",
                                        size = "md"
                                   ),
                                align = 'center'
                        )
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
                                        tags$h2(strong("Simulation Results"), align = 'center')
                                ),
                                wellPanel(
                                        tags$div(
                                                id = "blurb",
                                                textOutput("blurb_explanation")
                                        )
                                ),
                                wellPanel(
                                        # addition of concentration or other plot
                                        plotOutput("dispPlot") %>% withSpinner(color = "#000000")
                                ),
                                # selection of displayed graph
                                fluidRow(
                                        column(
                                                width = 4,
                                                tags$br(),
                                                selectInput(
                                                        "graph_select",
                                                        label = "Select the graph you would like to view",
                                                        choices = c("All Concentrations", "Yield vs. Time", "Catalyst Activity", "Relative Rates of Ester:Soap", "Emulisification Risk Assessment")
                                                )
                                        ),
                                        # selection of displayed species in concentration graph
                                        column(
                                                width = 8,
                                                conditionalPanel(condition = "input.graph_select == 'All Concentrations'",
                                                                 wellPanel(
                                                                         tags$div(
                                                                                 id = "selection_species",
                                                                                 checkboxGroupButtons(
                                                                                         "species_sel",
                                                                                         label = "Select Species for Concentration Graph",
                                                                                         choiceNames = c("E", "TG", "DG", "MG", "ROH", "G", "S", "OH"),
                                                                                         choiceValues = c(1, 2, 3, 4, 5, 6, 7, 8),
                                                                                         selected = c(1, 2, 3, 4, 5, 6, 7, 8),
                                                                                         justified = TRUE,
                                                                                         checkIcon = list(yes = icon("ok",
                                                                                                                     lib = "glyphicon"))
                                                                                         ),
                                                                                 align = 'center'
                                                                         )
                                                                 ),
                                                ),
                                                conditionalPanel(condition = "input.graph_select == 'Yield vs. Time'",
                                                                 wellPanel(
                                                                         tags$p("Select Accumulation Timepoint (minutes)", align = 'center'),
                                                                         sliderInput(
                                                                                 "gen_rate_slider",
                                                                                 NULL,
                                                                                 min = 0,
                                                                                 max = 90,
                                                                                 value = 0,
                                                                                 width = "100%"
                                                                         )
                                                                 )
                                                ),
                                                conditionalPanel(condition = "input.graph_select == 'Relative Rates of Ester:Soap'",
                                                                 wellPanel(
                                                                         tags$p("Select Accumulation Timepoint (minutes)", align = 'center'),
                                                                         sliderInput(
                                                                                 "rel_rates_slider",
                                                                                 NULL,
                                                                                 min = 0,
                                                                                 max = 90,
                                                                                 value = 0,
                                                                                 width = "100%"
                                                                         )
                                                                 )
                                                )
                                                
                                        )
                                ),
                                tags$hr(),
                                wellPanel(
                                        fluidRow(# selection of timepoint of interest
                                                column(
                                                        width = 12,
                                                        tags$p("Select the Timepoint of Interest (minutes)", align = 'center'),
                                                        sliderInput(
                                                                "timept_select",
                                                                NULL,
                                                                min = 0,
                                                                max = 90,
                                                                value = 0,
                                                                width = "100%"
                                                        )
                                                )
                                        ),
                                        tags$hr(),
                                        # table displaying species concentrations and title of table
                                        tags$div(textOutput("table_label"), align = 'center'),
                                        dataTableOutput("sim_tab"),
                                        hr(),
                                        plotOutput("yield_pie_plot"),
                                        tags$hr(),
                                        tags$h4("Download Raw Data From Reaction Simulation", align = 'center'),
                                        fluidRow(column(6,
                                                        tags$div(
                                                                tags$p("Concentration Data (mol/L)"),
                                                                downloadBttn("download_sim_conc",
                                                                             label = "Download Simulated Concentration Data As .csv Document",
                                                                             size = "xs"),
                                                                align = 'center'
                                                        )),
                                                 column(6,
                                                        tags$div(
                                                                tags$p("Mass Composition Data (g)"),
                                                                downloadBttn("download_sim_mass",
                                                                             label = "Download Simulated Weight Data As .csv Document",
                                                                             size = "xs"),
                                                                align = 'center'
                                                        )))
                                        
                                        
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
        
        # Document what initial conditions for displayed results, pass to output variable
        observeEvent(sim_df(), ({
                        if (input$inputType == "By Mass"){
                                output$blurb_explanation <- renderText(
                                        isolate(
                                                paste(
                                                        "Triglyceride Mass (g) =",input$tg_initial,
                                                        "g  ||  Alcohol Mass (g) =",input$alc_initial,
                                                        "g  ||  Sodium Hydroxide Mass (g) =",input$oh_initial,
                                                        "g  ||  Reaction Temperature (ºC) =",input$temp_initial,"ºC"
                                                )
                                        )
                                )
                        }
                        else{
                                output$blurb_explanation <- renderText(
                                        isolate(
                                                paste(
                                                        "Triglyceride Vol (mL) =",input$tg_initial,
                                                        "mL  ||  Alcohol Vol (mL) =",input$alc_initial,
                                                        "mL  ||  Sodium Hydroxide Mass (g) =",input$oh_initial,
                                                        "g  ||  Reaction Temperature (ºC) =",input$temp_initial,"ºC"
                                                )
                                        )
                                )
                        }
        }))
        
        # Function to convert input masses/volumes into IC data frame form 
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
        
        # Function to generate volume based on initial reactant inputs (constant volume assumptiuon used)
        get_vol <- eventReactive(input$go, {
                # Densities of reactants (g/L or M)
                pTG <- 0.93*1000
                pAlc <- 0.791*1000
                if (input$inputType == "By Mass") {
                        get_vol <- (input$tg_initial/pTG + input$alc_initial/pAlc)
                }
                else{
                        get_vol <- (input$tg_initial + input$alc_initial)/1000
                }
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
        
        output$table_label <- renderText(
                paste("Simulated Species Mass and Concentration Values at",input$timept_select,"min")
        )
        
        #### RK4 Simulation ####
        sim_df <- eventReactive(input$go, {
            print(paste("reaction simulated",input$go))
        
            # calculate k values
            k_df <- k_set((input$temp_initial + 273)) / 60
            # numerical integration
            sim_conc <-
                RK4(k_df, IC_df(), input$t_length, input$precision_sel, scl_fctr())
            # setting timepoint variable
            sim_conc[, (ncol(sim_conc) + 1)] <-
                seq(
                    from = 0,
                    to = input$t_length,
                    length.out = nrow(sim_conc)
                )
            colnames(sim_conc)[ncol(sim_conc)] <- "minutes"
            
            # warning message for failed simulation
            failcondition <- (anyNA.data.frame(sim_conc[2,]) || any(sim_conc[2,] < 0))
            if(failcondition){
                    sendSweetAlert(
                            session = session,
                            title = "Warning!",
                            text = "Simulation precision may be too low for specified conditions, higher precision may be required.",
                            type = "warning"
                    )
            }
            
            # set concentration dataframe as output
            return(sim_conc)
        })
        
        #### Determine Displayed Timepoint and Pie Chart ####
        observeEvent(c(input$go, input$timept_select), {
            # find concentrations at the selected timepoint value
            print(paste("timepoint changed", input$go))
                
            # selects the timepoint nearest to the one selected, saves to dataframe
            tp_df <- filter(sim_df(), minutes >= input$timept_select) %>%
                    select(minutes, E:OH)
            # renders pie plot to display yield
            output$yield_pie_plot <-
                    renderPlot(pieYield(tp_df[1,]))
            
            # Initialize table data frame 
            tab_df <- data.frame(matrix(0, ncol = 8, nrow = 2))
            
            # Calculates and adds to table redimensionalized concentrations
            tab_df[1,] <- tp_df[1,2:ncol(tp_df)]*scl_fctr()
            
            # Calculates and adds to table mass values, calculated from dimensional concentrations
            tab_df[2,] <- (tab_df[1,]*get_vol())*data.frame(matrix(c(300, 885.4, 665, 445, 32.04, 92.09, 250, 39.997), ncol = 8, nrow = 1))

            # rounds concentration and total mass values to fewer decimal places
            for (colval in 1:ncol(tab_df)){
                    tab_df[1, colval] <- as.numeric(format(round(tab_df[1, colval], 3), nsmall = 3))
                    tab_df[2, colval] <- as.numeric(format(round(tab_df[2, colval], 1), nsmall = 1))
            }
            
            # sets column and row names for data table
            rownames(tab_df) <- c("Concentration (mol/L)", "Total Mass (grams)")
            colnames(tab_df) <- c("Ester", "TriG", "DiG", "MonoG", "Alcohol", "Glycerol", "Soap", "Hydroxide")

            # render timepoint data table
            output$sim_tab <- renderDataTable(tab_df, option = list(dom = 't'))
        })
        
        #gives option to download concentration and mass data
        output$download_sim_conc <- downloadHandler(
                filename = function() {
                        paste("conc_(M)-", gsub(" ","-",Sys.time()), ".csv", sep="")
                },
                content = function(file) {
                        write.csv(sim_df()*scl_fctr(), file)
                }
        )
        
        output$download_sim_mass <- downloadHandler(
                filename = function() {
                        paste("Wieght_(g)-", gsub(" ","-",Sys.time()), ".csv", sep="")
                },
                content = function(file) {
                        write.csv(sim_df()*scl_fctr()*get_vol()*data.frame(matrix(c(300, 885.4, 665, 445, 32.04, 92.09, 250, 39.997), ncol = 8, nrow = 1)), file)
                }
        )
        
        # Change slider length to match time input
        observeEvent(input$go, {
                slider_length <- input$t_length
                updateSliderInput(
                        session,
                        "timept_select",
                        value = 0,
                        min = 0,
                        max = slider_length)
                updateSliderInput(
                        session,
                        "gen_rate_slider",
                        value = 0,
                        min = 0,
                        max = slider_length
                )
                updateSliderInput(
                        session,
                        "rel_rates_slider",
                        value = 0,
                        min = 0,
                        max = slider_length
                )
        })

        #### Generate All Graphs ####
        # generates graphs upon simulation trigger
        
        #### Selection of Displayed Plot ####
        observeEvent(c(input$graph_select,input$disp_species, input$gen_rate_slider, input$rel_rates_slider), ({
                output$dispPlot <- renderPlot({
                        switch(
                                input$graph_select,
                                "All Concentrations" = totalConcPlot(sim_df(), sim_temp(), IC_df(),input$species_sel),
                                "Yield vs. Time" = progConcBar(sim_df(), sim_temp(), input$gen_rate_slider),
                                "Catalyst Activity" = CatActivity(sim_df()),
                                "Relative Rates of Ester:Soap" = rel_Rates(sim_df(),sim_temp(),input$rel_rates_slider),
                                "Emulisification Risk Assessment" = emulRisk(sim_df(), scl_fctr(), get_vol(), sim_temp())
                        )
                }) 
        }))
        
        # update labels on input functions for mass/volume addition
        observeEvent(input$inputType, ({
                if(input$inputType == "By Mass") {
                        output$TG_Label <- renderText("Mass of Triglyceride Added (g)")
                        output$Alc_Label <- renderText("Mass of Alcohol Added (g)")
                        output$OH_Label <- renderText("Mass of Sodium Hydroxide Added (g)")
                }
                else{
                        output$TG_Label <- renderText("Volume of Triglyceride Added (mL)")
                        output$Alc_Label <- renderText("Volume of Alcohol Added (mL)")
                        output$OH_Label <- renderText("Mass of Sodium Hydroxide Added (g)")
                }
        })
        )
        
        # hide loading screen, show rest of content
        Sys.sleep(1.25)
        hide(id = "loading-content", anim = TRUE, animType = "slide")    
        show("app-content")
}

# Run the application 
shinyApp(ui = ui, server = server)
