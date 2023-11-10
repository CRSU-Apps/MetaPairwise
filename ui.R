# MetaImpact UI #
#---------------#

# load libraries #
#----------------#
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
library(rintrojs)
library(shinydashboard)
library(markdown)


# load user-written functions #
#-----------------------------#



# UI Content #
#------------#
shinyUI(fluidPage(
  
  #includeCSS("www/app.css"),
  
  navbarPage(id="MetaPairwise", title="MetaPairwise",
             theme = shinytheme("readable"),
             
            
             # Data Tab #
             #----------#
             
             tabPanel("Data",
                      column(4, h4("Choose Data"),                    # Insert own data or choose example data
                             p("Please upload your data as a .csv file, formatted as described on the right-hand side of this page. Treatment coding (i.e. numbering rather than labels) and specifying whether the outcome is continuous or binary is not necessary."),
                             fileInput(inputId="data", label="", buttonLabel="Select", placeholder="No file selected"),
                             br(),
                             p("If you wish to explore the app without using your own data, you are welcome to choose one of the example datasets below."),
                             p("Example datasets are based on (network) meta-analyses reviewing the effect anti-vasuclar endothelial growth factor has on diabetic macular oedema.
                  Visual acuity (VA) outcomes were reported and chosen for these examples.
                  The continuous outcome example is extracted from a meta-analysis by Virgili et al which can be found ", a(href="https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD007419.pub6/full", "here."),
                               "The binary outcome example is extracted from a meta-analysis by Pham et al which can be found ", a(href="https://bmjopen.bmj.com/content/9/5/e022031", "here.")),
                             p("Options to explore an example network meta-analysis will be available in the near future."),
                             #p("To explore an example network meta-analysis, NMA options are available on the 'Evidence Synthesis' tab."),
                             radioButtons("ChooseExample", "Example Datasets Available", c("Continuous outcome: Change in VA in terms of LogMAR (negative change in LogMAR = improved vision)" = "continuousEx",
                                                                                           "Binary outcome: Number of people that improved their best-corrected VA by gaining 15+ letters during a vision test" = "binaryEx"), width='100%')),
                      column(4, h4("View Data"),          # View data
                             uiOutput("data")),
                      column(4, h4("Format Requirements"),
                             tabsetPanel(id='format_instructions',
                                         tabPanel("Binary Data",
                                                  p("MetaImpact can take data in wide format (every row is per study) or long format (every row is per  study arm)."),
                                                  p("The data file should contain five or eight columns columns for long or wide format respectively. Headings of columns are case sensitive."),
                                                  p("The following columns are needed:"),
                                                  p(tags$ul(tags$li("A column labelled ", tags$strong("StudyID"), " containing the study identifier, starting from 1, then 2, 3, 4... etc."))),
                                                  p(tags$ul(tags$li("A column labelled ", tags$strong("Study"), " containing the name (e.g., author,year) of the study. The study name must be unique for each study."))),
                                                  p(tags$ul(tags$li("For long format, a column labelled ", tags$strong("T"), " containing the name or label of treatment used in each arm of the study.
                                                       For wide format, two columns labelled ", tags$strong("T.1 & T.2"), " containing the name or label of treatment given for study arm 1 and 2 respectively "))),
                                                  p(tags$ul(tags$li("For long format, a column labelled ", tags$strong("R"), " containing the number of participants with the outcome of interest in each arm of the study.
                                                       For wide format, two columns labelled ", tags$strong("R.1 & R.2"), " containing the number of participants with the outcome of interest for study arm 1 and 2 respectively "))),
                                                  p(tags$ul(tags$li("For long format, a column labelled ", tags$strong("N"), " containing the number of participants in each arm of the study.
                                                       For wide format, two columns labelled ", tags$strong("N.1 & N.2"), " containing the number of participants for study arm 1 and 2 respectively ")))
                                         ),
                                         tabPanel("Continuous Data",
                                                  p("MetaImpact can take data in wide format (every row is per study) or long format (every row is per  study arm)."),
                                                  p("The data file should contain six or ten columns columns for long or wide format respectively. Headings of columns are case sensitive."),
                                                  p("The following columns are needed:"),
                                                  p(tags$ul(tags$li("A column labelled ", tags$strong("StudyID"), " containing the study identifier, starting from 1, then 2, 3, 4... etc."))),
                                                  p(tags$ul(tags$li("A column labelled ", tags$strong("Study"), " containing the name (e.g., author,year) of the study. The study name must be unique for each study."))),
                                                  p(tags$ul(tags$li("For long format, a column labelled ", tags$strong("T"), " containing the name or label of treatment used in each arm of the study.
                                                       For wide format, two columns labelled ", tags$strong("T.1 & T.2"), " containing the name or label of treatment given for study arm 1 and 2 respectively "))),
                                                  p(tags$ul(tags$li("For long format, a column labelled ", tags$strong("Mean"), " containing the mean value of the outcome in each arm of the study.
                                                       For wide format, two columns labelled ", tags$strong("Mean.1 & Mean.2"), " containing the the mean value of the outcome for study arm 1 and 2 respectively "))),
                                                  p(tags$ul(tags$li("For long format, a column labelled ", tags$strong("SD"), " containing the standard deviation value of the outcome in each arm of the study.
                                                       For wide format, two columns labelled ", tags$strong("SD.1 & SD.2"), " containing the the standard deviation value of the outcome for study arm 1 and 2 respectively "))),
                                                  p(tags$ul(tags$li("For long format, a column labelled ", tags$strong("N"), " containing the number of participants in each arm of the study.
                                                       For wide format, two columns labelled ", tags$strong("N.1 & N.2"), " containing the number of participants for study arm 1 and 2 respectively "))))
                             ))
             ),
             # Make it such that a user can still use example data even after uploading their own (maybe a tick box after uploading their own to 'use' example instead or 'remove data')
             
             
             
             # Evidence Synthesis Tab #
             #------------------------#
             
             tabPanel("Calculator",
                      # Meta-analysis #
                      #---------------#
                      sidebarLayout(
                        sidebarPanel(
                          h4("Synthesis Options"),
                          fluidRow(column(6,selectInput(inputId = "Pair_Trt", label = "Select Treatment", choices = NULL)),
                                   column(6,selectInput(inputId = "Pair_Ctrl", label = "Select Comparator", choices = NULL))),
                          radioButtons("FixRand", "Model selection:", c("Fixed-effects model (FE)" = "fixed", "Random-effects model (RE)" = "random")),
                          conditionalPanel(condition = "output.ContBin=='continuous'",
                                           radioButtons("OutcomeCont", "Outcome for continuous data:", c("Mean Difference (MD)" = "MD","Standardised Mean Difference (SMD)" = "SMD"))),
                          conditionalPanel(condition = "output.ContBin=='binary'",
                                           radioButtons("OutcomeBina", "Outcome for binary data:", c("Odds Ratio (OR)" = "OR","Risk Ratio (RR)" = "RR", "Risk Difference (RD)" = "RD"))),
                          conditionalPanel(condition= "input.AnalysisType=='Bayesian Analysis'",
                                           h5("Bayesian Options"),
                                           column(6, radioButtons("prior", "Vague prior for between study standard deviation:", c("Half-Cauchy(0,0.5)" = "half-cauchy", "Uniform(0,2)" = "uniform", "Half-Normal(0,1)" = "half-normal")),
                                                  actionButton("bayes_help", "Help", class="btn-xs", style="position: absolute; left: 0; top: 220px")),
                                           column(6, numericInput("chains", "Number of chains:", value=2, min=1),
                                                  numericInput("iter", "Number of iterations:", value=4000, min=1),
                                                  numericInput("burn", "Burn-in:", value=400, min=1))
                        ),width=3),
                         mainPanel(

                          tabsetPanel(id="AnalysisType",
                                      type="tabs",
                                      tabPanel("Frequentist Analysis",
                                               fluidRow(align="center",
                                                        br(),
                                                        actionButton("FreqRun", "Run frequentist meta-analysis", class="btn-primary btn-lg")),
                                                                   conditionalPanel(condition = "input.FreqRun!=0",
                                                                                    fluidRow(p(htmlOutput("SynthesisSummaryFreq"))),
                                                                                             p("To change the model options, please adjust synthesis options and re-run analysis."),
                                                                                    fluidRow(align='center', withSpinner(htmlOutput("SummaryTableF"))),
                                                                                    fluidRow(p(strong("Model fit statistics"))),
                                                                                             dropMenu(dropdownButton(size='xs',icon=icon('info')), align='left',
                                                                                             h6("Model fit statistics"),
                                                                                             p("Akaike information criterion (AIC) and Bayesian information criterion (BIC) measure 'model performance' whilst taking into account model complexity."),
                                                                                             htmlOutput("ModelFitF")),
                                                                                    fluidRow(column(10, withSpinner(plotOutput("ForestPlotPairF"))),
                                                                                             column(2,radioButtons('forestpairF_choice', "Download forest plot as:", c('pdf','png')),
                                                                                                      downloadButton('forestpairF_download', "Download forest plot"),    #Forest plot
                                                                                                    )))),
                                      tabPanel("Bayesian Analysis",
                                               fluidRow(align="center",
                                                        br(),
                                                        actionButton("BayesRun", "Run Bayesian meta-analysis", class="btn-primary btn-lg")),
                                                                   conditionalPanel(condition = "input.BayesRun!=0",
                                                                                    fluidRow(p(htmlOutput("SynthesisSummaryBayes")),
                                                                                             p("To change the model options, please adjust synthesis options above and re-run analysis.")),
                                                                                   #  fluidRow(align='center', withSpinner(htmlOutput("SummaryTableB"))),   # Summary table
                                                                                     fluidRow(p(strong("Model assessment"))),
                                                                                             dropMenu(dropdownButton(size='xs',icon=icon('info')), align='left',
                                                                                             h6("Model assessment"),
                                                                                             p("For Bayesian models it is key that the model has converged (i.e. that the MCMC algorithm found the optimal solution)"),
                                                                                             p("If a model has converged, Rhat should be smaller than 1.01 and the trace plot (parameter estimates over all iterations) should be 'spiky' and show no signs of distinct pattens. Also note that for ORs and RRs, the parameter estimate has been log-transformed.")))),
                                                                                             htmlOutput("ModelFitB"),
                                                                                             plotOutput("TracePlot"),                            # Trace plot
                                                                                             downloadButton('tracepair_download', "Download trace plot"), radioButtons('tracepair_choice', "", c('pdf','png'))),
                                                                                       #      withSpinner(plotOutput("ForestPlotPairB"),   # Forest plot
                                                                                             downloadButton('forestpairB_download', "Download forest plot"),
                                                                                             radioButtons('forestpairB_choice', "", c('pdf','png'))
                                                                                             ))))))