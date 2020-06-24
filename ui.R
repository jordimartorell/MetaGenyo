collapseStyle2 =
  "min-height: 0px;
  padding: 0px;
  margin-bottom: 0px;
  background-color: #ecf0f1;
  border: 0px solid transparent;
  border-radius: 0px;
  -webkit-box-shadow: inset 0 0px 0px rgba(0, 0, 0, 0);
  box-shadow: inset 0 0px 0px rgba(0, 0, 0, 0);"

shinyUI(fluidPage(
  useShinyjs(),
  fluidRow(
    column(12,br(),
           fluidRow(
             column(6, a(href = "http://www.genyo.es/en/bioinformatics-unit", img(src = "logoBioinfo.png", width="25%", height="25%"),
                         target="_blank"), windowTitle = "MetaGenyoBioinfo"),
             column(6, fluidRow(
               column(6, ""),
               column(6, h1(""),a(href = "http://www.genyo.es/en", img(src = "logo.png", width="100%", height="100%"),
                                  target="_blank"), windowTitle = "MetaGenyo")))))),
  #   h1(a(href = "http://www.genyo.es/", img(src = "logo.png", width="25%", height="25%", style="display: block; margin-left: 0; margin-right: auto;"), target="_blank"), windowTitle = "MetaGenyo"),
  
  navbarPage(
    "MetaGenyo: Meta-Analysis of Genetic Association Studies",
    theme="bootstrap.css",
    id="navbar",
    

    tabPanel("Data input",
             wellPanel(
               p("New in MetaGenyo? Go to Help tab."),
               
               radioButtons("inputMode", "Choice a input  mode", c("Interactive table", "Submit a file")),
               
               conditionalPanel(
                 condition = "input.inputMode == 'Interactive table'",
                 selectInput("riskAllele", "Risk Allele", c("A", "T", "G", "C"), width = "10%"),
                 selectInput("refAllele", "Reference Allele", c("A", "T", "G", "C"), selected = "T",  width = "10%"),
                 textInput("newCol1", "Add columns", placeholder = "Column name", width = "20%"),
                 actionButton("addCol", "ADD"),
                 br(),
                 br(),
                 actionLink("loadEx", "Load example data (Zhang et al, 2015)"),
                 br(),
                 rHandsontableOutput("hotable1")
               ),
               
               conditionalPanel(
                 condition = "input.inputMode == 'Submit a file'",
                 h4("Upload a file (Excel or txt)"),
                 fileInput('inputfile', NULL,
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv', '.txt', '.tsv', ".xls", ".xlsx")),
                 bsCollapse(
                   bsCollapsePanel(
                     p("See input format"),
                     wellPanel(img(src="exampleTable.png", width="45%", height="45%"))
                   ),
                   bsCollapsePanel(
                     p("See examples"),
                     wellPanel(style = "padding: 0;",
                               p('You can try these example datasets from published papers:'),
                               tags$ul(
                                 tags$li(align = "justify", a(href = 'Dai.xlsx', 'Dai.xlsx', target="_blank"), " [Dai et al., 2016]"),
                                 tags$li(align = "justify", a(href = 'Zhang.xlsx', 'Zhang.xlsx', target="_blank"), " [Zhang et al., 2015]"),
                                 tags$li(align = "justify", a(href = 'Tarabay.txt', 'Tarabay.txt', target="_blank"), " [Tarabay et al., 2016]")
                               )
                     )
                   )
                 ),
                 uiOutput("refallele")
                 
                 
               ),
               uiOutput("buttonnext1"),

               uiOutput("char")
               
               
               
               
               
             ),
             p("If you use MetaGenyo, please include this reference:"),
             p(""),
             p("Martorell-Marugan J, Toro-Dominguez D, Alarcon-Riquelme ME, Carmona-Saez P. (2017) ",
               strong("MetaGenyo: A web tool for meta-analysis of genetic association studies."),
               em("BMC Bioinformatics. "), "18:563")
    ),
    
    tabPanel("Your data",
             wellPanel(
               textOutput("yournodata"),
               shinyjs::hidden(list(br(id="br1"), br(id="br2"))),
               DT::dataTableOutput("yourdatafile"),
               uiOutput("buttonnext2")
               
             )
             
    ),
    
    tabPanel("Hardy-Weinberg table",
             wellPanel(
               textOutput("hwnodata"),
               # shinyjs::hidden(list(br(id="br3"),
               #                      br(id="br4"),
               #                      selectInput("format", "Choose a format to download your data:", 
               #                                  choices = c("Excel format (.xls)",
               #                                              "Plain-text format (.csv)",
               #                                              "Plain-text format (.tsv)"), width = 200),
               #                      downloadButton('downhw', 'Download'),
               #                      tags$hr(id="hr1"),
               #                      sliderInput("thresholdHW", label = "Select a p-value threshold", min = 0, 
               #                                  max = 1, value = 0.05, width = "400px")
               #                      )),
               uiOutput("hwPanel"),
               
               
               
#                conditionalPanel(
#                  condition = "output.hw",
#                  br(),
#                  br(),
#                  selectInput("format", "Choose a format to download your data:", 
#                              choices = c("Excel format (.xls)", "Plain-text format (.csv)", "Plain-text format (.tsv)"), width = 200),
#                  downloadButton('downhw', 'Download'),              
#                  tags$hr(),
#                  sliderInput("thresholdHW", label = "Select a p-value threshold", min = 0, 
#                              max = 1, value = 0.05, width = "400px")
#                ),
               DT::dataTableOutput("hw"),
               uiOutput("buttonnext3")
             )
    ),
    
    tabPanel("Association values",
             textOutput("white"),
             wellPanel(
               id="wellpanel1",
               textOutput("assnodata")
             ),

             conditionalPanel(
               condition = "output.white == ' '",
               
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("modelasso")
                 ),
                 mainPanel(wellPanel(
                     div(id="hide1",img(src="25.gif"),style="text-align: center;"),
                   
                   conditionalPanel(
                     condition = "input.methods",

                     h3(id="hide2", "Association test results"),
                     textOutput("metaresults1"),
                     br(),
                     tableOutput("metaresults2"),
                     br(),
                     h4(id = "hide3", "Heterogeneity tests:"),
                     tableOutput("metanuevo"),
                     textOutput("metaresults4"),
                     textOutput("metaresults5"),
                     textOutput("metaresults6"),
                     br(),
                     textOutput("metaresults7"),
                     tableOutput("metaresults8"),
                     br(),
                     textOutput("metaresults9"),
                     textOutput("metaresults10"),
                     textOutput("metaresults11"),
                     uiOutput("buttonnext4")
                   )
                   
                 ))))
    ),
    
    
    
    tabPanel("Forest plots",
             textOutput("white2"),
             wellPanel(
                 id="wellpanel2",
                 textOutput("fornodata")
              ),
             
             conditionalPanel(
               condition ="output.white2 == ' '",
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("modelforest"),
                   radioButtons("fixRan", "Select model",
                                c("Both" = "both",
                                  "Fixed Effect Model" = "fixed",
                                  "Random Effect Model" = "random"))
                 ),
                 mainPanel(wellPanel(
                   conditionalPanel(
                     condition = "input.methodsf",
                     plotOutput("forest", inline=T),
                     p(""),
                     downloadButton('downforest', 'Download plot in high resolution'),
                     uiOutput("buttonnext5")
                   )
                   
                 ))))
    ),
    
    tabPanel("Publication bias",
             textOutput("white3"),
             wellPanel(
               id="wellpanel3",
               textOutput("funnodata")
             ),

             
             conditionalPanel(
               condition ="output.white3 == ' '",
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("modelbias"),
                   checkboxInput("funLab", "Show study labels")
                 ),
                 mainPanel(wellPanel(
                   conditionalPanel(
                     condition = "input.methodsfun",
                     h3("Funnel plot"),
                     plotOutput("funnel", width = 800, height = 600),
                     p(""),
                     downloadButton('downfunnel', 'Download plot in high resolution'),
                     h3("Egger's test"),
                     textOutput("egger3"),
                     # p("Regression Test for Funnel Plot Asymmetry"),
                     p(""),
                     p("model:     weighted regression with multiplicative dispersion"),
                     # p("model:     meta-analytic models"),
                     p("predictor: standard error"),
                     uiOutput("buttonnext6")
                   )
                   
                 ))))
    ),
    
    tabPanel("Subgroup analysis",
             id="Subgroup analysis",
             wellPanel(
               tags$head(
                 tags$style(type = "text/css",
                            HTML("th { text-align: center; }")
                            
                 ),
                 tags$head(
                   tags$style(HTML(".cell-border-right{border-right: 1px solid #000}")))
               ),
               textOutput("strnodata"),
               uiOutput("col"),
               
               # shinyjs::hidden(div(id="straPanel",
               #            br(),
               #            
               #            
               #            br(),
               #            selectInput("strformat", "Choose a format to download your data:", 
               #                        choices = c("Excel format (.xls)", "Plain-text format (.csv)", "Plain-text format (.tsv)"), width = 200),
               #            downloadButton('downstr', 'Download'),              
               #            tags$hr(),
               #            sliderInput("thresholdASSO", label = "Select a p-value threshold", min = 0, 
               #                        max = 1, value = 0.05, width = "400px"))),
               uiOutput("straPanel"),


               uiOutput("buttonnext7")
               
             )
    ),

  tabPanel("Sensitivity",
           textOutput("white5"),
           wellPanel(
             id="wellpanel5",
             textOutput("robnodata")
           ),
           
           conditionalPanel(
             condition ="output.white5 == ' '",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("modelrob"),
                 radioButtons("fixRanRob", "Select model",
                              c("Fixed Effect Model" = "fixed",
                                "Random Effect Model" = "random"))
               ),
               mainPanel(wellPanel(
                 conditionalPanel(
                   condition = "input.methodsrob",
                   h3("Leave-1-out Forest plot"),
                   plotOutput("robplot", inline=T),
                   p(""),
                   downloadButton('downrobPlot', 'Download plot in high resolution')
#                    h3("Robustness analysis"),
#                    dataTableOutput("robtable"),
#                    p(""),
#                    selectInput("robformat", "Choose a format to download your data:", 
#                                choices = c("Excel format (.xls)", "Plain-text format (.csv)", "Plain-text format (.tsv)"), width = 500),
#                    downloadButton('downrobTable', 'Download')
                   # uiOutput("buttonnext5")
                 )
                 
               ))))
  ),
    
    tabPanel("Help",
             wellPanel(
               h3("Introduction"),
               p(align="justify", "MetaGenyo is a simple, ready-to-use software which has been designed to perform meta-analysis of genetic association studies."),
               br(),
               h3("Prepare your data"),
               p(align="justify", "MetaGenyo requires a specific data format to worth with. The first n columns of your file can contain any type of data of the different studies that will be used at the Subgroup analysis step. N is defined by yourself. The following columns must be exactly like this:"),
               img(src="prepareTable.png", width="35%", height="35%"),
               br(),
               br(),
               p(align="justify", "The meaning of each column are the following:"),
               tags$ul(
                 tags$li(align = "justify", strong("AA cases: "), "The number of subjects with the disease (cases) that are homozygote for the ", strong("A"), "variant."),
                 tags$li(align = "justify", strong("AT cases: "), "The number of cases that are heterozygote for the ", strong("A"), "and ", strong("T"), "variants."),
                 tags$li(align = "justify", strong("TT cases: "), "The number of cases that are homozygote for the ", strong("T"), "variant."),
                 tags$li(align = "justify", strong("AA/AT/TT controls: "), "The same as the previous columns, but for the amounts of subjects with no disease (controls)")
               ),
               span(align="justify", strong("IMPORTANT: A"), "and ", strong("T"), "variants are merely examples with no more significance. That means that A and T can correspond to any variant that you want to study.", style = "background-color: yellow"),
               br(),
               br(),
               p(align="justify", 'In conclusion, your data should be something like this (remember that previous columns to "AA cases" column are chosen by yourself):'),
               img(src="exampleTable.png", width="45%", height="45%"),
               br(),
               br(),
               p(align="justify", "You can write the column names that you want to (but there MUST be column names). But there are some exceptions: column names with genotyping data must start with the genotype (AA.cases it's correct, but cases.AA it's not). Missing values are not allowed. Be sure that your data is in the correct order."),
               p(align="justify", "Once you have all your data, you must save it in a suitable format for MetaGenyo. The tool accepts Excel files (.xls and .xlsx) and plain text files."),
               br(),
               h3("Submit your data"),
               p(align="justify", 'Once you have prepared your data, you are ready to use MetaGenyo. First, enter to the webpage, press "Browse..." button and select your data file. Now:'),
               tags$ul(
                 tags$li(align = "justify", "If you submitted an ", strong("Excel file"), " you have to select which is the risk allele in your data (th."),
                 tags$li(align = "justify", "If you submitted a ", strong("plain text file"), " you have to specify separator and quote type of your file in addition to the risk allele.")
               ),
               br(),
               h3("Review your data"),
               p(align="justify", 'Once you have submitted your data, you must click "Your data" tab to review it. If your data looks nice and no error appears, it means that you submitted your data correctly and you can continue with the following steps. Otherwise, you must return to "Data input" tab and review that options selected correspond to your data characteristics. Maybe you should review your input file too.'),
               br(),
               h3("Hardy-Weinberg table"),
               p(align="justify", "In this tab, you will find your data with two columns added:", strong("HW.P.value"), " and ", strong("HW.adjusted.P.value"), ". The first one contains the p-value of chi-square test for Hardy-Weinberg Equilibrium (HWE) for the control data of each study you included. The second one contains these p-values corrected for multiple testing by FDR mehod. This is a way to check study quality: if p-value is significant, control genotypes may not be in HWE and you should be careful with that study data or exclude it. That's because departures from HWE can occur due to genotyping errors, selection bias in the choosing of controls and stratification [1]. If you want to exclude some study, just delete it from your data and resubmit your file."),
               p(align="justify", 'You can download the results in Excel (.xls), comma-separated values (.csv) and tab-separated values (.tsv) formats. Simply choose your desired format and press "Download" button. You can also interact with the results in the table showed.'),
               br(),
               h3("Association values"),
               p(align="justify", "In this tab, you will find association test results calculated for the comparison that you choose on the left panel. These results are:"),
               tags$ul(
                 tags$li(align = "justify", strong("Association test results")),
                 tags$ul(
                   tags$li(align = "justify", strong("Number of studies included in your meta-analysis")),
                   tags$li(align = "justify", strong("Results table for fixed and random models:")),
                   tags$ul(
                     tags$li(align = "justify", strong("OR: "), "Odds-ratio"),
                     tags$li(align = "justify", strong("95%-CI: "), "95 % confidence interval for the OR"),
                     # tags$li(align = "justify", strong("z")),
                     tags$li(align = "justify", strong("p-value: "), "Probability that the SNP is NOT associated with the studied phenotype"),
                     tags$li(align = "justify", strong("adjusted p-value: "), "P-value corrected for testing the association in different ways by Bonferroni method")
                   )
                 ),
                 tags$li(align = "justify", strong("Test of heterogeneity:")),
                 tags$ul(
                   tags$li(align = "justify", strong("tau^2:"), "Estimate of total amount of heterogeneity"),
                   tags$li(align = "justify", strong("H:"), "Proportion of total variability due to heterogeneity"),
                   tags$li(align = "justify", strong("I^2:"), "Proportion of inter-study variability attributed to chance rather than heterogeneity"),
                   tags$li(align = "justify", strong("Q:"), "Q value for Cochran's Q test. This test is used to determine if variations are due to chance or they are due to differences between studies [2]"),
                   tags$li(align = "justify", strong("p.value:"), "Heterogeneity p-value. If this value is lower than 0.10, it indicates the presence of heterogeneity [3]")
                   
                 )
               ),
               br(),
               h3("Forest plots"),
               p(align="justify", "In this tab, you will find a forest plot for the comparison that you choose on the left. A forest plot gives a visual representation of the variation between the studies included in your meta-analysis for the selected comparison [4]. It also provides an estimation of the global result of all the studies."),
               br(),
               h3("Publication bias"),
               p(align="justify", "In this tab, you will first find a funnel plot for the chosen comparison on the left. This plot is a graphical test to check if publication bias exists in your meta-analysis [5]. If the funnel plot is similar to a symmetrical inverted funnel, there is not publication bias in your data. On the other hand, if there is asymmetry maybe publication bias exists ini your data, so the results of your meta-analysis must be interpreted carefully [6]."),
               p(align="justify", "In this tab you will also find Egger's test results for your data and selected comparison. This is a more objective way to detect publication bias [5]. If Egger's regression test p-value is lower than 0.05, publication bias is possibly present in your meta-analysis."),
               br(),
               h3("Subgroup analysis"),
               p(align="justify", "In this tab, you can choose a column of your data and stratify the studies based on this column. A table will be generated with some results of the meta-analysis for each of the groups formed. The test of association is calculated with fixed or random effect model, depending on the test of heterogeneity: If p-value < 0.1, random effect model will be used. Otherwise, fixed effect model will be used instead. If there is any group formed by one single study, this group will not appear in the results (obviously, meta-analysis can't be performed with one study)."),
               p(align="justify", "These tables can be very useful to easily detect statistically significant associations in subgroups when no results are obtained including all the studies. The generated table can be also useful as a summary if subgrouping column is not selected."),
               br(),
               h3("Sensitivity"),
               p(align="justify", "In this tab, you will find a forest plot for the robustness analysis by leave.one-out method. The analysis is repeated excluding one study each time in order to visualize if any study has a significantly greater contribution to overall statistics that the other studies."),
               br(),
               h3("References"),
               tags$ol(
                 tags$li(align = "justify", "Salanti G, Amountza G, Ntzani EE, loannidis JP.  Hardy-Weinberg equilibrium in genetic association studies: an empirical evaluation of reporting, deviations, and power. Eur J Hum Genet 2005;13:840-8."),
                 tags$li(align = "justify", "Whitehead A and Whitehead J. A general parametric approach to the meta-analysis of randomized clinical trials. Stat Med 1991;10:1665-77."),
                 tags$li(align = "justify", "Munafò MR and Flint J. Meta-analysis of genetic association studies. Trends Genet 2004;20:439-44."),
                 tags$li(align = "justify", "Lewis S, Clarke M (2001). Forest Plots: Trying to See the Wood and the Trees. British Medical Journal, 322(7300), 1479–1480."),
                 tags$li(align = "justify", "Egger M, Davey Smith G, Schneider M, Minder C. Bias in meta-analysis detected by a simple, graphical test. BMJ 1997;315:629-34."),
                 tags$li(align = "justify", "Y Lau J, loannidis JP, Terrin N, Schmid CH, Olkin I. The case of the misleading funnel plot. BMJ 2006;333:597-600."),
                 br(),
                 strong("Example datasets extracted from:"),
                 tags$li(align = "justify", "Dai X, Zhang X, Wang B, Wang C, Jiang J, Wu C. Association Between Polymorphism rs678653 in Human Cyclin D1 Gene (CCND1) and Susceptibility to Cancer: A Meta-Analysis. Medical Science Monitor : International Medical Journal of Experimental and Clinical Research. 2016;22:863-874."),
                 tags$li(align = "justify", "Zhang Y, Zeng X, Lu H, Li Y, Ji H. Association between Interleukin-8-251A/T polymorphism and gastric cancer susceptibility: a meta-analysis based on 5286 cases and 8000 controls. International Journal of Clinical and Experimental Medicine. 2015;8(12):22393-22402."),
                 tags$li(align = "justify", "Tarabay M, Elshazli R, Settin A. African vs. Caucasian and Asian difference for the association of interleukin-10 promotor polymorphisms with type 2 diabetes mellitus (a meta-analysis study). Meta Gene. 2016;9:10-17.")
                 
               ),
               br(),
               h3("Contact info"),
               p("If you have any doubt, question or suggestion, you can write us to ", a("bioinfo@genyo.es",href="mailto:bioinfo@genyo.es"), ".")
               #                br(),
               #                h3("Authors"),
               #                tags$ul(
               #                  tags$li(p(strong("Pedro Carmona."), "Genyo Bioinformatics Unit head: ", a("pedro.carmona@genyo.es", href="mailto:pedro.carmona@genyo.es"))),
               #                  tags$li(p(strong("Jordi Martorell."), "Genyo Bioinformatics Unit technician: ", a("jordi.martorell@genyo.es", href="mailto:jordi.martorell@genyo.es")))
               #                )
               
               
               
               
               
               
               
               
               
             ))


    
    
    
    
  )))
