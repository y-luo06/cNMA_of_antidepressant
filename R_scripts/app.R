library(shiny)
library(netmeta)
library(dplyr)
library(ggplot2)
library(scatterpie)
library(rdrop2)
library(scales)
library(ggrepel)

#link to the dataset
def_active <- read.csv("data/active-efficacy.csv", header = TRUE, stringsAsFactors = FALSE) 
def_activep <- subset(def_active, unpublished == 0)
dac_active <- read.csv("data/active-acceptability.csv", header = TRUE, stringsAsFactors = FALSE)
dac_activep <- subset(dac_active, unpublished == 0)
def_full <- read.csv("data/full-efficacy.csv", header = TRUE, stringsAsFactors = FALSE) 
def_fullp <- subset(def_full, unpublished == 0)
dac_full <- read.csv("data/full-acceptability.csv", header = TRUE, stringsAsFactors = FALSE)
dac_fullp <- subset(dac_full, unpublished == 0)
dfef_fluo <- read.csv("data/fluo-ef.csv", header = TRUE, stringsAsFactors = FALSE) 
dfac_fluo <- read.csv("data/fluo-ac.csv", header = TRUE, stringsAsFactors = FALSE) 
dfef_plac <- read.csv("data/plac-ef.csv", header = TRUE, stringsAsFactors = FALSE) 
dfac_plac <- read.csv("data/plac-ac.csv", header = TRUE, stringsAsFactors = FALSE)

#confidence in the evidence data
dcon16 <- data.frame(trts = c("agom", "amit", "bupr", "cita", "clom", "dulo", "esci", "fluo", "fluv", 
                              "miln", "mirt", "nefa", "paro", "rebo", "sert", "traz", "venl", "vort"),
                     vlow = c(0, 0.1765, 0.1471, 0, 0.1765, 0.1176, 0, 0.0294, 0.2059, 0.0882, 0, 0.3529,
                              0.0295, 0.1471, 0.0588, 0.2353, 0.0588, 0),
                     low = c(0.2647, 0.7647, 0.6176, 0.4706, 0.7059, 0.3529, 0.3235, 0.4706, 0.6176, 0.9118,
                             0.3235, 0.4706, 0.4412, 0.7353, 0.5, 0.6765, 0.4412, 0.2059), 
                     mod = c(0.6765, 0.0588, 0.2353, 0.5, 0.1176, 0.4706, 0.4706, 0.2647, 0.1764, 0, 0.5588, 0.1765,
                             0.4118, 0.1176, 0.4118, 0.0882, 0.4412, 0.7353),
                     high = c(0.0588, 0, 0, 0.0294, 0, 0.0588, 0.2059, 0.2353, 0, 0, 0.0882, 0, 0.1176, 0, 0.0294, 0,
                              0.0588, 0.0588))
dcon10 <- data.frame(trts = c("agom", "amit", "bupr", "cita", "clom", "dulo", "esci", "fluo", "fluv", 
                              "miln", "mirt", "nefa", "paro", "rebo", "sert", "traz", "venl"),
                     vlow = c(0, 0.15625, 0.125, 0.03125, 0.1875, 0.1875, 0, 0, 0.1875, 0.21875, 0, 0.28125,
                              0.0625, 0.125, 0.0625, 0.15625, 0.0625),
                     low = c(0.09375, 0.65625, 0.5625, 0.375, 0.71875, 0.25, 0.1875, 0.5, 0.5625, 0.6875,
                             0.375, 0.53125, 0.40625, 0.8125, 0.46875, 0.65625, 0.375), 
                     mod = c(0.53125, 0.1875, 0.3125, 0.3125, 0.09375, 0.3125, 0.46875, 0.25, 0.25, 0.09375, 0.3125, 0.15625,
                             0.3125, 0.0625, 0.34375, 0.1875, 0.375),
                     high = c(0.375, 0, 0, 0.28125, 0, 0.25, 0.34375, 0.25, 0, 0, 0.3125, 0.03125, 0.21875, 0, 0.125, 0,
                              0.1875))
dcon05 <- data.frame(trts = c("agom", "amit", "bupr", "cita", "clom", "dulo", "esci", "fluo", "fluv", 
                              "miln", "mirt", "nefa", "paro", "rebo", "sert", "traz", "venl"),
                     vlow = c(0.03125, 0.21875, 0.21875, 0.0625, 0.25, 0.1875, 0.09375, 0.0625, 0.28125, 0.28125, 0, 0.28125,
                              0.03125, 0.34375, 0.0625, 0.25, 0.09375),
                     low = c(0.40625, 0.6875, 0.6875, 0.40625, 0.75, 0.46875, 0.4375, 0.4375, 0.5625, 0.65625,
                             0.4375, 0.625, 0.59375, 0.65625, 0.46875, 0.71875, 0.46875), 
                     mod = c(0.4375, 0.09375, 0.09375, 0.40625, 0, 0.28125, 0.34375, 0.46875, 0.15625, 0.0625, 0.46875, 0.09375,
                             0.34375, 0, 0.40625, 0.03125, 0.34375),
                     high = c(0.125, 0, 0, 0.125, 0, 0.0625, 0.125, 0.03125, 0, 0, 0.09375, 0, 0.03125, 0, 0.0625, 0,
                              0.09375))
dcon00 <- data.frame(trts = c("amit", "bupr", "cita", "clom", "fluo", "fluv", 
                              "miln", "mirt", "nefa", "paro", "rebo", "sert", "traz", "venl"),
                     vlow = c(0.4231, 0.3846, 0.1154, 0.4231, 0.1154, 0.3846, 0.3846, 0.1923, 0.3846, 0.1154, 1, 0.1154,
                              0.5, 0.1538),
                     low = c(0.5385, 0.6154, 0.4615, 0.5769, 0.5384, 0.5769, 0.6154, 0.4615, 0.5769, 0.5769, 0, 0.6154,
                             0.5, 0.5385), 
                     mod = c(0.0385, 0.0385, 0.3846, 0, 0.3077, 0.0385, 0, 0.1923, 0.0385, 0.2692, 0, 0.2308,
                             0, 0.3077),
                     high = c(0, 0, 0.0385, 0, 0.0385, 0, 0, 0.1538, 0, 0.0385, 0, 0.0385,
                              0, 0))
dcon95 <- data.frame(trts = c("amit", "bupr", "cita", "clom", "fluo", "fluv", 
                              "miln", "paro", "rebo", "sert", "traz", "venl"),
                     vlow = c(0.5909, 0.5455, 0.2273, 0.4545, 0.2273, 0.5, 0.5909, 0.3636, 1, 0.1818, 0.7273, 0.3182),
                     low = c(0.4091, 0.4091, 0.4545, 0.5455, 0.4545, 0.5, 0.4091, 0.4091, 0, 0.5, 0.2727, 0.5), 
                     mod = c(0, 0.0455, 0.3182, 0, 0.3182, 0, 0, 0.2273, 0, 0.3182, 0, 0.1818),
                     high = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
dcon90 <- data.frame(trts = c("amit", "cita", "clom", "fluo", "fluv", "miln", "paro", "traz"),
                     vlow = c(0.5714, 0.2143, 0.4286, 0.3572, 0.7143, 0.9286, 0.5, 0.7857),
                     low = c(0.4286, 0.5714, 0.5714, 0.4286, 0.2857, 0.0714, 0.3571, 0.2143), 
                     mod = c(0, 0.2143, 0, 0.2143, 0, 0, 0.1429, 0),
                     high = c(0, 0, 0, 0, 0, 0, 0, 0))

colors <- c(agom = "thistle", amit = "lavenderblush4", bupr = "coral3", cita = "cadetblue4", clom = "orange3", dulo = "azure4", desv = "yellow",
            esci = "aquamarine4", fluo = "lightslateblue", fluv = "dodgerblue4", levo = "orange", miln = "goldenrod4", mirt = "palegreen4", nefa = "darkgrey",
            paro = "gray9", rebo = "lightblue3", sert = "lightpink3", traz = "brown", venl= "mediumvioletred", vila = "springgreen", vort = "plum3", plac = "peru")

#This is for loading message and spinner
mycss <- "
#plot-container1 {
  position: absolute; left: 50%; top: 18%; z-index: -1;
}
#plot-container2 {
  position: absolute; left: 50%; top: 58%; z-index: -1;
}
#plot-container3 {
  position: absolute; left: 50%; top: 8%; z-index: -1;
}
#plot-container4 {
  position: absolute; left: 50%; top: 58%; z-index: -1;
}
#plot-container5 {
  position: absolute; left: 50%; top: 25%; z-index: -1;
}
#plot-container6 {
  position: absolute; left: 50%; top: 73%; z-index: -1;
}
#loading-spinner1 {
  position: absolute; left: 50%; top: 50%; z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#loading-spinner2 {
  position: absolute; left: 50%; top: 50%; z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
}
#loadmessage1 {
  position: absolute; top: 23%; left: 10%; width: 80%; padding: 5px 0px 5px 0px; 
  text-align: center; font-size:130%;  font-style:italic; color: #708090;
  background-color:white; z-index: -1;
}
#loadmessage2 {
  position: absolute; top: 63%; left: 10%; width: 80%; padding: 5px 0px 5px 0px;
  text-align: center; font-size:130%; font-style:italic; color: #708090;
  background-color:white; z-index: -1;
}
#loadmessage3 {
  position: absolute; top: 11%; left: 10%; width: 80%; padding: 5px 0px 5px 0px;
  text-align: center; font-size:130%; font-style:italic; color: #708090;
  background-color:white; z-index: -1;
}
#loadmessage4 {
  position: absolute; top: 61%; left: 10%; width: 80%; padding: 5px 0px 5px 0px;
  text-align: center; font-size:130%; font-style:italic; color: #708090;
  background-color:white; z-index: -1;
}
#loadmessage5 {
  position: absolute; top: 30%; left: 10%; width: 80%; padding: 5px 0px 5px 0px;
  text-align: center; font-size:130%; font-style:italic; color: #708090;
  background-color:white; z-index: -1;
}
#loadmessage6 {
  position: absolute; top: 78%; left: 10%; width: 80%; padding: 5px 0px 5px 0px;
  text-align: center; font-size:130%; font-style:italic; color: #708090;
  background-color:white; z-index: -1;
}
"

#prepare the function for log transformation (2D plots)
##it is not possible to reverse + log for an axis at the same time
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}


ui <- fluidPage(tags$head(tags$style(HTML("
      h2 {
        font-family: 'Fjalla One', sans-serif;
        font-weight: 800;
        line-height: 1.5;
        color: #4682B4;
      }
    "))),
                headerPanel(h2("Interactive Plots to Demonstrate the Evolution of Evidence for Antidepressants in the Acute Treatment of Major Depression")),
                
                fluidRow(column(5,
                                br(),
                                img(src = "drugs.jpg", height = 350, width = "100%", align = "left")
                         ),
                         column(6,
                                br(),
                                p("The data used in this study comes from GRISELDA 
                                  (Group of Researchers Investigating Specific Efficacy of individual Drugs for Acute depression) [1]. 
                                  Double-blind RCTs of the acute phase treatment with ",
                                  strong("21 antidepressants"),
                                  " for adult patients (≥18 years old) with a primary diagnosis of ",
                                  strong("major depression"),
                                  "were ", 
                                  strong("systematically"),
                                  " searched and included. About one in four trials included in the dataset comes from unpublished data, and two come from both published and unpublished sources.",
                                  style = "font-size: 17px"),
                                p("In the ", 
                                  strong("network meta-analysis,"),
                                  "we use two primary outcomes: efficacy and acceptability.",
                                  strong("Efficacy"),
                                  " was measured by the response rate of the intervention, defined as the proportion of patients 
                                  who showed 50% or greater reduction on a validated depression severity scales from baseline. ",
                                  strong("Acceptability"),
                                  " was measured by the all-cause discontinuation rate, defined as the proportion of patients 
                                  who withdrew early due to any reasons.",
                                  style = "font-size: 17px"),
                                hr(),
                                p("[1] Cipriani A, Furukawa TA, Salanti G, et al, 2018. Comparative efficacy and acceptability of 21 antidepressant drugs 
                                  for the acute treatment of adults with major depressive disorder: a systematic review and network meta-analysis. 
                                  Lancet 391, 1357-1366.",
                                  style = "font-size: 14px; color: grey")
                         )
                ),
                
                hr(),
                
                
                sidebarLayout(position = "left",
                              sidebarPanel(style = "width: 300px",
                                           helpText("Choose the type of RCT you would like to synthesize: 
               the reference drug will be placebo if all the trials are included, 
               and the reference will be citalopram if only head-to-head trials are included."),
                                           radioButtons("RCTtype", label = "Choose the type of RCT",
                                                        choices = list("Including placebo-controlled trials", "Only head-to-head trials"),
                                                        selected = "Only head-to-head trials"),
                                           hr(),
                                           helpText("Choose the period of network meta-analysis: RCTs conducted during this period will be pooled for the analysis."),
                                           selectInput("yearsince", label = "Show the pooled evidence since", 
                                                       choices = list("1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", 
                                                                      "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1994", "1995",
                                                                      "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003",
                                                                      "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
                                                                      "2013", "2014", "2015", "2016"), 
                                                       selected = "1978"),
                                           selectInput("yearuntil", label = "Show the pooled evidence until", 
                                                       choices = list("1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", 
                                                                      "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1994", "1995",
                                                                      "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003",
                                                                      "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
                                                                      "2013", "2014", "2015", "2016"),
                                                       selected = "2016"),
                                           hr(),
                                           helpText("The dataset includes all published and unpublised data. 
               If you would like to see the results from published data only, please check the box below."),
                                           strong("Check the publication status of RCTs", style = "font-size: 14px"),
                                           checkboxInput("published", label = "Pool data from published RCTs only", value = FALSE)
                              ),
                              mainPanel(width = 9, style = "position:absolute; margin-left: 370px; margin-right: 100px",
                                        tabsetPanel(
                                          tabPanel("Network Plots and Basic Information",
                                                   br(),
                                                   fluidRow(
                                                     h4("The efficacy network: ", align = "center"),
                                                     tags$head(tags$style(HTML(mycss))),
                                                     conditionalPanel(condition = "$('html').hasClass('shiny-busy')", 
                                                                      tags$div(id = "plot-container1", tags$img(src = "spinner.gif", id = "loading-spinner1")),
                                                                      tags$div("Please wait. The calculations of network meta-analysis may take up to several minutes.",id="loadmessage1")),
                                                     div(plotOutput("netgraph_efficacy", height = "500px", width = "800px"), align = "center"),
                                                     textOutput("study_numbers_ef"),
                                                     textOutput("treat_numbers_ef"),
                                                     p("The network includes the following treatments:"),
                                                     textOutput("treat_list_ef"),
                                                     br(),
                                                     textOutput("heterog_ef"),
                                                     textOutput("inconsist_ef"),
                                                     textOutput("I2_ef"),
                                                     textOutput("tau_ef")
                                                   ),
                                                   br(),
                                                   fluidRow(
                                                     h4("The acceptability network: ", align = "center"),
                                                     tags$head(tags$style(HTML(mycss))),
                                                     conditionalPanel(condition = "$('html').hasClass('shiny-busy')", 
                                                                      tags$div(id = "plot-container2",tags$img(src = "spinner.gif", id = "loading-spinner2")),
                                                                      tags$div("Please wait. The calculations of network meta-analysis may take up to several minutes.",id="loadmessage2")),
                                                     div(plotOutput("netgraph_acceptability", height = "500px", width = "800px"), align = "center"),
                                                     textOutput("study_numbers_ac"),
                                                     textOutput("treat_numbers_ac"),
                                                     p("The network includes the following treatments:"),
                                                     textOutput("treat_list_ac"),
                                                     br(),
                                                     textOutput("heterog_ac"),
                                                     textOutput("inconsist_ac"),
                                                     textOutput("I2_ac"),
                                                     textOutput("tau_ac")
                                                   ),
                                                   fluidRow(
                                                     hr(),
                                                     strong("Note:"),
                                                     p("(1) The size of the node is proportionate to the number of randomized patients, and the thickness of the line is proportionate to the number of studies.", style = "font-size: 14px; color: grey"),
                                                     p("(2) The ", em("netmeta"), " package in R adopts generalized ",
                                                       span("Cochran’s Q statistic", style = "color:blue"),
                                                       " to assess the assumption of homogeneity and consistency. It is calculated as the weighted sum of squared differences between 
                                                       individual study effects and the pooled effect across studies, with the weights being those used in the pooling method. 
                                                       This Q statistic can be decomposed 
                                                       in a sum of within-design Q statistics (used to assess heterogeneity among direct comparisons) 
                                                       and one between-designs Q statistic (used to assess inconsistency between direct and indirect evidence). 
                                                       We provide both Q statistics and their p values for the test.", style = "font-size: 14px; color: grey"),
                                                     p("(3) The ",
                                                       span("I-squared statistic",style = "color:blue"),
                                                       " describes the percentage of variation across studies that is due to heterogeneity rather than chance.", style = "font-size: 14px; color: grey"),
                                                     p("(4) In random-effects meta-analysis, the extent of variation among the effects observed in different studies (between-study variance) 
                                                       is referred to as ",
                                                       span("tau-squared", style = "color:blue"),
                                                       ". tau-squared is the variance of the effect size parameters across the population of studies and 
                                                       it reflects the variance of the true effect sizes. The square root of this number is referred to as tau.", style = "font-size: 14px; color: grey")
                                                   )
                                          ),
                                          tabPanel("2 Dimensional Plots", fluid = TRUE,
                                                   fluidRow(
                                                     br(),
                                                     h4("The 2 Dimensional Plot for Both Efficacy and Acceptability", align = "center"),
                                                     #Add a spinner indicating waiting, because it really takes a long time
                                                     tags$head(tags$style(HTML(mycss))),
                                                     conditionalPanel(condition = "$('html').hasClass('shiny-busy')", 
                                                                      tags$div(id = "plot-container3",tags$img(src = "spinner.gif", id = "loading-spinner1")),
                                                                      tags$div("Please wait. The calculations of network meta-analysis may take up to several minutes.",id="loadmessage3")),
                                                     div(plotOutput("twodplot", width = "500px", height = "450px", click = "plot_click"), align = "center"),
                                                     p("Please click the plot to see a zoomed one with the optimal ranges of x and y axis. (The zoomed plot will be displayed below.)", style = "color:red", align = "center"),
                                                     strong("Note: "),
                                                     p("(1) Drugs in the upper right area are better balanced in both efficacy and acceptability.", align = "left"),
                                                     p("(2) The size of the node is proportionate to the number of randomized patients.", align = "left"),
                                                     p("(3) If placebo-controlled trials are included, the plot will display 2 sets of result. 
                                                       The solid circles indicate results from full dataset, while the empty circles indicate results from only published data.", align = "left"),
                                                     br(),
                                                     div(plotOutput("twodplot_flex", width = "500px", height = "450px"), align = "center"),
                                                     br(),
                                                     hr()),
                                                   fluidRow(
                                                     h4("The 2 Dimensional Plot Showing Efficacy and Acceptability Together with Confidence in the Evidence", align = "center"),
                                                     p("We assessed the confidence in the evidence every 5 years since 1990, for head-to-head trials. We believe that it should be as important
       to see how credible the evidence are, as with the relative effect estimates. We evaluated it at 3 levels: RCT-level, comparison-level and drug-level. At ",
                                                       strong("RCT-level, "),
                                                       "we assessed the risk of bias for each RCT based on Cochrane Collaboration risk of bias tool. At ",
                                                       strong("comparison-level, "),
                                                       "we evaluated it at each time point using ", em("CINeMA framework"), " [2] through the dedicated software. Finally,
        the information was synthesized at ", strong("drug-level. "),
                                                       "We combined the efficacy, acceptability and confidence in the evidence (at drug-level) in one 2 dimensional plot at each time point.", style = "font-size: 16px"),
                                                     p("**Since the confidence in the evidence was only assessed for ",
                                                       span("head-to-head trials", style = "color: red"),
                                                       " at", span(" 5-year interval", style = "color: red"),
                                                       ", this 2 dimensional plot is not as versatile as other functions. The conditions in the left sidebar are not applicable to it.", 
                                                       strong("ONLY THE YEARS LISTED BELOW CAN BE SELECTED FOR THIS PLOT."),
                                                       style = "font-size: 14px"),
                                                     br(),
                                                     helpText("All the evidence before the chosen year are synthesized. Please choose the ending time point."),
                                                     selectInput("year_evi", label = "Show the pooled evidence until", 
                                                                 choices = list("1990", "1995", "2000", "2005", "2010", "2016"),
                                                                 selected = "2016"),
                                                     hr(),
                                                     p("[2] CINeMA: Confidence in Network Meta-Analysis [Software]. Institute of Social and Preventive Medicine, University of Bern, 2017. Available from cinema.ispm.unibe.ch.",
                                                       style = "font-size: 14px; color: grey"),
                                                     br(),
                                                     #Add a spinner indicating waiting, because it really takes a long time
                                                     tags$head(tags$style(HTML(mycss))),
                                                     conditionalPanel(condition = "$('html').hasClass('shiny-busy')", 
                                                                      tags$div(id = "plot-container4", tags$img(src = "spinner.gif", id = "loading-spinner2")),
                                                                      tags$div("Please wait. The calculations of network meta-analysis may take up to several minutes.",id="loadmessage4")),
                                                     div(plotOutput("eviplot", width = "650px", height = "700px", click = "plot_click1"), align = "center"),
                                                     p("Please click the plot to see a zoomed one with the optimal ranges of x and y axis. (The zoomed plot will be displayed below.)", style = "color:red", align = "center"),  
                                                     h4("2 Dimensional Plot for Efficacy, Acceptability and Confidence in the Evidence", align = "center"),
                                                     hr(),
                                                     p(strong("Note: "),
                                                       "(1) Drugs in the upper right area are better balanced in both efficacy and acceptability.", align = "left"),
                                                     p("(2) The size of the node is proportionate to the inverse of the width of 95%CI regarding efficacy. The bigger the node, the more precise the CI is.", align = "left"),
                                                     p("(3) The colors in each pie chart is proportionate to the numbers of mixed estimates for the drug in question evaluated at four levels of GRADE certainty of evidence.
                             The four levels are: ",
                                                       span("green-high, ", style = "color:limegreen"), 
                                                       span("blue-moderate, ", style = "color:steelblue"), 
                                                       span("yellow-low, ", style = "color:yellow"), 
                                                       span("red-very low.", style = "color:red"), align = "left"),
                                                     div(plotOutput("eviplot_flex", width = "650px", height = "700px"), align = "center"),
                                                     br()
                                                   )
                                          ),
                                          tabPanel("Forest Plots",
                                                   fluidRow(
                                                     br(),
                                                     h4("The Forest Plot for Efficacy", align = "center"),
                                                     #Add a spinner indicating waiting, because it really takes a long time
                                                     tags$head(tags$style(HTML(mycss))),
                                                     conditionalPanel(condition = "$('html').hasClass('shiny-busy')", 
                                                                      tags$div(id = "plot-container5", tags$img(src = "spinner.gif", id = "loading-spinner1")),
                                                                      tags$div("Please wait. The calculations of network meta-analysis may take up to several minutes.",id="loadmessage5")),
                                                     plotOutput("forest_efficacy"),
                                                     h5(textOutput("ref_drug1"), align = "center", style = "color:red"),
                                                     br(),
                                                     br(),
                                                     br()
                                                   ),
                                                   fluidRow(
                                                     h4("The Forest Plot for Acceptability", align = "center"),
                                                     tags$head(tags$style(HTML(mycss))),
                                                     conditionalPanel(condition = "$('html').hasClass('shiny-busy')", 
                                                                      tags$div(id = "plot-container6", tags$img(src = "spinner.gif", id = "loading-spinner2")),
                                                                      tags$div("Please wait. The calculations of network meta-analysis may take up to several minutes.",id="loadmessage6")),
                                                     plotOutput("forest_acceptability"),
                                                     h5(textOutput("ref_drug2"), align = "center", style = "color:red")
                                                   )
                                          ),
                                          tabPanel("League Tables", 
                                                   fluidRow(
                                                     br(),
                                                     br(),
                                                     downloadButton("downloadleaguetable", "Download", align = "center"),
                                                     br(),
                                                     h4("You can download league table (in the form of csv file) here."),
                                                     hr(),
                                                     p(strong("Note: "),
                                                       "The lower left half shows efficacy, and the upper right half shows acceptability."),
                                                     p(strong("Caution: "),
                                                       "The league table cannot be constructed if the drugs in the efficacy network and the acceptability network do not match. So please make sure of it in the 'Basic Information', before you try to download the league table.")
                                                   ),
                                                   fluidRow(
                                                     br(),
                                                     br(),
                                                     hr(),
                                                     br(),
                                                     h4("The Colored League Table Showing Efficacy and Acceptability Together with Confidence in the Evidence", align = "center"),
                                                     p("We assessed the confidence in the evidence every 5 years since 1990, for head-to-head trials. We believe that it should be as important
       to see how credible the evidence are, as with the relative effect estimates. We evaluated it at 3 levels: RCT-level, comparison-level and drug-level. At ",
                                                       strong("RCT-level, "),
                                                       "we assessed the risk of bias for each RCT based on Cochrane Collaboration risk of bias tool. At ",
                                                       strong("comparison-level, "),
                                                       "we evaluated it at each time point using ", em("CINeMA framework"), " [2] through the dedicated software. Finally,
        the information was synthesized at ", strong("drug-level. "),
                                                       "We combined the efficacy, acceptability and confidence in the evidence (at drug-level) in one 2 dimensional plot at each time point.", style = "font-size: 16px"),
                                                     p("This colored league table shows the confidence in the evidence together with efficacy and accetability at comparison level. We colored 
                                                       each cell of the league table in terms of the overall confidence in evidence between two drugs: ",
                                                       span("green indicated high, ", style = "color:limegreen"), 
                                                       span("blue indicated moderate,", style = "color:steelblue"),
                                                       span("yellow indicated low,", style = "color:yellow"),
                                                       span("and red indicated very low", style = "color:red"), 
                                                       "confidence in evidence. ", style = "font-size: 16px"),
                                                     p("**Since the confidence in the evidence was only assessed for ",
                                                       span("head-to-head trials", style = "color: red"),
                                                       " at", span(" 5-year interval", style = "color: red"),
                                                       ", this table is not as versatile as other functions. The conditions in the left sidebar are not applicable to it.", 
                                                       strong("ONLY THE YEARS LISTED BELOW CAN BE SELECTED FOR THIS TABLE."),
                                                       style = "font-size: 14px"),
                                                     br(),
                                                     helpText("All the evidence before the chosen year are synthesized. Please choose the ending time point."),
                                                     selectInput("year_evi1", label = "Show the pooled evidence until", 
                                                                 choices = list("1990", "1995", "2000", "2005", "2010", "2016"),
                                                                 selected = "2016"),
                                                     hr(),
                                                     p("[2] CINeMA: Confidence in Network Meta-Analysis [Software]. Institute of Social and Preventive Medicine, University of Bern, 2017. Available from cinema.ispm.unibe.ch.",
                                                       style = "font-size: 14px; color: grey"),
                                                     br(), 
                                                     uiOutput("coloredleaguetable"),
                                                     hr(),
                                                     p(strong("Note: "),
                                                       "The lower left half shows efficacy, and the upper right half shows acceptability."),
                                                     br()
                                                   )
                                          ),
                                          tabPanel("Pairwise Comparisons", 
                                                   br(),
                                                   br(),
                                                   p("Select the 2 two drugs that you would like to compare the relative effect:", style = "font-size: 18px", align = "center"),
                                                   br(),
                                                   br(),
                                                   fluidRow(column(4,
                                                                   selectInput("drug1", label = "Drug A",
                                                                               choices = list("agomelatine", "amitriptyline", "bupropion", "citalopram", "clomipromine",
                                                                                              "desvenlafaxine", "duloxetine", "escitalopram", "fluoxetine", "fluvoxamine",
                                                                                              "levomilnacipran", "milnacipran", "mirtazapine", "nefazodone", "paroxetine",
                                                                                              "reboxetine", "sertraline", "trazadone", "venlafaxine", "vilazodone", "vortioxetine", "placebo"), 
                                                                               selected = "amitriptyline")),
                                                            column(4, 
                                                                   h3("VS.", align = "center")
                                                            ),
                                                            column(4,
                                                                   selectInput("drug2", label = "Drug B (reference drug)", 
                                                                               choices = list("agomelatine", "amitriptyline", "bupropion", "citalopram", "clomipromine",
                                                                                              "desvenlafaxine", "duloxetine", "escitalopram", "fluoxetine", "fluvoxamine",
                                                                                              "levomilnacipran", "milnacipran", "mirtazapine", "nefazodone", "paroxetine",
                                                                                              "reboxetine", "sertraline", "trazadone", "venlafaxine", "vilazodone", "vortioxetine", "placebo"), 
                                                                               selected = "fluoxetine"))
                                                   ),
                                                   fluidRow(
                                                     hr(),
                                                     textOutput("comparison_ef"),
                                                     textOutput("comparison_ac"),
                                                     hr(),
                                                     p(strong("Note: "),
                                                       "Please wait a moment for the result, since it needs time to process. The OR is with reference to Drug B.", align = "left"),
                                                     br(),
                                                     br()
                                                   )
                                          ),
                                          tabPanel("Funnel Plots",
                                                   fluidRow(
                                                     br(),
                                                     h4("The Comparison-adjusted Funnel Plot for Efficacy", align = "center"),
                                                     #Add a spinner indicating waiting, because it really takes a long time
                                                     tags$head(tags$style(HTML(mycss))),
                                                     conditionalPanel(condition = "$('html').hasClass('shiny-busy')", 
                                                                      tags$div(id = "plot-container5", tags$img(src = "spinner.gif", id = "loading-spinner1")),
                                                                      tags$div("Please wait. The calculations of network meta-analysis may take up to several minutes.",id="loadmessage5")),
                                                     div(imageOutput("funnel_efficacy", width = "650px", height = "700px"), align = "center"),
                                                     h5(textOutput("note_drug1"), align = "center", style = "color:red"),
                                                     br(),
                                                     br(),
                                                     br()
                                                   ),
                                                   fluidRow(
                                                     h4("The Comparison-adjusted Funnel Plot for Acceptability", align = "center"),
                                                     tags$head(tags$style(HTML(mycss))),
                                                     conditionalPanel(condition = "$('html').hasClass('shiny-busy')", 
                                                                      tags$div(id = "plot-container6", tags$img(src = "spinner.gif", id = "loading-spinner2")),
                                                                      tags$div("Please wait. The calculations of network meta-analysis may take up to several minutes.",id="loadmessage6")),
                                                     div(imageOutput("funnel_acceptability", width = "650px", height = "700px"), align = "center"),
                                                     h5(textOutput("note_drug2"), align = "center", style = "color:red"),
                                                     br(),
                                                     hr(),
                                                     p(strong("Note: "),
                                                       "The comparison-adjusted funnel plot is used to check if there were significant differences between precise and imprecise trials. 
                                                       If placebo-controlled trials are included, the plot shows all the direct comparisons with placebo. 
                                                       If only head-to-head trials are included, the plot shows only the studies directly compared with fluoxetine, which is the most frequently used active controls.", align = "left"),
                                                   )
                                          )
                                        )
                              )
                ),
                
                fluidRow(
                  column(6, div(style = "height:2620px;background-color:rgba(0,0,0,0);"))),
                fluidRow(
                  fillRow(div(style = "height:150px;background-color:rgba(95,158,209,1.0);")),
                  br(),
                  column(1),
                  column(11,
                         strong("Please cite:", style = "color:white"),
                         p("Luo Y, Chaimani A, Furukawa TA, Kataoka Y, Ogawa Y, Cipriani A, Salanti G.
                            Visualizing the Evolution of Evidence: Cumulative Network Meta-Analyses of  
                            New Generation Antidepressants in the Last 40 Years.", em(" (In submission)"), style = "font-size: 14px; color: white"),
                         br(),
                         strong("Copyright©2019 Department of Health Promotion and Human Behavior, 
                            Graduate School of Medicine, Kyoto University. All Rights Reserved.", style = "font-size: 15px;color:darkslategrey")
                )        
                  ))

server <- function(input, output, session) {
  #use reactive to increase efficiency
  #RCT type level -> different dataset
  dsefInput <- reactive({
    switch(input$RCTtype, 
           "Including placebo-controlled trials" = def_full,
           "Only head-to-head trials" = def_active)
  })
  dsacInput <- reactive({
    switch(input$RCTtype, 
           "Including placebo-controlled trials" = dac_full,
           "Only head-to-head trials" = dac_active)
  })
  
  #RCT type level -> different dataset for funnel plots
  dsfefInput <- reactive({
    switch(input$RCTtype, 
           "Including placebo-controlled trials" = dfef_plac,
           "Only head-to-head trials" = dfef_fluo)
  })
  dsfacInput <- reactive({
    switch(input$RCTtype, 
           "Including placebo-controlled trials" = dfac_plac,
           "Only head-to-head trials" = dfac_fluo)
  })
  
  #change period ane publication status (long data)
  defInput <- reactive({
    tmp.dsef <- subset(dsefInput(), study_year >= input$yearsince & study_year < input$yearuntil)
    if (input$published == TRUE) {
      subset(tmp.dsef, unpublished == 0)
    } else {
      tmp.dsef
    }
  })
  
  #change period and publication status (long data) : for funnel plots
  dfefInput <- reactive({
    tmp.dsfef <- subset(dsfefInput(), study_year >= input$yearsince & study_year < input$yearuntil)
    if (input$published == TRUE) {
      subset(tmp.dsfef, unpublished == 0)
    } else {
      tmp.dsfef
    }
  })
  
  #for confidence data
  defconInput <- reactive({
    subset(def_active, study_year < input$year_evi)
  })
  
  # NMA
  netefInput <- reactive({
    tmp.dsef <- subset(dsefInput(), study_year >= input$yearsince & study_year < input$yearuntil)
    if (input$published == TRUE) {
      tmp.def <- subset(tmp.dsef, unpublished == 0)
    } else {
      tmp.def <- tmp.dsef
    }
    tmp.def1 <- pairwise(treat = t, n, event = r, data = tmp.def, studlab = id, sm = "OR")
    
    if (any(grepl("placebo", defInput()$t)) == TRUE) {
      netmeta(tmp.def1, comb.fixed = FALSE, comb.random = TRUE, reference.group = "placebo")
    } else {
      netmeta(tmp.def1, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
    }
  })
  
  # NMA for funnel plots
  netfefInput <- reactive({
    tmp.dsfef <- subset(dsfefInput(), study_year >= input$yearsince & study_year < input$yearuntil)
    if (input$published == TRUE) {
      tmp.dfef <- subset(tmp.dsfef, unpublished == 0)
    } else {
      tmp.dfef <- tmp.dsfef
    }
    tmp.dfef1 <- pairwise(treat = t, n, event = r, data = tmp.dfef, studlab = id, sm = "OR")
    netmeta(tmp.dfef1, comb.fixed = FALSE, comb.random = TRUE)
  })
  
  # NMA for published data only: because we need 2 data sets in one 2D plot
  netefpInput <- reactive({
    tmp.dsefp <- subset(dsefInput(), study_year >= input$yearsince & study_year < input$yearuntil & unpublished == 0)
    tmp.defp1 <- pairwise(treat = t, n, event = r, data = tmp.dsefp, studlab = id, sm = "OR")
    
    if (any(grepl("placebo", defInput()$t)) == TRUE) {
      netmeta(tmp.defp1, comb.fixed = FALSE, comb.random = TRUE, reference.group = "placebo")
    } else {
      netmeta(tmp.defp1, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
    }
  })
  
  
  #NMA for confidence data
  netefconInput <- reactive({
    tmp.defcon <- pairwise(treat = t, n, event = r, data = defconInput(), studlab = id, sm = "OR")
    netmeta(tmp.defcon, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
  })
  
  dacInput <- reactive({
    tmp.dsac <- subset(dsacInput(), study_year >= input$yearsince & study_year < input$yearuntil)
    if (input$published == TRUE) {
      subset(tmp.dsac, unpublished == 0)
    } else {
      tmp.dsac
    }
  })
  dac1Input <- reactive({
    tmp.dsac <- subset(dsacInput(), study_year >= input$yearsince & study_year < input$yearuntil)
    if (input$published == TRUE) {
      tmp.dac <- subset(tmp.dsac, unpublished == 0)
    } else {
      tmp.dac <- tmp.dsac
    }
    pairwise(treat = t, n, event = r, data = tmp.dac, studlab = id, sm = "OR")
  })
  #change period and publication status (long data) : for funnel plots
  dfacInput <- reactive({
    tmp.dsfac <- subset(dsfacInput(), study_year >= input$yearsince & study_year < input$yearuntil)
    if (input$published == TRUE) {
      subset(tmp.dsfac, unpublished == 0)
    } else {
      tmp.dsfac
    }
  })
  
  dacconInput <- reactive({
    subset(dac_active, study_year < input$year_evi)
  })
  
  netacInput <- reactive({
    tmp.dsac <- subset(dsacInput(), study_year >= input$yearsince & study_year < input$yearuntil)
    if (input$published == TRUE) {
      tmp.dac <- subset(tmp.dsac, unpublished == 0)
    } else {
      tmp.dac <- tmp.dsac
    }
    tmp.dac1 <- pairwise(treat = t, n, event = r, data = tmp.dac, studlab = id, sm = "OR")
    
    if (any(grepl("placebo", dacInput()$t)) == TRUE) {
      netmeta(tmp.dac1, comb.fixed = FALSE, comb.random = TRUE, reference.group = "placebo")
    } else {
      netmeta(tmp.dac1, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
    }
  })
  
  # NMA for funnel plots
  netfacInput <- reactive({
    tmp.dsfac <- subset(dsfacInput(), study_year >= input$yearsince & study_year < input$yearuntil)
    if (input$published == TRUE) {
      tmp.dfac <- subset(tmp.dsfac, unpublished == 0)
    } else {
      tmp.dfac <- tmp.dsfac
    }
    tmp.dfac1 <- pairwise(treat = t, n, event = r, data = tmp.dfac, studlab = id, sm = "OR")
    netmeta(tmp.dfac1, comb.fixed = FALSE, comb.random = TRUE)
  })
  
  netacpInput <- reactive({
    tmp.dsacp <- subset(dsacInput(), study_year >= input$yearsince & study_year < input$yearuntil & unpublished == 0)
    tmp.dacp1 <- pairwise(treat = t, n, event = r, data = tmp.dsacp, studlab = id, sm = "OR")
    
    if (any(grepl("placebo", dacInput()$t)) == TRUE) {
      netmeta(tmp.dacp1, comb.fixed = FALSE, comb.random = TRUE, reference.group = "placebo")
    } else {
      netmeta(tmp.dacp1, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
    }
  })
  netacconInput <- reactive({
    tmp.daccon <- pairwise(treat = t, n, event = r, data = dacconInput(), studlab = id, sm = "OR")
    netmeta(tmp.daccon, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
  })
  
  dconInput <- reactive({
    if (input$year_evi == 2016) {
      dcon16
    } else if (input$year_evi == 2010) {
      dcon10
    } else if (input$year_evi == 2005) {
      dcon05
    } else if (input$year_evi == 2000) {
      dcon00
    } else if (input$year_evi == 1995) {
      dcon95
    } else {
      dcon90
    }
  })
  
  dator <- reactive({
    defor <- data.frame(trts = netefInput()$trts,
                        ORef = exp(netefInput()$TE.random[, netefInput()$reference.group]),
                        loweref = exp(netefInput()$lower.random[, netefInput()$reference.group]),
                        upperef = exp(netefInput()$upper.random[, netefInput()$reference.group]),
                        sample_ef = netefInput()$n.trts)
    defor$trts <- substr(defor$trts, 0, 4)
    dacor <- data.frame(trts = netacInput()$trts,
                        ORac = exp(netacInput()$TE.random[, netacInput()$reference.group]),
                        lowerac = exp(netacInput()$lower.random[, netacInput()$reference.group]),
                        upperac = exp(netacInput()$upper.random[, netacInput()$reference.group]),
                        sample_ac = netacInput()$n.trts)
    dacor$trts <- substr(dacor$trts, 0, 4)
    merge(defor, dacor, by = "trts", all = TRUE)
  }) 
  
  datorp <- reactive({
    deforp <- data.frame(trts = netefpInput()$trts,
                         ORef = exp(netefpInput()$TE.random[, netefpInput()$reference.group]),
                         loweref = exp(netefpInput()$lower.random[, netefpInput()$reference.group]),
                         upperef = exp(netefpInput()$upper.random[, netefpInput()$reference.group]),
                         sample_ef = netefpInput()$n.trts)
    deforp$trts <- substr(deforp$trts, 0, 4)
    dacorp <- data.frame(trts = netacpInput()$trts,
                         ORac = exp(netacpInput()$TE.random[, netacpInput()$reference.group]),
                         lowerac = exp(netacpInput()$lower.random[, netacpInput()$reference.group]),
                         upperac = exp(netacpInput()$upper.random[, netacpInput()$reference.group]),
                         sample_ac = netacpInput()$n.trts)
    dacorp$trts <- substr(dacorp$trts, 0, 4)
    merge(deforp, dacorp, by = "trts", all = TRUE)
  })
  
  datorcon <- reactive({
    deforcon <- data.frame(trts = netefconInput()$trts,
                           ORef = exp(netefconInput()$TE.random[, netefconInput()$reference.group]),
                           loweref = exp(netefconInput()$lower.random[, netefconInput()$reference.group]),
                           upperef = exp(netefconInput()$upper.random[, netefconInput()$reference.group]),
                           radius = 1/(5*netefconInput()$seTE.random[, netefconInput()$reference.group]))
    deforcon["citalopram", "radius"] = 1
    deforcon$trts <- substr(deforcon$trts, 0, 4)
    dacorcon <- data.frame(trts = netacconInput()$trts,
                           ORac = exp(netacconInput()$TE.random[, netacconInput()$reference.group]),
                           lowerac = exp(netacconInput()$lower.random[, netacconInput()$reference.group]),
                           upperac = exp(netacconInput()$upper.random[, netacconInput()$reference.group]))
    dacorcon$trts <- substr(dacorcon$trts, 0, 4)
    merge(deforcon, dacorcon, by = "trts", all = TRUE)
  }) 
  
  datorev <- reactive({
    na.omit(merge(datorcon(), dconInput(), by = "trts", all = TRUE))
  })
  
  #link to the R scripts for NMA
  output$study_numbers_ef <-renderText({
    study_numbers_ef <- n_distinct(defInput()$id)
    paste("The number of included studies:", study_numbers_ef)
  })
  output$treat_numbers_ef <-renderText({
    treat_numbers_ef <- n_distinct(defInput()$t)
    paste("The number of included treatments:", treat_numbers_ef)
  })
  output$treat_list_ef <-renderText({
    treat_list_ef <- unique(defInput()$t)
  })
  output$heterog_ef <-renderText({
    h_ef <- round(netefInput()$Q.heterogeneity, digits = 3)
    if (round(netefInput()$pval.Q.heterogeneity, digits = 3)==0) {
      pval.h_ef <- "<0.001"
    } else {
      pval.h_ef <- round(netefInput()$pval.Q.heterogeneity, digits = 3)
    }
    paste("The overall heterogeneity Q-statistic is:", h_ef, ", and the p value for the test is ", pval.h_ef)
  })
  output$inconsist_ef <-renderText({
    i_ef <- round(netefInput()$Q.inconsistency, digits = 3)
    if (round(netefInput()$pval.Q.inconsistency, digits = 3)==0) {
      pval.i_ef <- "<0.001"
    } else {
      pval.i_ef <- round(netefInput()$pval.Q.inconsistency, digits = 3)
    }
    paste("The overall inconsistency Q-statistic is:", i_ef, ", and the p value for the test is ", pval.i_ef)
  })
  output$I2_ef <-renderText({
    I2_ef <- round(netefInput()$I2,digits = 3)
    paste("The I-squared is:", I2_ef)
  })
  output$tau_ef <-renderText({
    tau_ef <- round(netefInput()$tau,digits = 3)
    paste("The tau (square-root of between-study variance) is:", tau_ef)
  })
  
  output$study_numbers_ac <-renderText({
    study_numbers_ac <- n_distinct(dacInput()$id)
    paste("The number of included studies:", study_numbers_ac)
  })
  output$treat_numbers_ac <-renderText({
    treat_numbers_ac <- n_distinct(dacInput()$t)
    paste("The number of included treatments:", treat_numbers_ac)
  })
  output$treat_list_ac <-renderText({
    treat_list_ac <- unique(dacInput()$t)
  })
  output$heterog_ac <-renderText({
    h_ac <- round(netacInput()$Q.heterogeneity,digits = 3)
    if (round(netacInput()$pval.Q.heterogeneity, digits = 3)==0) {
      pval.h_ac <- "<0.001"
    } else {
      pval.h_ac <- round(netacInput()$pval.Q.heterogeneity, digits = 3)
    }
    paste("The overall heterogeneity Q-statistic is:", h_ac, ", and the p value for the test is ", pval.h_ac)
  })
  output$inconsist_ac <-renderText({
    i_ac <- round(netacInput()$Q.inconsistency,digits = 3)
    if (round(netacInput()$pval.Q.inconsistency,digits = 3)==0) {
      pval.i_ac <- "<0.001"
    } else {
      pval.i_ac <- round(netacInput()$pval.Q.inconsistency,digits = 3)
    }
    paste("The overall inconsistency Q-statistic is:", i_ac, ", and the p value for the test is ", pval.i_ac)
  })
  output$I2_ac <-renderText({
    I2_ac <- round(netacInput()$I2,digits = 3)
    paste("The I-square is:", I2_ac)
  })
  output$tau_ac <-renderText({
    tau_ac <- round(netacInput()$tau,digits = 3)
    paste("The tau (square-root of between-study variance) is:", tau_ac)
  })
  
  output$ref_drug1 <- renderText({
    if (any(grepl("placebo", defInput()$t)) == TRUE) {
      ref_drug <- "placebo"
    } else {
      ref_drug <- "citalopram"
    }
    paste("Note: the reference is ", ref_drug)
  })
  output$ref_drug2 <- renderText({
    if (any(grepl("placebo", dacInput()$t)) == TRUE) {
      ref_drug <- "placebo"
    } else {
      ref_drug <- "citalopram"
    }
    paste("Note: the reference is ", ref_drug)
  })
  
  output$note_drug1 <- renderText({
    if (any(grepl("placebo", dfefInput()$t)) == TRUE) {
      note_drug1 <- "Note: only placebo-controlled studies are included in this plot."
    } else {
      note_drug1 <- "Note: only fluoxetine-controlled studies are included in this plot."
    }
  })
  output$note_drug2 <- renderText({
    if (any(grepl("placebo", dfacInput()$t)) == TRUE) {
      note_drug2 <- "Note: only placebo-controlled studies are included in this plot."
    } else {
      note_drug2 <- "Note: only fluoxetine-controlled studies are included in this plot."
    }
  })
  
  output$netgraph_efficacy <- renderPlot({
    netgraph_efficacy <- netgraph(netefInput(), start ="circle", cex = 0.7, col = "black", plastic = FALSE, 
                                  points = TRUE, col.points = "darkgreen", cex.points =15*sqrt(n.trts/max(n.trts)), 
                                  thickness = "number.of.studies", lwd.max = 12, lwd.min = 1, multiarm = FALSE)
    
  })
  output$netgraph_acceptability <- renderPlot({
    netgraph_acceptability <- netgraph(netacInput(), start ="circle", cex = 0.7, col = "black", plastic = FALSE, 
                                       points = TRUE, col.points = "darkred", cex.points =15*sqrt(n.trts/max(n.trts)), 
                                       thickness = "number.of.studies", lwd.max = 12, lwd.min = 1, multiarm = FALSE)
    
  })
  output$forest_efficacy <- renderPlot({
    if (any(grepl("placebo", defInput()$t)) == TRUE) {
      forest_efficacy <- forest(netefInput(), ref = "placebo", pooled = "random", digits = 2, smlab = "Random effects model", 
                                label.left = "Favors placebo", label.right = "Favors the active drug", leftlabs = "Efficacy", 
                                drop = TRUE, sortvar = -TE, col.by = "blue")
    } else {
      forest_efficacy <- forest(netefInput(), ref = "citalopram", pooled = "random", digits = 2, smlab = "Random effects model", 
                                label.left = "Favors citalopram", label.right = "Favors the other", leftlabs = "Efficacy", 
                                drop = TRUE, sortvar = -TE, col.by = "blue")
    }
    
  })
  output$forest_acceptability <- renderPlot({
    if (any(grepl("placebo", dacInput()$t)) == TRUE) {
      forest_acceptability <- forest(netacInput(), ref = "placebo", pooled = "random", digits = 2, smlab = "Random effects model", 
                                     label.left = "Favors the active drug", label.right = "Favors placebo", leftlabs = "Acceptability", 
                                     drop = TRUE, sortvar = TE, col.by = "blue")
    } else {
      forest_acceptability <- forest(netacInput(), ref = "citalopram", pooled = "random", digits = 2, smlab = "Random effects model", 
                                     label.left = "Favors the other", label.right = "Favors citalopram", leftlabs = "Acceptability", 
                                     drop = TRUE, sortvar = TE, col.by = "blue")
    }
  })
  ##create 2D plot by using gglpot2
  output$twodplot <- renderPlot({
    if (any(grepl("placebo", defInput()$t)) == TRUE) {
      ggplot(dator(), aes(x = ORef, y = ORac, colour = trts, label = trts)) + labs(x = "OR of efficacy", y = "OR of acceptability") +
        geom_point(aes(size = log(sample_ef + sample_ac))) + geom_point(data = datorp(), shape = 1, aes(size = log(sample_ef + sample_ac))) + 
        scale_y_continuous(trans = reverselog_trans(10), limits = c(2.2, 0.2), breaks = seq(2.2, 0.2, -0.2)) + 
        scale_x_continuous(limits = c(0.6, 3.6), breaks = seq(0.6, 3.6, 0.2), trans = "log10") +
        annotation_logticks() +
        geom_text_repel(data = dator(), point.padding = 0.4, force = 5, segment.size = 0.2) + 
        geom_hline(yintercept = 1, linetype = "dotted") + geom_vline(xintercept = 1, linetype = "dotted") + theme_bw() + theme(legend.position = "none")
    } else {
      ggplot(dator(), aes(x = ORef, y = ORac, colour = trts, label = trts)) + labs(x = "OR of efficacy", y = "OR of acceptability") +
        geom_point(aes(size = log(sample_ef + sample_ac))) +
        scale_y_continuous(trans = reverselog_trans(10), limits = c(2.2, 0.2), breaks = seq(2.2, 0.2, -0.2)) + 
        scale_x_continuous(limits = c(0.6, 3.6), breaks = seq(0.6, 3.6, 0.2), trans = "log10") +
        annotation_logticks() +
        geom_text_repel(point.padding = 0.2, force = 3, segment.size = 0.2) + 
        geom_hline(yintercept = 1, linetype = "dotted") + geom_vline(xintercept = 1, linetype = "dotted") + theme_bw() + theme(legend.position = "none")
    }
  })
  
  count<-0
  observeEvent(input$plot_click, {
    count <<- count + 1
    if (count %% 2 == 0) {
      output$twodplot_flex <- renderPlot({
        NULL
      })
    }
    else {
      output$twodplot_flex <- renderPlot({
        if (any(grepl("placebo", defInput()$t)) == TRUE) {
          ggplot(dator(), aes(x = ORef, y = ORac, colour = trts)) + labs(x = "OR of efficacy", y = "OR of acceptability") +
            geom_point(aes(size = log(sample_ef + sample_ac))) + geom_point(data = datorp(), shape = 1, aes(size = log(sample_ef + sample_ac))) + scale_y_continuous(trans = "reverse") + 
            geom_text(aes(label = trts), hjust=1, vjust=2, check_overlap = T, data = dator()) + geom_text(aes(label = trts), hjust=1, vjust=2, check_overlap = T, data = datorp()) + theme_bw() + theme(legend.position = "none") + 
            geom_hline(yintercept = 1, linetype = "dotted") + geom_vline(xintercept = 1, linetype = "dotted")
        } else {
          ggplot(dator(), aes(x = ORef, y = ORac, colour = trts)) + labs(x = "OR of efficacy", y = "OR of acceptability") +
            geom_point(aes(size = log(sample_ef + sample_ac)))  + scale_y_continuous(trans = "reverse") + 
            geom_text(aes(label = trts), hjust=1, vjust=2, check_overlap = T, data = dator()) + theme_bw() + theme(legend.position = "none") + 
            geom_hline(yintercept = 1, linetype = "dotted") + geom_vline(xintercept = 1, linetype = "dotted")
        }
      }) 
    }
  })
  
  output$downloadleaguetable <- downloadHandler(
    filename = function(){
      paste(input$yearsince, "-", input$yearuntil, "-", input$RCTtype, "-", "League table.csv", sep = "")
    },
    content = function(file){
      league_table <- netleague(netefInput(), netacInput(), backtransf = TRUE, direct = FALSE, comb.fixed = FALSE, digits =2, bracket = "(", separator = " to ")
      write.table(league_table$random, file, row.names = FALSE, col.names = FALSE, sep = ",")
    }
  )
  
  output$coloredleaguetable <- renderUI({
    if (input$year_evi1 == 2016) {
      img(src = '2016.jpg', width = "90%")
    } else if (input$year_evi1 == 2010) {
      img(src = '2010.jpg', width = "90%")
    } else if (input$year_evi1 == 2005) {
      img(src = '2005.jpg', width = "90%")
    } else if (input$year_evi1 == 2000) {
      img(src = '2000.jpg', width = "90%")
    } else if (input$year_evi1 == 1995) {
      img(src = '1995.jpg', width = "90%")
    } else {
      img(src = '1990.jpg', width = "90%")
    }
  })
  
  output$comparison_ef <-renderText({
    validate(
      need(input$drug1 %in% defInput()$t, "It is likely that drug A is missing in the efficacy network you select. Please choose another drug."),
      need(input$drug2 %in% defInput()$t, "It is likely that drug B is missing in the efficacy network you select. Please choose another drug.")
    )
    comparison_ef_or <- round(exp(netefInput()$TE.random[input$drug1, input$drug2]), digits = 3)
    comparison_ef_lo <- round(exp(netefInput()$lower.random[input$drug1, input$drug2]), digits = 3)
    comparison_ef_up <- round(exp(netefInput()$upper.random[input$drug1, input$drug2]), digits = 3)
    paste("The OR for efficacy of ", input$drug1, " compared with ", input$drug2, "is: ", 
          comparison_ef_or, " (", comparison_ef_lo, " to ", comparison_ef_up, ")")
  })
  output$comparison_ac <-renderText({
    validate(
      need(input$drug1 %in% dacInput()$t, "It is likely that drug A is missing in the acceptability network you select. Please choose another drug."),
      need(input$drug2 %in% dacInput()$t, "It is likely that drug B is missing in the acceptability network you select. Please choose another drug.")
    )
    comparison_ac_or <- round(exp(netacInput()$TE.random[input$drug1, input$drug2]), digits = 3)
    comparison_ac_lo <- round(exp(netacInput()$lower.random[input$drug1, input$drug2]), digits = 3)
    comparison_ac_up <- round(exp(netacInput()$upper.random[input$drug1, input$drug2]), digits = 3)
    paste("The OR for acceptability of ", input$drug1, " compared with ", input$drug2, "is: ", 
          comparison_ac_or, " (", comparison_ac_lo, " to ", comparison_ac_up, ")")
  })
  
  output$eviplot <- renderPlot({
    ggplot(datorev(), aes(x = ORef, y = ORac, label = trts)) + labs(x = "OR of efficacy", y = "OR of acceptability") +
      scale_y_continuous(trans = reverselog_trans(10), limits = c(2.2, 0.2), breaks = seq(2.2, 0.2, -0.2)) + 
      scale_x_continuous(limits = c(0.6, 3.6), breaks = seq(0.6, 3.6, 0.2), trans = "log10") +
      annotation_logticks() +
      geom_text_repel(data = datorev(), 
                      col = "gray29", 
                      cex = 4, point.padding = 0.7, force = 5, segment.size = 0.2, segment.color = "grey50") +
      geom_hline(yintercept = 1, linetype = "dotted", col = "lightblue4") + geom_vline(xintercept = 1, linetype = "dotted", col = "lightblue4") +
      geom_scatterpie(data = datorev(), aes(x = ORef, y = ORac, r = 0.012*radius), cols = colnames(datorev()[, 9:12]), alpha = 0.9, color = NA, show.legend = FALSE) +
      scale_fill_manual(values = c(vlow = "red", low = "yellow", mod = "steelblue3", high = "limegreen")) + theme_bw()
  })
  
  count1<-0
  observeEvent(input$plot_click1, {
    count1 <<- count1 + 1
    if (count1 %% 2 == 0) {
      output$eviplot_flex <- renderPlot({
        NULL
      })
    }
    else {
      output$eviplot_flex <- renderPlot({
        ggplot(datorev(), aes(x = ORef, y = ORac, label = trts)) + labs(x = "OR of efficacy", y = "OR of acceptability") +
          scale_y_continuous(trans = "reverse") +
          geom_text_repel(data = datorev(), 
                          col = "gray29", 
                          cex = 4, point.padding = 0.7, force = 5, segment.size = 0.2, segment.color = "grey50") +
          geom_hline(yintercept = 1, linetype = "dotted", col = "lightblue4") + geom_vline(xintercept = 1, linetype = "dotted", col = "lightblue4") +
          geom_scatterpie(data = datorev(), aes(x = ORef, y = ORac, r = 0.012*radius), cols = colnames(datorev()[, 9:12]), alpha = 0.9, color = NA, show.legend = FALSE) +
          scale_fill_manual(values = c(vlow = "red", low = "yellow", mod = "steelblue3", high = "limegreen")) + theme_bw()
      })
    }
  })
  
  output$funnel_efficacy <- renderImage({
    if (any(grepl("placebo", dfefInput()$t)) == TRUE) {
      list <- substr(netfefInput()$trts, 0, 4)
      comparison <- list[list != "plac"] 
      ord <- c(comparison, "plac")
      #A temp file to save the output.
      #This file will be removed later by renderImage
      funnel_plot_ef <- tempfile(fileext = '.png')
      #generate the png
      png(funnel_plot_ef, width = 6, height = 6,
          units = 'in', res = 300)
      funnel(netfefInput(), order = ord, pch = rep(19), col = colors[comparison], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
      legend("topleft", legend = comparison, pch = rep(19),col = colors[comparison], cex = 0.75) 
      dev.off()
      #return a list containing the file name
      list(src = funnel_plot_ef, contentType = 'image/png', width = "650px", height = "700px")
    } else {
      list <- substr(netfefInput()$trts, 0, 4)
      comparison <- list[list != "fluo"] 
      ord <- c(comparison, "fluo")
      #A temp file to save the output.
      #This file will be removed later by renderImage
      funnel_plot_ef <- tempfile(fileext = '.png')
      #generate the png
      png(funnel_plot_ef, width = 6, height = 6,
          units = 'in', res = 300)
      funnel(netfefInput(), order = ord, pch = rep(19), col = colors[comparison], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
      legend("topleft", legend = comparison, pch = rep(19),col = colors[comparison], cex = 0.75) 
      dev.off()
      #return a list containing the file name
      list(src = funnel_plot_ef, contentType = 'image/png', width = "650px", height = "700px")
    }
  }, deleteFile = TRUE)
  
  output$funnel_acceptability <- renderImage({
    if (any(grepl("placebo", dfacInput()$t)) == TRUE) {
      listac <- substr(netfacInput()$trts, 0, 4)
      comparisonac <- listac[listac != "plac"] 
      ordac <- c(comparisonac, "plac")
      #A temp file to save the output.
      #This file will be removed later by renderImage
      funnel_plot_ac <- tempfile(fileext = '.png')
      #generate the png
      png(funnel_plot_ac, width = 6, height = 6,
          units = 'in', res = 300)
      funnel(netfacInput(), order = ordac, pch = rep(19), col = colors[comparisonac], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
      legend("topleft", legend = comparisonac, pch = rep(19),col = colors[comparisonac], cex = 0.75) 
      dev.off()
      #return a list containing the file name
      list(src = funnel_plot_ac, contentType = 'image/png', width = "650px", height = "700px")
    } else {
      listac <- substr(netfacInput()$trts, 0, 4)
      comparisonac <- listac[listac != "fluo"] 
      ordac <- c(comparisonac, "fluo")
      #A temp file to save the output.
      #This file will be removed later by renderImage
      funnel_plot_ac <- tempfile(fileext = '.png')
      #generate the png
      png(funnel_plot_ac, width = 6, height = 6,
          units = 'in', res = 300)
      funnel(netfacInput(), order = ordac, pch = rep(19), col = colors[comparisonac], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
      legend("topleft", legend = comparisonac, pch = rep(19),col = colors[comparisonac], cex = 0.75) 
      dev.off()
      #return a list containing the file name
      list(src = funnel_plot_ac, contentType = 'image/png', width = "650px", height = "700px")
    }
  }, deleteFile = TRUE)
  
}

shinyApp(ui = ui, server = server)
