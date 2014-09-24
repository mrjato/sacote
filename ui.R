# This file is part of Sample Comparison Tests.
# 
# Copyright (c) 2014, Miguel Reboiro Jato and Daniel González Peña, 
# All rights reserved.
#
# Sample Comparison Tests is free software: you can redistribute it and/or 
# modify it under the terms of the GNU Lesser General Public License as 
# published by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Sample Comparison Tests is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Lesser General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Sample Comparison Tests. If not, see 
# <http://www.gnu.org/licenses/>.

library(shiny);
source("characterization.R");

shinyUI(pageWithSidebar(
  titlePanel("Sample Comparison Tests"),
  
  sidebarPanel(
    fileInput('datafile', 'Upload Dataset',
      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
    ),
    downloadButton("downloadSample1", "Sample Dataset 1 (Survey)"),
    downloadButton("downloadSample2", "Sample Dataset 2 (Oil)")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Dataset",
        dataTableOutput("dataset")
      ),
      tabPanel("Normality & Homoscedasticity", 
        fluidRow(
          column(6, selectInput("inFactor", label="Factor", choices=c())),
          column(6, selectInput("inTarget", label="Target", choices=c()))
        ),
        tabsetPanel(
          tabPanel("Tests",
            checkboxGroupInput("inTransformations", label = "Transformations",
              choices = names(characterize.transfomations), 
              selected = "None",
              inline = TRUE
            ),
            htmlOutput("normality")
          ),
          tabPanel("Plots",
            selectInput("inTransformation", label="Transformation", 
              choices=names(characterize.transfomations), 
              selected="None"
            ),
            plotOutput("boxplot"),
            selectInput("barplotType", label="Error Bars",
              choices=list("Standard Deviation" = "sd", "Standard Error" = "se", "Confidence Interval" = "ci"),
              selected="sd"
            ),
            plotOutput("barplot"),
            uiOutput("plots")
          )
        )
      ),
      tabPanel("Hypothesis Tests",
        wellPanel(
          fluidRow(
            column(4, selectInput("testsFactor", label="Factor", choices = c())),
            column(4, selectInput("testsTarget", label="Target", choices = c())),
            column(4, selectInput("testsTransformation", label="Transformation",
              choices=names(characterize.transfomations), 
              selected="None")
            )
          ),
          checkboxInput("autoTest", label = "Autocheck normality and homoscedasticity", value = TRUE),
          conditionalPanel("!input.autoTest",
            fluidRow(
              column(4, checkboxInput("testsIsNormal", label="Is Normal?", value = FALSE)),
              column(4, checkboxInput("testsIsHomoscedastic", label="Is Homoscedastic?", value = FALSE))
            )
          )
        ),
        htmlOutput("tests")
      ),
      tabPanel("Batch Hypothesis Tests",
        conditionalPanel("output.dataset",
          wellPanel(
            tags$h4("Configuration"),
            fluidRow(
              column(4, checkboxGroupInput("batchFactors", label = "Factors", choices = c(""))),
              column(4, checkboxGroupInput("batchTargets", label = "Targets", choices = c(""))),
              column(4, checkboxGroupInput("batchTransformations", label = "Transformations",
                choices=names(characterize.transfomations), 
                selected="None"
              ))
            ),
            tags$hr(),
            sliderInput("batchShapiroThreshold", label = "Shapiro Threshold (Normal if greater or equal)", min = 0.01, max = 0.99, step = 0.01, value = 0.1),
            numericInput("batchSampleSizeThreshold", label = "Sample Size Threshold (Normal if each group of samples is greater or equal)", min = 1, max = 1000, step = 1, value = 30),
            sliderInput("batchBartlettThreshold", label = "Bartlett Threshold (Homoscedastic if lower)", min = 0.01, max = 0.99, step = 0.01, value = 0.05),
            sliderInput("batchFlignerThreshold", label = "Fligner Threshold (Homoscedastic if lower)", min = 0.01, max = 0.99, step = 0.01, value = 0.05),
            actionButton("batchDoIt", label = "Calculate")
          ),
          downloadButton("batchDownload", "Download Table"),
          htmlOutput("batch")
        )
      ),
      tabPanel("Help",
        tags$h3("References"),
        tags$div(class="well",
          tags$ul(
            tags$li(tags$a(
              "Normality Tests for Statistical Analysis: A Guide for Non-Statisticians", 
              href="ijem-10-486.pdf"
            )),
            tags$li(tags$a(
              "The Unicorn, The Normal Curve, and Other Improbable Creatures [Micceri's Test]", 
              href="micceri89.pdf"
            )),
            tags$li(tags$a(
              "Homoscedasticity and heteroscedasticity", 
              href="http://udel.edu/~mcdonald/stathomog.html"
            )),
            tags$li(tags$a(
              "Fligner-Killeen Test", 
              href="http://stat.ethz.ch/R-manual/R-patched/library/stats/html/fligner.test.html"
            )),
            tags$li(tags$a(
              "Kruskal–Wallis test and Mann–Whitney U test",
              href="http://udel.edu/~mcdonald/statkruskalwallis.html"
            )),
            tags$li(tags$a(
              "Two Sample t-Test for Difference of the Population Means (Unequal Variances)",
              href="http://mathnstats.com/index.php/hypothesis-testing/82-tests-for-means/122-two-sample-t-test-unequal-variances.html"
            )),
            tags$li(tags$a(
              "The Normal Distribution",
              href="The Normal Distribution"
            ))
          )
        ),
        tags$h3("Automatic Tests Decision Tree"),
        tags$img(src = "tests.png")
      )
    )
  )
));