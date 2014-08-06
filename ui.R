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
    downloadButton("downloadData", "Sample Dataset")
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
            htmlOutput("normality")
          ),
          tabPanel("Plots",
            selectInput("inTransformation", label="Transformation", 
              choices=names(characterize.transfomations), 
              selected="None"
            ),
            uiOutput("plots")
          )
        )
      ),
      tabPanel("Tests",
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
      tabPanel("Batch Tests",
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
            sliderInput("batchShapiroThreshold", label = "Shapiro Threshold", min = 0.01, max = 0.99, step = 0.01, value = 0.1),
            sliderInput("batchBartlettThreshold", label = "Bartlett Threshold", min = 0.01, max = 0.99, step = 0.01, value = 0.05),
            sliderInput("batchFlignerThreshold", label = "Fligner Threshold", min = 0.01, max = 0.99, step = 0.01, value = 0.05),
            actionButton("batchDoIt", label = "Generate")
          ),
          htmlOutput("batch")
        )
      )
    )
  )
));