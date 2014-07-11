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
          column(6, selectizeInput("inFactor", label="Factor", choices=c(), options=c(placeholder="Select variable..."))),
          column(6, selectizeInput("inTarget", label="Target", choices=c(), options=c(placeholder="Select factor...")))
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
      )
    )
  )
))