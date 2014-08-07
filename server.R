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
library(plyr);
source("characterization.R");
source("micceri.R");
source("tests.R");

shinyServer(function(input, output, session) {
  output$downloadSample1 <- downloadHandler(
    filename = function() { "data-survey.csv" },
    content = function(file) {
      write.csv(read.csv("data-survey.csv"), file, row.names=FALSE, quote=FALSE);
    }
  );
  output$downloadSample2 <- downloadHandler(
    filename = function() { "data-oil.csv" },
    content = function(file) {
      write.csv(read.csv("data-oil.csv"), file, row.names=FALSE, quote=FALSE);
    }
  );
  
  loadDataset <- reactive({
    if (is.null(input$datafile)) {
      return (NULL);
    } else {
      return (read.csv(input$datafile$datapath));
    }
  });
  
  output$dataset <- renderDataTable({
    return (loadDataset());
  }, options = list(iDisplayLength = 10));
  
  observe({
    dataset <- loadDataset();
    
    if (!is.null(dataset)) {
      # Factor selection checkbox group update
      choices <- as.list(names(dataset));
      names(choices) <- names(dataset);
      
      updateSelectInput(session, "inFactor", choices=choices, selected=choices[1]);
      updateSelectInput(session, "testsFactor", choices=choices, selected=choices[1]);
      updateCheckboxGroupInput(session, "batchFactors", choices = choices, selected = choices[1]);
      updateCheckboxGroupInput(session, "batchTargets", choices = choices, selected = choices[length(choices)]);
    }
  });
  
  observe({
    dataset <- loadDataset();
    factor <- input$inFactor;
    
    if (!is.null(dataset) && !is.na(factor) && factor != "") {
      # Factor selection checkbox group update
      choices <- as.list(names(dataset));
      names(choices) <- names(dataset);
      
      choices <- choices[choices != factor];
      updateSelectInput(session, "inTarget", choices=choices, selected=choices[length(choices)]);
      updateSelectInput(session, "testsTarget", choices=choices, selected=choices[length(choices)]);
    }
  });
  
  output$normality <- renderText({
    dataset <- loadDataset();
    factor <- input$inFactor;
    target <- input$inTarget;
    transformations <- input$inTransformations;
    
    if (is.null(dataset) || 
      is.na(factor) || factor == "" || 
      is.na(target) || target == "" ||
      length(transformations) == 0
    ) {
      return ("");
    } else {
      filteredDataset <- dataset[,(names(dataset) %in% c(factor, target))];
      filteredDataset[,factor] <- as.factor(filteredDataset[[factor]]);
      
      transformationsParam <- characterize.transfomations[names(characterize.transfomations) %in% transformations];
      
      characterization <- characterize(filteredDataset, transformationsParam);
      
      descStats <- ddply(filteredDataset, c(factor), function(x) {
        data.frame(
          Size = length(x[[target]]),
          Median = median(x[[target]]),
          Mean = mean(x[[target]]),
          SD = sd(x[[target]]),
          Min = min(x[[target]]),
          Max = max(x[[target]])
        )
      });
      
      return (paste(sep="", 
        tags$table(class="table table-bordered table-hover table-condensed",
          tags$caption(paste(factor, "vs", target)),
          lapply(c(names(descStats)), function(stats) {
            tags$tr(
              tags$td(colspan="3", stats),
              apply(descStats, 1, function(x) tags$td(x[stats]))
            ) 
          }),
          lapply(characterization, function(charact) {
            isNormal <- all(charact@shapiro$p.value >= 0.1);
            columns <- length(charact@shapiro$p.value);
            
            tagList(
              tags$tr(
                tags$td(rowspan="5", charact@transformation.name),
                tags$td(colspan="2", "Shapiro (Normality)"),
                apply(descStats, 1, function(stats) {
                  p.value <- charact@shapiro$p.value[charact@shapiro[[factor]] == stats[factor]];
                  
                  tags$td(
                    HTML(ifelse(p.value >= 0.1, paste(sep="", "<strong>", p.value, "</strong>"), p.value))
                  );
                })
              ),
              tags$tr(
                tags$td(rowspan="2", "Micceri"),
                tags$td("Tail"),
                apply(descStats, 1, function(stats) {
                  tail_weight_label <- charact@micceri$tail_weight_label[charact@micceri[[factor]] == stats[factor]];
                  
                  tags$td(style = paste("background-color:", micceri.colorForTailWeight(tail_weight_label)),
                    tail_weight_label
                  );
                })
              ),
              tags$tr(
                tags$td("Symmetry"),
                apply(descStats, 1, function(stats) {
                  symmetry_label <- charact@micceri$symmetry_label[charact@micceri[[factor]] == stats[factor]];
                  
                  tags$td(style = paste("background-color:", micceri.colorForSymmetry(symmetry_label)), 
                    symmetry_label
                  );
                })
              ),
              tags$tr(class = ifelse(isNormal, "success", "warning"),
                tags$td(colspan="2", "Bartlett (Homos. if normal)"),
                tags$td(colspan=columns, 
                  HTML(ifelse(isNormal && charact@bartlett < 0.05, paste(sep="", "<strong>", charact@bartlett, "</strong>"), charact@bartlett))
                )
              ),
              tags$tr(class = ifelse(!isNormal, "success", "warning"),
                tags$td(colspan="2", "Fligner (Homos. if non-normal)"),
                tags$td(colspan=columns, 
                  HTML(ifelse(!isNormal && charact@fligner < 0.05, paste(sep="", "<strong>", charact@fligner, "</strong>"), charact@fligner))
                )
              )
            )
          })
        )
      ));
    }
  });
  
  output$boxplot <- renderPlot({
    dataset <- loadDataset();
    factor <- input$inFactor;
    target <- input$inTarget;
    transformation <- input$inTransformation;
    
    if (is.null(dataset) || 
      is.na(factor) || factor == "" || 
      is.na(target) || target == "" ||
      is.na(transformation)
    ) {
      return (NULL);
    } else {
      formula <- as.formula(paste(sep="~", target, factor));
      dataset[[target]] <- characterize.transfomations[[transformation]](dataset[[target]]);
      
      boxplot(formula, data=dataset, main=paste(factor, "vs", target), xlab=factor, ylab=target);
    }
  });
  
  # Multiple plot visualization based on https://gist.github.com/wch/5436415/
  # Insert the right number of plot output objects into the web page
  output$plots <- renderUI({
    dataset <- loadDataset();
    factor <- input$inFactor;
    target <- input$inTarget;
    transformation <- input$inTransformation;
    
    if (is.null(dataset) || 
      is.na(factor) || factor == "" || 
      is.na(target) || target == "" ||
      is.na(transformation)
    ) {
      plots <- list();
    } else {
      numPlots <- length(unique(dataset[[factor]]));
      plots <- lapply(1:numPlots, function(i) {
        plotname <- paste("plot", i, sep="")
        plotOutput(plotname)
      })
    
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plots);
    }
  });
  
  observe({
    dataset <- loadDataset();
    factor <- input$inFactor;
    target <- input$inTarget;
    transformation <- input$inTransformation;
    
    if (!(is.null(dataset) || 
      is.na(factor) || factor == "" || 
      is.na(target) || target == "" ||
      is.na(transformation))
    ) {
      transform <- characterize.transfomations[[transformation]];
      
      filteredDataset <- dataset[,(names(dataset) %in% c(factor, target))];
      filteredDataset[[target]] <- transform(filteredDataset[[target]]);
      
      # Call renderPlot for each one. Plots are only actually generated when they
      # are visible on the web page.
      factors <- unique(filteredDataset[[factor]]);
      numPlots <- length(factors);
      for (i in 1:numPlots) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
          my_i <- i;
          f <- factors[my_i];
          plotname <- paste("plot", my_i, sep="");
          values <- filteredDataset[[target]][filteredDataset[[factor]] == f];
          
          output[[plotname]] <- renderPlot({
            old.par <- par(mfrow=c(1, 2));
            plot(density(values), main = paste(factor, "=", f));
            qqnorm(values, main = paste(factor, "=", f));
            qqline(values);
            par(old.par)
          });
        });
      }
    }
  });
  
  output$tests <- renderText({
    dataset <- loadDataset();
    factor <- input$testsFactor;
    target <- input$testsTarget;
    transformation <- input$testsTransformation;
    
    if (is.null(dataset) || 
      is.na(factor) || factor == "" || 
      is.na(target) || target == "" ||
      is.na(transformation)
    ) {
      return ("");
    } else {
      transform <- characterize.transfomations[[transformation]];
      
      filteredDataset <- dataset[,(names(dataset) %in% c(factor, target))];
      filteredDataset[[factor]] <- as.factor(filteredDataset[[factor]]);
      filteredDataset[[target]] <- transform(filteredDataset[[target]]);
      
      characterization <- characterize(filteredDataset, transformations=list(None = function(x) {x}));
      charact <- characterization[[1]];
      
      isNormal <- ifelse(input$autoTest, all(charact@shapiro$p.value >= 0.05), input$testsIsNormal);
      isHomoscedastic <- ifelse(input$autoTest, 
        ifelse(isNormal, charact@bartlett < 0.05, charact@fligner < 0.05),
        input$testsIsHomoscedastic
      );
      
      test.result <- test.groups(filteredDataset, factor, target, isNormal, isHomoscedastic);
      
      paste(sep="", tags$table(class="table table-bordered table-hover table-condensed",
        tags$tr(
          tags$th("Variable"),
          tags$th("Value")
        ),
        lapply(names(test.result), function(name) {
          tags$tr(
            tags$td(name),
            tags$td(test.result[[name]])
          )
        })
      ));
    }
  });
  
#   observe({
#     factors <- input$batchFactors;
#     targets <- input$batchTargets;
#     transformations <- input$batchTransformations;
#     
#     if ((length(factors) > 1 || (length(factors) == 1 && factors[1] == "")) &&
#       (length(targets) > 1 || (length(targets) == 1 && targets[1] == ""))
#     ) {
#       
#     }
#   });
  
  output$batch <- renderText({
    input$batchDoIt; # Trigger
    
    dataset <- isolate(loadDataset());
    factors <- isolate(input$batchFactors);
    targets <- isolate(input$batchTargets);
    transformations <- isolate(input$batchTransformations);
    
    shapiroThreshold <- isolate(input$batchShapiroThreshold);
    sampleSizeThreshold <- isolate(input$batchSampleSizeThreshold);
    bartlettThreshold <- isolate(input$batchBartlettThreshold);
    flignerThreshold <- isolate(input$batchFlignerThreshold);
    
#     showSummary <- isolate(input$batchSummary);
#     showPosthoc <- isolate(input$batchPosthoc);
    
    if (is.null(dataset) ||
      (length(factors) == 0 || (length(factors) == 1 && factors[1] == "")) ||
      (length(targets) == 0 || (length(targets) == 1 && targets[1] == "")) ||
      length(transformations) == 0
    ) {
      return ("");
    } else {
      tests <- list();
      
      index <- 1;
      for (factor in factors) {
        for (target in targets) {
          filteredDataset <- dataset[,(names(dataset) %in% c(factor, target))];
          filteredDataset[[factor]] <- as.factor(filteredDataset[[factor]]);
          
          for (transformation in transformations) {
            transformationParam <- list();
            transformationParam[[transformation]] <- characterize.transfomations[[transformation]];
            characterization <- characterize(filteredDataset, transformationParam)[[1]];
            
            isNormal <- all(characterization@shapiro$p.value >= shapiroThreshold);
            isHomoscedastic <- ifelse(isNormal,
              characterization@bartlett <= bartlettThreshold,
              characterization@fligner <= flignerThreshold
            );
            
            testResult <- test.groups(filteredDataset, factor, target, isNormal, isHomoscedastic, sampleSizeThreshold);
            testResult$transformation <- transformation;
            
            tests[[index]] <- testResult;
            index <- index + 1;
          }
        }
      }
      
      return (paste(sep="", 
        tags$table(class="table table-bordered table-hover table-condensed",
          tags$tr(
            tags$th("Factor"),
            tags$th("Target"),
            tags$th("Transformation"),
            tags$th("Test"),
            tags$th("p-value")
          ),
          lapply(tests, function(result) {
            tagList(
              tags$tr(
                tags$td(result$factor),
                tags$td(result$target),
                tags$td(result$transformation),
                tags$td(result$test.name),
                tags$td(result$p.value)
              )
            )
          })
        )
      ));
    }
  })
})