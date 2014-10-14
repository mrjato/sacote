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

library(plyr);

source('micceri.R');

# Results class
setClass("Characterization", 
  slots = c(
    factor="character", 
    target="character",
    transformation.name="character",
    transformation="function",
    samples="list",
    micceri="list",
    shapiro="list",
    bartlett="numeric",
    fligner="numeric"
  )
);

setGeneric("checkNormality", 
  def = function(object, shapiroThreshold = 0.1, samplesThreshold = 30) {
    standardGeneric("checkNormality");
  }
);

setGeneric("checkHomoscedasticity", 
  def = function(object, isNormal = TRUE, bartlettThreshold = 0.05, flignerThreshold = 0.05) {
   standardGeneric("checkHomoscedasticity");
  }
);

setMethod("checkNormality", 
  signature = "Characterization",
  definition = function(object, shapiroThreshold = 0.1, samplesThreshold = 30) {
    all(object@shapiro$p.value >= shapiroThreshold || object@samples$n >= samplesThreshold);
  }
);

setMethod("checkHomoscedasticity", 
  signature = "Characterization",
  def = function(object, isNormal = TRUE, bartlettThreshold = 0.05, flignerThreshold = 0.05) {
    ifelse(isNormal,
       object@bartlett >= bartlettThreshold,
       object@fligner >= flignerThreshold
    );
  }
);

characterize.transfomations <- list(
  None = function(x) x, Sqrt = sqrt, 
  Log = log, Log2 = log2, Log10 = log10, 
  Pow2 = function(x) x^2, Pow3 = function(x) x^3, Pow5 = function(x) x^5
);

characterize <- function(dataset, transformations = list(
  None = function(x) x, Sqrt = sqrt, 
  Log = log, Log2 = log2, Log10 = log10, 
  Pow2 = function(x) x^2, Pow3 = function(x) x^3, Pow5 = function(x) x^5
)) {
  # Factors and target names.
  isfactor <- sapply(names(dataset), FUN=function(x) is.factor(dataset[[x]]));
  factors <- names(dataset)[isfactor];
  targets <- names(dataset)[!isfactor];
  
  # Normality and homoscedasticity of the dataset are evaluated.
  results <- c();
  for (factor in factors) {
    for (target in targets) {
      for (transformation.name in names(transformations)) {
        transformation <- transformations[[transformation.name]];
        tdataset <- dataset;
        tdataset[[target]] <- transformation(tdataset[[target]]);
        
        results <- c(results, new("Characterization",
          factor = factor,
          target = target,
          transformation.name = transformation.name,
          transformation = transformation,
          samples = ddply(tdataset, c(factor), function(x) { 
            data.frame(n = length(x[[target]]));
          }),
          micceri = tryCatch(
            {
              ddply(tdataset, c(factor), function(x) { 
                mc <- micceri(x[[target]]);
                
                data.frame(
                  tail_weight_label = mc$tail_weight_label,
                  symmetry_label = mc$symmetry_label
                );
              })
            }, 
            error = function(e) {
              data.frame(
                tail_weight_label = NA_character_,
                symmetry_label = NA_character_
              );
            }
          ),
          shapiro = tryCatch(
            {
              ddply(tdataset, c(factor), function(x) { 
                data.frame(p.value = shapiro.test(x[[target]])$p.value);
              })
            },
            error = function(e) {
              data.frame(p.value = NA_real_)
            }
          ),
          bartlett = bartlett.test(tdataset[[target]], tdataset[[factor]])$p.value,
          fligner = fligner.test(tdataset[[target]], tdataset[[factor]])$p.value
        ));
      }
    }
  }
  
  return (results);
}
