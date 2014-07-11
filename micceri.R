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

# This function prints out the Micceri-criteria for tail weight and symmetry
# of a distribution Micceri, T. (1989). The unicorn, the normal curve, and 
# other improbable creatures. Psychological Bulletin, 105, 156-166. 
# doi:10.1037/0033-2909.105.1.156
# Source: http://stats.stackexchange.com/questions/16646/what-is-a-good-index
# -of-the-degree-of-violation-of-normality-and-what-descriptiv
library(fBasics)

micceri <- function(x) {
  QS <- (quantile(x, prob=c(.975, .95, .90)) - median(x)) / 
    (quantile(x, prob=c(.75)) - median(x))
  
  n <- length(x)
  x.s <- sort(x)
  U05 <- mean(x.s[(.95*n ):n])
  L05 <- mean(x.s[1:(.05*n)])
  U20 <- mean(x.s[(.80*n):n])
  L20 <- mean(x.s[1:(.20*n)])
  U50 <- mean(x.s[(.50*n):n])
  L50 <- mean(x.s[1:(.50*n)])
  M25 <- mean(x.s[(.375*n):(.625*n)])
  Q <- (U05 - L05)/(U50 - L50)
  Q1 <- (U20 - L20)/(U50 - L50)
  Q2 <- (U05 - M25)/(M25 - L05)
  
  # mean/median interval
  QR <- quantile(x, prob=c(.25, .75)) # Interquartile range
  MM <- abs(mean(x) - median(x)) / (1.4807*(abs(QR[2] - QR[1])/2))
  
  SKEW <- skewness(x)
  
  tail_weight <- round(c(QS, Q=Q, Q1=Q1), 2)
  symmetry <- round(c(Skewness=SKEW, MM=MM, Q2=Q2), 2)
  
  cat.tail <- matrix(c(1.9, 2.75, 3.05, 3.9, 4.3,
                       1.8, 2.3, 2.5, 2.8, 3.3,
                       1.6, 1.85, 1.93, 2, 2.3,
                       1.9, 2.5, 2.65, 2.73, 3.3,
                       1.6, 1.7, 1.8, 1.85, 1.93), ncol=5, nrow=5)
  
  cat.sym <- matrix(c(0.31, 0.71, 2,
                      0.05, 0.18, 0.37,
                      1.25, 1.75, 4.70), ncol=3, nrow=3)
  
  
  ts <- c()
  for (i in 1:5) {ts <- c(ts, sum(abs(tail_weight[i]) > cat.tail[,i]) + 1)}
  
  ss <- c()
  for (i in 1:3) {ss <- c(ss, sum(abs(symmetry[i]) > cat.sym[,i]) + 1)}
  
  tlabels <- c(
    "Uniform", 
    "Less than Gaussian", 
    "Near Gaussian", 
    "Moderate contamination", 
    "Extreme contamination", 
    "Double exponential contamination"
  )
  
  micceri.slabels <- c(
    "Relatively symmetric", 
    "Moderate asymmetry", 
    "Extreme asymmetry", 
    "Exponential asymmetry"
  )
  
  tail.cat <- factor(
    max(ts), levels=1:length(micceri.tlabels), labels=micceri.tlabels, ordered=TRUE
  )
  sym.cat  <- factor(
    max(ss), levels=1:length(micceri.slabels), labels=micceri.slabels, ordered=TRUE
  )
  
  invisible(list(
    tail_weight=tail_weight, 
    symmetry=symmetry,
    tail_weight_label=micceri.tlabels[max(ts)],
    symmetry_label=micceri.slabels[max(ss)],
    tail.cat=tail.cat, 
    sym.cat=sym.cat
  ))
}

micceri.tlabels <- c(
  "Uniform", 
  "Less than Gaussian", 
  "Near Gaussian", 
  "Moderate contamination", 
  "Extreme contamination", 
  "Double exponential contamination"
)

micceri.slabels <- c(
  "Relatively symmetric", 
  "Moderate asymmetry", 
  "Extreme asymmetry", 
  "Exponential asymmetry"
)

micceri.tlabels.color <- c(
  "#F7686A", 
  "#FAA976", 
  "#62BD7A", 
  "#B0D47F", 
  "#FAA976", 
  "#F7686A"
)

micceri.slabels.color <- c(
  "#62BD7A", 
  "#B0D47F", 
  "#FAA976", 
  "#F7686A"
)

micceri.colorForTailWeight <- function(tailLabel) {
  micceri.tlabels.color[match(tailLabel, micceri.tlabels)];
}

micceri.colorForSymmetry <- function(symmetryLabel) {
  micceri.slabels.color[match(symmetryLabel, micceri.slabels)];
}

micceri.eval <- function(dataset,
 transformations=list(
   identity = function(x) x, sqrt = sqrt, 
   log = log, log2 = log2, log10 = log10, 
   pow2 = function(x) x^2, pow3 = function(x) x^3, pow5 = function(x) x^5
 )
) {
  # Factors and target names.
  isfactor <- sapply(names(dataset), FUN=function(x) is.factor(dataset[[x]]));
  factors <- names(dataset)[isfactor];
  targets <- names(dataset)[!isfactor];
  
  results <- list(
    factor = character(0), 
    target = character(0), 
    target_value=character(0), 
    transformation = character(0), 
    tail_weight = character(0), 
    symmetry = character(0)
  );
  for (factor in factors) {
    for (target in targets) {
      for (transformation.name in names(transformations)) {
        transformation <- transformations[[transformation.name]];
        edataset <- dataset;
        edataset[[target]] <- transformation(edataset[[target]]);
        
        current <- ddply(edataset, c(factor), function(x) { 
          m <- micceri(x[[target]]);
          c(m$tail_weight_label, m$symmetry_label);
        });
        
        for (target_value in current[[factor]]) {
          tail_weight <- current$V1[current[[factor]] == target_value];
          symmetry <- current$V2[current[[factor]] == target_value];
          
          results <- list(
            factor = c(factor, results$factor),
            target = c(target, results$target),
            target_value = c(target_value, results$target_value),
            transformation = c(transformation.name, results$transformation),
            tail_weight = c(tail_weight, results$tail_weight),
            symmetry = c(symmetry, results$symmetry)
          );
        }
      }
    }
  }
  
  return(as.data.frame(results));
}