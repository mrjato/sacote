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
#
# Recommended readings:
# http://udel.edu/~mcdonald/statkruskalwallis.html
#	http://mathnstats.com/index.php/hypothesis-testing/82-tests-for-means/122-two-
# sample-t-test-unequal-variances.html
# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3693611/
#	http://www.bmj.com/content/310/6975/298?view=long&pmid=7866172
# 		A key theoretical result, called the central limit theorem, underpins many 
#		methods of analysis. It states that the means of random samples from any 
#		distribution will themselves have a normal distribution. As a consequence, 
#		when we have samples of hundreds of observations we can often ignore the 
#		distribution of the data. Nevertheless, because most clinical studies are 
#		of a modest size, it is usually advisable to transform non-normal data, 
#		especially when they have a skewed distribution.
#

test.groups.print <- function(
  test.name, factor, target, test.result, test.posthoc=NULL, 
  p.value.function=function(test) { test$p.value; },
  print.test.summary=FALSE, print.posthoc=FALSE
) {
  output <- paste("\n", test.name, factor, target, 
    "p-value =", p.value.function(test.result), "\n"
  );
  
  if (print.test.summary) {
    output <- paste(sep="\n", output, summary(test.result));
  }
  
  if (print.posthoc && !is.null(test.posthoc)) {
    output <- paste(sep="\n", output, test.posthoc(test.result));
  }
  
  return (output);
};

test.groups.cat <- function(
  dataset, 
  factor, target,
  isNormal, isHomoscedastic,
  print.test.summary=FALSE, 
  print.posthoc=FALSE
) {
  factors <- unique(dataset[[factor]]);
  
  if (isNormal) {
    if (isHomoscedastic) {
      anova <- aov(as.formula(paste(target, "~", factor)), data=dataset);
      
      test.groups.print("ANOVA", factor, target, anova, TukeyHSD, 
        p.value.function=function(test) {
          summary(test)[[1]][["Pr(>F)"]][1];
        }, 
        print.test.summary, 
        print.posthoc
      );
    } else if (length(factors) == 2) {
      x <- dataset[[target]][dataset[[factor]] == factors[1]];
      y <- dataset[[target]][dataset[[factor]] == factors[2]];
      
      wttest <- t.test(x, y);
      
      test.groups.print("Welch's T-Test", factor, target, wttest, 
        print.test.summary=print.test.summary, 
        print.posthoc=print.posthoc
      );
    } else if (length(factors) > 2) {
      wanova.formula <- as.formula(paste(target, factor, sep="~"));
      wanova <- oneway.test(wanova.formula, data=dataset);
      
      test.groups.print("Welch's ANOVA", factor, target, wanova, 
        print.test.summary=print.test.summary, 
        print.posthoc=print.posthoc
      );
    }  
  } else if (isHomoscedastic) {
    kw <- kruskal.test(dataset[[target]] ~ dataset[[factor]]);
    
    test.groups.print("KW", factor, target, kw, function(kw) {
      kruskalmc(dataset[[target]] ~ dataset[[factor]], probs=0.05, cont=NULL);
    }, print.test.summary=print.test.summary, print.posthoc=print.posthoc);
# # Review this test
# mwc <- wilcox.test(as.formula(paste(target, "~", factor)), data=dataset);
# 
# test.groups.print("Mann-Whitney-Wilcoxon", factor, target, mwc);
  } else {
    paste("\nERROR NON-NORMAL HETEROSCEDASTIC", factor, target, "\n");
  }
};

test.groups <- function(
  dataset, 
  factor, target,
  isNormal, isHomoscedastic
) {
  factors <- unique(dataset[[factor]]);
  
  p.value <- NA;
  test.name <- "";
  tryCatch(
    { 
      if (isNormal) {
        if (isHomoscedastic) {
          anova <- aov(as.formula(paste(target, "~", factor)), data=dataset);
          
          test.name <- "ANOVA";
          p.value <- summary(anova)[[1]][["Pr(>F)"]][1];
        } else if (length(factors) == 2) {
          x <- dataset[[target]][dataset[[factor]] == factors[1]];
          y <- dataset[[target]][dataset[[factor]] == factors[2]];
          
          wttest <- t.test(x, y);
          
          test.name <- "Welch's t-test";
          p.value <- wttest$p.value;
        } else if (length(factors) > 2) {
          wanova.test <- as.formula(paste(target, factor, sep="~"));
          wanova <- oneway.test(wanova.test, data=dataset);
          
          test.name <- "Welch's ANOVA";
          p.value <- wanova$p.value;
        }  
      } else if (isHomoscedastic) {
        kw <- kruskal.test(dataset[[target]] ~ dataset[[factor]]);
        
        test.name <- "Kruskal-Wallis";
        p.value <- kw$p.value;
      } else {
        test.name <- "Non-Normal Heteroscedastic";
      }
    },
    error = function(e) {
      # Not working
      test.name <- paste("ERROR:", e);
    }
  );
  
  return (list(
    factor = factor,
    target = target,
    isNormal = isNormal, 
    isHomoscedastic = isHomoscedastic,
    test.name = test.name,
    p.value = p.value
  ));
}