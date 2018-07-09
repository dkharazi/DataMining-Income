## Overview

This data was extracted from the [1994 Census Bureau database](http://www.census.gov/en.html) by Ronny Kohavi and Barry Becker (Data Mining and Visualization, Silicon Graphics). A set of reasonably clean records was extracted using the following conditions: ((AAGE>16) && (AGI>100) && (AFNLWGT>1) && (HRSWK>0)). The prediction task is to determine whether a person makes over $50K a year. For more information about the data, please either refer to the [Census Bureau](http://www.census.gov/en.html) or [Kaggle](https://www.kaggle.com/uciml/adult-census-income/home) - a popular company that distributes data, along with insightful descriptions about the data.

Also, the analysis will include a kNN implementation within the model building steps. There are many packages that provide already implemented kNN models for you, but one of the goals for this analysis is to demonstrate how to both implement and use the kNN algorithm, while using both Jacaard and Cosine similarities.

Dataset: [Adult Census Income Data](https://www.kaggle.com/uciml/adult-census-income/home) [465 KB]

## Variable Descriptions

- `ID:` The ID of the individual.
- `Age:` The age of the individual.
- `Workclass:` The working class of the individual.
- `Education:` The level of education of the individual.
- `Education-Cat:` The categorized level of education of the individual.
- `Marital-Status:` The stuatus of marriage for the individual.
- `Occupation:` The occupation of the individual.
- `Relationship:` The relationship status of the individual.
- `Race:` The race of the individual.
- `Sex:` The sex of the individual.
- `Capital-Gain:` The capital gain of the individual.
- `Capital-Loss:` The capital loss of the individual.
- `Hours-per-Week:` The number of hours that the individual works per week.
- `Native-Country:` The native country of the individual.
- `Class:` The income class of the individual.
- `Final Weight:` The weights on the Current Population Survey (CPS) files are controlled to independent estimates of the civilian noninstitutional population of the US. These are prepared monthly for the Census Bureau by Population Division. The Census Bureau uses 3 sets of controls, which are included below. The Census Bureau uses all three sets of controls in their weighting program and "rake" through them 6 times so that by the end, they come back to all the controls they used. The term estimate refers to population totals derived from CPS by creating "weighted tallies" of any specified socio-economic characteristics of the population. People with similar demographic characteristics should have similar weights. There is one important caveat to remember about this statement. That is that since the CPS sample is actually a collection of 51 state samples, each with its own probability of selection, the statement only applies within state.
  1. A single cell estimate of the population 16+ for each state.
  2. Controls for Hispanic Origin by age and sex.
  3. Controls by Race, age and sex.  


## Relevant Papers
Ron Kohavi, "Scaling Up the Accuracy of Naive-Bayes Classifiers: a Decision-Tree Hybrid", Proceedings of the Second International Conference on Knowledge Discovery and Data Mining, 1996. (PDF)
