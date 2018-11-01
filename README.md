# Optimal intensive care outcome prediction over time using machine learning

# Overview

This repository contains the code underlying the article entitled ‘Optimal intensive care outcome prediction over time using machine learning’. In this file is presented the abstract, to outline the motivation for the work and the findings, and then a brief description of the code which generates these finding and achieves this objective.
The code on this repository is commented throughout to provide a description of each step alongside the code which achieves it.

# Abstract
## Background
Prognostication is an essential tool for risk adjustment and decision making in the intensive care unit (ICU). Research into prognostication in ICU has so far been limited to data from admission or the first 24 hours. Most ICU admissions last longer than this, decisions are made throughout an admission, and some admissions are explicitly intended as time-limited prognostic trials. Despite this, temporal changes in prognostic ability during ICU admission has received little attention to date. Current predictive models, in the form of prognostic clinical tools, are typically derived from linear models and do not explicitly handle incremental information from trends. Machine learning (ML) allows predictive models to be developed which use non-linear predictors and complex interactions between variables, thus allowing incorporation of trends in measured variables over time; this has made it possible to investigate prognosis throughout an admission.
## Methods and findings
This study uses ML to assess the predictability of ICU mortality as a function of time. Logistic regression against physiological data alone outperformed APACHE-II and demonstrated several important interactions including between lactate & noradrenaline dose, between lactate & MAP, and between age & MAP consistent with the current sepsis definitions. ML models consistently outperformed logistic regression with Deep Learning giving the best results. Predictive power was maximal on the second day and was further improved by incorporating trend data. Using a limited range of physiological and demographic variables, the best machine learning model on the first day showed an area under the receiver-operator characteristic curve (AUC) of 0.883 (s.d = 0:008), compared to 0.846 (s.d = 0:010) for a logistic regression from the same predictors and 0.836 (s.d = 0:007) for a logistic regression based on the APACHE-II score. Adding information gathered on the second day of admission improved the maximum AUC to 0.895 (  = 0:008). Beyond the second day, predictive ability declined.
## Conclusion
This has implications for decision making in intensive care and provides a justification for time-limited trials of ICU therapy; the assessment of prognosis over more than one day may be a valuable strategy as new information on the second day helps to differentiate outcomes. New ML models based on trend data beyond the first day could greatly improve upon current risk stratification tools.
# Code

The code is arranged as five ‘master’ scripts which call on a range of user-defined functions.

## Master 1: Prepare data
This script loads the dataset and formats the data for further analysis, before plotting descriptive graphs of the variables. The data are then cleaned, transforming PaO2/FiO2 ratio to correct for multiple units of measurement, and removing outliers according to predefined rules for each variable.
Age, sex, admission, and APACHE-II score, as well as total daily adrenaline (epinephrine), noradrenaline (norepinephrine) and vasopressin, mechanical ventilation during the 24hour period, and daily minimum and maximum for: heart rate (HR), mean arterial pressure (MAP), PaO2/FiO2 ratio, sodium, potassium, lactate, creatinine, CRP, and pH on each day for the first five days after ICU admission are available in the dataset.
The missing values for the drug infusion are replaced with values of zero and the remaining variables are multiply imputed by chained equations to generate nine complete datasets. One imputation is then taken forward for further analysis at this stage.

## Master 2: Test glm interactions
This script fits logistic regression models to test previously suggested interactions in ICU mortality prediction, initially by including all possible interactions between the min/max values for the two interacting variables, and then only including those with the strongest predictive value.

# Master 3: Tune machine learning models
This script tunes each of the machine learning models used, determining near-optimal hyper-parameters for each model on each day. The models use subsets of the dataset and formulae which mean that the data is considered on each day, either using only values from that day, or all values recorded up to and including that day, and only predicting for outcomes which occur on or after that day. Thus there are nine discrete modelling datasets: day 1, day 2 simple (only values measured on day 2), day 2 cumulative (all values measure on day 1 and day 2), day 3 simple, day 4 cumulative, and so on to day 5.

For each of these, models are fitted, and hyper-parameters are selected from re-fitting according to pre-specified tuning grids. Predictions are made and the optimal hyper-parameters selected to maximise mean AUC across 20 folds. Two rounds of tuning are performed for each machine learning method. Prior to fitting the models, for each fold, the data are centred, scaled and power transformed. 
Most methods were ‘tuned’ in parallel using the `caret' package v6.0-77. ‘Deep learning’ was implemented using the `keras' package v2.1.4.9000 for R to implement a six hidden-layer neural network with `TensorFlow', with folding, pre-processing, parallelisation and tuning achieved manually. The script will recruit two thirds of available cores for parallelisation.

## Master 4: Final models
This script uses the selected hyper-parameters to fit the models to twenty folds of each of the nine imputations. The variability due to folding and the variability due to imputation are calculated and added to generate a combined, conservative estimate of variability in model performance, and represented graphically around the mean AUC across all folds of all imputations. 
This metric of model performance can then be compared between days to achieve the objective of understanding the temporal change in prognostic ability in the ICU.

## Master 5: Explore predictions
This script takes the predictions made by the best performing model on each day (deep learning) and explores the differences in age and length of admission between the correct and incorrect predictions, plotting them graphically.

# Acknowledgements
This research was conducted using NIHR Health Informatics Collaborative (NIHR HIC) data resources. This research was supported by the National Institute for Health Research Cambridge Biomedical Research Centre and the Clinical Research Informatics Unit, University College London Hospitals Biomedical Research Centre.
