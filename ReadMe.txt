A multi-measurement study of the relation between deliberation and volition

Guillaume P. Pech, Emilie A. Caspar, Elisabeth Pacherie, Axel Cleeremans, Uri Maoz

[Data and Code]

Abstract

Historically, voluntary action and volition more generally have been investigated through the lens of meaningless decisions. Importantly, these findings have been used in the debate about key notions like free will and moral responsibility. However, more recent claims have challenged the possibility of generalizing findings from a meaningless context to a more meaningful one. In this context, the current study investigates the markers of volition, specifically comparing meaningful and meaningless decisions. In an effort to maximize their monetary gain, 50 participants repeatedly deliberated between two options, making either rewarded choices—hard-deliberation decisions (where the options differed along two dimensions) or easy-deliberation decisions (where the options differed along a single dimension)—or unrewarded choices, a.k.a. arbitrary decision. This enabled us to contrast rewarded and unrewarded decisions as well as the degree of deliberation between easy- and hard-deliberation choices. We found evidence that rewarded and unrewarded decisions differed along several measures related to volition: participants reported a higher Sense of Volition, exhibited a stronger Readiness Potential, had increased Temporal Binding, and demonstrated increased Effort Exerted in the rewarded condition. In contrast, we found evidence for similarity across these measures between easy-deliberation and hard-deliberation conditions. Our results suggest that it is not the complexity of the deliberation process prior to the action that makes it more volitional, but ratherthat the decision serves a meaningful goal. Our study also introduced a new implicit measure of volition—exerted effort—that well aligned with other measures of volition and should therefore prove useful in future studies.

This repository contains the data, experiment, and analysis scripts for this manuscript.

Contents

codes/*:

analysis/* : Scripts to preprocess and extract the EEG data using Python. As well as sripts to organize and analyses the data using R.

analysis_beh_deliberation.R : Script that organized the concatenate data, remove behavioral outliers, and run the bayesian models. clean_models.R : Script to detect and remove outliers in the models. Then re-run clean models. plot_models.R : Script for the figures. post_eeg_deliberation_erp.ipynb: Jupyter Notebook to extract and visualize the EEG results. prep_beh_deliberation.R: organize and concatenate the behavioral data Prep_eeg.ipynb : Jupyter Notebook to prep-proces the EEG data. test_models : Script to run inferential test on the Bayesian models.

task/* : Script and material to run the experimental task.

Deliberation.py-: Script to run the task materials/* : all the material to run the experimental task

data/*

beh/* : behavioral data. One file for the trial based data. One for the questionnaires

eeg_derivatives/* : We provide the preprocessed EEG data for this experiment, as the raw data were too heavy for OSF. We can send the raw data upon request.

list_bad_channels.txt : removed channels for each participants.

list_ica.txt : removed component for each participants

models/* : bayesian models runned. The clean are the one without outlier identified by the clean_models script.

Requirements

Python, R, various packages.