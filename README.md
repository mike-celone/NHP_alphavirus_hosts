##################################################################################
## Code for BRT analysis of Alphavirus reservoir status in NHPs
## Data includes: 
##        1) imputed data from Fischhoff et al. (mostly variables from Pantheria)
##        2) Several variables from Ecological Traits of NHPs
##        3) EltonTrais database
##################################################################################

This repo contains code and data for boosted regression tree modeling of NHP Alphavirus reservoir status.

To reproduce analyses, download or clone this repo and run the R code found in ######.Rmd.

Briefly, here are the steps implemented in the code (numbers here match numbers in #####.Rmd):

	1. Install and load needed packages.
		packages <- c("gbm", "caret", "Matrix", "pdp", "caTools", "ROCR", "dplyr", "readxl", 
              "foreach", "dismo", "doSNOW", "parallel", "tidyr", "Metrics",
              "patchwork", "tidyverse", "gtable", "magrittr")

		sapply(packages, library, character.only = T)
		
	2. Make folders for outputs.
	
	3. Initialize cores used for parallel processing in modeling.

	4. Define the data for training and evaluating the model, and for using the model to make predictions. The "input" folder in this repo has *.csv files for......

	5. Run grid search to evaluate and select hyperparameters for boosted regression tree modeling. We evaluate several alternative learning rates, maximum interaction depths, and number of minimum observations in terminal nodes. For each both possible combination of learning rate, interaction depth, and minimun observations, we fit a boosted regression tree model and evaluate its accuracy. 

	6. Using the hyperparameters chosen through grid search, fit boosted regression tree models. Bootstrap modeling, fitting multiple models with different seeds for training and test splits of the data. Bootstrapping generates distributions of evaluation test AUC, variable importance scores, and predicted zoonotic capacities across 5,400 mammals. A separate set of null model bootstrapping, in which labels are shuffled prior to model fitting, enables computing a corrected test AUC. Label shuffling allows for correcting the test AUC for any potential effects of structure in the data that may influence AUC without reflecting meaningful relationships to the labels.

	7. Using all of the available training data (rather than with a training and test split), fit one model to use in making predictions across all 375 NHPs.
