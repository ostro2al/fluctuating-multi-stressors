# fluctuating-multi-stressors

All code and data for 'Fluctuating fortunes: stressor synchronicity and fluctuating intensity influence biological impacts'


## **About**

This repository provides the data and code to reproduce all figures and tables, including supplemental materials, found in Ostrowski et al. (in prep).


## **Scripts**

Create a conceptual model for how biological responses to changes in stressor intensity and synchronicity might differ based on the type of stressor interaction  
     Figure 1_Conceptual-model-stressor-interactions.R
  
Analyse experimental results using lmer to make model predictions for seagrass biomass proportional change  
     Figure 3_Biomass-change.R
 
Analyse experimental results using lmer to make model predictions for seagrass photosynthetic capacity change  
     Figure 4_Photosynthetic-capacity.R
  
Run GAMs to test the conceptual model and alternate hypotheses to explain differences in seagrass proportional biomass across changes in stressors intensity and synchronicity  
     Table 1_Testing-alternate-hypotheses.R
 
Create a conceptual model for how biological responses to changes in stressor intensity and synchronicity might differ based on the type of stressor interaction while incorporating low intensity stressors where there could be periods of no stressor overlap (i.e., no interactions)  
     Figure S1_Conceptual-model-low-intensity.R
  
Use AIC to select the best lmer model to make model predictions for seagrass photosynthetic capacity change  
     Table S2_AIC-model-selection.R
  
Determine coefficient estimates of static-static GAM model used to interpret stressor interaction type across treatments from the output of Table 1  
     Table S3_Coefficient-estimates-static-model.R
