# Read me
This GitHub repository contains all R-code for the scientific internship of the MSc Medicine (Leiden University), performed by Lana Broer. The project concluded in June 2024, and was supervised by Dr. Matteo Quartagno (University College London). 

The repository contains:
* Folder 1: data
  * manuscript_information: xlsx file with necessary information from the 65 included trials;
  * reconstructed: txt file with survival information of the 65 reconstructed trial datasets - 35 of these trials (ID's 36 and up) were digitized by Weir and Trinquart [1];
  * NI_outcomes: Rdata file containing the outcomes of the NI tests for all summary measures for each trial.
* Folder 2: code
  * preprocessing_data: preprocesses the data for analysis;
  * margin_conversion_exponential: converts the margin of the original summary measure to the other measures assuming the dsitribution of the control arm to be exponential;
  * margin_conversion_flexsurv: converts the margin of the original summary measure to the other measures estimating the distribution of the control arm with flexible parametric methods;
  * NI_testing: tests the non-inferiority hypothesis;
  * flex_outcomes: outcomes of the primary analysis;
  * flex_outcomes_subgroup_EventRisk: subgroup analysis split by event rate;
  * flex_outcomes_subgroup_OriginalSumMeasure: subgroup analysis split by original summary measure;
  * flex_outcomes_subgroup_PH: subgroup analysis on trials without evidence of non-PH;
  * exp_outcomes: outcomes when doing margin conversion under the exponential distribution.
 
### References
[1] Weir, IR., Trinquart, L. Design of non-inferiority randomized trials using the difference in restricted mean survival times. Clin Trials 2018; 15: 499-508. https://www.ncbi.nlm.nih.gov/pubmed/30074407
