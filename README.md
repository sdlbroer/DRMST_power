This GitHub repository contains all R-code for the scientific internship of the MSc Medicine (Leiden University), performed by Lana Broer. The project concluded in [June, 2024], and was supervised by Matteo Quartagno (University College London). 

The repository contains:
* reconstructed: txt file with survival information of the 65 reconstructed trial datasets - 35 of these trials were digitized by Weir and Trinquart [1];
* margin_conversion_exponential :converts the margin of the original summary measure to the other measures assuming the dsitribution of the control arm to be exponential;
* margin_conversion_flexsurv: converts the margin of the original summary measure to the other measures estimating the distribution of the control arm with flexible parametric methods;
* NI_testing: tests non-inferiority;
* PH_testing: test the proportional hazards assumption for each of the 65 reconstructed datasets.

## references
[1] Weir, IR., Trinquart, L. Design of non-inferiority randomized trials using the difference in restricted mean survival times. Clin Trials 2018; 15: 499-508. https://www.ncbi.nlm.nih.gov/pubmed/30074407
