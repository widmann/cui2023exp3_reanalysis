# Re-analysis of Cui, M. E., & Herrmann, B. (2023) Eye movements decrease during effortful speech listening. *Journal of Neuroscience*, 43, 5856–5869.

## Data structure (eyedata.csv)
1. subj
2. trg: trigger code (cond * 10 + snr)
3. trial: # 28s segment per condition
4. id: unique trial id (per participant)
5. block: 1 or 2 in temporal order
6. snr: 1 to 5 (-4, 1, 6, 11, 16 dB SNR; note: max. two-step rule described in manuscript was apparently not implemented)
7. condition: 1 (intact) or 2 scrambled

8. pa: pupil area (used for re-analysis)
9. pd: pupil diameter
10. pa_pfe: pupil area corrected for pupil foreshortening error by regression (PFE; used in original publication)
11. pd_pfe: pupil diameter corrected for PFE

12. fd_int: fixation duration with interpolation
13. fd: fixation duration without interpolation (used for re-analysis and in original publication)

14. sgd_int: spatial gaze dispersion with interpolation
15. sgd_i: spatial gaze dispersion without interpolation, excluded if > 10% NaN per time window (used for re-analysis and in original publication)
16. sgd: spatial gaze dispersion without interpolation, excluded if any NaN per time window

## Repository structure
The scripts contain the following parts of the analyses: 
* import_data: convert csv into a data.frame in R with readable labels etc.
* estimate_models: estimate initial Mixed Models in lme4 (all outputs available in respective HTML file)
* estimate_bayes_models: estimate the more complex Mixed Models in brms (the large results file available on request)
* report_bayes_models: extracts the model outputs from the bayes models (all outputs available in respective HTML file)
* fig: scripts to recreate all results and illustrative figures

## Comments
* I have tried to fully replicate the analysis **except** for PFE correction. PFE correction by regression appeared to introduce rather than reduce noise. Correction maps were implausible for some participants. Possibly the sampled display area was too small?
