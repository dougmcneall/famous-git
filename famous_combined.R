# Master analysis for McNeall et al (201X) FAMOUS paper.

# Reproduce the figures in the paper!


# -----------------------------------------------------------------
# 0. Packages, functions and data
# -----------------------------------------------------------------

source('famous_common.R')

# -----------------------------------------------------------------
# 1.Introduction: figures 1 and 2, basic visualistaion of the
# ensemble
# Figures
# ... BL_obs_fraction_sd
# ... frac_pairs
# -----------------------------------------------------------------

source('famous_intro.R')

# -----------------------------------------------------------------
# 2. Sensitivity analysis and two-at-a-time measures of implausibility 
# -----------------------------------------------------------------
# -----------------------------------------------------------------

source('famous_sensitivity.R')


# -----------------------------------------------------------------
# 3. Inputs implied by various data points

# best_inputs ...
# best_inputs_swaps_hists_Paired
# just_maybe (overlapping)
# Prop_NROY_tolerance_unc.pdf
# discrepancy_parameter_space
# fraction_histogram_with_discrepancy_standard.pdf
# best_X_maps.pdf

# -----------------------------------------------------------------

source('famous_direct.R')


# -----------------------------------------------------------------
# 4. Standard climate variables (temperature and precip) in FAMOUS 
# -----------------------------------------------------------------

source('temp_precip_famous.R')

