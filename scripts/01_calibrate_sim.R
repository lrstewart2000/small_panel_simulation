library(here)
library(dplyr)
library(tidyr)
library(readr)
library(synthdid)

source(here("scripts", "ar_helper_functions.R"))

#-------------------------------------------------
# load and aggregate individual data
# adapted from https://github.com/PeirongHao/scm_indiv_survey
#-------------------------------------------------

# loading the 2023 combined file
# available at: https://www.cdc.gov/yrbs/files/sadc_2023/HS/sadc_2023_district.dat
# more information available at: https://www.cdc.gov/yrbs/data/index.html
source(here("scripts", "parsed_fwf_spec.R"))
df <- readr::read_fwf(here("data", "raw", "sadc_2023_district.dat"), col_positions=col_positions, col_types=col_types)

# attach site codes for each city
site_codes <- c("FT", "LO", "MM", "OL", "PB", "PH", "SA")
filt_df <- df %>%
	filter(sitecode %in% site_codes, year >= 2007) %>%
	mutate(
		soda = case_when(
			q74 == "1" ~ 0,
			q74 == "2" ~ 2,
			q74 == "3" ~ 5,
			q74 == "4" ~ 1*7,
			q74 == "5" ~ 2*7,
			q74 == "6" ~ 3*7,
			q74 == "7" ~ 4*7
		))

# aggregate individual level data
aggregated_df = filt_df %>% dplyr::select(sitecode, year, weight, soda) %>%
	group_by(sitecode, year) %>%
	summarize(mean_soda = weighted.mean(soda, weight, na.rm=TRUE)) %>%
	ungroup() %>%
	mutate(treat = sitecode=="PH") %>% arrange(treat, sitecode, year) %>%
	dplyr::select(-treat)

#-----------------------------------------------------
# estimate systematic component (L) and error structure
# follows procedure and adapts code from Arkhangelsky et al. (2021)
#-----------------------------------------------------

# decompose outcome matrix into systematic (additive and interacted) and error components
# from Arkhangelsky et al. (2021) and Bai (2009)
decompose_Y = function(Y,rank, method="ife") {
	N <- dim(Y)[1]
	T <- dim(Y)[2]
	
	if (method == "ife") {
		eigen_decomp <- eigen(t(Y) %*% Y)
		factor_time	<- sqrt(T)*eigen_decomp$vectors[,1:rank]
		factor_unit <- t((1/T)*t(factor_time) %*% t(Y))
		
		L <- t(factor_time %*% t(factor_unit))
		E <- Y - L
		
		gamma <- factor_unit
		psi <- t(factor_time)
	} else if (method == "sqrt") {
		svd_data_mat <- svd(Y)
		factor_unit <- as.matrix(svd_data_mat$u[,1:rank]*sqrt(N))
		factor_time <- as.matrix(svd_data_mat$v[,1:rank]*sqrt(T))
		
		magnitude <- svd_data_mat$d[1:rank]/sqrt(N*T)
		sigma <- diag(magnitude, nrow = rank, ncol = rank)
		gamma <- factor_unit %*% sigma^(1/2)
		psi <- sigma^(1/2) %*% t(factor_time)
		L <- gamma %*% upsilon
		
		E <- Y-L
	}
	
	return(list(L=L, E=E, unit_factors=gamma, time_factors=psi))
}

# take unit factors and error structure from 2007-2015 panel with PHL
Y_unit = t(as.matrix(tidyr::pivot_wider(aggregated_df %>% filter(year < 2017), 
													names_from="sitecode", 
													values_from="mean_soda"))[,2:8])

components = decompose_Y(Y_unit, rank=2)

# needs a negative because we're taking factors from two different matrices, won't always be the case
unit_factors = -components$unit_factors 

# get error covariance matrix for each unit
ar_results = fit_ar1(components$E)
ar_coef = ar_results[[1]]
var = ar_results[[2]]

# take time factors from 2007-2017 panel without PHL
Y_time = t(as.matrix(tidyr::pivot_wider(aggregated_df %>% filter(sitecode != "PH", year <= 2017), 
													names_from="sitecode", 
													values_from="mean_soda"))[,2:7])
time_factors = decompose_Y(Y_time, rank=2)$time_factors

# generate L as outer product
L = unit_factors %*% time_factors
cities = c("FT", "LO", "MM", "OL", "PB", "SA", "PH")
rownames(L) = cities

# save parameters
params = list(unit_factors, time_factors, ar_coef, var)
dir.create(here("data", "temp"), recursive = TRUE, showWarnings = FALSE)
saveRDS(params, here("data", "temp", "calibrated_params_soda.rds"))

# save standard deviation of outcome after period fixed effects are removed
# used to standardize bias and empirical SE of estimators
fit <- lm(mean_soda ~ factor(year), data=aggregated_df %>% filter(year <= 2017))
sd_soda <- sd(fit$residuals)

#-----------------------------------------------------
# load in CPS data from Arkhangelsky et al. and decompose
#-----------------------------------------------------
# CPS data is loaded in synthdid package as CPS
cps <- synthdid::CPS %>% dplyr::select(state, year, log_wage)

# decompose CPS data
Y_cps <- t(as.matrix(tidyr::pivot_wider(cps, 
																 names_from="state", 
																 values_from="log_wage")))
Y_cps <- Y_cps[2:nrow(Y_cps),]
components <- decompose_Y(Y_cps, rank=4, method="ife")

# get unit and time factors
unit_factors <- components$unit_factors 
time_factors <- components$time_factors 

# compute error covariance parameters
ar_results = fit_ar2(components$E)
ar_coef = ar_results[[1]]
var = ar_results[[2]]

# save parameters
params = list(unit_factors, time_factors, ar_coef, var)
saveRDS(params, here("data", "temp", "calibrated_params_cps.rds"))

# compute standard deviation of outcome after removing year fixed effects
# save this along with that of soda consumption data
fit <- lm(log_wage ~ factor(year), data=CPS)
sd_cps <- sd(fit$residuals)

saveRDS(c("sd_soda"=sd_soda, "sd_cps"=sd_cps), here("data", "temp", "outcome_sds.rds"))