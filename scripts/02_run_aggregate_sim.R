library(here)
library(dplyr)
library(tibble)
library(tidyr)
library(MASS)
library(pracma)
library(parallel)
library(synthdid)
library(sandwich)
library(mvtnorm)
library(augsynth)

source(here("scripts", "sim_helper_functions.R"))
source(here("scripts", "ar_helper_functions.R"))

#-----------------------------------------------------
# compute synth-did estimator and standard error for a set of simulations
# record estimate, confidence interval bounds, and whether CI covers 0

# then aggregate results for each simulation
#-----------------------------------------------------
sim_sdid <- function(spec) {
	N <- spec$N
	T <- spec$T
	N_co <- spec$N_co
	T_pre <- spec$T_pre
	N_tr <- spec$N_tr
	T_post <- spec$T_post
	tau <- spec$tau
	
	# generate data
	L <- spec$L[[1]]

	N <- dim(L)[1]
	T <- dim(L)[2]
	N_co <- N - N_tr
	T_pre <- T - T_post
	
	# compute treat weights from L
	Lmeans <- apply(L, MARGIN=1, mean)
	centered_Lmeans <- Lmeans - mean(Lmeans)
	treat_weights <- exp(centered_Lmeans)/(1+exp(centered_Lmeans))
	
	Ysim <- simulate_Y_treat_probabilistic(L,
																				 spec$cov[[1]],
																				 spec$TE_type,
																				 tau,
																				 N_tr,
																				 T_post,
																				 treat_weights,
																				 spec$error_dist)
	
	# compute synthdid estimator and CI
	sdid_results <- compute_sdid(Ysim, N_co, T_pre, tau)

	# compute augsynth estimator and CI
	augsynth_results <- compute_augsynth(Ysim, N_co, T_pre, tau)
		
	# compute did and CIs
	did_results <- compute_did(Ysim, N_co, T_pre, tau)
	
	as_tibble_row(c(sdid_results, did_results, augsynth_results))
}


#-----------------------------------------------------
# Load parameters and set up simulations
#-----------------------------------------------------
set.seed(1)
grid_soda <- generate_specs("soda")
grid_cps <- generate_specs("cps")
grid_cps_parallel <- generate_specs("cps", parallel=TRUE) 

grid <- bind_rows(grid_soda, grid_cps, grid_cps_parallel) %>% mutate(param_id = row_number())

## simulation parameters and final setup
# chosen so that Monte Carlo SE for type I error is 0.005 (if nominal coverage is attained)
Nsim <- 1
mc_cores <- 12

tasks <- tidyr::crossing(
	sim_id = seq_len(Nsim),
	grid
)

# Reproducible parallel RNG streams
RNGkind("L'Ecuyer-CMRG")

#-----------------------------------------------------
# Main simulation loop
#-----------------------------------------------------
res_list <- mclapply(seq_len(nrow(tasks)), function(i) {
	row <- tasks[i, ]
	
	seed <- .Random.seed
	out <- sim_sdid(spec=row) %>% mutate(seed = list(seed))
	
	# Return a single tidy row with grid info attached
	bind_cols(
		row %>% dplyr::select(sim_id, param_id, N, T, N_co, N_tr, T_pre, T_post, tau,
													treat_assign, error_dist, gamma, cov, L, parallel),
		out
	)
}, mc.cores=mc_cores, mc.set.seed=TRUE, mc.preschedule = FALSE)

res_df <- bind_rows(res_list)

dir.create(here("out", "results"), recursive = TRUE, showWarnings = FALSE)
saveRDS(here("out", "results", "sim_results.rds"))
