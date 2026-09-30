library(here)
library(dplyr)

sim_df <- readRDS(here("out", "results", "sim_results.rds")) %>% dplyr::select(-c("L", "cov", "gamma", "seed"))

# computes summary statistics aggregating across simulations runs within each specification
res_df <- sim_df %>% group_by(param_id) %>%
	summarize(
		nsim = n(),
		est_mean_sdid = mean(tau_hat_sdid),
		tau = mean(tau),
		emp_se_sdid = sd(tau_hat_sdid),
		avg_se_sdid = sqrt(mean(se_sdid^2)),
		mcse_avg_se_sdid_numer = sqrt(var(se_sdid^2)),
		mcse_avg_se_sdid_denom = mean(se_sdid)^2,
		coverage_sdid = mean(covers_sdid),
		est_mean_did = mean(tau_hat_did),
		emp_se_did = sd(tau_hat_did),
		avg_se_did = sqrt(mean(se_did^2)),
		coverage_did = mean(covers_did),
		mcse_avg_se_did_numer = sqrt(var(se_did^2)),
		mcse_avg_se_did_denom = mean(se_did)^2,
		est_mean_augsynth = mean(tau_hat_augsynth),
		emp_se_augsynth = sd(tau_hat_augsynth),
		coverage_augsynth = mean(covers_augsynth),
		est_mean_msyn_nu0 = mean(tau_hat_msyn_nu0),
		emp_se_msyn_nu0 = sd(tau_hat_msyn_nu0),
		coverage_msyn_nu0 = mean(covers_msyn_nu0),
		est_mean_msyn_nu1 = mean(tau_hat_msyn_nu1),
		emp_se_msyn_nu1 = sd(tau_hat_msyn_nu1),
		coverage_msyn_nu1 = mean(covers_msyn_nu1),
		est_mean_msyn_nuheur = mean(tau_hat_msyn_nuheur),
		emp_se_msyn_nuheur = sd(tau_hat_msyn_nuheur),
		coverage_msyn_nuheur = mean(covers_msyn_nuheur)
	) %>% ungroup() %>%
	mutate(
		bias_sdid = est_mean_sdid - tau,
		bias_did = est_mean_did - tau,
		bias_augsynth = est_mean_augsynth - tau,
		bias_msyn_nu0 = est_mean_msyn_nu0 - tau,
		bias_msyn_nu1 = est_mean_msyn_nu1 - tau,
		bias_msyn_nuheur = est_mean_msyn_nuheur - tau,
		mcse_bias_sdid = emp_se_sdid/sqrt(nsim),
		mcse_emp_se_sdid = emp_se_sdid/sqrt(2*(nsim-1)),
		mcse_avg_se_sdid = mcse_avg_se_sdid_numer/(4*nsim*mcse_avg_se_sdid_denom),
		mcse_coverage_sdid = sqrt(coverage_sdid * (1 - coverage_sdid) / nsim),
		mcse_bias_did = emp_se_did/sqrt(nsim),
		mcse_emp_se_did = emp_se_did/sqrt(2*(nsim-1)),
		mcse_avg_se_did = mcse_avg_se_did_numer/(4*nsim*mcse_avg_se_did_denom),
		mcse_coverage_did = sqrt(coverage_did * (1 - coverage_did) / nsim),
		mcse_bias_augsynth = emp_se_augsynth/sqrt(nsim),
		mcse_emp_se_augsynth = emp_se_augsynth/sqrt(2*(nsim-1)),
		mcse_coverage_augsynth = sqrt(coverage_augsynth * (1 - coverage_augsynth) / nsim),
		mcse_bias_msyn_nu0 = emp_se_msyn_nu0/sqrt(nsim),
		mcse_emp_se_msyn_nu0 = emp_se_msyn_nu0/sqrt(2*(nsim-1)),
		mcse_coverage_msyn_nu0 = sqrt(coverage_msyn_nu0 * (1 - coverage_msyn_nu0) / nsim),
		mcse_bias_msyn_nu1 = emp_se_msyn_nu1/sqrt(nsim),
		mcse_emp_se_msyn_nu1 = emp_se_msyn_nu1/sqrt(2*(nsim-1)),
		mcse_coverage_msyn_nu1 = sqrt(coverage_msyn_nu1 * (1 - coverage_msyn_nu1) / nsim),
		mcse_bias_msyn_nuheur = emp_se_msyn_nuheur/sqrt(nsim),
		mcse_emp_se_msyn_nuheur = emp_se_msyn_nuheur/sqrt(2*(nsim-1)),
		mcse_coverage_msyn_nuheur = sqrt(coverage_msyn_nuheur * (1 - coverage_msyn_nuheur) / nsim)
	) %>% dplyr::select(-c("mcse_avg_se_sdid_numer",
												 "mcse_avg_se_sdid_denom",
												 "mcse_avg_se_did_numer",
												 "mcse_avg_se_did_denom"))

# attaches parameters of each simulation to the data frame of summarized results 
param_df <- sim_df %>% group_by(param_id) %>% filter(row_number() == 1) %>%
	dplyr::select(param_id, N, T, N_co, N_tr, T_pre, T_post, treat_assign, error_dist)
res_df <- as_tibble(merge(res_df, param_df, by="param_id"))

saveRDS(res_df, here("out", "results", "summarized_results.rds"))



