library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(kableExtra)

# load simulation results
res_df <- readRDS(here("out", "results", "summarized_results_7_3_26.rds"))

soda_res_df <- res_df[1:10,]
cps_res_df <- res_df[11:73,]
cps_res_df_par <- res_df[74:136,]

# standardize simulation results (bias and empirical SE) by outcome standard deviations
outcome_sds <- readRDS(here("data", "temp", "outcome_sds.rds"))

soda_res_df <- soda_res_df |>
	mutate(emp_se_sdid=emp_se_sdid/outcome_sds[1],
				 emp_se_augsynth=emp_se_augsynth/outcome_sds[1],
				 emp_se_msyn_nu0=emp_se_msyn_nu0/outcome_sds[1],
				 emp_se_msyn_nu1=emp_se_msyn_nu1/outcome_sds[1],
				 emp_se_msyn_nuheur=emp_se_msyn_nuheur/outcome_sds[1],
				 emp_se_did=emp_se_did/outcome_sds[1],
				 mcse_emp_se_sdid=mcse_emp_se_sdid/outcome_sds[1],
				 mcse_emp_se_augsynth=mcse_emp_se_augsynth/outcome_sds[1],
				 mcse_emp_se_msyn_nu0=mcse_emp_se_msyn_nu0/outcome_sds[1],
				 mcse_emp_se_msyn_nu1=mcse_emp_se_msyn_nu1/outcome_sds[1],
				 mcse_emp_se_msyn_nuheur=mcse_emp_se_msyn_nuheur/outcome_sds[1],
				 mcse_emp_se_did=mcse_emp_se_did/outcome_sds[1],
				 bias_sdid=bias_sdid/outcome_sds[1],
				 bias_augsynth=bias_augsynth/outcome_sds[1],
				 bias_msyn_nu0=bias_msyn_nu0/outcome_sds[1],
				 bias_msyn_nu1=bias_msyn_nu1/outcome_sds[1],
				 bias_msyn_nuheur=bias_msyn_nuheur/outcome_sds[1],
				 bias_did=bias_did/outcome_sds[1],
				 mcse_bias_sdid=mcse_bias_sdid/outcome_sds[1],
				 mcse_bias_augsynth=mcse_bias_augsynth/outcome_sds[1],
				 mcse_bias_msyn_nu0=mcse_bias_msyn_nu0/outcome_sds[1],
				 mcse_bias_msyn_nu1=mcse_bias_msyn_nu1/outcome_sds[1],
				 mcse_bias_msyn_nuheur=mcse_bias_msyn_nuheur/outcome_sds[1],
				 mcse_bias_did=mcse_bias_did/outcome_sds[1]
				 )

cps_res_df <- cps_res_df |>
	mutate(emp_se_sdid=emp_se_sdid/outcome_sds[2],
				 emp_se_augsynth=emp_se_augsynth/outcome_sds[2],
				 emp_se_msyn_nu0=emp_se_msyn_nu0/outcome_sds[2],
				 emp_se_msyn_nu1=emp_se_msyn_nu1/outcome_sds[2],
				 emp_se_msyn_nuheur=emp_se_msyn_nuheur/outcome_sds[2],
				 emp_se_did=emp_se_did/outcome_sds[2],
				 mcse_emp_se_sdid=mcse_emp_se_sdid/outcome_sds[2],
				 mcse_emp_se_augsynth=mcse_emp_se_augsynth/outcome_sds[2],
				 mcse_emp_se_msyn_nu0=mcse_emp_se_msyn_nu0/outcome_sds[2],
				 mcse_emp_se_msyn_nu1=mcse_emp_se_msyn_nu1/outcome_sds[2],
				 mcse_emp_se_msyn_nuheur=mcse_emp_se_msyn_nuheur/outcome_sds[2],
				 mcse_emp_se_did=mcse_emp_se_did/outcome_sds[2],
				 bias_sdid=bias_sdid/outcome_sds[2],
				 bias_augsynth=bias_augsynth/outcome_sds[2],
				 bias_msyn_nu0=bias_msyn_nu0/outcome_sds[2],
				 bias_msyn_nu1=bias_msyn_nu1/outcome_sds[2],
				 bias_msyn_nuheur=bias_msyn_nuheur/outcome_sds[2],
				 bias_did=bias_did/outcome_sds[2],
				 mcse_bias_sdid=mcse_bias_sdid/outcome_sds[2],
				 mcse_bias_augsynth=mcse_bias_augsynth/outcome_sds[2],
				 mcse_bias_msyn_nu0=mcse_bias_msyn_nu0/outcome_sds[2],
				 mcse_bias_msyn_nu1=mcse_bias_msyn_nu1/outcome_sds[2],
				 mcse_bias_msyn_nuheur=mcse_bias_msyn_nuheur/outcome_sds[2],
				 mcse_bias_did=mcse_bias_did/outcome_sds[2]
				 )

cps_res_df_par <- cps_res_df_par |>
	mutate(emp_se_sdid=emp_se_sdid/outcome_sds[2],
				 emp_se_augsynth=emp_se_augsynth/outcome_sds[2],
				 emp_se_msyn_nu0=emp_se_msyn_nu0/outcome_sds[2],
				 emp_se_msyn_nu1=emp_se_msyn_nu1/outcome_sds[2],
				 emp_se_msyn_nuheur=emp_se_msyn_nuheur/outcome_sds[2],
				 emp_se_did=emp_se_did/outcome_sds[2],
				 mcse_emp_se_sdid=mcse_emp_se_sdid/outcome_sds[2],
				 mcse_emp_se_augsynth=mcse_emp_se_augsynth/outcome_sds[2],
				 mcse_emp_se_msyn_nu0=mcse_emp_se_msyn_nu0/outcome_sds[2],
				 mcse_emp_se_msyn_nu1=mcse_emp_se_msyn_nu1/outcome_sds[2],
				 mcse_emp_se_msyn_nuheur=mcse_emp_se_msyn_nuheur/outcome_sds[2],
				 mcse_emp_se_did=mcse_emp_se_did/outcome_sds[2],
				 bias_sdid=bias_sdid/outcome_sds[2],
				 bias_augsynth=bias_augsynth/outcome_sds[2],
				 bias_msyn_nu0=bias_msyn_nu0/outcome_sds[2],
				 bias_msyn_nu1=bias_msyn_nu1/outcome_sds[2],
				 bias_msyn_nuheur=bias_msyn_nuheur/outcome_sds[2],
				 bias_did=bias_did/outcome_sds[2],
				 mcse_bias_sdid=mcse_bias_sdid/outcome_sds[2],
				 mcse_bias_augsynth=mcse_bias_augsynth/outcome_sds[2],
				 mcse_bias_msyn_nu0=mcse_bias_msyn_nu0/outcome_sds[2],
				 mcse_bias_msyn_nu1=mcse_bias_msyn_nu1/outcome_sds[2],
				 mcse_bias_msyn_nuheur=mcse_bias_msyn_nuheur/outcome_sds[2],
				 mcse_bias_did=mcse_bias_did/outcome_sds[2]
				 )

# function that reshapes results to a more useful format
reshape_results <- function(raw_df, methods_keep,
														ntr_levels, method_labels = NULL) {
	metrics <- c("bias", "emp_se", "coverage")
	if (is.null(method_labels)) method_labels <- toupper(methods_keep)
	
	# Build a regex that matches any known method string as a suffix,
	# e.g. "(sdid|augsynth|msyn_nu0)$"
	method_pattern <- paste0("(", paste(methods_keep, collapse = "|"), ")$")
	
	raw_df |>
		pivot_longer(
			cols      = matches("^(bias|emp_se|coverage|mcse)_"),
			names_to  = "variable",
			values_to = "value"
		) |>
		mutate(
			is_mcse  = str_starts(variable, "mcse_"),
			variable = if_else(is_mcse, str_remove(variable, "^mcse_"), variable),
			method   = str_extract(variable, method_pattern),
			metric   = str_remove(variable, paste0("_", method, "$"))
		) |>
		filter(metric %in% metrics, method %in% methods_keep) |>
		pivot_wider(
			id_cols     = c(param_id, N_co, N_tr, T_pre, T_post, method, metric),
			names_from  = is_mcse,
			values_from = value
		) |>
		rename(estimate = `FALSE`, mcse = `TRUE`) |>
		mutate(
			N_tr_label = factor(N_tr, levels = ntr_levels),
			method     = factor(method, levels = methods_keep, labels = method_labels),
			metric     = factor(metric,
													levels = c("bias", "emp_se", "coverage"),
													labels = c("Bias", "Empirical SE", "Coverage"))
		)
}

# Digits for point estimates and MCSEs
EST_DIGITS  <- 3
MCSE_DIGITS <- 3

fmt_cell <- function(estimate, mcse, digits_est = EST_DIGITS,
										 digits_mcse = MCSE_DIGITS) {
	# Coverage arrives as a proportion (e.g. 0.947); multiply to percentage
	sprintf("%.*f (%.*f)", digits_est, estimate, digits_mcse, mcse)
}

build_table_df <- function(long_df, methods_order,
													 t_pre = NULL, t_post = NULL) {
	
	df <- long_df
	if (!is.null(t_pre))  df <- filter(df, T_pre  == t_pre)
	if (!is.null(t_post)) df <- filter(df, T_post == t_post)
	
	wide <- df |>
		mutate(
			# Coverage: proportion → percentage for display
			estimate = if_else(metric == "Coverage", estimate * 100, estimate),
			mcse     = if_else(metric == "Coverage", mcse     * 100, mcse),
			cell     = map2_chr(estimate, mcse, fmt_cell),
			# Column name: "Metric__|__Method"
			col_key  = paste(metric, method, sep = "__|__")
		) |>
		dplyr::select(N_tr, N_co, col_key, cell) |>
		pivot_wider(names_from = col_key, values_from = cell)
	
	# Reorder columns: metrics first, methods nested within each metric
	metric_order <- c("Bias", "Empirical SE", "Coverage")
	ordered_keys <- as.vector(t(outer(metric_order, methods_order,
																	paste, sep = "__|__")))
	ordered_keys <- ordered_keys[ordered_keys %in% names(wide)]
	
	wide |>
		dplyr::select(N_tr, N_co, all_of(ordered_keys)) |>
		arrange(N_tr, N_co)
}

# ── Table 1: Soda  (T_pre = 5, T_post = 1, methods: SDiD & ASCM) ─────────────
# Uses reshape_results() from plot_all_dgps.R
long_soda <- reshape_results(
	soda_res_df,
	methods_keep  = c("augsynth", "msyn_nu1", "msyn_nuheur"),
	method_labels = c("ASCM", "Multisynth (nu=1)", "Multisynth (nuhat)"),
	ntr_levels    = c("N_tr = 1", "N_tr = 6", "N_tr = 10", "N_tr = 25")
)

wide_soda <- build_table_df(long_soda,
	methods_order = c("ASCM", "Multisynth (nu=1)", "Multisynth (nuhat)"),
	t_pre = 5, t_post = 1)

wide_soda$N_tr <- c("\\multirow{4}{*}{1}", "", "", "",
										"\\multirow{3}{*}{6}", "", "",
										"\\multirow{2}{*}{10}", "", 
										"25")

cat(kable(wide_soda, format="latex", escape=FALSE, linesep="", booktabs=TRUE) |>
	row_spec(c(4,7,9),extra_latex_after = "\\cmidrule{1-11}"),
	file = here("out", "tables", "part_pool_soda.tex"))

# ── Table 2: CPS  (methods: SDiD & ASCM) ─────────────────────────────────────
long_cps <- reshape_results(
	cps_res_df |> filter(N_tr %in% c(1, 10)),
	methods_keep  = c("augsynth", "msyn_nu1", "msyn_nuheur"),
	method_labels = c("ASCM", "Multisynth (nu=1)", "Multisynth (nuhat)"),
	ntr_levels    = c("N_tr = 1", "N_tr = 10")
)

# Stack all three T combos into one table, add a T-grouping column
wide_cps <- map_dfr(
	list(c(5, 1), c(15, 5), c(30, 10)),
	function(tc) {
		build_table_df(long_cps, methods_order = c("ASCM", "Multisynth (nu=1)", "Multisynth (nuhat)"),
			t_pre = tc[1], t_post = tc[2]) |>
		mutate(T_label = sprintf("Tpre=%d,Tpost=%d",
			tc[1], tc[2]),
			.before = 1)
	}
)

wide_cps$N_tr <- c("\\multirow{4}{*}{1}", "", "", "",
										"\\multirow{2}{*}{10}", "",
									 "\\multirow{4}{*}{1}", "", "", "",
									 "\\multirow{2}{*}{10}", "",
									 "\\multirow{4}{*}{1}", "", "", "",
									 "\\multirow{2}{*}{10}", "")

cat(kable(wide_cps[1:6,2:12], format="latex", escape=FALSE, linesep="", booktabs=TRUE) |>
	row_spec(4,extra_latex_after = "\\cmidrule{1-11}"),
	file = here("out", "tables", "part_pool_cps_a.tex"))
cat(kable(wide_cps[7:12,2:12], format="latex", escape=FALSE, linesep="", booktabs=TRUE) |>
	row_spec(4,extra_latex_after = "\\cmidrule{1-11}"),
	file = here("out", "tables", "part_pool_cps_b.tex"))
cat(kable(wide_cps[13:18,2:12], format="latex", escape=FALSE, linesep="", booktabs=TRUE) |>
	row_spec(4,extra_latex_after = "\\cmidrule{1-11}"),
	file = here("out", "tables", "part_pool_cps_c.tex"))

# ── Table 3: CPS parallel trends  (methods: SDiD, ASCM, DiD) ─────────────────
long_cps_par <- reshape_results(
	cps_res_df_par |> filter(N_tr %in% c(1, 10)),
	methods_keep  = c("augsynth", "msyn_nu1", "msyn_nuheur"),
	method_labels = c("ASCM", "Multisynth (nu=1)", "Multisynth (nuhat)"),
	ntr_levels    = c("N_tr = 1", "N_tr = 10")
)

wide_cps_par <- map_dfr(
	list(c(5, 1), c(15, 5), c(30, 10)),
	function(tc) {
		build_table_df(long_cps_par, methods_order = c("ASCM", "Multisynth (nu=1)", "Multisynth (nuhat)"),
				t_pre = tc[1], t_post = tc[2]) |>
			mutate(T_label = sprintf("Tpre=%d,Tpost=%d",
				tc[1], tc[2]),
				.before = 1)
	}
)

wide_cps_par$N_tr <- c("\\multirow{4}{*}{1}", "", "", "",
									 "\\multirow{2}{*}{10}", "",
									 "\\multirow{4}{*}{1}", "", "", "",
									 "\\multirow{2}{*}{10}", "",
									 "\\multirow{4}{*}{1}", "", "", "",
									 "\\multirow{2}{*}{10}", "")

cat(kable(wide_cps_par[1:6,2:12], format="latex", escape=FALSE, linesep="", booktabs=TRUE) |>
	row_spec(4,extra_latex_after = "\\cmidrule{1-11}"),
	file = here("out", "tables", "part_pool_cps_par_a.tex"))
cat(kable(wide_cps_par[7:12,2:12], format="latex", escape=FALSE, linesep="", booktabs=TRUE) |>
	row_spec(4,extra_latex_after = "\\cmidrule{1-11}"),
	file = here("out", "tables", "part_pool_cps_par_b.tex"))
cat(kable(wide_cps_par[13:18,2:12], format="latex", escape=FALSE, linesep="", booktabs=TRUE) |>
	row_spec(4,extra_latex_after = "\\cmidrule{1-11}"),
	file = here("out", "tables", "part_pool_cps_par_c.tex"))
