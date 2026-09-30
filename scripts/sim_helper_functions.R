#-----------------------------------------------------
# functions that generate outcomes
#-----------------------------------------------------
simulate_Y_treat_fixed <- function(L, cov, tau, N0, T0) {
	N = dim(L)[1]
	T = dim(L)[2]

	error_mat = mvrnorm(n=N, mu=rep(0, T), Sigma=cov)
	
	tau_mat = matrix(0, nrow=N, ncol=T)
	tau_mat[(N0+1):N, (T0+1):T] = tau
	
	return(L + tau_mat + error_mat)
}

simulate_Y_treat_probabilistic <- function(L, cov, TE_type, tau, N_tr, T_post, treat_weights, error_dist) {
	# compute dimensions and expected number of treated units
	N <- dim(L)[1]
	T <- dim(L)[2]
	N_co <- N - N_tr
	T_pre <- T - T_post
	
	# sample treatment indicators using weights and sequential sampling
	treated_rows <-	sample(1:N, size=N_tr, replace=FALSE, prob=treat_weights)
	
	# sample full error matrix with calibrated AR cov structure
	if (error_dist == "gaussian") {
		error_mat <- mvrnorm(n=N, mu=rep(0, T), Sigma=cov)
	} else if (error_dist == "t") {
		error_mat <- rmvt(n=N, sigma=(1/3)*cov, df=3, delta=rep(0, T))
	}
	
	# generate outcome with treated units in the last rows by rearranging rows of Y
	Y <- L + error_mat
	control_rows <- setdiff(1:N, treated_rows)
	Y <- Y[c(control_rows, treated_rows), ]
	
	tau_mat <- matrix(0, nrow=N, ncol=T)
	if (TE_type == "static") {
		tau_mat[(N_co+1):N, (T_pre+1):T] <- tau
	} else if (TE_type == "dynamic") {
		tau_path <- seq(2*tau, 0, length.out=T_post)
		
		if (abs(mean(tau_path) -tau) > 0.000001) {
			print(TE_type)
			print(tau)
			print(tau_path)
			print(N_tr)
			print(T_post)
			print(L)
			stop("TE path incorrectly specified, tau not equal to specified tau.")	
		}
		
		tau_mat[(N_co+1):N, (T_pre+1):T] <- matrix(rep(tau_path, each=N_tr), nrow=N_tr)
	}
	
	return(Y + tau_mat)
}

#-----------------------------------------------------
# functions that compute estimates
#-----------------------------------------------------
compute_did <- function(Ysim, N_co, T_pre, tau) {
	N <- dim(Ysim)[1]
	T <- dim(Ysim)[2]
	N_tr <- N - N_co
	T_post <- T - T_pre
	
	# obtain pretreatment average for each unit
	Ybar_pre <- apply(Ysim[, 1:T_pre], MARGIN=1, mean)
	Ybar_pre_mat <- matrix(rep(Ybar_pre, times=T_post), ncol=T_post)
	
	# compute Ydot for each post-intervention period
	Ypost <- Ysim[, (T_pre+1):T]
	Ydot <- Ypost - Ybar_pre_mat
	Ydot_bar <- apply(Ydot, MARGIN=1, mean)
	
	D <- c(rep(0, N_co), rep(1, N_tr))
	
	df <- data.frame(Ydot_bar, D)
	
	fit <- lm(Ydot_bar ~ D, data=df)
	tau_hat <- unname(fit$coefficients[2])
	
    if (N_tr == 1) {
        se = sqrt(vcov(fit)[2,2])
        lci = tau_hat - qt(0.975, df=N-2)*se
        uci = tau_hat + qt(0.975, df=N-2)*se
        covers = (lci <= tau) & (tau <= uci)
    } else {
        se = sqrt(vcovHC(fit, type="HC3")[2,2])
        lci = tau_hat - qt(0.975, df=N-2)*se
        uci = tau_hat + qt(0.975, df=N-2)*se
        covers = (lci <= tau) & (tau <= uci)
    }
	
	return(c("tau_hat_did"=tau_hat, 
    "se_did"=se, 
    "lci_did"=lci, 
    "uci_did"=uci, 
    "covers_did"=covers))
}

compute_sdid <- function(Ysim, N_co, T_pre, tau) {
	tau_hat_sdid <- synthdid_estimate(Ysim, N_co, T_pre)
	se_sdid <- placebo_se_alt(tau_hat_sdid, replications=200)
	lci_sdid <- tau_hat_sdid - qnorm(0.975)*se_sdid
	uci_sdid <- tau_hat_sdid + qnorm(0.975)*se_sdid
	covers_sdid <- (lci_sdid <= tau) & (tau <= uci_sdid)

    return(c("tau_hat_sdid"=as.double(tau_hat_sdid), 
    "se_sdid"=as.double(se_sdid), 
    "lci_sdid"=as.double(lci_sdid), 
    "uci_sdid"=as.double(uci_sdid), 
    "covers_sdid"=covers_sdid))
}

compute_augsynth <- function(Ysim, N_co, T_pre, tau) {
	N <- nrow(Ysim)
	T <- ncol(Ysim)
	N_tr <- N - N_co
	T_post <- T - T_pre
	
	df <- data.frame(Ysim) %>%
		mutate(unit=row_number()) %>% pivot_longer(starts_with("X")) %>% 
		mutate(time=as.integer(gsub("X", "", name)),
					 treat=as.integer((unit > N_co) & (time > T_pre)))
	
	augsynth_fit <- augsynth(value ~ treat, unit, time, data=df, 
					 progfunc="Ridge")
	augsynth_summ <- summary(augsynth_fit)
	tau_hat_augsynth <- augsynth_summ$average_att$Estimate
	covers_augsynth <- augsynth_summ$average_att$p_val > 0.05
	
	multisynth_nu0_fit <- multisynth(value ~ treat, unit, time, data=df, nu=0)
	multisynth_nu0_summ <- summary(multisynth_nu0_fit)
	msyn_nu0_att_est <- multisynth_nu0_summ$att %>% filter(Level=="Average", is.na(Time))
	tau_hat_msyn_nu0 <- msyn_nu0_att_est$Estimate
	covers_msyn_nu0 <- msyn_nu0_att_est$lower_bound <= tau & msyn_nu0_att_est$upper_bound >= tau
	
	multisynth_nu1_fit <- multisynth(value ~ treat, unit, time, data=df, nu=1)
	multisynth_nu1_summ <- summary(multisynth_nu1_fit)
	msyn_nu1_att_est <- multisynth_nu1_summ$att %>% filter(Level=="Average", is.na(Time))
	tau_hat_msyn_nu1 <- msyn_nu1_att_est$Estimate
	covers_msyn_nu1 <- msyn_nu1_att_est$lower_bound <= tau & msyn_nu1_att_est$upper_bound >= tau
	
	multisynth_nuheur_fit <- multisynth(value ~ treat, unit, time, data=df)
	multisynth_nuheur_summ <- summary(multisynth_nuheur_fit)
	msyn_nuheur_att_est <- multisynth_nuheur_summ$att %>% filter(Level=="Average", is.na(Time))
	tau_hat_msyn_nuheur <- msyn_nuheur_att_est$Estimate
	covers_msyn_nuheur <- msyn_nuheur_att_est$lower_bound <= tau & msyn_nuheur_att_est$upper_bound >= tau
	msyn_nuheur_nuhat <- multisynth_nuheur_fit$nu
	
	return(c("tau_hat_augsynth"=tau_hat_augsynth,
				 "covers_augsynth"=as.integer(covers_augsynth),
				 "tau_hat_msyn_nu0"=tau_hat_msyn_nu0,
				 "covers_msyn_nu0"=as.integer(covers_msyn_nu0),
				 "tau_hat_msyn_nu1"=tau_hat_msyn_nu1,
				 "covers_msyn_nu1"=as.integer(covers_msyn_nu1),
				 "tau_hat_msyn_nuheur"=tau_hat_msyn_nuheur,
				 "covers_msyn_nuheur"=as.integer(covers_msyn_nuheur),
				 "msyn_nuheur_nuhat"=msyn_nuheur_nuhat))
}

#-----------------------------------------------------
# two versions of a synth-did SE estimator
# the first is pulled from the original github
# the second is a fix for optimization issues found
# in the github issues: https://github.com/synth-inference/synthdid/issues/128
#-----------------------------------------------------
placebo_se <- function(estimate, replications) {
	setup = attr(estimate, 'setup')
	opts = attr(estimate, 'opts')
	weights = attr(estimate, 'weights')
	N1 = nrow(setup$Y) - setup$N0
	if (setup$N0 <= N1) { stop('must have more controls than treated units to use the placebo se') }
	theta = function(ind) {
		N0 = length(ind)-N1
		weights.boot = weights
		weights.boot$omega = sum_normalize(weights$omega[ind[1:N0]])
		do.call(synthdid_estimate, c(list(Y=setup$Y[ind,], N0=N0,  T0=setup$T0,  X=setup$X[ind, ,], weights=weights.boot), opts))
	}
	sqrt((replications-1)/replications) * sd(replicate(replications, theta(sample(1:setup$N0))))
}

placebo_se_alt <- function(estimate, replications) {
	setup = attr(estimate, 'setup')
	opts = attr(estimate, 'opts')
	weights = attr(estimate, 'weights')
	N1 = nrow(setup$Y) - setup$N0
	if (setup$N0 <= N1) { stop('must have more controls than treated units to use the placebo se') }
	theta = function(ind) {
		N0 = length(ind)-N1
		noise.level = sd(apply(setup$Y[ind[1:(length(ind)-1)], 1:setup$T0], 1, diff))
		opts$min.decrease = 1e-05 * noise.level
		if((opts$zeta.omega/noise.level) > 1e-05){
			eta.omega = (N1 * (ncol(setup$Y) - setup$T0))^(1/4)
			opts$zeta.omega = eta.omega * noise.level
		}else{
			eta.omega = 1e-06
			opts$zeta.omega = eta.omega * noise.level
		}
		if((opts$zeta.lambda/noise.level) < 1e-05){
			eta.lambda = 1e-06
			opts$zeta.lambda = eta.lambda * noise.level
		}
		weights.boot = list()
		if(opts$update.lambda == TRUE){
			weights.boot$lambda = rep(1/setup$T0, setup$T0)
		}else{
			weights.boot$lambda = weights$lambda
		}
		if(opts$update.omega == TRUE){
			weights.boot$omega = rep(1/N0, N0)
		}else{
			weights.boot$omega = sum_normalize(weights$omega[ind[1:N0]])
		}
		do.call(synthdid_estimate, c(list(Y=setup$Y[ind,], N0=N0,  T0=setup$T0, weights=weights.boot), opts))
	}
	sqrt((replications-1)/replications) * sd(replicate(replications, theta(sample(1:setup$N0))))
}

#-----------------------------------------------------
# function that generates dataframes of simulation specs
#-----------------------------------------------------
generate_specs <- function(dataset, parallel=FALSE) {
    ## load plasmode parameters calibrated from real data and generate Ls and covariances
    calibrated_params <- readRDS(here("data", "temp", paste0(dataset, ".rds")))

    # use negatives because upsilon becomes more interpretable
    gamma_actual <- calibrated_params[[1]]
    psi_actual <- calibrated_params[[2]]
    ar_coef <- calibrated_params[[3]]
    var <- calibrated_params[[4]]

    # different sample sizes depending on datasets
    if (dataset == "soda") {
        grid <- expand.grid(
            N_co = c(6, 10, 25, 40),
            N_tr = c(1, 6, 10, 25),
            T_pre = 5,
            T_post = 1,
            tau = 0,
            TE_type = "static",
            treat_assign = "probabilistic",
            error_dist = "gaussian",
            parallel = parallel
        ) %>% filter(N_tr < N_co)
    } else if (dataset == "cps") {
        grid <- expand.grid(
            N_co = c(6, 10, 25, 40),
            N_tr = c(1, 6, 10),
            T_pre = c(5, 15, 30),
            T_post = c(1, 5, 10),
            tau = 0,
            TE_type = "static",
            treat_assign = "probabilistic",
            error_dist = "gaussian",
            parallel = parallel
        ) %>% filter(N_tr < N_co, T_post < T_pre)
    }

    grid <- grid %>% mutate(N = N_co + N_tr,
                            T = T_pre + T_post)

    # get means and variances for MVN from which we generate gammas
    # our modelling does not take these as stochastic, this is just a way
    # of generating representative data
    gamma_mean <- apply(gamma_actual, MARGIN=2, mean)
    gamma_variances <- cov(gamma_actual)

    # generate gammas, covs, and L for different control and treatment sample sizes
    set.seed(1)
    
    # choose correct AR function for data
    if (length(ar_coef) == 1) {
    	get_cov <- function(T) {
    		get_cov_ar1(ar_coef, var, T)
    	}
    } else if (length(ar_coef) == 2) {
    	get_cov <- function(T) {
    		get_cov_ar2(ar_coef[1], ar_coef[2], var, T)
    	}	
    }

    if (!parallel) {
        grid <- grid %>% rowwise() %>%
            mutate(
                gamma = list(mvrnorm(n = N, mu = gamma_mean, Sigma = gamma_variances)),
                cov = list(get_cov(T)),
                L = list(gamma %*% psi_actual[, (ncol(psi_actual)-T+1):ncol(psi_actual)])
            ) %>%
            ungroup()
    } else {
        psi_parallel <- rbind(psi_actual[1,], 1)
        grid <- grid %>% rowwise() %>%
            mutate(
                gamma = list(cbind(gamma_mean[1], rnorm(n = N, mean = gamma_mean[2], sd = sqrt(gamma_variances[2,2])))),
                cov = list(get_cov(T)),
                L = list(gamma %*% psi_parallel[, (ncol(psi_parallel)-T+1):ncol(psi_parallel)]) 
            ) %>%
            ungroup()
    }
    
    grid
}
