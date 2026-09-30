# fit AR(1) process to error component
# adapted from Arkhangelsky et al. (2021)
fit_ar1 <- function(E){
	T_full <- dim(E)[2]
	E_ts <- c(E[,2:T_full])
	E_lag_1 <- c(E[,1:(T_full-1)])
	
	ar_coef <- cov(E_ts, E_lag_1)/var(E_lag_1)
	ar_resid <- E_ts - ar_coef*E_lag_1
	var <- var(ar_resid)
	return(list(ar_coef, var))
}

# compute AR2 coefficients instead of AR1 coefficients
fit_ar2 <- function(E){
	
	T_full <- dim(E)[2]
	E_ts <- E[,3:T_full]
	E_lag_1 <- E[,2:(T_full-1)]
	E_lag_2 <- E[,1:(T_full-2)]
	
	a_1 <- sum(diag(E_lag_1%*%t(E_lag_1)))
	a_2 <- sum(diag(E_lag_2%*%t(E_lag_2)))
	a_3 <- sum(diag(E_lag_1%*%t(E_lag_2)))
	
	matrix_factor <- rbind(c(a_1,a_3),c(a_3,a_2))
	
	b_1 <- sum(diag(E_lag_1%*%t(E_ts)))
	b_2 <- sum(diag(E_lag_2%*%t(E_ts)))
	
	ar_coef <- as.vector(solve(matrix_factor)%*%c(b_1,b_2))
	ar_resid <- E_ts - ar_coef[1]*E_lag_1 - ar_coef[2]*E_lag_2
	var <- var(as.vector(ar_resid))
	
	return(list(ar_coef, var))
}

ar1_gamma <- function(phi, sigma2_u, T) {
	gamma0 <- sigma2_u / (1 - phi^2)
	gamma <- gamma0 * phi^(0:(T-1))
	gamma
}

# AR(2) stationary autocovariances via Yule-Walker recursion.
# First compute gamma0, gamma1, then gamma_k = phi1*gamma_{k-1} + phi2*gamma_{k-2}.
ar2_gamma <- function(phi1, phi2, sigma2_u, T) {
	# Stationary variance gamma0 derived from Yule-Walker:
	# gamma0 = sigma_u^2 * (1 - phi2) / ((1 + phi2) * ((1 - phi2)^2 - phi1^2))
	denom <- (1 + phi2) * ((1 - phi2)^2 - phi1^2)
	if (denom <= 0) return(rep(NA_real_, T))  # should not happen if stationary, but guard
	
	gamma0 <- sigma2_u * (1 - phi2) / denom
	gamma1 <- phi1 * gamma0 / (1 - phi2)
	
	gamma <- numeric(T)
	gamma[1] <- gamma0
	if (T >= 2) gamma[2] <- gamma1
	if (T >= 3) {
		for (k in 3:T) {
			gamma[k] <- phi1 * gamma[k-1] + phi2 * gamma[k-2]
		}
	}
	gamma
}

get_cov_ar1 <- function(phi, sigma2_u, T) {
	gamma0 <- sigma2_u / (1 - phi^2)
	idx <- 0:(T - 1)
	toeplitz(gamma0 * phi^idx)
}

get_cov_ar2 <- function(phi1, phi2, sigma2_u, T) {
	gamma <- ar2_gamma(phi1, phi2, sigma2_u, T)
	toeplitz(gamma)
}