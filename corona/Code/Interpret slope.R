if(!is.null(dev.list())) dev.off()

run <- function(){

	set.seed(42)

	# Set x
	x <- 0:10
	
	#
	exp_coef <- 3
	log_base <- 10
	
	# Create exponential
	y <- exp_coef^x + rnorm(length(x), 0, 0.01)
	
	#y_change <- (tail(y, 1) - y[1])
	#x_change <- y_change
	
	# Measure rate
	y_log <- log(y, base=log_base)
	
	# Fit linear regression
	lm_fit <- lm(y ~ x, data=list(y=y_log, x=x))
	
	# Get slope
	line_slope <- lm_fit$coefficients['x']

	# Calculate doubling rate from regression
	dbl_rate <- log(2, base=log_base) / line_slope

	# Calculate n-fold increase per day
	nfold_inc_pday <- log_base^line_slope

	print(nfold_inc_pday)
	print(dbl_rate)

	# Calculate doubling rate from given coefficients
	dbl_rate <- log(2, base=log_base) / log(exp_coef, base=log_base)
	print(dbl_rate)
	
	print(round(cbind(x, y), 2))
}

run()