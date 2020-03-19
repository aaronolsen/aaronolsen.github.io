if(!is.null(dev.list())) dev.off()

run <- function(){

	num_rank <- 7
	min_val_lm <- 5		# Minimum number of positives with which to fit regression
	min_num_val_lm <- 6	# Minimum number of days to include in regression

	# Get names of data files
	csv_files <- list.files('Data csv')

	# Find most recent file
	csv_file <- tail(sort(csv_files), 1)

	# Read data
	read_csv <- read.csv(file=paste0('Data csv/', csv_file))

	# Remove NA rows
	read_csv <- read_csv[!is.na(read_csv[, 'Date']), ]

	# Add first reported positive cases
	if(FALSE){
		# From Wikipedia
		read_csv <- rbind(read_csv, data.frame('Date'=20200125, 'State'='CA', 'Positive'=1, 'Negative'='NA', 'Pending'='NA', 'Death'='NA', 'Total'='NA'))
		read_csv <- rbind(read_csv, data.frame('Date'=20200121, 'State'='WA', 'Positive'=1, 'Negative'='NA', 'Pending'='NA', 'Death'='NA', 'Total'='NA'))
		read_csv <- rbind(read_csv, data.frame('Date'=20200301, 'State'='NY', 'Positive'=1, 'Negative'='NA', 'Pending'='NA', 'Death'='NA', 'Total'='NA'))
		read_csv <- rbind(read_csv, data.frame('Date'=20200301, 'State'='FL', 'Positive'=1, 'Negative'='NA', 'Pending'='NA', 'Death'='NA', 'Total'='NA'))
		read_csv[(read_csv$Date == '20200311')+(read_csv$State == 'AR') == 2, 'Positive'] <- 1
		#read_csv <- rbind(read_csv, data.frame('Date'=20200311, 'State'='AR', 'Positive'=1, 'Negative'='NA', 'Pending'='NA', 'Death'='NA', 'Total'='NA'))
		# From https://www.phoenixnewtimes.com/news/arizona-coronavirus-cases-total-to-date-covid-19-list-11457756
		read_csv <- rbind(read_csv, data.frame('Date'=20200126, 'State'='AZ', 'Positive'=1, 'Negative'='NA', 'Pending'='NA', 'Death'='NA', 'Total'='NA'))
	}

	# Format dates
	read_csv$AsDate <- as.Date(as.character(read_csv$Date), "%Y%m%d")

	# Get unique list of states
	states_unique <- as.character(sort(unique(read_csv[, 'State'])))

	# Create structures
	df_by_state <- list()
	first_reported <- setNames(rep(NA, length(states_unique)), states_unique)
	first_positive <- setNames(rep(NA, length(states_unique)), states_unique)
	y_ranges <- list(
		'Positive'=matrix(NA, length(states_unique), 2, dimnames=list(states_unique, c('min', 'max'))),
		'Positive_log'=matrix(NA, length(states_unique), 2, dimnames=list(states_unique, c('min', 'max')))
	)
	x_ranges <- list(
		'Days_SFP'=matrix(NA, length(states_unique), 2, dimnames=list(states_unique, c('min', 'max')))
	)

	# Regression stats matrix
	col_names <- c('slope', 'intercept')
	log_sfp_lm_mat <- matrix(NA, length(states_unique), length(col_names), dimnames=list(states_unique, col_names))

	# For each state
	for(state in states_unique){

		# Save dataframe for state
		df_by_state[[state]] <- read_csv[read_csv[, 'State'] == state, ]
		
		# Get first reported date
		first_reported[state] <- sort(df_by_state[[state]][, 'AsDate'])[1]

		# Get positive values as character
		positive_vals <- as.numeric(gsub(',', '', as.character(df_by_state[[state]][, 'Positive'])))

		# Save positive values
		df_by_state[[state]][, 'Positive'] <- positive_vals

		# Get first positive reported date
		first_positive[state] <- sort(df_by_state[[state]][positive_vals > 0, 'AsDate'])[1]
		
		# Get days since first reported
		df_by_state[[state]][, 'Days_SFP'] <- as.numeric(df_by_state[[state]][, 'AsDate'] - first_positive[state])

		# Get days since first positive
		df_by_state[[state]][, 'Days_SFR'] <- as.numeric(df_by_state[[state]][, 'AsDate'] - first_reported[state])
		
		# Get log positive
		df_by_state[[state]][, 'Positive_log'] <- NA

		# Get positive values not NA and over 0
		which_positive <- rep(FALSE, length(positive_vals))
		which_positive[is.na(positive_vals)] <- FALSE
		which_positive[!is.na(positive_vals)] <- positive_vals[!is.na(positive_vals)] > 0

		if(!any(which_positive)) next

		# Get log values
		df_by_state[[state]][which_positive, 'Positive_log'] <- log(positive_vals[which_positive], base=10)
		
		# Find when values are greater than 1
		which_over_min <- rep(FALSE, length(positive_vals))
		which_over_min[is.na(positive_vals)] <- FALSE
		which_over_min[!is.na(positive_vals)] <- positive_vals[!is.na(positive_vals)] > min_val_lm
		
		# Fit linear regression to log values
		if(sum(which_over_min) >= min_num_val_lm){
		
			# Set data list
			dlist <- list(y=df_by_state[[state]][which_over_min, 'Positive_log'], x=df_by_state[[state]][which_over_min, 'Days_SFP'])
			lm_fit_log_sfp <- lm(y ~ x, data=dlist)
			
			# Save regression stats
			log_sfp_lm_mat[state, 'intercept'] <- lm_fit_log_sfp$coefficients['(Intercept)']
			log_sfp_lm_mat[state, 'slope'] <- lm_fit_log_sfp$coefficients['x']
		}

		# Get y ranges
		y_ranges[['Positive']][state, ] <- range(positive_vals, na.rm=TRUE)
		y_ranges[['Positive_log']][state, ] <- range(df_by_state[[state]][which_positive, 'Positive_log'], na.rm=TRUE)

		# Get x ranges
		x_ranges[['Days_SFP']][state, ] <- range(df_by_state[[state]][, 'Days_SFP'], na.rm=TRUE)
	}
	
	#
	log_sfp_lm_mat <- log_sfp_lm_mat[!is.na(log_sfp_lm_mat[, 1]), ]

	# Rank states
	rank_order <- list(
		'slope'=rownames(log_sfp_lm_mat)[order(log_sfp_lm_mat[, 'slope'])],
		'intercept'=rownames(log_sfp_lm_mat)[order(log_sfp_lm_mat[, 'intercept'])]
	)

	# Create color schemes
	#state_cols <- setNames(svg.pal(length(states_unique), yellow=FALSE), states_unique)
	#print(state_cols)
	xlab <- c('Days_SFP'='Days since first reported positive test result or March 4, whichever is later')

	# Plot
	for(ranked_by in c('slope', 'intercept')[1]){
		for(y_val in c('Positive', 'Positive_log')[2]){
			for(x_val in c('Days_SFP')){
			
				# Set which states to plot
				states_plot_low <- head(rank_order[[ranked_by]], num_rank)
				states_plot_high <- tail(rank_order[[ranked_by]], num_rank)
				
				# States to plot
				states_plot <- c(states_plot_low, states_plot_high)
				
				# Set state colors
				state_cols <- setNames(rev(rainbow(length(states_plot), start=0, end=0.7)), states_plot)

				pch_set <- rep(1:5, 55)
				state_pch <- setNames(pch_set[1:length(states_plot)], states_plot)
				#print(state_cols)

				# Set variable ranges
				y_range <- range(y_ranges[[y_val]][states_plot, ], na.rm=TRUE)
				x_range <- range(x_ranges[[x_val]][states_plot, ], na.rm=TRUE)
				
				#
				y_range <- c(1, 1.1)*y_range
		
				# Set default plot parameters
				yaxt <- 's'

				# Adjust default plot parameters
				if(y_val == 'Positive_log') yaxt <- 'n'
				if(y_val == 'Positive_log' && x_val == 'Days_SFP') x_range <- c(0, x_range[2])
	
				# Open PDF
				pdf(paste0('Plots/', y_val, ' vs ', x_val, ' ranked_by=', ranked_by, '.pdf'), height=6, width=8)
				
				#
				par('mar'=c(5,5,4,8))
	
				# Create plot
				plot(x_range, y_range, type='n', xlab=xlab[x_val], ylab='Number of reported positive COVID-19 test results', yaxt=yaxt, 
					main=paste0('Seven states with lowest and highest LOG growth curve\nslopes, respectively, as of ', format(read_csv$AsDate[1], format="%B %d, %Y")))
		
				# For each state
				n <- 1
				for(state in states_plot){
		
					# Plot points as line
					points(x=df_by_state[[state]][, x_val], y=df_by_state[[state]][, y_val], 
						pch=state_pch[state], col=state_cols[state])
					points(x=df_by_state[[state]][, x_val], y=df_by_state[[state]][, y_val], type='l', 
						lwd=2, col=state_cols[state])
			
					n <- n + 1
				}
		
				# Create log axis
				if(y_val == 'Positive_log'){
					log_range2 <- nchar(round(10^y_range[2]))
					axis_at <- seq(0, log_range2, length=log_range2+1)
					axis(side=2, at=axis_at, labels=round(10^axis_at))
				}
				
				#
				legend(x=x_range[2] + 0.1*diff(x_range), y=y_range[2], xpd=TRUE, 
					legend=c(states_plot_low, '...', states_plot_high), 
					col=c(state_cols[states_plot_low], NA, state_cols[states_plot_high]), 
					pch=c(state_pch[states_plot_low], NA, state_pch[states_plot_high]), lty=1, lwd=1.5, bty='n')
	
				#
				dev.off()
			}
		}
	}
	

	## Plot single state against background
	state_highlight <- 'RI'
	y_val <- 'Positive_log'
	x_val <- 'Days_SFP'

	# Set variable ranges
	y_range <- range(y_ranges[[y_val]], na.rm=TRUE)
	x_range <- range(x_ranges[[x_val]], na.rm=TRUE)
	
	# Adjust default plot parameters
	yaxt <- 'n'
	x_range <- c(0, x_range[2])

	# Open PDF
	pdf(paste0('Plots/By state/', y_val, ' vs ', x_val, ' ', state_highlight, '.pdf'), height=6, width=8)
	
	#
	par('mar'=c(5,5,2,2))

	# Create plot
	plot(x_range, y_range, type='n', xlab=xlab[x_val], ylab='Number of reported positive COVID-19 test results', yaxt=yaxt)

	# For each state
	for(state in states_unique){
	
		if(state == state_highlight) next

		# Plot points as line
		points(x=df_by_state[[state]][, x_val], y=df_by_state[[state]][, y_val], type='l', lwd=2, col=gray(0.75))
	}

	# Plot points as line
	points(x=df_by_state[[state_highlight]][, x_val], y=df_by_state[[state_highlight]][, y_val], type='l', lwd=2, col='black')

	# Create log axis
	if(y_val == 'Positive_log'){
		log_range2 <- nchar(round(10^y_range[2]))
		axis_at <- seq(0, log_range2, length=log_range2+1)
		axis(side=2, at=axis_at, labels=round(10^axis_at))
	}
	
	#
	dev.off()


	
	## Plot single state against background
	state_highlight <- 'RI'
	y_val <- 'Positive_log'
	x_val <- 'Days_SFP'

	# Set variable ranges
	y_range <- range(y_ranges[[y_val]], na.rm=TRUE)
	x_range <- range(x_ranges[[x_val]], na.rm=TRUE)
	
	# Adjust default plot parameters
	yaxt <- 'n'
	x_range <- c(0, x_range[2])

	# Open PDF
	pdf(paste0('Plots/', y_val, ' vs ', x_val, ' All states.pdf'), height=30, width=40)
	
	#
	layout(matrix(1:length(states_unique), 7, 8, byrow=TRUE))
	par('mar'=c(5,5,2,2))

	# For each state
	for(state_highlight in states_unique){

		# Create plot
		plot(x_range, y_range, type='n', xlab=xlab[x_val], ylab='Number of reported positive COVID-19 test results', main=state_highlight, yaxt=yaxt)

		# For each state
		for(state in states_unique){
	
			if(state == state_highlight) next

			# Plot points as line
			points(x=df_by_state[[state]][, x_val], y=df_by_state[[state]][, y_val], type='l', lwd=1, col=gray(0.8))
		}

		# Plot points as line
		points(x=df_by_state[[state_highlight]][, x_val], y=df_by_state[[state_highlight]][, y_val], type='l', lwd=2, col='black')

		# Create log axis
		if(y_val == 'Positive_log'){
			log_range2 <- nchar(round(10^y_range[2]))
			axis_at <- seq(0, log_range2, length=log_range2+1)
			axis(side=2, at=axis_at, labels=round(10^axis_at))
		}
	}
	
	#
	dev.off()
}

run()