if(!is.null(dev.list())) dev.off()

run <- function(){

	num_rank <- 7
	min_val_lm <- 5		# Minimum number of positives with which to fit regression
	min_num_val_lm <- 6	# Minimum number of days to include in regression

	# 
	num_spelled <- c('One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine', 'Ten')

	# Get names of data files
	csv_files <- list.files('../Data/')

	# Find most recent file
	csv_file <- tail(sort(csv_files), 1)
	
	# Get file date
	file_date <- gsub('[.]csv', '', csv_file)

	# Read data
	read_csv <- read.csv(file=paste0('../Data/', csv_file))

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
	
	print(rank_order[['slope']])
	cat(paste0(rank_order[['slope']], collapse='<'))

	xlab <- c('Days_SFP'='Days since first reported positive\ntest result or March 4, whichever is later')

	# Plot
	for(ranked_by in c('slope', 'intercept')[1]){

		for(x_val in c('Days_SFP')){
		
			# Set which states to plot
			states_plot_low <- head(rank_order[[ranked_by]], num_rank)
			states_plot_high <- tail(rank_order[[ranked_by]], num_rank)
			
			# States to plot
			states_plot <- c(states_plot_low, states_plot_high)
			
			# Set state colors
			state_cols_a <- setNames(rev(rainbow(length(states_plot), start=0, end=0.8, alpha=0.7)), states_plot)
			state_cols <- setNames(rev(rainbow(length(states_plot), start=0, end=0.8, alpha=1)), states_plot)

			pch_set <- rep(1:num_rank, 55)
			state_pch <- setNames(pch_set[1:length(states_plot)], states_plot)

			# Open PDF
			pdf(paste0('../Plots/Positive vs ', x_val, ' ranked_by=', ranked_by, '/', file_date, '.pdf'), height=5.5, width=10.5)
			
			layout(matrix(c(1,1,2:3), 2, 2, byrow=TRUE), width=c(1.1, 1), height=c(0.07, 1))

			par('mar'=c(1,1,1,1))
			
			plot(x=c(0,1), y=c(0,1), type='n', bty='n', ylab='', xlab='', xaxt='n', yaxt='n')

			# Write main plot title
			main <- paste0(num_spelled[num_rank], ' states with lowest and highest growth curve slopes, respectively, as of ', 
				format(read_csv$AsDate[1], format="%B %d, %Y"))
			text(x=0.5, y=-0.5, labels=main, font=2, cex=1.3, xpd=TRUE)

			for(y_val in c('Positive', 'Positive_log')[1:2]){

				mar <- c(6,4.5,0.5,5.5)
				if(y_val == 'Positive'){
					par('mar'=mar)
				}else{
					par('mar'=c(mar[1:3], 1))
				}
			
				# Set variable ranges
				y_range <- range(y_ranges[[y_val]][states_plot, ], na.rm=TRUE)
				x_range <- range(x_ranges[[x_val]][states_plot, ], na.rm=TRUE)
			
				if(y_val == 'Positive'){
					line_lab_shift <- c(0.05*diff(x_range), 0.01*diff(y_range))
					x_range <- c(1, 1.07)*x_range
				}else{
					line_lab_shift <- 0.03*c(diff(x_range), diff(y_range))
					x_range <- c(1, 1.04)*x_range
				}

				#
				y_range <- c(1, 1.1)*y_range
	
				# Adjust default plot parameters
				if(y_val == 'Positive_log' && x_val == 'Days_SFP') x_range <- c(0, x_range[2])

				# Create plot
				plot(x_range, y_range, type='n', xlab='', xaxt='n', ylab='', yaxt='n')
			
				mtext(text=xlab[x_val], side=1, line=3)
				mtext(text='Number of reported positive COVID-19 tests', side=2, line=2.5)

				axis(1, mgp=c(3, 0.7, 0))
	
				# For each state
				for(state in states_plot){
					
					# Set xy points
					xy <- df_by_state[[state]][, c(x_val, y_val)]
	
					# Plot points as line
					points(x=xy, pch=state_pch[state], col=state_cols_a[state])
					points(x=xy, type='l', lwd=2, col=state_cols_a[state])
				}

				for(state in states_plot){

					# Set xy points
					xy <- df_by_state[[state]][, c(x_val, y_val)]

					# Add state abbreviation to end
					text(x=xy[1, 1] + line_lab_shift[1], y=xy[1, 2] + line_lab_shift[2], labels=state, col=state_cols[state])
				}
	
				# Compose upper left plot note
				upp_left_note <- 'Values not log-transformed'
				if(y_val == 'Positive_log') upp_left_note <- 'Values log-transformed'

				# Add note to upper left of plot
				text(x=x_range[1] - 0.03*diff(x_range), y=y_range[2], labels=upp_left_note, pos=4, cex=1.2, font=2)
				
				if(y_val == 'Positive'){

					# Add legend
					legend(x=x_range[2] + 0.1*diff(x_range), y=y_range[2], xpd=TRUE, 
						legend=c(states_plot_low, '...', states_plot_high), 
						col=c(state_cols[states_plot_low], NA, state_cols[states_plot_high]), 
						pch=c(state_pch[states_plot_low], NA, state_pch[states_plot_high]), lty=1, lwd=1.5, bty='n')

					# Add axis
					axis(2, mgp=c(3, 0.7, 0))
				}

				if(y_val == 'Positive_log'){

					# Add source
					text(x=x_range[2] + 0.05*diff(x_range), y=y_range[1] - 0.27*diff(y_range), 
						labels='Source data: covidtracking.com', pos=2, xpd=TRUE, cex=1.2, col=gray(0.25))
					#mtext(text='Source: covidtracking.com', side=1, line=4, adj=1., xpd=TRUE)

					# Create log axis
					log_range2 <- nchar(round(10^y_range[2]))
					axis_at <- seq(0, log_range2, length=log_range2+1)
					axis(side=2, at=axis_at, labels=round(10^axis_at), mgp=c(3, 0.7, 0))
				}
			}

			dev.off()
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
	pdf(paste0('../Plots/By state/RI/', file_date, '.pdf'), height=6, width=8)
	
	#
	par('mar'=c(5,5,2,2))

	# Create plot
	plot(x_range, y_range, type='n', xlab=xlab[x_val], ylab='Number of reported positive COVID-19 test results', yaxt=yaxt)
	
	# Set color for states
	cols <- setNames(rainbow(length(states_unique), alpha=0.3), states_unique)

	# For each state
	for(state in states_unique){
	
		if(state == state_highlight) next

		# Plot points as line
		points(x=df_by_state[[state]][, x_val], y=df_by_state[[state]][, y_val], type='l', lwd=1, col=cols[state])
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
	pdf(paste0('../Plots/', y_val, ' vs ', x_val, ' All states/', file_date, '.pdf'), height=30, width=40)
	
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