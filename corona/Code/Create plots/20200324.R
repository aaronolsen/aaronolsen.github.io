if(!is.null(dev.list())) dev.off()

run <- function(){

	## * Add code to add 0 negative for case where only positives and no negative value given

	mar_rank <- c(5,5)		# Number of states to include at slowest and fastest of ranking
	rank_mid <- 5			# Number of middle ranking states to show
	min_val_lm <- 5			# Minimum number of positives with which to fit regression
	min_num_val_lm <- 6		# Minimum number of days to include in regression
	log_base <- 10			# Base to use for log function
	start_cases <- 5		# Number of cases when to align curves
	num_days_in_rate <- 10	# Number of days to include in fit for projection
	num_days_proj <- 2		# Number of days to project in advance of furthest day

	# Written-out numbers for plot titles
	num_spelled <- c('One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine', 'Ten')

	# Get names of data files
	csv_files <- list.files('../../Data/', pattern='[0-9]+[.]csv')

	# Find most recent file
	csv_file <- tail(sort(csv_files), 1)
	
	# Get file date
	file_date <- gsub('[.]csv', '', csv_file)

	# Read data
	read_csv <- read.csv(file=paste0('../../Data/', csv_file))

	# Remove NA rows
	read_csv <- read_csv[!is.na(read_csv[, 'date']), ]

	# Format dates
	read_csv$asdate <- as.Date(as.character(read_csv$date), "%Y%m%d")

	# Remove rows without negative
	read_csv <- read_csv[!is.na(read_csv$negative), ]

	# Get unique list of states
	states_unique <- as.character(sort(unique(read_csv[, 'state'])))

	# Create various structures
	df_by_state <- list()
	first_reported <- setNames(rep(NA, length(states_unique)), states_unique)
	first_positive <- setNames(rep(NA, length(states_unique)), states_unique)
	y_ranges <- list(
		'positive'=matrix(NA, length(states_unique), 2, dimnames=list(states_unique, c('min', 'max'))),
		'positive_log'=matrix(NA, length(states_unique), 2, dimnames=list(states_unique, c('min', 'max')))
	)
	x_ranges <- list(
		'Days_SFP'=matrix(NA, length(states_unique), 2, dimnames=list(states_unique, c('min', 'max'))),
		'Days_SSD'=matrix(NA, length(states_unique), 2, dimnames=list(states_unique, c('min', 'max'))),
		'Days_SSD_log'=matrix(NA, length(states_unique), 2, dimnames=list(states_unique, c('min', 'max')))
	)
	
	# Create testing stats matrix
	col_names <- c('pop_size', 'num_positive', 'total_tests', 'per_positive', 'per_negative', 'per_tested', 'testing_score', 'nfold_pday')
	testing_stats <- matrix(NA, nrow=length(states_unique), ncol=length(col_names), dimnames=list(states_unique, col_names))

	# Read state pop sizes
	read_pops <- read.csv('../../Data/us_state_pop_sizes.csv')
	
	# Convert population size df to vector
	pop_sizes <- setNames(as.numeric(gsub(',', '', read_pops[, 'est_pop'])), read_pops[, 'abbr'])

	# Add population size of each state
	for(state in names(pop_sizes)[names(pop_sizes) %in% rownames(testing_stats)]){
		testing_stats[state, 'pop_size'] <- pop_sizes[state]
	}

	# Create regression stats matrix
	col_names <- c('slope', 'intercept', 'nfold_pday', 'dbl_days', 'start_date')
	log_sfp_lm_mat <- matrix(NA, length(states_unique), length(col_names), dimnames=list(states_unique, col_names))
	col_names <- c('slope', 'intercept', 'nfold_pday')
	log_rate_lm_mat <- matrix(NA, length(states_unique), length(col_names), dimnames=list(states_unique, col_names))

	# For each state
	for(state in states_unique){

		# Save dataframe for state
		df_by_state[[state]] <- read_csv[read_csv[, 'state'] == state, ]
		
		# Get first reported date
		first_reported[state] <- sort(df_by_state[[state]][, 'asdate'])[1]

		# Get positive values as numeric
		positive_vals <- as.numeric(gsub(',', '', as.character(df_by_state[[state]][, 'positive'])))

		# Save positive values
		df_by_state[[state]][, 'positive'] <- positive_vals

		# Get negative values as numeric
		negative_vals <- as.numeric(gsub(',', '', as.character(df_by_state[[state]][, 'negative'])))

		# Save negative values
		df_by_state[[state]][, 'negative'] <- negative_vals

		# Get first positive reported date
		first_positive[state] <- sort(df_by_state[[state]][positive_vals > 0, 'asdate'])[1]
		
		# Get days since first reported
		df_by_state[[state]][, 'Days_SFP'] <- as.numeric(df_by_state[[state]][, 'asdate'] - first_positive[state])

		# Get days since first positive
		df_by_state[[state]][, 'Days_SFR'] <- as.numeric(df_by_state[[state]][, 'asdate'] - first_reported[state])
		
		# Get log positive
		df_by_state[[state]][, 'positive_log'] <- NA

		# Get positive values not NA and over 0
		which_positive <- rep(FALSE, length(positive_vals))
		which_positive[is.na(positive_vals)] <- FALSE
		which_positive[!is.na(positive_vals)] <- positive_vals[!is.na(positive_vals)] > 0

		# Skip if not any positives reported yet
		if(!any(which_positive)) next

		# Get log values
		df_by_state[[state]][which_positive, 'positive_log'] <- log(positive_vals[which_positive], base=log_base)
		
		# Find when values are greater than 1
		which_over_min <- rep(FALSE, length(positive_vals))
		which_over_min[is.na(positive_vals)] <- FALSE
		which_over_min[!is.na(positive_vals)] <- positive_vals[!is.na(positive_vals)] > min_val_lm
		
		# Fit linear regression to log values
		if(sum(which_over_min) >= min_num_val_lm){
		
			# Set data list
			dlist <- list(y=df_by_state[[state]][which_over_min, 'positive_log'], x=df_by_state[[state]][which_over_min, 'Days_SFP'])
			lm_fit_log_sfp <- lm(y ~ x, data=dlist)
			
			# Save regression stats
			log_sfp_lm_mat[state, 'intercept'] <- lm_fit_log_sfp$coefficients['(Intercept)']
			log_sfp_lm_mat[state, 'slope'] <- lm_fit_log_sfp$coefficients['x']
			
			# Calculate number of days for positives to double
			log_sfp_lm_mat[state, 'dbl_days'] <- log(2, base=log_base) / log_sfp_lm_mat[state, 'slope']
			
			# Calculate n-fold increase per day (see Interpret slope.R for validation)
			log_sfp_lm_mat[state, 'nfold_pday'] <- log_base ^ log_sfp_lm_mat[state, 'slope']

			# Find days since start_cases
			dlist <- list(y=df_by_state[[state]][which_over_min, 'positive_log'], x=df_by_state[[state]][which_over_min, 'asdate'])
			lm_fit_log_asdate <- lm(y ~ x, data=dlist)
			start_day_int <- -(lm_fit_log_asdate$coefficients['(Intercept)'] - log(start_cases, base=log_base)) / (lm_fit_log_asdate$coefficients['x'])
			start_date <- as.Date(start_day_int, origin='1970-01-01')

			# Set day corresponding to start_cases
			log_sfp_lm_mat[state, 'start_date'] <- start_date
			
			# Get days since start date
			df_by_state[[state]][, 'Days_SSD'] <- as.numeric(df_by_state[[state]][, 'asdate'] - start_date)
			df_by_state[[state]][, 'Days_SSD_log'] <- suppressWarnings(log(df_by_state[[state]][, 'Days_SSD'], base=log_base))
			x_ranges[['Days_SSD']][state, ] <- range(df_by_state[[state]][, 'Days_SSD'], na.rm=TRUE)
			x_ranges[['Days_SSD_log']][state, ] <- range(df_by_state[[state]][, 'Days_SSD_log'], na.rm=TRUE)
			
			if(state == 'MI' && FALSE){
				
				print(df_by_state[[state]])
				
				pdf('~/Documents/Outreach/Coronavirus analysis/Plots/align curves.pdf')
				
				y_range <- range(log(df_by_state[[state]][which_over_min, 'positive'], base=log_base), na.rm=TRUE)
				x_range <- range(as.numeric(dlist$x), na.rm=TRUE)

				plot(x_range, y_range, type='n')

				points(as.numeric(dlist$x), log(df_by_state[[state]][which_over_min, 'positive'], base=log_base))

				abline(a=lm_fit_log_asdate$coefficients['(Intercept)'], b=lm_fit_log_asdate$coefficients['x'], lty=2)
				
				dev.off()
				
				return(1)
			}
			
			# Get rate
			
			# Get indices to include
			ind_over_min <- c(1:length(which_over_min))[which_over_min]
			
			# Shorten to the most recent number of days to include in calculating rate
			if(length(ind_over_min) > num_days_in_rate) ind_over_min <- head(ind_over_min, num_days_in_rate)
			
			# Fit regression to more recent data to get rate
			dlist <- list(y=df_by_state[[state]][ind_over_min, 'positive_log'], x=df_by_state[[state]][ind_over_min, 'Days_SFP'])
			lm_fit_log_rate <- lm(y ~ x, data=dlist)

			# Save intercept and slope for rate
			log_rate_lm_mat[state, 'intercept'] <- lm_fit_log_rate$coefficients['(Intercept)']
			log_rate_lm_mat[state, 'slope'] <- lm_fit_log_rate$coefficients['x']
			log_rate_lm_mat[state, 'nfold_pday'] <- log_base ^ log_rate_lm_mat[state, 'slope']
			
			if(state == 'MI'){
			}

		}else{
		
			df_by_state[[state]][, 'Days_SSD'] <- rep(NA, nrow(df_by_state[[state]]))
			df_by_state[[state]][, 'Days_SSD_log'] <- rep(NA, nrow(df_by_state[[state]]))
			#print('Not enough values for regression')
		}

		# Get y ranges
		y_ranges[['positive']][state, ] <- range(positive_vals, na.rm=TRUE)
		y_ranges[['positive_log']][state, ] <- range(df_by_state[[state]][which_positive, 'positive_log'], na.rm=TRUE)

		# Get x ranges
		x_ranges[['Days_SFP']][state, ] <- range(df_by_state[[state]][, 'Days_SFP'], na.rm=TRUE)
		
		# Save testing statistics
		testing_stats[state, 'num_positive'] <- df_by_state[[state]][1, 'positive']
		testing_stats[state, 'total_tests'] <- as.numeric(as.character(gsub(',','',df_by_state[[state]][1, 'total'])))
		testing_stats[state, 'per_positive'] <- 100*(df_by_state[[state]][1, 'positive'] / testing_stats[state, 'total_tests'])
		testing_stats[state, 'per_negative'] <- 100*(df_by_state[[state]][1, 'negative'] / testing_stats[state, 'total_tests'])
		testing_stats[state, 'per_tested'] <- 100*(testing_stats[state, 'total_tests'] / testing_stats[state, 'pop_size'])
	}
	
	# Calculate testing score (higher is better) and add to matrix
	#testing_stats[, 'testing_score'] <- testing_stats[, 'per_tested']*(1/testing_stats[, 'per_positive'])
	testing_stats[, 'testing_score'] <- testing_stats[, 'per_tested']*testing_stats[, 'per_negative']

	# Normalize testing scores from 0 to 1
	#testing_stats[, 'testing_score'] <- (testing_stats[, 'testing_score'] - min(testing_stats[, 'testing_score'], na.rm=TRUE)) / 
	#	diff(range(testing_stats[, 'testing_score'], na.rm=TRUE))

	# Add nfold_pday to testing stats matrix
	testing_stats[rownames(log_rate_lm_mat), 'nfold_pday'] <- log_rate_lm_mat[, 'nfold_pday']

	# Remove NA rows
	log_sfp_lm_mat_nna <- log_sfp_lm_mat[!is.na(log_sfp_lm_mat[, 1]), ]
	log_rate_lm_mat_nna <- log_rate_lm_mat[!is.na(log_rate_lm_mat[, 1]), ]

	# Rank states by each variable
	rank_order <- list(
		'slope'=rownames(log_rate_lm_mat_nna)[order(log_rate_lm_mat_nna[, 'slope'])],
#		'slope'=rownames(log_sfp_lm_mat_nna)[order(log_sfp_lm_mat_nna[, 'slope'])],
#		'intercept'=rownames(log_sfp_lm_mat_nna)[order(log_sfp_lm_mat_nna[, 'intercept'])],
		'nfold_pday'=rownames(log_rate_lm_mat_nna)[order(log_rate_lm_mat_nna[, 'nfold_pday'])]
	)

	# Set ranked by variable
	ranked_by <- 'slope'
	
	# Set rank numbers for states to include
	mid_ranks <- round(seq(mar_rank[1], length(rank_order[[ranked_by]])-mar_rank[2], length=rank_mid+2)[2:(rank_mid+1)])
	states_plot_rank <- c(1:mar_rank[1], mid_ranks, (length(rank_order[[ranked_by]])-mar_rank[2]+1):length(rank_order[[ranked_by]]))
	
	# Set states to plot
	states_plot <- rank_order[[ranked_by]][states_plot_rank]

	# Print out string of states in order
	#cat(paste0(rank_order[['slope']], collapse='<'))

	xlab <- c(
		'Days_SFP'='Days since first reported positive\ntest result or March 4, whichever is later', 
		'Days_SSD'=paste0('Days since ', start_cases, ' reported positive cases'),
		'Days_SSD_log'=paste0('Days since ', start_cases, ' reported positive cases, log-transformed')
	)

	# Set x_val
	x_val <- 'Days_SSD_log' #'Days_SFP'
	y_val <- 'positive_log'

	# Set state colors
	state_cols_a <- setNames(rev(rainbow(length(states_plot), start=0, end=0.8, alpha=0.7, v=0.8)), states_plot)
	state_cols_a2 <- setNames(rev(rainbow(length(states_plot), start=0, end=0.8, alpha=0.4, v=0.8)), states_plot)
	state_cols <- setNames(rev(rainbow(length(states_plot), start=0, end=0.8, alpha=1, v=0.8)), states_plot)

	pch_set <- rep(1:5, 55)
	state_pch <- setNames(pch_set[1:length(states_plot)], states_plot)

	# Open PDF
	height <- 11.5
	w_ratio <- 1.4
	mtext_cex <- 0.8
	pdf(paste0('~/Documents/Outreach/Coronavirus analysis/Plots/positive vs ', x_val, '/', file_date, ' positive vs ', x_val, '.pdf'), height=height, width=9.24)
	
	layout(matrix(c(1,1,2,2,3,3,4,4,5,6), 5, 2, byrow=TRUE), width=c(1, 1), height=c(0.08, 1, 0.5, 0.08, 1))

	par('mar'=c(0,0,0,0))
	plot(x=c(0,1), y=c(0,1), type='n', bty='n', ylab='', xlab='', xaxt='n', yaxt='n')

	# Write main plot title
	main <- paste0('Changes in reported positive COVID19 tests (ranked) for select states as of ', format(read_csv$asdate[1], format="%B %d, %Y"))
	text(x=0.5, y=0.5, labels=main, font=2, cex=1.5*mtext_cex, xpd=TRUE)

	# Set margins
	mar <- c(3,5,0,2)
	par('mar'=mar)

	# Set variable ranges
	y_range <- range(y_ranges[[y_val]][states_plot, ], na.rm=TRUE)
	x_range <- range(x_ranges[[x_val]][states_plot, ], na.rm=TRUE)

	# Set line label shift
	line_lab_shift <- 0.007*c(diff(x_range), diff(y_range))
	
	# Adjust x-range
	x_range <- c(-0.2, 1.04)*x_range

	# Adjust y-range to fit extra text
	y_range[1] <- log(2*start_cases, base=log_base)
	y_range <- c(1, 1.0)*y_range

	# Adjust default plot parameters
	if(x_val == 'Days_SSD') y_range[1] <- 0.9*log(start_cases, base=log_base)

	# Create plot
	plot(x_range, y_range, type='n', xlab='', xaxt='n', ylab='', yaxt='n')

	# Add xy axis labels
	mtext(text=xlab[x_val], side=1, line=2, cex=mtext_cex)
	mtext(text='Number of reported positive COVID19 tests, log-transformed', side=2, line=2.5, cex=mtext_cex)

	# Add y background lines
	h_ats <- 1:7
	for(h_at in h_ats) abline(h=h_at, lty=2, col=gray(0.8))

	# Add x-axis ticks
	axis_at <- seq(x_range[1], x_range[2], length=12)
	axis_at_labels <- round(10^axis_at)
	
	# Remove duplicates
	for(i in 2:length(axis_at_labels)){
		if(axis_at_labels[i] %in% axis_at_labels[1:(i-1)]){
			axis_at_labels[i] <- NA
			axis_at[i] <- NA
		}
	}
	axis_at <- axis_at[!is.na(axis_at)]
	axis_at_labels <- axis_at_labels[!is.na(axis_at_labels)]

	axis(1, at=axis_at, labels=round(10^axis_at), mgp=c(3, 0.5, 0))

	# For each state
	for(state in states_plot){
		
		# Set xy points
		xy <- df_by_state[[state]][, c(x_val, y_val)]
		
		# Plot points as line
		points(x=xy, pch=state_pch[state], col=state_cols_a[state])
		points(x=xy, type='l', lwd=2, col=state_cols_a[state])

		# Add state abbreviation to end
		text(x=xy[1, 1] + line_lab_shift[1], y=xy[1, 2] + line_lab_shift[2], labels=state, col=state_cols[state])
	}
	
	# Add log tick labels to y-axis
	log_range2 <- nchar(round(10^y_range[2]))
	axis_at <- seq(0, log_range2, length=log_range2+1)
	options(scipen=999)
	axis(side=2, at=axis_at, labels=gsub(' ', '', format(round(10^axis_at), nsmall=0, big.mark=",")), mgp=c(3, 0.7, 0))

	# Write legend labels
	legend_labels <- paste0(states_plot, ' (#', states_plot_rank, ')')
	
	# Add ... and NA between non-consecutive labels/colors/symbols
	legend_labels_new <- c()
	states_plot_rank_new <- c()
	state_cols_new <- c()
	state_pch_new <- c()
	for(i in 1:(length(legend_labels))){
	
		legend_labels_new <- c(legend_labels_new, legend_labels[i])
		states_plot_rank_new <- c(states_plot_rank_new, states_plot_rank[i])
		state_cols_new <- c(state_cols_new, state_cols[i])
		state_pch_new <- c(state_pch_new, state_pch[i])

		if(i < length(legend_labels) && states_plot_rank[i+1] - states_plot_rank[i] > 1){
			legend_labels_new <- c(legend_labels_new, '...')
			states_plot_rank_new <- c(states_plot_rank_new, NA)
			state_cols_new <- c(state_cols_new, NA)
			state_pch_new <- c(state_pch_new, NA)
		}
	}

	# Replace with new
	legend_labels <- legend_labels_new
	states_plot_rank <- states_plot_rank_new
	state_cols <- state_cols_new
	state_pch <- state_pch_new
	
	# Add notes to first and last legend labels
	legend_labels[1] <- paste0(legend_labels[1], ', slowest')
	legend_labels[length(legend_labels)] <- paste0(legend_labels[length(legend_labels)], ', fastest')

	# Add legend to plot
	legend(x=x_range[1] - 0.01*diff(x_range), y=y_range[2] + 0.01*diff(y_range), xpd=TRUE, 
		legend=rev(legend_labels), 
		col=rev(state_cols), 
		pch=rev(state_pch), lty=1, lwd=1.5, bty='n')

	# Add note about rate calculation to bottom right of plot
	text(x=0.9*x_range[2], y=y_range[1], labels=paste0('(Rate rankings determined based on changes\nin reported cases over the last ', num_days_in_rate, ' days)'), 
		cex=1.2*mtext_cex, xpd=TRUE, pos=3)


	## Create bar plot of rates
	# Set regression var
	y_lm_var <- 'nfold_pday'

	# Set order in which to plot states
	order_plot <- rank_order[[y_lm_var]]
	if(y_lm_var == 'nfold_pday') order_plot <- rev(order_plot)

	# Add states with NA regression values
	#order_plot <- c(order_plot, rownames(log_sfp_lm_mat)[is.na(log_sfp_lm_mat[,1])])

	# Set x sequence
	x_seq <- 1:length(order_plot)

	y_range <- range(log_rate_lm_mat[, y_lm_var], na.rm=TRUE)
	y_range <- c(0.95, 1.0)*y_range
	x_range <- range(x_seq)
	x_range <- c(2, 0.98)*x_range

	# Set bar width
	bar_width <- 0.9*diff(x_seq[1:2])

	# Set amount to shift text above and below bar
	num_up_shift <- 0.02*diff(y_range)
	text_up_shift <- 0.08*diff(y_range)
	text_down_shift <- 0.02*diff(y_range)
	
	# Set colors for each state proportional to rate
	gray_scale <- 1 - (0.9*((log_rate_lm_mat[order_plot, y_lm_var] - y_range[1]) / diff(y_range)) + 0.1)
	cols_gray <- setNames(gray(gray_scale), order_plot)

	par('mar'=c(2, mar[2], 2, mar[4]))

	plot(x_range, y_range, type='n', bty='n', xaxt='n', xlab='', ylab='', yaxt='n', main='')

	n <- 1
	for(state in order_plot){

		# Set bar vertices
		bar_verts <- rbind(
			c(x_seq[n]-bar_width/2, 0),
			c(x_seq[n]-bar_width/2, log_rate_lm_mat[state, y_lm_var]),
			c(x_seq[n]+bar_width/2, log_rate_lm_mat[state, y_lm_var]),
			c(x_seq[n]+bar_width/2, 0)
		)

		# Plot polygon
		polygon(x=bar_verts, col=cols_gray[state], border=NA)

		# Add rate as number
		text(x=x_seq[n], y=log_rate_lm_mat[state, y_lm_var] + text_up_shift, 
			labels=paste0(round(log_rate_lm_mat[state, y_lm_var], 2)), xpd=TRUE, srt=90, cex=mtext_cex)

		# Add state label
		text(x=x_seq[n], y=y_range[1]-text_down_shift, labels=state, pos=1, cex=0.8, xpd=TRUE)
		
		# Set inside bar text color
		inbar_col <- 'black'
		if(gray_scale[state] < 0.6) inbar_col <- 'white'

		# Add number label
		text(x=x_seq[n], y=y_range[1]+num_up_shift, labels=n, cex=0.85, xpd=TRUE, col=inbar_col, font=2)

		n <- n + 1
	}

	# Add y-axis label
	mtext(text='n-fold increase in reported\npositive tests per day', side=2, line=2.5, cex=mtext_cex)

	# Add plot title
	mtext(side=3, paste0('n-fold increase in reported positive COVID19 tests per day by state as of ', 
		format(read_csv$asdate[1], format="%B %d, %Y")), font=2, cex=mtext_cex, xpd=TRUE)
	mtext(side=3, paste0('(Rate determined based on changes in reported cases over the last ', num_days_in_rate, ' days)'), line=-1.2, cex=0.9*mtext_cex, xpd=TRUE)

	#
	axis_at <- seq(y_range[1], y_range[2], length=4)
	axis(side=2, at=axis_at, labels=paste0(round(axis_at, 1), 'x'), mgp=c(3, 0.7, 0))


	## Create middle panel figure title
	par('mar'=c(0,0,0,0))
	plot(x=c(0,1), y=c(0,1), type='n', bty='n', ylab='', xlab='', xaxt='n', yaxt='n')

	# Write main plot title
	text(x=0.5, y=0.5, labels=paste0('Testing performance for COVID19 by state as of ', 
		format(read_csv$asdate[1], format="%B %d, %Y")), font=2, cex=1.5*mtext_cex, xpd=TRUE)


	## Plot percent positive vs testing per capita
	#par('mar'=c(5,4,3,2))
	par('mar'=c(5, mar[2], mar[3], 2))

	# Set which state labels to move outside points
	labels_out <- c(
		'CA'='upper',
		'CT'='upper',
		'FL'='right',
		'GA'='lower right',
		'ID'='left',
		'IN'='right',
		'MA'='lower right',
		'MS'='upper',
		'MT'='upper',
		'MN'='lower',
		'NC'='lower',
		'OR'='upper',
		'RI'='left',
		'SC'='upper left',
		'UT'='lower'
	)
	label_offset <- c(0.01, 0.5)
	
	# Set size of circle with text
	circle_txt_cex <- 3.1
	txt_cex <- 0.71
	
	y_range <- range(testing_stats[, 'per_positive'], na.rm=TRUE)
	x_range <- range(testing_stats[, 'per_tested'], na.rm=TRUE)
	y_range <- c(1, 1)*y_range

	# Open plot
	plot(x_range, y_range, type='n', xlab='', ylab='', xaxt='n', yaxt='n', log='xy')

	# Set background text color and size
	bg_text_cex <- 0.9
	bg_txt_col <- gray(0.7)

	# Add background quadrant labels
	text(x=0.015, y=2, labels=toupper('Lower testing rate\nLower % positive'), col=bg_txt_col, cex=bg_text_cex)
	text(x=0.13, y=32, labels=toupper('Higher testing rate\nHigher % positive'), col=bg_txt_col, cex=bg_text_cex)
	text(x=0.015, y=24, labels=toupper('Lower testing rate\nHigher % positive'), col=bg_txt_col, cex=bg_text_cex)
	text(x=0.13, y=1.55, labels=toupper('Higher testing rate\nLower % positive'), col=bg_txt_col, cex=bg_text_cex)

	# Add horizontal background line and label
	h_at <- 5
	abline(h=5, lty=2, col=bg_txt_col, lwd=2)
	text(x=h_at*0.01, y=y_range[2], labels=paste0(h_at*0.01, '% tested'), cex=1.1*bg_text_cex, xpd=TRUE, adj=c(0.85,-0.5), srt=90, col=bg_txt_col)

	# Add vertical background line and label
	v_at <- 0.05
	abline(v=v_at, lty=2, col=bg_txt_col, lwd=2)
	text(x=x_range[1], y=v_at*100, labels=paste0(v_at*100, '% positive'), cex=1.1*bg_text_cex, xpd=TRUE, adj=c(0.2,-0.5), col=bg_txt_col)
	
	# Set line label length
	line_lab_len <- 0.04*c(1, diff(y_range) / diff(x_range))

	for(state in rownames(testing_stats)){
	
		xy <- testing_stats[state, c('per_tested', 'per_positive')]

		if(state %in% names(labels_out)){
		
			# Add point
			points(x=xy[1], y=xy[2], cex=0.7)

			# Set text line label lengths
			line_len_state <- c((line_lab_len[1]*xy[1]) / diff(x_range), (line_lab_len[2]*xy[2]) / diff(y_range))
			
			# Set line label sign
			if(grepl('left', labels_out[state])) line_len_state[1] <- -line_len_state[1]
			if(grepl('lower', labels_out[state])) line_len_state[2] <- -line_len_state[2]
			if(!grepl('left|right', labels_out[state])){ line_len_state[1] <- 0 ; line_len_state[2] <- sqrt(2)*line_len_state[2]}
			if(!grepl('lower|upper', labels_out[state])){ line_len_state[2] <- 0 ; line_len_state[1] <- sqrt(2)*line_len_state[1]}

			if(labels_out[state] == 'lower left') text_adj <- c(1, 0.7)
			if(labels_out[state] == 'lower right') text_adj <- c(-0.2,0.5)
			if(labels_out[state] == 'left') text_adj <- c(1, 0.5)
			if(labels_out[state] == 'right') text_adj <- c(-0.2, 0.5)
			if(labels_out[state] == 'upper left') text_adj <- c(1,-0.5)
			if(labels_out[state] == 'upper right') text_adj <- c(-0.2,-0.5)
			if(labels_out[state] == 'upper') text_adj <- c(0.5,-0.5)
			if(labels_out[state] == 'lower') text_adj <- c(0.5,1)
			
			# Set end of line
			line_end <- c(xy[1]+line_len_state[1], xy[2]+line_len_state[2])

			# Draw line
			segments(x0=xy[1], y0=xy[2], x1=line_end[1], y1=line_end[2])

			# Add text labels inside circle
			text(x=line_end[1], y=line_end[2], labels=state, cex=txt_cex, adj=text_adj)

		}else{

			# Add circle
			points(x=xy[1], y=xy[2], cex=circle_txt_cex)
		
			# Add text labels inside circle
			text(x=xy[1], y=xy[2], labels=state, cex=txt_cex)
		}
	}

	# Add axis labels
	mtext(side=1, text='Number of tests relative to state population (%), log-transformed', line=2.5, cex=mtext_cex)
	mtext(side=2, text='Percent of all tests reported \'Positive\' (%), log-transformed', line=2.5, cex=mtext_cex)
	
	# Add axis ticks
	axis(1, mgp=c(3, 0.7, 0))
	axis(2, mgp=c(3, 0.7, 0))

	## Plot n-fold increase in reported positives vs testing score
	# Set which state labels to move outside points
	labels_out <- c(
		'AK'='upper right',
		'CO'='right',
		'CT'='right',
		'GU'='lower left',
		'HI'='right',
		'ID'='lower',
		'NH'='lower',
		'ND'='lower',
		'OR'='lower',
		'SC'='lower',
		'TX'='lower',
		'UT'='right',
		'VA'='lower',
		'WI'='lower'
	)
	label_offset <- c(1,0.05)

	# Get ranges
	y_range <- range(testing_stats[!is.na(testing_stats[, 'nfold_pday']), 'nfold_pday'], na.rm=TRUE)
	x_range <- range(testing_stats[!is.na(testing_stats[, 'nfold_pday']), 'testing_score'], na.rm=TRUE)

	# Create plot
	plot(x_range, y_range, type='n', xlab='', ylab='', log='x', xaxt='n', yaxt='n')
	
	# Set line label length
	line_lab_len <- 0.6*c(10, 0.025)

	for(state in rownames(testing_stats)){

		xy <- testing_stats[state, c('testing_score', 'nfold_pday')]

		if(state %in% names(labels_out)){
			
			# Add point
			points(x=xy[1], y=xy[2], cex=0.7)
			
			# Set text line label lengths
			line_len_state <- c((line_lab_len[1]*xy[1]) / diff(x_range), line_lab_len[2])
			
			# Set line label sign
			if(grepl('left', labels_out[state])) line_len_state[1] <- -line_len_state[1]
			if(grepl('lower', labels_out[state])) line_len_state[2] <- -line_len_state[2]
			if(!grepl('left|right', labels_out[state])){ line_len_state[1] <- 0 ; line_len_state[2] <- sqrt(2)*line_len_state[2]}
			if(!grepl('lower|upper', labels_out[state])){ line_len_state[2] <- 0 ; line_len_state[1] <- sqrt(2)*line_len_state[1]}

			if(labels_out[state] == 'lower left') text_adj <- c(1, 0.7)
			if(labels_out[state] == 'lower right') text_adj <- c(-0.2,0.5)
			if(labels_out[state] == 'left') text_adj <- c(1, 0.5)
			if(labels_out[state] == 'right') text_adj <- c(-0.2, 0.5)
			if(labels_out[state] == 'upper left') text_adj <- c(1,-0.5)
			if(labels_out[state] == 'upper right') text_adj <- c(-0.2,-0.5)
			if(labels_out[state] == 'upper') text_adj <- c(0.5,-0.5)
			if(labels_out[state] == 'lower') text_adj <- c(0.5,1)

			# Set end of line
			line_end <- c(xy[1]+line_len_state[1], xy[2]+line_len_state[2])

			# Draw line
			segments(x0=xy[1], y0=xy[2], x1=line_end[1], y1=line_end[2])

			# Add text labels inside circle
			text(x=line_end[1], y=line_end[2], labels=state, cex=txt_cex, adj=text_adj)

		}else{

			# Add circle
			points(x=xy[1], y=xy[2], cex=circle_txt_cex)
		
			# Add text labels inside circle
			text(x=xy[1], y=xy[2], labels=state, cex=txt_cex)
		}
	}

	# Add axis labels
	mtext(side=1, text='Testing score (% tested per pop x % negative), log-transformed', line=2, cex=mtext_cex)
	mtext(side=2, text='n-fold increase in reported positive tests per day', line=2.5, cex=mtext_cex)

	# Add axis ticks
	axis(1, mgp=c(3, 0.7, 0))
	axis(2, mgp=c(3, 0.7, 0))

	# Add background corner labels
	text(x=9, y=1.13, labels=toupper('Higher testing rate\nLower % positive\nSlower inc in % positive'), col=bg_txt_col, cex=bg_text_cex)
	text(x=0.3, y=1.58, labels=toupper('Lower testing rate\nHigher % positive\nFaster inc in % positive'), col=bg_txt_col, cex=bg_text_cex)

	# Add source
	text(x=x_range[2] + 1*diff(x_range), y=y_range[1] - 0.2*diff(y_range), 
		labels='Plot created by @aarolsen; Source data: covidtracking.com', pos=2, xpd=TRUE, cex=1.4*mtext_cex, col=gray(0.25))

	dev.off()

#print(head(testing_stats))
}

run()