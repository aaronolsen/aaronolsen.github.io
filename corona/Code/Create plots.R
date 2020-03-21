if(!is.null(dev.list())) dev.off()

run <- function(){

	num_rank <- 7		# The number of states to highlight as top slowest/fastest 
	min_val_lm <- 5		# Minimum number of positives with which to fit regression
	min_num_val_lm <- 6	# Minimum number of days to include in regression
	log_base <- 10		# Base to use for log function

	# Written-out numbers for plot titles
	num_spelled <- c('One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine', 'Ten')

	# Get names of data files
	csv_files <- list.files('../Data/', pattern='[0-9]+[.]csv')

	# Find most recent file
	csv_file <- tail(sort(csv_files), 1)
	
	# Get file date
	file_date <- gsub('[.]csv', '', csv_file)

	# Read data
	read_csv <- read.csv(file=paste0('../Data/', csv_file))

	# Remove NA rows
	read_csv <- read_csv[!is.na(read_csv[, 'Date']), ]

	# Format dates
	read_csv$AsDate <- as.Date(as.character(read_csv$Date), "%Y%m%d")

	# Get unique list of states
	states_unique <- as.character(sort(unique(read_csv[, 'State'])))

	# Create various structures
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
	
	# Create testing stats matrix
	col_names <- c('pop_size', 'num_positive', 'total_tests', 'per_positive', 'per_tested')
	testing_stats <- matrix(NA, nrow=length(states_unique), ncol=length(col_names), dimnames=list(states_unique, col_names))

	# Read state pop sizes
	read_pops <- read.csv('../Data/us_state_pop_sizes.csv')
	
	# Convert population size df to vector
	pop_sizes <- setNames(as.numeric(gsub(',', '', read_pops[, 'est_pop'])), read_pops[, 'abbr'])
	
	# Add population size of each state
	for(state in names(pop_sizes)) testing_stats[state, 'pop_size'] <- pop_sizes[state]

	# Create regression stats matrix
	col_names <- c('slope', 'intercept', 'nfold_pday', 'dbl_days')
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

		# Skip if not any positives reported yet
		if(!any(which_positive)) next

		# Get log values
		df_by_state[[state]][which_positive, 'Positive_log'] <- log(positive_vals[which_positive], base=log_base)
		
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
			
			# Calculate number of days for positives to double
			log_sfp_lm_mat[state, 'dbl_days'] <- log(2, base=log_base) / log_sfp_lm_mat[state, 'slope']
			
			# Calculate n-fold increase per day (see Interpret slope.R for validation)
			log_sfp_lm_mat[state, 'nfold_pday'] <- log_base ^ log_sfp_lm_mat[state, 'slope']
		}

		# Get y ranges
		y_ranges[['Positive']][state, ] <- range(positive_vals, na.rm=TRUE)
		y_ranges[['Positive_log']][state, ] <- range(df_by_state[[state]][which_positive, 'Positive_log'], na.rm=TRUE)

		# Get x ranges
		x_ranges[['Days_SFP']][state, ] <- range(df_by_state[[state]][, 'Days_SFP'], na.rm=TRUE)
		
		# Save testing statistics
		testing_stats[state, 'num_positive'] <- df_by_state[[state]][1, 'Positive']
		testing_stats[state, 'total_tests'] <- as.numeric(as.character(gsub(',','',df_by_state[[state]][1, 'Total'])))
		testing_stats[state, 'per_positive'] <- 100*(df_by_state[[state]][1, 'Positive'] / testing_stats[state, 'total_tests'])
		testing_stats[state, 'per_tested'] <- 100*(testing_stats[state, 'total_tests'] / testing_stats[state, 'pop_size'])
	}

	# Remove NA rows
	log_sfp_lm_mat_nna <- log_sfp_lm_mat[!is.na(log_sfp_lm_mat[, 1]), ]

	# Rank states by each variable
	rank_order <- list(
		'slope'=rownames(log_sfp_lm_mat_nna)[order(log_sfp_lm_mat_nna[, 'slope'])],
		'intercept'=rownames(log_sfp_lm_mat_nna)[order(log_sfp_lm_mat_nna[, 'intercept'])],
		'nfold_pday'=rownames(log_sfp_lm_mat_nna)[order(log_sfp_lm_mat_nna[, 'nfold_pday'])]
	)
	
	# Print out string of states in order
	#cat(paste0(rank_order[['slope']], collapse='<'))

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
			height <- 6.6
			w_ratio <- 1.4
			mtext_cex <- 0.8
			pdf(paste0('../Plots/Positive vs ', x_val, ' ranked_by=', ranked_by, '/', file_date, ' Positive vs ', x_val, ' ranked_by=', ranked_by, '.pdf'), height=height, width=w_ratio*height)
			
			layout(matrix(c(1,1,2:3,4,4,5,5), 4, 2, byrow=TRUE), width=c(1, 1), height=c(0.1, 1, 0.1, 0.4))

			par('mar'=c(0,0,0,0))
			plot(x=c(0,1), y=c(0,1), type='n', bty='n', ylab='', xlab='', xaxt='n', yaxt='n')

			# Write main plot title
			main <- paste0('Growth curves for the ', tolower(num_spelled[num_rank]), 
				' states with the slowest and fastest growth rates\nin reported positive COVID-19 tests as of ', 
				format(read_csv$AsDate[1], format="%B %d, %Y"))
			text(x=0.5, y=0.5, labels=main, font=2, cex=1.5*mtext_cex, xpd=TRUE)

			for(y_val in c('Positive', 'Positive_log')[1:2]){

				# Set margins
				mar <- c(4,5.5,0.5,2)
				if(y_val == 'Positive'){
					par('mar'=mar)
				}else{
					par('mar'=c(mar[1], 3, mar[3], 2))
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

				# Adjust y-range to fit extra text
				y_range <- c(1, 1.1)*y_range
	
				# Adjust default plot parameters
				if(y_val == 'Positive_log' && x_val == 'Days_SFP') x_range <- c(0, x_range[2])

				# Create plot
				plot(x_range, y_range, type='n', xlab='', xaxt='n', ylab='', yaxt='n')
			
				# Add xy axis labels
				mtext(text=xlab[x_val], side=1, line=3, cex=mtext_cex)
				mtext(text='Number of reported positive COVID-19 tests', side=2, line=2.5, cex=mtext_cex)

				# Add x-axis ticks
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
				upp_left_note <- '(Values not log-transformed)'
				if(y_val == 'Positive_log') upp_left_note <- '(Values log-transformed)'

				# Add note to upper left of plot
				text(x=x_range[2] + 0.04*diff(x_range), y=y_range[2], labels=upp_left_note, pos=2, cex=1.4*mtext_cex, font=2)
				
				if(y_val == 'Positive'){
				
					# Write legend labels
					legend_labels <- c(paste0(states_plot_low, ' (#', length(rank_order[[ranked_by]]):(length(rank_order[[ranked_by]])-num_rank+1), ')'), 
						paste0('... (#', num_rank+1, '-#', length(rank_order[[ranked_by]])-num_rank, ')'), paste0(states_plot_high, ' (#', num_rank:1, ')'))
					
					legend_labels[1] <- paste0(legend_labels[1], ', slowest')
					legend_labels[length(legend_labels)] <- paste0(legend_labels[length(legend_labels)], ', fastest')

					# Add legend
					legend(x=x_range[1] + 0.02*diff(x_range), y=y_range[2] - 0.02*diff(y_range), xpd=TRUE, 
						legend=rev(legend_labels), 
						col=rev(c(state_cols[states_plot_low], NA, state_cols[states_plot_high])), 
						pch=rev(c(state_pch[states_plot_low], NA, state_pch[states_plot_high])), lty=1, lwd=1.5, bty='n')

					# Add axis
					axis(2, mgp=c(3, 0.7, 0))
				}

				if(y_val == 'Positive_log'){

					# Create log axis
					log_range2 <- nchar(round(10^y_range[2]))
					axis_at <- seq(0, log_range2, length=log_range2+1)
					axis(side=2, at=axis_at, labels=round(10^axis_at), mgp=c(3, 0.7, 0))
				}
			}

			## Create lower panel figure title
			par('mar'=c(0,0,0,0))
			plot(x=c(0,1), y=c(0,1), type='n', bty='n', ylab='', xlab='', xaxt='n', yaxt='n')

			# Write main plot title
			text(x=0.5, y=0.5, labels=paste0('n-fold increase in reported positive tests per day by state as of ', 
				format(read_csv$AsDate[1], format="%B %d, %Y")), font=2, cex=1.5*mtext_cex, xpd=TRUE)

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

			y_range <- range(log_sfp_lm_mat[, y_lm_var], na.rm=TRUE)
			y_range <- c(0.95, 1.02)*y_range
			x_range <- range(x_seq)
	
			# Set bar width
			bar_width <- 0.9*diff(x_seq[1:2])
	
			# Set amount to shift text above and below bar
			num_up_shift <- 0.02*diff(y_range)
			text_up_shift <- 0.06*diff(y_range)
			text_down_shift <- 0.02*diff(y_range)
			
			# Set colors for each state proportional to rate
			gray_scale <- 1 - (0.9*((log_sfp_lm_mat[order_plot, y_lm_var] - y_range[1]) / diff(y_range)) + 0.1)
			cols_gray <- setNames(gray(gray_scale), order_plot)
	
			par('mar'=c(3.5, mar[2], 0, 1))

			plot(x_range, y_range, type='n', bty='n', xaxt='n', xlab='', ylab='', yaxt='n')
	
			n <- 1
			for(state in order_plot){
		
				# Set bar vertices
				bar_verts <- rbind(
					c(x_seq[n]-bar_width/2, 0),
					c(x_seq[n]-bar_width/2, log_sfp_lm_mat[state, y_lm_var]),
					c(x_seq[n]+bar_width/2, log_sfp_lm_mat[state, y_lm_var]),
					c(x_seq[n]+bar_width/2, 0)
				)
		
				# Plot polygon
				polygon(x=bar_verts, col=cols_gray[state], border=NA)
		
				# Add rate as number
				text(x=x_seq[n], y=log_sfp_lm_mat[state, y_lm_var] + text_up_shift, 
					labels=round(log_sfp_lm_mat[state, y_lm_var], 2), xpd=TRUE, cex=mtext_cex)

				# Add state label
				text(x=x_seq[n], y=y_range[1]-text_down_shift, labels=state, pos=1, cex=0.9, xpd=TRUE)
				
				# Set inside bar text color
				inbar_col <- 'black'
				if(gray_scale[state] < 0.6) inbar_col <- 'white'

				# Add number label
				text(x=x_seq[n], y=y_range[1]+num_up_shift, labels=n, cex=0.9, xpd=TRUE, col=inbar_col, font=2)
		
				n <- n + 1
			}

			# Add source
			text(x=x_range[2] + 0.04*diff(x_range), y=y_range[1] - 0.32*diff(y_range), 
				labels='Plot created by @aarolsen; Source data: covidtracking.com', pos=2, xpd=TRUE, cex=1.4*mtext_cex, col=gray(0.25))

			mtext(text='n-fold increase in reported\npositive tests per day', side=2, line=2.5, cex=mtext_cex)
			
			#
			axis_at <- seq(y_range[1], y_range[2], length=4)
			axis(side=2, at=axis_at, labels=paste0(round(axis_at, 1), 'x'), mgp=c(3, 0.7, 0))

			dev.off()
		}
	}


	## Plot testing rates
	# Set which state labels to move outside points
	labels_out <- list(
		'AK'=c(1,0.2,-0.2,0.5),
		'NV'=c(1,0,-0.2,0.5),
		'MT'=c(-1,0,1,0.5),
		'UT'=c(1,-1,-0.2,0.5),
		'WI'=c(0,1.5,0.5,-0.3),
		'WY'=c(-1,0,1,0.5),
		'OK'=c(-0.3,0,1,0.5)
	)
	label_offset <- c(0.01, 0.5)
	
	# Open PDF
	pdf(paste0('../Plots/Per_positive vs per_tested/', file_date, ' Per_positive vs per_tested.pdf'), width=7, height=7)
	
	par('mar'=c(5,4,3,2))
	
	y_range <- range(testing_stats[, 'per_positive'], na.rm=TRUE)
	x_range <- range(testing_stats[, 'per_tested'], na.rm=TRUE)
	y_range <- c(0.8, 1)*y_range

	main_text <- paste0('COVID-19 testing rates by US state as of ', format(read_csv$AsDate[1], format="%B %d, %Y"), '')

	plot(x_range, y_range, type='n', xlab='', ylab='', log='xy', main=main_text)

	bg_text_cex <- 0.9
	bg_txt_col <- gray(0.7)
	text(x=0.01, y=2, labels=toupper('Low testing rate\nLow % positive'), col=bg_txt_col, cex=bg_text_cex)
	text(x=0.13, y=32, labels=toupper('High testing rate\nHigh % positive'), col=bg_txt_col, cex=bg_text_cex)
	text(x=0.01, y=39, labels=toupper('Low testing rate\nHigh % positive'), col=bg_txt_col, cex=bg_text_cex)
	text(x=0.13, y=0.9, labels=toupper('High testing rate\nLow % positive'), col=bg_txt_col, cex=bg_text_cex)

	# Add horizontal background line and label
	h_at <- 5
	abline(h=5, lty=2, col=bg_txt_col, lwd=2)
	text(x=h_at*0.01, y=y_range[2], labels=paste0(h_at*0.01, '% tested'), cex=bg_text_cex, xpd=TRUE, adj=c(0.85,-0.5), srt=90, col=bg_txt_col)

	# Add vertical background line and label
	v_at <- 0.05
	abline(v=v_at, lty=2, col=bg_txt_col, lwd=2)
	text(x=x_range[1], y=v_at*100, labels=paste0(v_at*100, '% positive'), cex=bg_text_cex, xpd=TRUE, adj=c(0.2,-0.5), col=bg_txt_col)
	
	for(state in rownames(testing_stats)){
	
		xy <- testing_stats[state, c('per_tested', 'per_positive')]

		if(state %in% names(labels_out)){
			
			# Add point
			points(x=xy[1], y=xy[2], cex=0.7)
			
			# Set text offset position
			text_xy <- c(xy[1] + label_offset[1]*labels_out[[state]][1], y=xy[2] + label_offset[2]*labels_out[[state]][2])

			# Draw line
			segments(x0=xy[1], y0=xy[2], x1=text_xy[1], y1=text_xy[2])
			
			# Add text labels inside circle
			text(x=text_xy[1], y=text_xy[2], labels=state, cex=0.7, adj=labels_out[[state]][3:4])

		}else{

			# Add circle
			points(x=xy[1], y=xy[2], cex=2.7)
		
			# Add text labels inside circle
			text(x=xy[1], y=xy[2], labels=state, cex=0.7)
		}
	}

	# Add axis labels
	mtext(side=1, text='Number of tests relative to state population (%), log-transformed', line=2.5)
	mtext(side=2, text='Percent of total tests reported as \'Positive\' (%), log-transformed', line=2.5)
	
	# Add source
	text(x=x_range[2] + 0.7*diff(x_range), y=y_range[1] - 0.0058*diff(y_range), 
		labels='Plot created by @aarolsen; Source data: covidtracking.com', pos=2, xpd=TRUE, cex=0.8, col=gray(0.4))
	
	dev.off()
}

run()