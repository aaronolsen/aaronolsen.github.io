source('r_source.R')

if(!is.null(dev.list())) dev.off()

run <- function(){

	## * Add code to add 0 negative for case where only positives and no negative value given
	#mar_rank <- c(12,3)		# Number of states to include at slowest and fastest of ranking
	#rank_mid <- 1			# Number of middle ranking states to show
	min_val_lm <- 5			# Minimum number of positives with which to fit regression
	min_num_val_lm <- 6		# Minimum number of days to include in regressions
	log_base <- 10			# Base to use for log function
	#start_cases <- 25		# Number of cases when to align curves
	num_days_in_pos_rate <- 10	# Number of days to include in fit for projection
	num_days_in_test_rate <- 10	# Number of days to include in fit for projection
	#num_days_proj <- 2		# Number of days to project in advance of furthest day

	## Read files
	# Read states-daily
	read_csv <- read.csv(file=paste0('../../Data/states-daily.csv'), header=TRUE)

	# Read state pop sizes
	read_pops <- read.csv('../../Metadata/us_state_pop_sizes.csv')
	
	# Read states-current (for current grade)
	read_grades <- read.csv('../../Metadata/us_state_grades.csv')

	## Format files
	# Convert population size df to vector
	pop_sizes <- setNames(as.numeric(gsub(',', '', read_pops[, 'est_pop'])), read_pops[, 'abbr'])

	# Format dates
	read_csv$asdate <- as.Date(as.character(read_csv$date), "%Y%m%d")

	# Get most recent date
	read_grades_as_date <- as.Date(gsub('T[0-9:]+Z', '', as.character(read_grades$dateChecked)), "%Y-%m-%d")

	# Convert to vector
	state_grades <- setNames(as.character(read_grades$grade), as.character(read_grades$state))
	state_grades[state_grades == ''] <- 'N'

	# Remove NA rows
	read_csv <- read_csv[!is.na(read_csv[, 'date']), ]

	# Remove rows without negative
	read_csv <- read_csv[!is.na(read_csv$negative), ]

	## Save archives
	# Archive states-daily
	read_csv_most_recent_date <- format(sort(read_csv$asdate, decreasing=TRUE)[1], format="%Y%m%d")
	write.csv(x=read_csv, file=paste0('../../Data/states-daily archive/', read_csv_most_recent_date, '.csv'))

	# Archive states
	read_grades_most_recent_date <- format(sort(read_grades_as_date, decreasing=TRUE)[1], format="%Y%m%d")
	write.csv(x=read_grades, file=paste0('../../Data/states archive/', read_grades_most_recent_date, '.csv'))

	# Set most recent date as file date
	file_date <- read_csv_most_recent_date

	# Get unique list of states
	states_unique <- as.character(sort(unique(read_csv[, 'state'])))

	# Create state stats dataframe
	col_names <- c('pop_size', 'num_positive', 'total_tests', 'pos_per_cap', 'per_pos_tested', 
		'per_neg_tested', 'tests_per_cap', 'report_grade', 'nfold_pos_pday', 'dbl_days_pos', 
		'nfold_tests_pday', 'total_score')
	state_stats <- as.data.frame(matrix(NA, nrow=length(states_unique), ncol=length(col_names), dimnames=list(states_unique, col_names)))

	## Fill state_stats
	# For each state
	for(state in states_unique){

		# Add state population
		state_stats[state, 'pop_size'] <- pop_sizes[state]

		# Add state grade
		state_stats[state, 'report_grade'] <- state_grades[state]

		# Save dataframe for state
		df_by_state <- read_csv[read_csv[, 'state'] == state, ]
		
		# Get positive values as numeric
		positive_vals <- as.numeric(gsub(',', '', as.character(df_by_state[, 'positive'])))

		# Save positive values
		df_by_state[, 'positive'] <- positive_vals

		# Get negative values as numeric
		negative_vals <- as.numeric(gsub(',', '', as.character(df_by_state[, 'negative'])))

		# Save negative values
		df_by_state[, 'negative'] <- negative_vals

		# Get first positive reported date
		first_positive <- sort(df_by_state[positive_vals > 0, 'asdate'])[1]
		
		# Get days since first positive
		df_by_state[, 'Days_SFP'] <- as.numeric(df_by_state[, 'asdate'] - first_positive)

		# Save current statistics
		state_stats[state, 'num_positive'] <- df_by_state[1, 'positive']
		state_stats[state, 'pos_per_cap'] <- 100*(df_by_state[1, 'positive'] / state_stats[state, 'pop_size'])
		state_stats[state, 'total_tests'] <- as.numeric(as.character(gsub(',','',df_by_state[1, 'totalTestResults'])))
		state_stats[state, 'per_pos_tested'] <- 100*(df_by_state[1, 'positive'] / state_stats[state, 'total_tests'])
		state_stats[state, 'per_neg_tested'] <- 100*(df_by_state[1, 'negative'] / state_stats[state, 'total_tests'])
		state_stats[state, 'tests_per_cap'] <- 100*(state_stats[state, 'total_tests'] / state_stats[state, 'pop_size'])

		# Get log positive
		df_by_state[, 'positive_log'] <- NA

		# Get positive values not NA and over 0
		which_positive <- rep(FALSE, length(positive_vals))
		which_positive[is.na(positive_vals)] <- FALSE
		which_positive[!is.na(positive_vals)] <- positive_vals[!is.na(positive_vals)] > 0

		# Skip if not any positives reported yet
		if(!any(which_positive)) next

		# Get log values
		df_by_state[which_positive, 'positive_log'] <- log(positive_vals[which_positive], base=log_base)
		
		# Find when values are greater than 1
		which_over_min <- rep(FALSE, length(positive_vals))
		which_over_min[is.na(positive_vals)] <- FALSE
		which_over_min[!is.na(positive_vals)] <- positive_vals[!is.na(positive_vals)] > min_val_lm

		# Skip if not enough values for regression
		if(sum(which_over_min) < min_num_val_lm) next

		# Get indices to include
		ind_over_min <- c(1:length(which_over_min))[which_over_min]

		## Calculate nfold increase in number of tests performed
		# Shorten to the most recent number of days to include in calculating rate
		if(length(ind_over_min) > num_days_in_test_rate) ind_over_min_test <- head(ind_over_min, num_days_in_test_rate)

		# Create dataframe
		dlist_test_rate <- list(y=log(df_by_state[ind_over_min_test, 'totalTestResults'], base=log_base), 
			x=df_by_state[ind_over_min_test, 'Days_SFP'])
		lm_fit_test <- lm(y ~ x, data=dlist_test_rate)

		# Save nfold increase in tests per day
		state_stats[state, 'nfold_tests_pday'] <- log_base ^ lm_fit_test$coefficients['x']

		# Create diagnostic plot
		pdf(paste0('~/Documents/Outreach/Coronavirus analysis/Plots/total_tests vs days_sfp/', state, '.pdf'))
		plot(as.numeric(dlist_test_rate$x), dlist_test_rate$y, 
			ylab='Number of tests performed', xlab='Time since first positive (days)')
		abline(a=lm_fit_test$coefficients['(Intercept)'], b=lm_fit_test$coefficients['x'], lty=2)
		dev.off()


		## Calculate nfold increase in number of positives
		# Shorten to the most recent number of days to include in calculating rate
		if(length(ind_over_min) > num_days_in_pos_rate) ind_over_min_pos <- head(ind_over_min, num_days_in_pos_rate)

		# Fit regression to more recent data to get rate
		dlist_pos_rate <- list(y=df_by_state[ind_over_min_pos, 'positive_log'], x=df_by_state[ind_over_min_pos, 'Days_SFP'])
		lm_fit_pos <- lm(y ~ x, data=dlist_pos_rate)

		# Save nfold increase in positives per day
		state_stats[state, 'nfold_pos_pday'] <- log_base ^ lm_fit_pos$coefficients['x']

		# Calculate number of days for positives to double
		state_stats[state, 'dbl_days_pos'] <- log(2, base=log_base) / lm_fit_pos$coefficients['x']

		# Create diagnostic plot
		pdf(paste0('~/Documents/Outreach/Coronavirus analysis/Plots/positive_log vs days_sfp/', state, '.pdf'))
		plot(as.numeric(dlist_pos_rate$x), df_by_state[ind_over_min_pos, 'positive_log'], 
			ylab='Number of positives, log-transformed', xlab='Time since first positive (days)')
		abline(a=lm_fit_pos$coefficients['(Intercept)'], b=lm_fit_pos$coefficients['x'], lty=2)
		dev.off()
	}

	# Remove rows

	## Perform PCA
	# Set columns to use for PCA
	pr_cols <- c('pos_per_cap', 'per_pos_tested', 'tests_per_cap', 'nfold_pos_pday', 'nfold_tests_pday')

	# Create matrix for PCA	
	pr_mat <- state_stats[rowSums(is.na(state_stats[, pr_cols])) == 0, pr_cols]

	# Set variables to transform
	tvars <- c('pos_per_cap', 'per_pos_tested', 'tests_per_cap')

	# Transform certain variables before PC to spread distribution
	for(tvar in tvars) pr_mat[, tvar] <- log(pr_mat[, tvar], base=log_base)
	
	# Run PCA
	pr_comp <- prcomp(pr_mat, center=TRUE, scale.=TRUE)
	
	# Create matrix of correlation between each variable and PC columns
	var_cor <- matrix(NA, nrow=ncol(pr_comp$x), ncol=length(pr_cols), dimnames=list(paste0('PC', 1:ncol(pr_comp$x)), pr_cols))

	# Find correlation of each axis with PC scores
	for(pr_var in pr_cols){
		for(pc_axis in 1:ncol(pr_comp$x)){
			var_cor[pc_axis, pr_var] <- cor(pr_mat[, pr_var], pr_comp$x[, pc_axis])
		}
	}
	
	# Get percent explained by each axis
	per_var <- (pr_comp$sdev^2) / sum(pr_comp$sdev^2)
	
	# Set the preferred direction for each variable (+1 if higher value is good)
	good_sign <- c('pos_per_cap'=-1, 'per_pos_tested'=-1, 'tests_per_cap'=1, 'nfold_pos_pday'=-1, 'nfold_tests_pday'=1)

	# Scale all variables
	pr_mat_scaled <- pr_mat
	for(pr_var in colnames(pr_mat_scaled)){
		pr_mat_scaled[, pr_var] <- (pr_mat_scaled[, pr_var] - min(pr_mat_scaled[, pr_var])) / diff(range(pr_mat_scaled[, pr_var]))
		if(good_sign[pr_var] < 0) pr_mat_scaled[, pr_var] <- -pr_mat_scaled[, pr_var] + 1
	}
	
	#print(pr_mat_scaled)
	print(sort(apply(pr_mat_scaled, 1, 'sum')))

	# Set overall ranking
	#print(apply(pr_comp$x[, plot_pcs_unique], 1, 'prod'))


	# Find axis that has highest correlation with variable
	axis_max <- setNames(rep(NA, length(pr_cols)), pr_cols)
	for(pr_col in pr_cols){
		
		# Find axis with greatest correlation with variable
		axis_max[pr_col] <- which.max(abs(var_cor[, pr_col]))
		
		# Set sign
		if(var_cor[axis_max[pr_col], pr_col] < 0) axis_max[pr_col] <- -axis_max[pr_col]
	}

	for(pc_num in 1:ncol(pr_comp$x)){

		# Get correlating variables
		pr_match <- names(axis_max)[abs(axis_max) == pc_num]
		
		# Skip if no correlating variables
		if(!length(pr_match)) next
		
		# Check if signs match
		signs_match <- (sign(axis_max[pr_match]) - good_sign[pr_match]) == 0

		# Check if same for all variables
		if(sum(signs_match) > 0 && sum(signs_match) < length(signs_match))
			print(paste0('Conflicting "good" signs for variables along axis', pc_num))

		# If first wrong sign, flip
		if(!signs_match[1]){
			pr_comp$x[, pc_num] <- -pr_comp$x[, pc_num]
			axis_max[pr_match] <- -axis_max[pr_match]
			var_cor[pc_num, ] <- -var_cor[pc_num, ]
		}
	}
	
	# Set PCs to plot
	plot_pcs <- list(1:2, 2:3)


	## Find clusters
	# Get unique PCs plotted
	plot_pcs_unique <- unique(unlist(plot_pcs))

	# Get mean axis length
	axis_len_mean <- mean(apply(apply(pr_comp$x, 2, 'range'), 2, 'diff')[plot_pcs_unique])
	
	# Set threshold for cluster
	cluster_dist <- 0.03*axis_len_mean

	# Set line label length
	line_lab_len <- rep(0.02*axis_len_mean, 2)

	# Find clusters
	labels_out <- find_clusters(plot_pcs, pr_comp$x, cluster_dist, line_lab_len)
	labels_out[[1]]['CO'] <- 'lower'
	labels_out[[1]]['WI'] <- 'left'

	labels_out[[2]]['PA'] <- 'left'
	labels_out[[2]]['IL'] <- 'left'
	labels_out[[2]]['TN'] <- 'right'
	labels_out[[2]]['WY'] <- 'upper left'
	labels_out[[2]]['OR'] <- 'lower'
	labels_out[[2]]['UT'] <- 'left'
	labels_out[[2]]['VT'] <- 'lower'
	
	## Plot
	# Set size of circle with text
	circle_txt_cex <- 3.1
	txt_cex <- 0.71
	mtext_cex <- 0.7
	bg_text_cex <- 1.5
	bg_txt_col <- gray(0.8)

	# Set variable labels
	pr_col_labels <- list(
		'pos_per_cap'=paste0(c('More', 'Fewer'), ' confirmed cases per capita'), 
		'per_pos_tested'=paste0(c('Greater', 'Lower'), ' % of test results are positive'), 
		'tests_per_cap'=paste0(c('More', 'Fewer'), ' tests performed per capita'), 
		'nfold_pos_pday'=paste0(c('Faster', 'Slower'), ' increase in confirmed cases'), 
		'nfold_tests_pday'=paste0(c('Faster', 'Slower'), ' increase in tests performed')
	)

	# Set grade colors
	grade_cols <- c('A'='black', 'B'='blue', 'C'='#808000', 'D'='red')

	# Set point color based on state grade
	point_col <- setNames(rep(NA, nrow(state_stats)), rownames(state_stats))
	for(state in names(point_col)){
		if(state_grades[state] == 'A') point_col[state] <- grade_cols['A']
		if(state_grades[state] == 'B') point_col[state] <- grade_cols['B']
		if(state_grades[state] == 'C') point_col[state] <- grade_cols['C']
		if(state_grades[state] == 'D') point_col[state] <- grade_cols['D']
		if(state_grades[state] == 'N') point_col[state] <- grade_cols['D']
	}

	# Open PDF
	pdf(paste0('~/Documents/Outreach/Coronavirus analysis/Plots/PCA/', file_date, ' PCA.pdf'), width=9, height=4.8)
	
	layout(matrix(c(1,1,2,3,4,4), 3, 2, byrow=TRUE), height=c(0.08, 1, 0.05))

	par('mar'=c(0,0,0,0))
	plot(x=c(0,1), y=c(0,1), type='n', bty='n', ylab='', xlab='', xaxt='n', yaxt='n')
	main <- paste0('PCA of five COVID-19 testing variables by state as of ', format(read_csv$asdate[1], format="%B %d, %Y"))
	text(x=0.5, y=0.6, labels=main, font=2, cex=1, xpd=TRUE)
	main <- '(1) confirmed cases per capita, (2) % of tests that are positive, (3) tests performed per capita, (4) Rate of increase in confirmed cases, (5) Rate of increase in testing'
	text(x=0.5, y=0.25, labels=main, font=1, cex=0.9, xpd=TRUE)

	par(mar=c(4,4,0,1))

	for(pcs_plot in plot_pcs){

		# Set ranges
		x_range <- range(pr_comp$x[, pcs_plot[1]])
		y_range <- range(pr_comp$x[, pcs_plot[2]])

		plot(x_range, y_range, type='n', xlab='', ylab='', xaxt='n', yaxt='n')
	
		#text(x=x_range[2] - 0.*diff(x_range), y=y_range[2] - 0.*diff(y_range), 
		#	labels='Relatively better', adj=c(1,0.5), cex=bg_text_cex, col=bg_txt_col, srt=45)

		for(i in 1:2){

			# Add axis label
			mtext(side=i, line=2.5, text=paste0('Principal component ', pcs_plot[i], ' (', round(per_var[pcs_plot[i]]*100), '%)'), 
				cex=1.2*mtext_cex)
			
			# Find variables that correlate with first axis
			which_vars <- abs(axis_max) == pcs_plot[i]
		
			#
			n <- 0
			for(cor_var in pr_cols[which_vars]){
				
				#
				if(axis_max[cor_var] > 0){
					adj <- c(1, 0)
				}else{
					adj <- c(0, 1)
				}
				
				# Set line
				line <- n

				# Add text
				for(j in 1:2){
					mtext(side=i, line=line, text=pr_col_labels[[cor_var]][j], adj=adj[j], col=gray(0.4), cex=1*mtext_cex)
				}

				n <- n + 1
			}
		}

		for(state in rownames(pr_comp$x)){
		
			# Set xy position
			xy <- pr_comp$x[state, pcs_plot]

			if(state %in% names(labels_out[[pcs_plot[1]]])){
			
				# Add point
				points(x=xy[1], y=xy[2], cex=0.7, col=point_col[state])
				
				# Set new label position
				new_label <- set_new_label_pos(xy, where=labels_out[[pcs_plot[1]]][state], line_len=line_lab_len)

				# Draw line
				segments(x0=xy[1], y0=xy[2], x1=new_label$at[1], y1=new_label$at[2], col=point_col[state])

				# Add text labels inside circle
				text(x=new_label$at[1], y=new_label$at[2], labels=state, cex=txt_cex, adj=new_label$adj, col=point_col[state])

			}else{

				# Add circle
				points(x=xy[1], y=xy[2], cex=circle_txt_cex, col=point_col[state])
	
				# Add text labels inside circle
				text(x=xy[1], y=xy[2], labels=state, cex=txt_cex, col=point_col[state])
			}
		}
		
		# Point color legend
		if(pcs_plot[1] == 1){
			legend(x=x_range[1], y=y_range[1], legend=paste0('Grade ', names(grade_cols), ' reporting'), 
				col=grade_cols, xjust=0.05, yjust=0.2, bty='n', pch=15)
		}
	}

	par('mar'=c(0,0,0,0))
	plot(x=c(0,1), y=c(0,1), type='n', bty='n', ylab='', xlab='', xaxt='n', yaxt='n')

	# Add source
	text(x=1.02, y=0.5, labels='Plot created by @aarolsen; Source data: covidtracking.com; Source code at github.com/aaronolsen/aaronolsen.github.io/tree/master/corona', 
		adj=c(1,0.5), xpd=TRUE, cex=1.4*mtext_cex, col=gray(0.25))

	dev.off()
}

run()