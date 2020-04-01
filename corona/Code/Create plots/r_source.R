
logitTransform <- function(p) { 

	in_log <- p / (1 - p)
	
	if(any(in_log < 0)) in_log <- in_log - 1.1*min(in_log)
	
	log(in_log) 
}

# Written-out numbers for plot titles
num_spelled <- c('One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine', 'Ten')

find_clusters <- function(plot_pcs, scores, cluster_dist, line_lab_len){

	labels_out <- list()

	clusters <- list()
	for(pcs_plot in plot_pcs){
	
		# Shorten
		pcs <- pcs_plot

		# Create list
		labels_out[[pcs[1]]] <- list()
	
		# Restart skip list
		skip_states <- c()
		
		# Set default label position
		labels_at <- scores[, pcs]
	
		for(state in rownames(labels_at)){
		
			# Check whether state has already been placed
			if(state %in% skip_states) next
		
			# Find other nearby states
			dppt_others <- sqrt(rowSums((matrix(labels_at[state, ], nrow(labels_at), length(pcs), byrow=TRUE) - labels_at)^2))
			
			# Exclude itself
			dppt_others <- dppt_others[names(dppt_others)[!names(dppt_others) %in% c(state, skip_states)]]
			
			# Skip if no other states within cluster distance
			if(!any(dppt_others < cluster_dist)) next
			
			# Find states within cluster distance
			states_near <- names(dppt_others)[dppt_others < cluster_dist]

			#
			if(length(states_near) == 1){
			
				if(abs(diff(labels_at[c(state, states_near), 1])) > abs(diff(labels_at[c(state, states_near), 2]))){

					# Find label
					move_to <- c('left', 'right')
					if(labels_at[state, 1] > labels_at[states_near, 1]) move_to <- rev(move_to)

				}else{

					# Find label
					move_to <- c('lower', 'upper')
					if(labels_at[state, 2] > labels_at[states_near, 2]) move_to <- rev(move_to)
				}

				# Set
				labels_out[[pcs[1]]][[state]] <- move_to[1]
				labels_out[[pcs[1]]][[states_near]] <- move_to[2]
				
				# Update position
				labels_at[state, ] <- set_new_label_pos(scores[state, pcs], labels_out[[pcs[1]]][[state]], line_lab_len)$at
				labels_at[states_near, ] <- set_new_label_pos(scores[states_near, pcs], labels_out[[pcs[1]]][[states_near]], line_lab_len)$at

				# Update distance to others
				dppt_state_others <- sqrt(rowSums((matrix(labels_at[state, ], nrow(labels_at), length(pcs), byrow=TRUE) - labels_at)^2))
				dppt_state_others <- dppt_state_others[names(dppt_state_others) != state]
				
				# Check if position is too close to another
				if(any(dppt_state_others < 1.5*cluster_dist)){
					
					# Find states within cluster distance
					states_near_state <- names(dppt_state_others)[dppt_state_others < 1.5*cluster_dist]
					
					#
					if(labels_out[[pcs[1]]][[state]] == 'upper') labels_out[[pcs[1]]][[state]] <- paste0(labels_out[[pcs[1]]][[state]], ' left')
					if(labels_out[[pcs[1]]][[state]] == 'lower') labels_out[[pcs[1]]][[state]] <- paste0(labels_out[[pcs[1]]][[state]], ' right')
					if(labels_out[[pcs[1]]][[state]] == 'left') labels_out[[pcs[1]]][[state]] <- paste0('lower ', labels_out[[pcs[1]]][[state]])
					if(labels_out[[pcs[1]]][[state]] == 'right') labels_out[[pcs[1]]][[state]] <- paste0('upper ', labels_out[[pcs[1]]][[state]])
				}
			}
			
			# Add all states to skip list
			skip_states <- c(skip_states, state, states_near)

			#break
		}

		#break
	}
	
	labels_out
}

set_new_label_pos <- function(xy, where, line_len){

	# Set line label sign
	if(grepl('left', where)) line_len[1] <- -line_len[1]
	if(grepl('lower', where)) line_len[2] <- -line_len[2]
	if(!grepl('left|right', where)){ line_len[1] <- 0 ; line_len[2] <- sqrt(2)*line_len[2]}
	if(!grepl('lower|upper', where)){ line_len[2] <- 0 ; line_len[1] <- sqrt(2)*line_len[1]}

	# one or two values in [0, 1] which specify the x (and optionally y) adjustment (ÔjustificationÕ) of the labels, 
	# with 0 for left/bottom, 1 for right/top, and 0.5 for centered. On most devices values outside [0, 1] will also work.
	if(where == 'lower left') text_adj <- c(1, 0.7)
	if(where == 'lower right') text_adj <- c(-0.2,0.5)
	if(where == 'left') text_adj <- c(1, 0.5)
	if(where == 'right') text_adj <- c(0, 0.5)
	if(where == 'upper left') text_adj <- c(1,-0.3)
	if(where == 'upper right') text_adj <- c(0,0.2)
	if(where == 'upper') text_adj <- c(0.5,-0.3)
	if(where == 'lower') text_adj <- c(0.5,1)

	# Set end of line
	line_end <- c(xy[1]+line_len[1], xy[2]+line_len[2])

	list('at'=line_end, 'adj'=text_adj)
}