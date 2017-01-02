GetUser <- function(query, direction, type) {  # query Are.na for a user's channel information

	message(query)

	contents = connections = NULL
	if(type == 'all') { type <- c('public', 'closed') }

	# contents
	if(direction != -1) {

		user.req <-
			str_c('https://api.are.na/v2/users/', query, '/channels') %>%
			fromJSON

		if(user.req$contents %>% length > 0) {

			user <-
				user.req$contents %>%
				.[,names(.) %in% c('title', 'slug', 'status', 'class', 'length')] %>%
				mutate(user.name = user.req$contents %$% user %$% `username`) %>%
				mutate(user.slug = user.req$contents %$% user %$% `slug`) %>%
				mutate(query = query, hierarchy = 'content') %>%
				filter(class == 'Channel')
		}
	}

	# return
	user %>%
	filter(status %in% type)

}



Graph <- function(web, lay, cluster, file, width = 15, height = 15, label.size, edge.width, node.min, node.max) {

	graph <-
			web$arrows %>%
			graph.data.frame(directed=TRUE)

	# node sizing
	pr <- page.rank(graph)$vector

	# choose layout type
	if(lay == 'fr') {
		layout <-
				layout_with_fr(graph,
									start.temp = 20,
									niter = 2000)
	}

	if(lay == 'drl') {
		layout <-
				layout_with_drl(graph,
									 options = list(cooldown.attraction = 0.1, init.damping.mult = 2))
	}

	if(lay == 'kk') {
		layout <-
				layout_with_kk(graph)
	}

	if(lay == 'mds') {
		layout <-
			layout_with_mds(graph)
	}

	# graph clustering
	if(cluster == 'walktrap') {
		mem <-
			graph %>%
			walktrap.community(steps = 6, modularity = TRUE) %>%
			membership
	}

	if(cluster == 'betweenness') {
		mem <-
			graph %>%
			edge.betweenness.community(.) %>%
			membership
	}

	# draw plot
	pdf(file = str_c(file, '.pdf'), width = width, height = height)
			plot( graph,
						layout              = layout,
						vertex.label.dist   = 0.01,
						vertex.size         = Mapper(pr, c(node.min, node.max)),
						vertex.color        = colors[mem],
						vertex.frame.color  = NA,
						vertex.label.color  = 'black',
						vertex.label.family = 'Helvetica',
						vertex.label.cex    = label.size,
						edge.arrow.size     = 0.005,
						edge.width          = edge.width,
						edge.color          = '#d9d9d9' )
	dev.off()

}



Mapper <- function(x, range=c(0, 1), from.range = NA) {

		if(any(is.na(from.range))) from.range <- range(x, na.rm = TRUE)

		# check if all values are the same
		if(!diff(from.range)) {
				matrix(mean(range), ncol = ncol(x), nrow = nrow(x), dimnames = dimnames(x))
			}

		# map to [0,1]
		x <- (x - from.range[1])
		x <- x / diff(from.range)

		# handle single values
		if(diff(from.range) == 0) x <- 0

		# map from [0,1] to [range]
		if (range[1] > range[2]) x <- 1 - x
		x <- x*(abs(diff(range))) + min(range)

		x[x < min(range) | x > max(range)] <- NA

		x
}



#---------------
# initialization
#---------------


label.size = 2.5
edge.width = 5
node.min   = 0.2                 # min node plot size
node.max   = 10                  # max node plot size
lay        = 'fr'                # layout type: fr | drl | kk | mds
cluster    = 'walktrap'          # walktrap | betweenness 
width      = 200
height     = 200

#-------------
# static graph
#-------------


#Graph(web, lay, cluster, file = origin, width, height, label.size, edge.width, node.min, node.max)

#----------------------------
# export csv for graphcommons
#----------------------------


# # edges
# edges <-
#   web$arrows %>%
#   mutate(`From Type` = 'Channel', `Edge` = 'Contains', `To Type` = 'Channel', `Weight` = '') %>%
#   select( `From Type`,
#           `From Name` = source,
#           `Edge`,
#           `To Type`,
#           `To Name`   = target,
#           `Weight` ) %>%
#   unique %>%
#   filter(row_number() > 1600)


# # nodes
# nodes <-
#   web$objects %>%
#   mutate(`Type` = 'Channel', `Description` = '') %>%
#   select( `Type`,
#           `Name`         = slug,
#           `Description`,
#           `Length`       = length,
#           `User Name`    = user.name ) %>%
#   unique %>%
#   filter(row_number() > 1600)


# # write table to xlsx
# write.xlsx(list('Edges' = edges, 'Nodes' = nodes), file = "graph_commons.xlsx")








