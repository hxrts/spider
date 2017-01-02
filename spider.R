#----------
# libraries
#----------


pacman::p_load(dplyr, tidyr, magrittr, stringr, purrr, rlist,
					jsonlite, reshape2, igraph, RColorBrewer, openxlsx,
					visNetwork, igraph,
					shiny)


#----------
# functions
#----------


GetChannel <- function(query, direction = 0, type = 'all') {  # query Are.na for channel information

	message(query)

	identity = contents = connections = NULL
	if(type == 'all') { type <- c('public', 'closed') }

	try({

		# identity
		identity.req <-
			str_c('https://api.are.na/v2/channels/', query, '/thumb') %>%
			fromJSON

		identity <-
			data_frame( query       = query,
							title       = identity.req$title,
							slug        = identity.req$slug,
							status      = identity.req$status,
							user.name   = unlist(identity.req$user$username),
							user.slug   = unlist(identity.req$user$slug),
							length      = identity.req$length,
							hierarchy   = 'identity',
							class       = 'Channel' )

		# contents
		if(direction != -1) {

			contents.req <-
				str_c('https://api.are.na/v2/channels/', query, '/contents') %>%
				fromJSON

			if(contents.req$contents %>% length > 0) {

				user.name <- contents.req$contents$user$username
				user.slug <- contents.req$contents$user$slug

				contents <-
					contents.req$contents %>%
					.[,names(.) %in% c('title', 'slug', 'status', 'class', 'length')] %>%
					mutate(user.name = user.name) %>%
					mutate(user.slug = user.slug) %>%
					mutate(query = query, hierarchy = 'content') %>%
					filter(class == 'Channel')
			}
		}

		# connections
		if(direction != 1) {

			connections.req <-
				str_c('https://api.are.na/v2/channels/', query, '/connections') %>%
				fromJSON

			if(connections.req$channels %>% length > 0) {

				user.name <- connections.req$channels$user$username
				user.slug <- connections.req$channels$user$slug

				connections <-
					connections.req$channels %>%
					.[,names(.) %in% c('title', 'slug', 'status', 'class', 'length')] %>%
					mutate(user.name = user.name) %>%
					mutate(user.slug = user.slug) %>%
					mutate(query = query, hierarchy = 'content') %>%
					filter(class == 'Channel')
			}
		}
	})

	# return
	bind_rows(identity, contents, connections) %>%
	filter(status %in% type)

}


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


FormArrows <- function(reply) {  # format reply as source -> target table

	reply %>%
	filter(hierarchy != 'identity') %>%
	mutate(source = ifelse(hierarchy == 'content', query, slug)) %>%
	mutate(target = ifelse(hierarchy == 'content', slug, query)) %>%
	select(source, target) %>%
	unique

}


FormObjects <- function(reply) {  # format reply as channel metadata table

	reply %>%
	select(title, slug, user.name, user.slug, length)

}


Crawl <- function(origin, depth, direction = 0, type = 'all') {  # main crawling loop

	pool    = list(origin)
	spent   = list()
	arrows  = list()
	objects = list()

	 # crawl recursion
	 for(level in 1:depth) {

			message(paste('\n[[ level', level, ']]\n'))

			if(level <= length(pool)) {
				 if(pool[[level]] %>% length > 0) {

						reply <-
							 pool[[level]] %>%
							 list.map(GetChannel(., direction, type)) %>%
							 bind_rows %>%
							 mutate(level = level)

						arrows[[level]] <- FormArrows(reply)

						objects[[level]] <- FormObjects(reply)

						spent[[level]] <- pool[[level]]

						pool[[level + 1]] <-
							 arrows[[level]] %>%
							 unlist %>%
							 unname %>%
							 unique %>%
							 sort %>%
							 list.filter(! . %in% unlist(spent))

				 }
			}
	 }

	 # return
	 list( arrows = arrows %>% bind_rows %>% unique,
				 objects = objects %>% bind_rows %>% unique)

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


#---------------
# initialization
#---------------


origin     = 'performance'
depth      = 3                   # recursive depth
direction  = 0                   # -1 = move up channel hierarchy | 0 = move up & down | 1 = move down
type       = 'all'               # public | closed | all
lay        = 'fr'                # layout type: fr | drl | kk | mds
cluster    = 'walktrap'          # walktrap | betweenness 
seed       = 0                   # graph layout seed
width      = 200
height     = 200
label.size = 2.5
edge.width = 5
node.min   = 0.2                 # min node plot size
node.max   = 10                  # max node plot size

set.seed(seed)

# palette
colors <- mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)) %>% unlist


#------
# crawl
#------


web <- Crawl(origin, depth, direction, type)


#------------------
# interactive graph
#------------------


nodes <-
	data_frame(id = web$arrows %>% unlist %>% unique) %>%
	left_join(web$objects, by = c('id' = 'slug')) %>%
	rename(label = title)

edges <-
		web$arrows %>%
		rename(from = source, to = target)

graph <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)

nodes$group <- walktrap.community(graph, steps = 6, modularity = TRUE) %>% membership
nodes$value <- betweenness(graph, directed = FALSE, normalized = TRUE)

nodes <-
	nodes %>%
	mutate(color = colors[nodes$group]) %>%
	rename(user = user.name) %>%
	mutate(title = str_c(
		'<a href="https://are.na/', id, '" target="_blank">', label, '</a>
		<ul>
			<li>user: <a href="https://are.na/', user.slug, '" target="_blank">', user, '</a></li>
			<li>blocks: ', length,'</li>
			<li>betweeness: ', round(value, 2),'</li>
		</ul>'
	))


vis.network <-
	visNetwork(
		nodes,
		edges,
		main = list(text = 'Are.na / ', style  = 'display: inline-block; font-family: Arial; color: #9d9d9d; font-weight: bold; font-size: 20px; text-align: left'),
		submain   = list(text = str_c('<a style="color:#585858; text-decoration: none;" href="https://are.na/', origin, '" target=_blank>', nodes %>% filter(id == origin) %$% label, '</a><br>'), style  = 'margin-left:0.3em;display:inline;font-family:Arial; font-weight:bold;font-size:20px;text-align:left'),
		width  = '100%',
		height = '800px'
	) %>%
	visIgraphLayout(
		#layout       = 'layout_nicely',
		layout       = 'layout_with_fr',
		physics      = FALSE,
		smooth       = FALSE,
		type         = 'full',
		randomSeed   = NULL,
		layoutMatrix = NULL
	) %>%
	visNodes(
		font = '16px arial black',
		shape = 'square',
		shadow = FALSE,
		scaling = list(min = 3, max = 30)
	) %>%
	visEdges(
		shadow = FALSE,
		selectionWidth = 0.4,
		arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5))
	) %>%
	visOptions(
		selectedBy = list(variable = 'user', style = 'font-family: Arial; margin-bottom: 20px; padding: 2px; border: solid 1px #cbcbcb; display: block; outline: none; -webkit-appearance: none; -webkit-border-radius: 0px;'),
		highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE), 
		nodesIdSelection = list(
			useLabels = TRUE,
			style = 'font-family: Arial; display: block; margin-top: 15px; padding: 2px; border: solid 1px #cbcbcb; outline: none; -webkit-appearance: none; -webkit-border-radius: 0px;')
	) %>%
	visInteraction(keyboard = TRUE) %>%
	visNodes(size = 10)

vis.network


#------
# shiny
#------


# server <- function(input, output) {
# 	output$network <- renderVisNetwork({
# 		vis.network
# 	})
# }

# ui <- fluidPage(
# 		visNetworkOutput("network")
# )

# shinyApp(ui = ui, server = server)


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








