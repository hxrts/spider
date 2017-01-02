#----------
# libraries
#----------


pacman::p_load(dplyr, tidyr, stringr, magrittr, purrr, rlist, jsonlite, visNetwork, igraph, shiny)


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

	message('\n[[ done ]]')

	# return
	list(arrows  = arrows %>% bind_rows %>% unique,
		 objects = objects %>% bind_rows %>% unique)

}


WorldMap <- function(web) {

	objects <-
		data_frame(id = web$arrows %>% unlist %>% unique) %>%
		left_join(web$objects, by = c('id' = 'slug')) %>%
		rename(label = title)

	arrows <-
			web$arrows %>%
			rename(from = source, to = target)

	graph <- graph_from_data_frame(arrows, directed = TRUE, vertices = objects)

	objects$group <- walktrap.community(graph, steps = 6, modularity = TRUE) %>% membership
	objects$value <- betweenness(graph, directed = FALSE, normalized = TRUE)

	objects <-
		objects %>%
		mutate(color = sample(colors, 50)[objects$group]) %>%
		rename(user = user.name) %>%
		mutate(title = str_c(
			'<a href="https://are.na/', id, '" target="_blank">', label, '</a>
			<ul>
				<li>user: <a href="https://are.na/', user.slug, '" target="_blank">', user, '</a></li>
				<li>blocks: ', length,'</li>
				<li>betweeness: ', round(value, 2),'</li>
			</ul>'
		))

	list(objects = objects, arrows = arrows, graph = graph)

}


#---------------
# initialization
#---------------


origin     = 'performance'
direction  = 1                   # -1 = move up channel hierarchy | 0 = move up & down | 1 = move down
depth      = 2                   # recursive depth
type       = 'all'               # public | closed | all
seed       = 0                   # graph layout seed

set.seed(seed)

# palette
colors <- c(
	'#d99aac',
	'#ffa9c0',
	'#e2979f',
	'#ffc6bf',
	'#ffb18f',
	'#d3a088',
	'#efb17b',
	'#ffcc95',
	'#bba98d',
	'#ffe2a8',
	'#fff9d3',
	'#cfce81',
	'#a1b279',
	'#dcf3a7',
	'#ebffc3',
	'#b0d791',
	'#ccffd5',
	'#80b88e',
	'#b7ffd1',
	'#56bd9c',
	'#7deac8',
	'#74b8a7',
	'#3ec1ad',
	'#53d4c1',
	'#92ffef',
	'#affff3',
	'#6ef4e8',
	'#d1fef9',
	'#85f8ff',
	'#a5d1d6',
	'#47e2f6',
	'#02c0d8',
	'#a5f0ff',
	'#6ab6cb',
	'#4fdcff',
	'#94bcc9',
	'#82deff',
	'#b2d5e8',
	'#76b2dc',
	'#98d2ff',
	'#8ac7ff',
	'#75b1e8',
	'#b6a2d9',
	'#e3cdff',
	'#ffdeff',
	'#ca9dc8',
	'#edabe8',
	'#f09ccc',
	'#ffb7de',
	'#e4bfcb')


#------
# shiny
#------


server <- function(input, output) {

	map <-
		Crawl(origin, depth, direction, type) %>%
		WorldMap

	output$network <- renderVisNetwork({

		visNetwork(
			map$objects,
			map$arrows,
			main = list(text = 'Are.na / ', style  = 'display: inline-block; font-family: Arial; color: #9d9d9d; font-weight: bold; font-size: 20px; text-align: left'),
			submain   = list(text = str_c('<a style="color:#585858 !important; text-decoration: none;" href="https://are.na/', origin, '" target=_blank>', map$objects %>% filter(id == origin) %$% label, '</a><br>'), style  = 'margin-left:0.3em;display:inline;font-family:Arial; font-weight:bold;font-size:20px;text-align:left'),
			width  = '100%',
			height = '800px'
		) %>%
		visLayout(improvedLayout = TRUE) %>%
		# visIgraphLayout(
		# 	#layout       = 'layout_nicely',
		# 	layout       = 'layout_with_fr',
		# 	physics      = FALSE,
		# 	smooth       = FALSE,
		# 	type         = 'full',
		# 	randomSeed   = NULL,
		# 	layoutMatrix = NULL
		# ) %>%
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
		visNodes(size = 10)

	})

	observe({

		update.map <-
			input$origin %>%
			Crawl(input$depth, input$direction, input$type) %>%
			WorldMap

		set.seed(input$seed)

		visNetworkProxy('network') %>%
		visRemoveNodes(map$objects %>% anti_join(update.map$objects) %$% id) %>%
		visUpdateNodes(update.map$objects %>% left_join(map$objects)) %>%
		visUpdateEdges(update.map$arrows %>% left_join(map$arrows)) %>%
		visLayout(improvedLayout = TRUE)

	})

}


ui <- fluidPage(

	titlePanel('World Mapper'),

	fluidRow(

		column(
			6,
			wellPanel(
				textInput(inputId = 'origin',    label = 'Origin [ network entry point ]',                                                    value = origin,    placeholder = origin),
				textInput(inputId = 'direction', label = 'Direction [ -1 = move up channel hierarchy | 0 = move up & down | 1 = move down ]', value = direction, placeholder = direction),
				textInput(inputId = 'depth',     label = 'Depth [ recursive depth ]',                                                         value = depth,     placeholder = depth),
				textInput(inputId = 'type',      label = 'Type [ public | closed | all ]',                                                    value = type,      placeholder = type),
				textInput(inputId = 'seed',      label = 'Seed [ layout & color seed ]',                                                      value = seed,      placeholder = seed),
				submitButton('→')
			)
		),

		column(
			12,
			visNetworkOutput('network', width = '1200px', height = '800px')
		)

	)
)


shinyApp(ui, server)















