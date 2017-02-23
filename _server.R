

message('|-- server')


Server <- shiny::shinyServer(function(input, output, session) {

	# initialize variables
	old.nodes = old.edges = NULL

	# system time
	log.file <- str_c('logs/record_', format(Sys.time(), "%d-%m-%y"), '.txt')


	# output
	output$network_proxy <- renderVisNetwork({

		map <- Crawl(origin, depth, direction, type, up.initial, pop)
		PlotGraph(map$objects, map$arrows, map$origin)

	})


	# observe
	build <- observe({

		# listener
		input$buildGraph

		isolate(if(input$direction == 'down') {
			direction = 1
		} else if(isolate(input$direction) == 'down & up') {
			direction = 0
		} else {
			direction = -1
		})

		isolate(if(isolate(input$up.initial) == 'yes') {
			up.initial = 1
		} else {
			up.initial = 0
		})

		depth <- isolate(as.numeric(input$depth))


		isolate(if(input$buildGraph) {

		map <- isolate(Crawl(input$origin, depth, direction, input$type, up.initial, input$pop))

			visNetworkProxy('network_proxy') %>%
			visUpdateNodes(map$objects %>% unique) %>%
			visUpdateEdges(map$arrows %>% unique) %>%
			visLayout(improvedLayout = TRUE)

		})

		visNetworkProxy("network_proxy") %>%
		visGetEdges()

		visNetworkProxy("network_proxy") %>%
		visGetNodes()

		isolate(cat(input$origin, '\n', file = log.file, append = TRUE))

	})

	clear <- observe({

		input$clearGraph

		isolate(if(input$clearGraph) {

			if(!is.null(input$network_proxy_edges) & !is.null(input$network_proxy_nodes)) {

				old.edges <-
					input$network_proxy_edges %>%
					map(~ { dplyr::as_data_frame(rbind(unlist(.x))) }) %>%
					bind_rows

				old.nodes <-
					input$network_proxy_nodes %>%
					map(~ { dplyr::as_data_frame(rbind(unlist(.x))) }) %>%
					bind_rows

				visNetworkProxy('network_proxy') %>%
				visRemoveNodes(old.nodes$id) %>%
				visRemoveEdges(old.edges$id)

			}

		})

	})

	# cleanup
	session$onSessionEnded(function() {
		build$suspend()
		clear$suspend()
		unlink(log.file)
	})

})
