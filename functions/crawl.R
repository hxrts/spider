

message('  - crawl')


Crawl <- function(origin, depth = 1, direction = 0, type = 'all', up.initial = TRUE, pop = '') {  # main crawling loop

	pool    = list(origin)
	spent   = list()
	arrows  = list()
	objects = list()
	exit    = 0

	pop %<>% strsplit(',') %>% map(~{ strip(.x) }) %>% unlist %>% list.filter(. != origin)

	if(length(pop) == 0) {
		pop = ''
	}

	# crawl recursion
	for(level in 1:depth) {

		level.msg <- str_c('\n[[ level ', level, ' ]]\n')
		message(level.msg)

		if(level == 1) {
			if(http_status(GET(str_c('https://are.na/', origin)))$category != 'Success' | origin %in% c('', 'explore', 'feed', 'tools', 'about', 'blog', 'pricing')) {
				message(str_c('no channel with the slug "', origin, '"'))
				pool       = list('failure')
				depth      = 1
				type       = 'all'
				up.initial = FALSE
			}
		}

		if(level <= length(pool)) {
			if(pool[[level]] %>% length > 0) {
				if(up.initial == FALSE & depth == 1) {

					reply <-
						pool[[level]] %>%
						list.map(GetChannel(., 1, type)) %>%
						bind_rows %>%
						mutate(level = level)

				} else {

					reply <-
						 pool[[level]] %>%
						 list.map(GetChannel(., direction, type)) %>%
						 bind_rows %>%
						 mutate(level = level)

				}

				if(!is.null(pop)) {
					reply %<>% filter(!query %in% pop)
				}

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

	web <- list(arrows  = arrows %>% bind_rows %>% unique,
		 objects = objects %>% bind_rows %>% unique)

	objects <-
		data_frame(id = web$objects$slug %>% unique) %>%
		left_join(web$objects, by = c('id' = 'slug')) %>%
		rename(label = title) %>%
		arrange(label)

	arrows <-
			web$arrows %>%
			mutate(id = str_c(source, '_', target)) %>%
			rename(from = source, to = target)

	graph <- graph_from_data_frame(arrows, directed = TRUE, vertices = objects)

	objects$group <- walktrap.community(graph, steps = 6, modularity = TRUE) %>% membership
	objects$value <- betweenness(graph, directed = FALSE, normalized = TRUE)

	objects <-
		objects %>%
		mutate(color = sample(colors, 50)[objects$group]) %>%
		rename(user = user.name) %>%
		mutate(title = str_c(
			'<p class="tooltip-info">user: <a href="https://are.na/', user.slug, '" target="_blank">', user, '</a><br>
			channel: <a href="https://are.na/', id, '" target="_blank">', label, '</a><br>
			id: ', id,'<br>
			type: ', type,'<br>
			blocks: ', length,'<br>
			betweeness: ', round(value, 2),'</p>'
		))

	message('\n[[ done ]]')

	list(origin = origin, objects = objects, arrows = arrows, graph = graph)

}

