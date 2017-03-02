

message('  - crawl')


Crawl <- function(origin, depth = 1, direction = 0, type = 'all', private = NULL, up.initial = TRUE, pop = '') {  # main crawling loop

	pool    = list(origin)
	spent   = list()
	arrows  = list()
	objects = list()
	exit    = 0

	pop %<>% strsplit(',') %>% map(~{ strip(.x) }) %>% unlist %>% list.filter(!. %in% origin)

	if(length(pop) == 0) {
		pop = ''
	}

	# crawl recursion
	for(level in 1:depth) {

		level.msg <- str_c('\n[[ level ', level, ' ]]\n')
		message(level.msg)

		if(level == 1) {
			if(is.null(private)) {
				channel.status <- str_c('https://are.na/', origin[1]) %>% GET %>% http_status %$% category
			} else {
				channel.status <- str_c('https://api.are.na/v2/channels/', origin[1], '/contents') %>% GET(add_headers(Authorization = str_c('bearer ', private))) %>% http_status %$% category
			}
			if(channel.status != 'Success' | origin[1] %in% c('', 'explore', 'feed', 'tools', 'about', 'blog', 'pricing', 'import')) {
				message(str_c('no channel with the slug "', origin[1], '"'))
				pool       = list('failure')
				depth      = 1
				type       = 'all'
				up.initial = FALSE
			}
		}

		if(level <= length(pool)) {
			if(pool[[level]] %>% length > 0) {
				if(up.initial == FALSE & depth == 1 & length(origin) == 1) {

					reply <-
						pool[[level]] %>%
						list.map(GetChannel(., 1, type, private)) %>%
						bind_rows %>%
						mutate(level = level)

				} else {

					queries <- split(pool[[level]], ceiling(seq_along(pool[[level]])/100))

					reply <- queries %>%
					map( ~ {
						sub.reply <-
							.x %>%
							list.map(GetChannel(., direction, type, private)) %>%
							bind_rows %>%
							mutate(level = level)

							if(level > 2) {
								message('\n-- pause --\n') ; Sys.sleep(5)
							}

							sub.reply
					}) %>%
					bind_rows
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
		unique %>%
		group_by(id) %>%
		arrange(desc(length)) %>%
		slice(1) %>%
		ungroup %>%
		arrange(label)

	arrows <-
			web$arrows %>%
			mutate(id = str_c(source, '_', target)) %>%
			rename(from = source, to = target) %>%
			unique

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

