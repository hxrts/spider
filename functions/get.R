

message('  - get')


#----------
# functions
#----------


GetChannel <- function(query, direction = 0, type = 'all', private = NULL) {  # query Are.na for channel information

	message(query)

	identity = contents = connections = NULL

	if(type == 'all') {
		type <- c('public', 'closed')
		if(!is.null(private)) {
			type <- c(type, 'private')
		}
	}

	if(direction == 0) {
		direction = c('identity', 'parent', 'child')
	} else if(direction == 1) {
		direction = c('identity', 'child')
	} else {
		direction = c('identity', 'parent')
	}

	try({

		if(depth > 3) { Sys.sleep(0.1) }

		# identity
		if(!is.null(private)) {
			identity.req <-
				tryCatch({
					str_c('https://api.are.na/v2/channels/', query, '/thumb') %>% GET(add_headers(Authorization = str_c('bearer ', private))) %>% content(type = 'text', encoding = 'UTF-8') %>% fromJSON
				}, warning = function(w) {
					Sys.sleep(4)
					str_c('https://api.are.na/v2/channels/', query, '/thumb') %>% GET(add_headers(Authorization = str_c('bearer ', private))) %>% content(type = 'text', encoding = 'UTF-8') %>% fromJSON
				}, error = function(e) {
					Sys.sleep(4)
					str_c('https://api.are.na/v2/channels/', query, '/thumb') %>% GET(add_headers(Authorization = str_c('bearer ', private))) %>% content(type = 'text', encoding = 'UTF-8') %>% fromJSON
				})
		} else {
			identity.req <-
				tryCatch({
					str_c('https://api.are.na/v2/channels/', query, '/thumb') %>% fromJSON
				}, warning = function(w) {
					Sys.sleep(4)
					str_c('https://api.are.na/v2/channels/', query, '/thumb') %>% fromJSON
				}, error = function(e) {
					Sys.sleep(4)
					str_c('https://api.are.na/v2/channels/', query, '/thumb') %>% fromJSON
				})
		}


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

		if(depth > 3) { Sys.sleep(0.3) }

		# contents
		contents = data_frame(slug = '')

		if(!is.null(private)) {
			contents.req <-
				tryCatch({
					str_c('https://api.are.na/v2/channels/', query, '/contents') %>% GET(add_headers(Authorization = str_c('bearer ', private))) %>% content(type = 'text', encoding = 'UTF-8') %>% fromJSON
				}, warning = function(w) {
					Sys.sleep(4)
					str_c('https://api.are.na/v2/channels/', query, '/contents') %>% GET(add_headers(Authorization = str_c('bearer ', private))) %>% content(type = 'text', encoding = 'UTF-8') %>% fromJSON
				}, error = function(e) {
					Sys.sleep(4)
					str_c('https://api.are.na/v2/channels/', query, '/contents') %>% GET(add_headers(Authorization = str_c('bearer ', private))) %>% content(type = 'text', encoding = 'UTF-8') %>% fromJSON
				})
		} else {
			contents.req <-
				tryCatch({
					str_c('https://api.are.na/v2/channels/', query, '/contents') %>% fromJSON
				}, warning = function(w) {
					Sys.sleep(4)
					str_c('https://api.are.na/v2/channels/', query, '/contents') %>% fromJSON
				}, error = function(e) {
					Sys.sleep(4)
					str_c('https://api.are.na/v2/channels/', query, '/contents') %>% fromJSON
				})
		}

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

		Sys.sleep(0.3)

		# connections
		if(!is.null(private)) {
			connections.req <-
				tryCatch({
					str_c('https://api.are.na/v2/channels/', query, '/connections') %>% GET(add_headers(Authorization = str_c('bearer ', private))) %>% content(type = 'text', encoding = 'UTF-8') %>% fromJSON
				}, warning = function(w) {
					Sys.sleep(4)
					str_c('https://api.are.na/v2/channels/', query, '/connections') %>% GET(add_headers(Authorization = str_c('bearer ', private))) %>% content(type = 'text', encoding = 'UTF-8') %>% fromJSON
				}, error = function(e) {
					Sys.sleep(4)
					str_c('https://api.are.na/v2/channels/', query, '/connections') %>% GET(add_headers(Authorization = str_c('bearer ', private))) %>% content(type = 'text', encoding = 'UTF-8') %>% fromJSON
				})
		} else {
			connections.req <-
				tryCatch({
					str_c('https://api.are.na/v2/channels/', query, '/connections') %>% fromJSON
				}, warning = function(w) {
					Sys.sleep(4)
					str_c('https://api.are.na/v2/channels/', query, '/connections') %>% fromJSON
				}, error = function(e) {
					Sys.sleep(4)
					str_c('https://api.are.na/v2/channels/', query, '/connections') %>% fromJSON
				})
		}


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

	})

	bind_rows(identity, contents, connections) %>%
	filter(status %in% type | hierarchy == 'identity') %>%
	mutate(hierarchy = ifelse(hierarchy == 'identity', hierarchy, ifelse(slug %in% contents$slug, 'child', 'parent'))) %>%
	filter(hierarchy %in% direction) %>%
	unique

}

