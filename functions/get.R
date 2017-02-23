

message('  - get')


#----------
# functions
#----------


GetChannel <- function(query, direction = 0, type = 'all') {  # query Are.na for channel information

	message(query)

	identity = contents = connections = NULL

	if(type == 'all') { type <- c('public', 'closed') }

	if(direction == 0) {
		direction = c('identity', 'parent', 'child')
	} else if(direction == 1) {
		direction = c('identity', 'child')
	} else {
		direction = c('identity', 'parent')
	}

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
		contents = data_frame(slug = '')

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

		# connections
		connections.req <-
			str_c('https://api.are.na/v2/channels/', query, '/connections') %>%
			fromJSON

		if(connections.req$channels %>% length > 0) {

			Sys.sleep(0.2)

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

