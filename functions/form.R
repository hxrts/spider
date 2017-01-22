

message('  - form')


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
	select(title, slug, user.name, user.slug, length, type = status)

}

