#----------
# libraries
#----------


pacman::p_load( dplyr, tidyr, magrittr, stringr, purrr, rlist,
                jsonlite, reshape2, igraph, RColorBrewer )


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

        contents <-
          contents.req$contents %>%
          .[,names(.) %in% c('title', 'slug', 'status', 'class', 'length')] %>%
          mutate(user.name = contents.req$contents %$% user %$% `username`) %>%
          mutate(user.slug = contents.req$contents %$% user %$% `slug`) %>%
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

        connections <-
          connections.req$channels %>%
          .[,names(.) %in% c('title', 'slug', 'status', 'class', 'length')] %>%
          mutate(user.name = connections.req$channels %$% user %$% `username`) %>%
          mutate(user.slug = connections.req$channels %$% user %$% `slug`) %>%
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


Graph <- function(web, lay, cluster, file, width = 15, height = 15) {

  graph <-
      web$arrows %>%
      graph.data.frame(directed=TRUE)

  # node sizing
  pr <- page.rank(graph)$vector

  # choose layout type
  if(lay == 'fr') {
    layout <-
        layout_with_fr( graph,
                        start.temp = 20,
                        niter = 2000 )
  }

  if(lay == 'drl') {
    layout <-
        layout_with_drl( graph,
                         options = list(cooldown.attraction = 0.1, init.damping.mult = 2) )
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

  # palette
  colors <- mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)) %>% unlist

  # draw plot
  pdf(file = str_c(file, '.pdf'), width = width, height = height)
      plot( graph,
            layout              = layout,
            vertex.label.dist   = 0.01,
            vertex.size         = Mapper(pr, c(1, 10)),
            vertex.color        = colors[mem],
            vertex.frame.color  = NA,
            vertex.label.color  = 'black',
            vertex.label.family = 'Helvetica',
            vertex.label.cex    = 0.5,
            edge.arrow.size     = 0.005,
            edge.width          = 0.5,
            edge.color          = '#d9d9d9' )
  dev.off()

}


#----------
# variables
#----------


origin    = 'root'     # entry channel
depth     = 11         # recursive depth
direction = 1          # -1 = move up channel hierarchy | 0 = move up & down | 1 = move down
type      = 'all'      # public | closed | all
lay       = 'fr'       # layout type: fr | drl | kk | mds
cluster   = 'walktrap' # walktrap | betweenness 
seed      = 0          # graph layout seed
width     = 30
height    = 30


#-------------
# static graph
#-------------


set.seed(seed)

web <- Crawl(origin, depth, direction, type)

Graph(web, lay, file = origin, width, height)



#----------------------------
# export csv for graphcommons
#----------------------------


# Edges
`From Type` `From Name` `Edge`  `To Type` `To Name` `Weight`

edges <-
  web$arrows %>%
  mutate(`From Type` = 'Channel', `Edge` = 'Contains', `To Type` = 'Channel', `Weight` = '') %>%
  select( `From Type`,
          `From Name` = source,
          `Edge`,
          `To Type`,
          `To Name`   = target,
          `Weight` )

edges %>% write_csv(path = str_c(origin, '_edges.csv'))


# Nodes
nodes <-
  web$objects %>%
  mutate(`Type` = 'Channel', `Description` = '') %>%
  select( `Type`,
          `Name`         = title,
          `Description`,
          `Length`       = slug,
          `User Name`    = user.name,
          `User Slug`    = user.slug )

nodes %>% write_csv(path = str_c(origin, '_nodes.csv'))
