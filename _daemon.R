

message('\n|-- functions')

source('functions/crawl.R')
source('functions/form.R')
source('functions/get.R')
source('functions/plot.R')

source('_server.R')
source('_ui.R')


#-----------------------
message('|-- libraries')
#-----------------------


pacman::p_load(dplyr, tidyr, stringr, magrittr, purrr, rlist, httr, jsonlite, visNetwork, igraph, shiny)


#--------------------
message('|-- daemon')
#--------------------


Spider <- function(){ shinyApp(

	UI(origin, pop, direction, up.initial, depth, type),
	Server

)}


#------------------------------
message('|-- initialization\n')
#------------------------------


# start parameters
origin     = 'research-tactics'
direction  = 1
depth      = 1
type       = 'all'
seed       = 0
up.initial = 1
pop        = 'superchannels, channels-categorizing-other-channels, root'

set.seed(seed)


# palette
colors <- c(
	'#d99aac', '#ffa9c0', '#e2979f', '#ffc6bf', '#ffb18f',
	'#d3a088', '#efb17b', '#ffcc95', '#bba98d', '#ffe2a8',
	'#fff9d3', '#cfce81', '#a1b279', '#dcf3a7', '#ebffc3',
	'#b0d791', '#ccffd5', '#80b88e', '#b7ffd1', '#56bd9c',
	'#7deac8', '#74b8a7', '#3ec1ad', '#53d4c1', '#92ffef',
	'#affff3', '#6ef4e8', '#d1fef9', '#85f8ff', '#a5d1d6',
	'#47e2f6', '#02c0d8', '#a5f0ff', '#6ab6cb', '#4fdcff',
	'#94bcc9', '#82deff', '#b2d5e8', '#76b2dc', '#98d2ff',
	'#8ac7ff', '#75b1e8', '#b6a2d9', '#e3cdff', '#ffdeff',
	'#ca9dc8', '#edabe8', '#f09ccc', '#ffb7de', '#e4bfcb')
