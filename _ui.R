

message('|-- ui')


UI <- function(origin, pop, direction, up.initial, depth, type) {

	fluidPage(

		includeCSS('style.css'),
		headerPanel('spider'),
		fluidRow(

			tags$head(tags$link(rel = 'shortcut icon', href = 'https://rawgit.com/hxrts/spider/master/favicon.png')),

			column(6, wellPanel(
				textInput(
				inputId = 'origin',
				label = 'Origin channel ID  ·  Are.na/sam-hart/research-tactics',
				value = origin,
				placeholder = origin))),

			column(6, wellPanel(textInput(
				inputId = 'pop',
				label = 'Prune channels  ·  IDs comma separated',
				value = pop,
				placeholder = pop))),

			column(2, wellPanel(selectInput(
				inputId = 'direction',
				label = 'Direction',
				choices = c('down', 'up', 'down & up'),
				multiple = FALSE))),

			column(2, wellPanel(selectInput(
				inputId = 'up.initial',
				label = 'Origin crawl up',
				choices = c('yes', 'no'),
				multiple = FALSE))),

			column(2, wellPanel(selectInput(
				inputId = 'depth',
				label = 'Depth', choices = 1:4,
				multiple = FALSE))),

			column(2, wellPanel(selectInput(
				inputId = 'type',
				label = 'Type',
				choices = c('all', 'public', 'closed'),
				multiple = FALSE))),

			column(2, wellPanel(selectInput(
				inputId = 'private',
				label = 'Include private',
				choices = c('yes', 'no'),
				multiple = FALSE))),

			column(1, wellPanel(actionButton('buildGraph', 'Build'))),

			column(1, wellPanel(actionButton('clearGraph', 'Clear'))),

			column(12, visNetworkOutput('network_proxy', width = '100%', height = '1200px'))
		)
	)
}

