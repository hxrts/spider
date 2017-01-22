

message('|-- ui')


UI <- function(origin, pop, direction, up.initial, depth, type) {

	fluidPage(

		includeCSS('style.css'),
		headerPanel('spider'),
		fluidRow(

			column(6, wellPanel(textInput(inputId = 'origin', label = 'origin channel ID, https://are.na/user/channel → channel', value = origin, placeholder = origin))),

			column(6, wellPanel(textInput(inputId = 'pop',    label = 'prune channels, comma separated IDs',               value = pop,    placeholder = pop))),

			column(2, wellPanel(selectInput(inputId = 'direction',  label = 'direction',       choices = c('down', 'up', 'down & up'), multiple = FALSE))),

			column(2, wellPanel(selectInput(inputId = 'up.initial', label = 'origin crawl up', choices = c('yes', 'no'),               multiple = FALSE))),

			column(2, wellPanel(selectInput(inputId = 'depth', label = 'depth', choices = 1:4,                         multiple = FALSE))),

			column(2, wellPanel(selectInput(inputId = 'type',  label = 'type',  choices = c('all', 'public', 'closed'), multiple = FALSE))),

			column(1, wellPanel(actionButton('buildGraph', 'build'))),

			column(1, wellPanel(actionButton('clearGraph', 'clear'))),

			column(12, visNetworkOutput('network_proxy', width = '100%', height = '1000px'))
		)
	)
}

