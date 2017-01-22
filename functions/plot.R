

message('  - plot')


PlotGraph <- function(objects, arrows, origin) {
	visNetwork(
			objects %>% unique,
			arrows %>% unique,
			main = list(text	    = 'Are.na / ',
						style	    = 'display:     inline-block;
									   font-family: Arial;
									   color:       #9d9d9d;
									   font-weight: bold;
									   font-size:   20px;
									   text-align:  left'),
			submain   = list(text   = str_c('<a style="color:#585858 !important; text-decoration: none;" href="https://are.na/',
									         origin, '" target=_blank>',
									         objects %>% filter(id == origin) %$% label,
									         '</a><br>'),
							 style  = 'margin-left: 0.3em;
									   display:     inline;
									   font-family: Arial;
									   font-weight: bold;
									   font-size:   20px;
									   text-align:  left'),
			width  = '100%',
			height = '800px'
		) %>%
		visLayout(improvedLayout = TRUE) %>%
		# visIgraphLayout(
		# 	#layout       = 'layout_nicely',
		# 	layout       = 'layout_with_fr',
		# 	physics      = FALSE,
		# 	smooth       = FALSE,
		# 	type         = 'full',
		# 	randomSeed   = NULL,
		# 	layoutMatrix = NULL
		# ) %>%
		visNodes(
			font = '20px arial #333',
			shape = 'square',
			shadow = FALSE,
			size = 10,
			scaling = list(min = 3, max = 30),
			borderWidthSelected = 0,
			labelHighlightBold = TRUE
		) %>%
		visEdges(
			shadow = FALSE,
			arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
			selectionWidth = 1,
			hoverWidth = 1
		) %>%
		visOptions(
			selectedBy		 = list(variable  = 'user'),
			highlightNearest = list(enabled   = TRUE,
									degree	  = 1,
									hover	  = TRUE),
			nodesIdSelection = list(useLabels = TRUE)
		) %>%
		visInteraction(
			tooltipStyle =
				'position:             fixed;
				 visibility:           hidden;
				 padding:              8px 12px 1px 12px;
				 white-space:          nowrap;
				 font-family:          Arial;
				 font-size:            14px;
				 color:                #333;
				 background-color:     white;
				 webkit-border-radius: 0;
				 border-radius:        0;
				 border:               solid 2px #cbcbcb',
			hover = TRUE)
}

