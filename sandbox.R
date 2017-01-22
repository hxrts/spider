

Server <- shinyServer(function(input, output, session) {

	# vis function
	Net <-	function() { visNetwork(
			nodes = data.frame(id = 1:3),
			edges = data.frame(from = c(1,2), to = c(1,3))
			)}

	# output
	output$network_proxy <- renderVisNetwork({ Net() })


	# # observe
	# observer <- observe({

	# 	input$getAll

	# 	visNetworkProxy("network_proxy") %>%
	# 	visGetEdges()

	# 	visNetworkProxy("network_proxy") %>%
	# 	visGetNodes()

	# 	print(input$network_proxy_edges)

	# 	print(input$network_proxy_nodes)

	# })


	# # output
	# output$edges_data_from_shiny <- renderPrint({
	# 	input$network_proxy_edges
	# })

	# output$nodes_data_from_shiny <- renderPrint({
	# 	input$network_proxy_nodes
	# })


	# # cleanup
	# session$onSessionEnded(function() {
	# 	observer$suspend()
	# })


})



UI <- shinyUI(fluidPage(
			actionButton("getAll", "fetch"),
			visNetworkOutput("network_proxy", height = "400px"),
			verbatimTextOutput("edges_data_from_shiny"),
			verbatimTextOutput("nodes_data_from_shiny")
))



Test <- function(){ shinyApp(
	UI, Server
)}




#'test' %>% str_c('https://api.are.na/v2/channels/', ., '/thumb') %>% fromJSON