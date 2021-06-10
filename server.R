selections <- vector(mode = "character", length = 0)
edgeLabels <- vector(mode = "character", length = 0)
tips <- vector(mode = "character", length = 0)

# Initialize empty data.frames for nodes and edges
nodes <- data.frame(id = integer(), label = character(), title = character(), 
                    shape = character(), icon.face = character(), icon.code = character(), 
                    color = character(), stringsAsFactors = FALSE)

# Initialize edges data
edges <- data.frame(from = numeric(), to = numeric(), length = numeric())

# Load all datasets
load("./data/item_pairs_1.rda") 
load("./data/item_pairs_2.rda")
load("./data/ref.rda")
item_ref <- ref

fieldsMandatory <- c("name", "email", "contactnum")

labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

appCSS <- ".mandatory_star { color: red; } #error { color: red; }"


# Database

fieldsAll <- c("name", "address", "email", "contactnum", "jobseeker", "highestedu", "cgpa")
responsesDir <- file.path("responses")
epochTime <- function() {
    as.integer(Sys.time())
}

shinyServer(function(input, output, session) {
    
    observeEvent(input$disconnect, {
        session$close()
    })
    
    # Navbar ------------------------------------------------------------------
    shinyjs::addClass(id = "navBar", class = "navbar-right")
 
    # Intro JS ----------------------------------------------------------------
    observeEvent(input$help,
                 introjs(session, options = list("nextLabel"="Next",
                                                 "prevLabel"="Back",
                                                 "skipLabel"="Exit"))
    )
    
    # Select dataset -----------------------------------------------------------
    item_pairs <- reactive({
        switch(input$selectData,
               "Mau Kerja" = item_pairs_2,
               "Job Street" = item_pairs_1)
    })
    
    # Initialize a variable to count how many times "btn1" is clicked.
    values <- reactiveValues(data = 1) 
    
    # Btn1 ---------------------------------------------------------------------
    # React to "btn1" being pressed by adding 1 to the values$data variable
    observeEvent( input$btn1, {
        if ( input$item_name == "" ) {
            showModal(modalDialog(title = "Pick a starting job first.",
                                  "It looks like you forgot to select a starting job. Please select a job from the drop-down
                                  menu to begin.",
                                  easyClose = FALSE, size = "s" ))
        } else { 
            values$data = values$data + 1 }
        
    })
    
    # Go Back Button -----------------------------------------------------------
    
    observeEvent( input$goBack, {
        
        if (values$data <= 5) {
            enable("btn1")
        }
        
        if( values$data == 5 & !is.null(input$select5_rows_selected) ) {
            showModal(
                modalDialog("Please remove your selection before going back.", size = "s",
                            title = "Oops!",
                            footer = modalButton(label = "", icon = icon("close")))
            )
        } else if ( values$data == 4 & !is.null(input$select4_rows_selected) ) {
            showModal(
                modalDialog("Please remove your selection before going back.", size = "s",
                            title = "Oops!",
                            footer = modalButton(label = "", icon = icon("close")))
            )
        } else if ( values$data == 3 & !is.null(input$select3_rows_selected) ) {
            showModal(
                modalDialog("Please remove your selection before going back.", size = "s",
                            title = "Oops!",
                            footer = modalButton(label = "", icon = icon("close")))
            )
        } else if ( values$data == 2 & !is.null(input$select2_rows_selected) ) {
            showModal(
                modalDialog("Please remove your selection before going back.", size = "s",
                            title = "Oops!",
                            footer = modalButton(label = "", icon = icon("close")))
            )
        } else {
            values$data = values$data - 1
        }
    })
    
    # Disable btn1 when step 5 is reached
    useShinyjs()
    observeEvent( input$btn1, {
        if( values$data == 5 )
            shinyjs::disable("btn1")
    })
    
    # Disable goBack button at start of session
    observe( 
        if(values$data == 1){
            disable("goBack")
        } else {
            enable("goBack")    
        }
    )
    
    # Show/Hide Settings -----------------------------------------------------------------
    # Hide settings at start of new Shiny session
    observe(c(hide("selectData"),
              hide("changeAvatar"),
              hide("userName")
    ))
    
    # Toggle visibility of settings
    observeEvent(input$settings, {
        shinyjs::toggle("selectData", anim = TRUE)  # toggle is a shinyjs function
        shinyjs::toggle("changeAvatar", anim = TRUE)
        shinyjs::toggle("userName", anim = TRUE)
    })
    
    # Determine which 'select' options to display (Input choices)
    output$btns <- renderUI({
        if (values$data == 0) {
            return()
        } else if (values$data == 1) {
            uiOutput("select1")
        } else if (values$data == 2) {
            dataTableOutput("select2")
        } else if (values$data == 3) {
            dataTableOutput("select3")
        } else if (values$data == 4) {
            dataTableOutput("select4")
        } else if (values$data >= 5) {
            dataTableOutput("select5")
        } 
    })
    
    # Start Button -------------------------------------------------------------
    observeEvent(input$startBtn, {
        updateNavbarPage(session, "navBar",
                         selected = "careerPF"
        )
    })
    
    # Select Input (First Job) -------------------------------------------------
    output$select1 <- renderUI({
        selectizeInput("item_name", label = "",
                       choices = item_ref$TitleLong,
                       width = "100%",
                       options = list(
                           placeholder = 'Start your path by choosing from one of our jobs.',
                           onInitialize = I('function() { this.setValue(""); }'))
        )
    })
    
    # Table Inputs (Next 2-5 Selections) ---------------------------------------
    
    # Table 1 (Step 2)
    # eventReactive( input$item_name,
    top1 <- reactive({
        
        top <- dplyr::filter(item_pairs(), Item1Name == input$item_name) %>%
            select(Item2Name, Item2, Salary2Min, SalaryDiff, Hyperlink)
    })
    
    output$select2 <- DT::renderDataTable({
        datatable( top1(), escape = FALSE,
                   extensions = 'Responsive',
                   selection = list(mode = 'single', target = 'row'),
                   colnames = c("Title", "Job Code", "Starting Salary", "Max Salary Difference", "Job Description"),
                   rownames = FALSE, style = "bootstrap", 
                   callback = JS("
                                 var tips = ['Classification Title', 'Title Code', 'Percent of employees that moved into that job from your last selected job',
                                 'Starting salary', 'Difference between the highest possible salaries for the selected jobs',
                                 'Number of employees currently holding the title', 'Link to requirements and description'],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 ")
        ) %>%
            formatCurrency('SalaryDiff') %>% 
            formatCurrency("Salary2Min")
    })
    
    proxy1 = dataTableProxy('select2')
    
    
    # Table 2 (Step 3)
    # eventReactive( input$select2_cell_clicked, 
    top2 <- reactive({
        
        itemName <- top1()[ input$select2_rows_selected,  "Item2Name"]
        
        top <- dplyr::filter(item_pairs(), Item1Name == itemName) %>%
            select(Item2Name, Item2, Salary2Min, SalaryDiff, Hyperlink)
        top
    })
    
    output$select3 <- DT::renderDataTable({
        datatable( top2(), escape = FALSE, 
                   extensions = 'Responsive',
                   selection = list(mode = 'single', target = 'row'),
                   colnames = c("Title", "Job Code", "Starting Salary", "Max Salary Difference", "Job Description"),
                   rownames = FALSE, style = "bootstrap", 
                   callback = JS("
                                 var tips = ['Classification Title', 'Title Code', 'Percent of employees that moved into that job from your last selected job',
                                 'Starting salary', 'Difference between the highest possible salaries for the selected jobs',
                                 'Number of employees currently holding the title', 'Link to requirements and description'],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 ")
        ) %>%
            formatCurrency('SalaryDiff') %>% 
            formatCurrency("Salary2Min")
    })
    
    proxy2 = dataTableProxy('select3')
    
    # Table 3 (Step 4)
    top3 <- reactive({
        
        itemName <- top2()[ input$select3_rows_selected,  "Item2Name"]
        
        top <- dplyr::filter(item_pairs(), Item1Name == itemName) %>%
            select(Item2Name, Item2, Salary2Min, SalaryDiff, Hyperlink)
        top
    })
    
    output$select4 <- DT::renderDataTable({
        datatable( top3(), escape = FALSE, 
                   extensions = 'Responsive',
                   selection = list(mode = 'single', target = 'row'),
                   colnames = c("Title", "Job Code", "Starting Salary", "Max Salary Difference", "Job Description"),
                   rownames = FALSE, style = "bootstrap", 
                   callback = JS("
                                 var tips = ['Classification Title', 'Title Code', 'Percent of employees that moved into that job from your last selected job',
                                 'Starting salary', 'Difference between the highest possible salaries for the selected jobs',
                                 'Number of employees currently holding the title', 'Link to requirements and description'],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 ")
        ) %>%
            formatCurrency('SalaryDiff') %>% 
            formatCurrency("Salary2Min")
    })
    
    proxy3 = dataTableProxy('select4')
    
    # Table 4 (Step 5)
    top4 <- reactive({
        
        itemName <- top3()[ input$select4_rows_selected,  "Item2Name"]
        
        top <- dplyr::filter(item_pairs(), Item1Name == itemName) %>%
            select(Item2Name, Item2, Salary2Min, SalaryDiff, Hyperlink)
        top
    })
    
    output$select5 <- DT::renderDataTable({
        datatable( top4(), escape = FALSE, 
                   extensions = 'Responsive',
                   selection = list(mode = 'single', target = 'row'),
                   colnames = c("Title", "Job Code", "Starting Salary", "Max Salary Difference", "Job Description"),
                   rownames = FALSE, style = "bootstrap", 
                   callback = JS("
                                 var tips = ['Classification Title', 'Title Code', 'Percent of employees that moved into that job from your last selected job',
                                 'Starting salary', 'Difference between the highest possible salaries for the selected jobs',
                                 'Number of employees currently holding the title', 'Link to requirements and description'],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 ")
        ) %>%
            formatCurrency('SalaryDiff') %>% 
            formatCurrency("Salary2Min")
    })
    
    
    proxy4 = dataTableProxy('select5')
    
    # User name ----------------------------------------------------------------
    plotTitle <- reactive({
        
        if(input$userName == "") {
            paste("Your Career Path")
        } else {
            paste(input$userName, "'s Career Path", sep = "")
        }
        
    })
    
    output$displayName <- renderUI({
        tags$h4( plotTitle() )
        
    })
    
    # Show the current step -------------------
    output$stepNo <- renderUI({
        if(values$data == 1) {
            tags$h4("Step 1:")
        } else if (values$data == 2) {
            # tags$h4("Step 2:")
            div(tags$h4("Step 2:"), div(tags$h6("Choose from one of the current jobs in the table below")))
        } else if (values$data == 3) {
            tags$h4("Step 3:")
        } else if (values$data == 4) {
            tags$h4("Step 4:")
        } else if (values$data >= 5) {
            tags$h4("Step 5:")
        }
        
    })
    
    # Get selection data for printing ------------------------------------------
    
    job_1_data <- reactive({
        # Obtain stats
        itemNo <- item_ref[ item_ref$TitleLong == input$item_name, "TitleCode"]
        salaryMin <- item_ref[ item_ref$TitleLong == input$item_name, "SalaryMin"]
        salaryMax <- item_ref[ item_ref$TitleLong == input$item_name, "SalaryMax"]
        
        salaryMax <- format(salaryMax, big.mark = ",")
        salaryMax <- paste0("RM", salaryMax)
        
        salaryMin <- format(salaryMin, big.mark = ",")
        salaryMin <- paste0("RM", salaryMin)
        
        v <- c(input$item_name, itemNo, salaryMin, salaryMax)
        
        v
        
    })
    
    # Print each selection to a panel in sidebar
    output$printInput1 <- renderUI({
        # Display if item is selected
        if(input$item_name == ""){
            return()
        } else {
            div(class="panel panel-default",
                div(class="panel-body",
                    div(tags$img(src = "one.svg", width = "25px", height = "25px"), tags$h6( paste0(input$item_name, " (", job_1_data()[2], ")") ),
                        paste0( job_1_data()[3], " - ", job_1_data()[4], " /month")
                    )
                ))
        }
    })
    
    # Create label for output report
    label_1 <- reactive({
        
        lab <- paste0( input$item_name, "\n",
                       job_1_data()[3], " - ", job_1_data()[4], " Monthly")
        
        lab
    })
    
    job_2_data <- reactive({
        # Obtain stats
        itemName <- top1()[ input$select2_rows_selected,  "Item2Name"]
        itemNo <- top1()[ input$select2_rows_selected,  "Item2"]
        salaryMin <- top1()[ input$select2_rows_selected,  "Salary2Min"] 
        salaryMax <- item_ref[ which( itemName == item_ref$TitleLong ), "SalaryMax" ]
        
        salaryMax <- format(salaryMax, big.mark = ",")
        salaryMax <- paste0("RM", salaryMax)
        
        salaryMin <- format(salaryMin, big.mark = ",")
        salaryMin <- paste0("RM", salaryMin)
        
        
        v <- c(itemName, itemNo, salaryMin, salaryMax)
        
        v
    })
    
    output$printInput2 <- renderUI({
        
        # Display if item is selected
        if( is.null(input$select2_rows_selected) ){
            return()
        } else {
            div(class="panel panel-default",
                div(class="panel-body",
                    div(tags$img(src = "two.svg", width = "25px", height = "25px"), tags$h6( paste0(job_2_data()[1], " (", job_2_data()[2], ")") ),
                        paste0( job_2_data()[3], " - ", job_2_data()[4], " /month")
                    )
                ))
        }
    })
    
    label_2 <- reactive({
        
        try(
            paste0( job_2_data()[1], "\n",
                    job_2_data()[3], " - ", job_2_data()[4], " Monthly", "\n"),
            
            TRUE
        )
        
    })
    
    job_3_data <- reactive({
        # Obtain stats
        itemName <- top2()[ input$select3_rows_selected,  "Item2Name"]
        itemNo <- top2()[ input$select3_rows_selected,  "Item2"]
        salaryMin <- top2()[ input$select3_rows_selected,  "Salary2Min"] 
        salaryMax <- item_ref[ which( itemName == item_ref$TitleLong ), "SalaryMax" ]
        
        salaryMax <- format(salaryMax, big.mark = ",")
        salaryMax <- paste0("RM", salaryMax)
        
        salaryMin <- format(salaryMin, big.mark = ",")
        salaryMin <- paste0("RM", salaryMin)
        
        
        v <- c(itemName, itemNo, salaryMin, salaryMax)
        
        v
    })
    
    output$printInput3 <- renderUI({
        
        # Display if item is selected
        if( is.null(input$select3_rows_selected) ){
            return()
        } else {
            div(class="panel panel-default",
                div(class="panel-body",
                    div(tags$img(src = "three.svg", width = "25px", height = "25px"), tags$h6( paste0(job_3_data()[1], " (", job_3_data()[2], ")") ),
                        paste0( job_3_data()[3], " - ", job_3_data()[4], " /month")
                    )
                ))
        }
    })
    
    label_3 <- reactive({
        
        try(
            paste0( job_3_data()[1], "\n",
                    job_3_data()[3], " - ", job_3_data()[4], " Monthly", "\n"),
            TRUE
        )
        
    })
    
    job_4_data <- reactive({
        # Obtain stats
        itemName <- top3()[ input$select4_rows_selected,  "Item2Name"]
        itemNo <- top3()[ input$select4_rows_selected,  "Item2"]
        salaryMin <- top3()[ input$select4_rows_selected,  "Salary2Min"] 
        salaryMax <- item_ref[ which( itemName == item_ref$TitleLong ), "SalaryMax" ]
        
        salaryMax <- format(salaryMax, big.mark = ",")
        salaryMax <- paste0("RM", salaryMax)
        
        salaryMin <- format(salaryMin, big.mark = ",")
        salaryMin <- paste0("RM", salaryMin)
        
        
        v <- c(itemName, itemNo, salaryMin, salaryMax)
        
        v
    })
    
    output$printInput4 <- renderUI({
        
        # Display if item is selected
        if( is.null(input$select4_rows_selected) ){
            return()
        } else {
            div(class="panel panel-default",
                div(class="panel-body",
                    div(tags$img(src = "four.svg", width = "25px", height = "25px"), tags$h6( paste0(job_4_data()[1], " (", job_4_data()[2], ")") ),
                        paste0( job_4_data()[3], " - ", job_4_data()[4], " /month") 
                      
                    )
                ))
        }
    })
    
    label_4 <- reactive({
        try(
            paste0( job_4_data()[1], "\n",
                    job_4_data()[3], " - ", job_4_data()[4], " Monthly", "\n"),
            TRUE
        )
        
    })
    
    job_5_data <- reactive({
        # Obtain stats
        itemName <- top4()[ input$select5_rows_selected,  "Item2Name"]
        itemNo <- top4()[ input$select5_rows_selected,  "Item2"]
        salaryMin <- top4()[ input$select5_rows_selected,  "Salary2Min"] 
        salaryMax <- item_ref[ which( itemName == item_ref$TitleLong ), "SalaryMax" ]
        
        salaryMax <- format(salaryMax, big.mark = ",")
        salaryMax <- paste0("RM", salaryMax)
        
        salaryMin <- format(salaryMin, big.mark = ",")
        salaryMin <- paste0("RM", salaryMin)
        
        
        v <- c(itemName, itemNo, salaryMin, salaryMax)
        
        v
    })
    
    output$printInput5 <- renderUI({
        
        # Display if item is selected
        if( is.null(input$select5_rows_selected) ){
            return()
        } else {
            div(class="panel panel-default",
                div(class="panel-body",
                    div(tags$img(src = "five.svg", width = "25px", height = "25px"), tags$h6( paste0(job_5_data()[1], " (", job_5_data()[2], ")") ),
                        paste0( job_5_data()[3], " - ", job_5_data()[4], " /month")
                    )
                ))
        }
    })
    
    label_5 <- reactive({
        
        try(
            paste0( job_5_data()[1], "\n",
                    job_5_data()[3], " - ", job_5_data()[4], " Monthly", "\n"),
            TRUE
        )
    })
    
    # Visualization ------------------------------------------------------------
    
    # Avatar to use in the visualization
    avatar <- reactive({
        switch(input$changeAvatar,
               "map-marker" = "f041",
               "rocket" = "f135")
    })
    
    colorIcon <- reactive({
        # Automatically change avatar color based on avatar selection
        switch(input$changeAvatar,
               "map-marker" = "#000000",  # Black
               "rocket" = "#f44141"       # Red
               
        )
    })
    
    tip1 <- reactive({
        paste0( "<h6>", job_1_data()[1], "</h6>")
        
    })
    
    visNode <- reactive({
        
        item_name1 <- input$item_name  
        item_name2 <- try( top1()[ input$select2_rows_selected,  "Item2Name"], TRUE ) 
        item_name3 <- try( top2()[ input$select3_rows_selected,  "Item2Name"], TRUE ) 
        item_name4 <- try( top3()[ input$select4_rows_selected,  "Item2Name"], TRUE ) 
        item_name5 <- try( top4()[ input$select5_rows_selected,  "Item2Name"], TRUE ) 
        
        # Collect user selections
        selections <- append(selections,
                             c(item_name1, item_name2, item_name3,
                               item_name4, item_name5))
        
        # tips <- append(tips,
        #                c(tip1(), tip2(), tip3(), tip4(), tip5() ))
        
       
        
        # Add selections to data.frame
        nodes[1:length(selections),2] <- selections
        
        # # Add tips to data.frame
        # nodes[1:length(tips), 3] <- tips
        
        # Add id
        nodes$id <- 1:length(selections)
        
        # Add icons, which requires defining 3 properties
        nodes$shape <- rep("icon", length(selections))
        nodes$icon.face <- rep('fontAwesome', length(selections))
        nodes$icon.code <- rep(avatar(), length(selections))
        # nodes$color <- rep(colorIcon(), length(selections))  
        # Color is now added via icon options in visNodes()
        
        # Keep only the rows that don't have errors
        nodes <- nodes[grep("Error", nodes$label, invert = TRUE),]
        
        # Keep rows that are not NA in Label column
        nodes <- nodes[ !is.na(nodes$label), ]  
        
    })
    
    visEdge <- reactive({
        
        num_selections <- nrow( visNode() )
        
        if ( num_selections > 0)
            for ( i in 1:(num_selections-1) ) {
                edges[i, ] <- c( i, i+1, 200)
            }
        
        edges
    })
    
    # Set the seed (layout) for the graph based on number of nodes in graph
    visSeed <- reactive({
        if( nrow(visNode()) == 1 ) {
            1
        } else if ( nrow(visNode()) == 2 ) {
            6
        } else if ( nrow(visNode()) == 3 ) {
            21
        } else if ( nrow(visNode()) == 4 ) {
            30
        } else if ( nrow(visNode()) == 5 ) {
            5432
        }
    })
    
    # Creating the dynamic graph
    output$visTest <- visNetwork::renderVisNetwork({
        
        # The below uses a different random seed to determine layout based on num of nodes
        
        visNetwork::visNetwork(visNode(), visEdge(), height = "275px", width = "100%") %>%
            addFontAwesome() %>%
            visNetwork::visEdges(dashes = TRUE, shadow = TRUE,
                                 arrows = list(to = list(enabled = TRUE, scaleFactor = 2)),
                                 color = list(color = "#587fb4", highlight = "red")) %>%
            visNodes(shadow = list(enabled = TRUE, size = 15),
                     icon = list( color = colorIcon() )) %>%
            visLayout(randomSeed = visSeed() ) %>%
            visPhysics(solver = "barnesHut", stabilization = list(enabled = FALSE))
    })
    
    # Determine how many plot labels are needed for the output report
    plot_labels <- reactive({
        if( nrow(visNode()) == 1 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
        } else if ( nrow(visNode()) == 2 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
            # Display job title 2
            text(5.5, 42, labels = label_2(), col = "white", pos = 4, cex = 0.75)
        } else if ( nrow(visNode()) == 3 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
            # Display job title 2
            text(5.5, 42, labels = label_2(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 3
            text(5.5, 33, labels = label_3(), col = "white", pos = 4, cex = 0.75)
        } else if ( nrow(visNode()) == 4 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
            # Display job title 2
            text(5.5, 42, labels = label_2(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 3
            text(5.5, 33, labels = label_3(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 4
            text(5.5, 24.5, labels = label_4(), col = "white", pos = 4, cex = 0.75)
        } else if ( nrow(visNode()) == 5 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
            # Display job title 2
            text(5.5, 42, labels = label_2(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 3
            text(5.5, 33, labels = label_3(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 4
            text(5.5, 24.5, labels = label_4(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 5
            text(5.5, 15, labels = label_5(), col = "white", pos = 4, cex = 0.75)
            
        }
    })
    
    observe({
        # check if all mandatory fields have a value
        mandatoryFilled <-
            vapply(fieldsMandatory,
                   function(x) {
                       !is.null(input[[x]]) && input[[x]] != ""
                   },
                   logical(1))
        mandatoryFilled <- all(mandatoryFilled)
        
        # enable/disable the submit button
        shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    formData <- reactive({
        data <- sapply(fieldsAll, function(x) input[[x]])
        data <- c(data, timestamp = epochTime())
        data <- t(data)
        data
    })
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
        shinyjs::disable("submit")
        shinyjs::show("submit_msg")
        shinyjs::hide("error")
        
        tryCatch({
            shinyjs::reset("form")
            shinyjs::hide("form")
            shinyjs::show("thankyou_msg")
        },
        error = function(err) {
            shinyjs::html("error_msg", err$message)
            shinyjs::show(id = "error", anim = TRUE, animType = "fade")
        },
        finally = {
            shinyjs::enable("submit")
            shinyjs::hide("submit_msg")
        })
    })
    observeEvent(input$submit_another, {
        shinyjs::show("form")
        shinyjs::hide("thankyou_msg")
    }) 
})
