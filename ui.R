library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)
library(shiny)
library(shinythemes)
library(readxl)
library(readr)
library(shinydashboard)
library(shinydisconnect)

panel_div <- function(class_type, content) {
    div(class = sprintf("panel panel-%s", class_type),
        div(class = "panel-body", content)
    )
}

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

ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage("IT Job Finder",
                           
                           tabPanel("Home",
                                    
                                    # Input values
                                    
                                    fluidRow(
                                        HTML("<section class='banner'>
                                     <h2 class='title'><center>IT Job Finder</center></h2>
                                     <p class='title_description'><center>A job finder and career path pinpoints your next job, the job
                                     after that, and beyond.</center></p>
                                     </section>"
                                        )
                                    ),
                                    
                                    fluidRow(
                                        column(3),
                                        column(6,
                                               shiny::HTML("<br><br><center> <h1>What you will find here</h1> </center><br>"),
                                               shiny::HTML("<h5>With our understanding and insights on IT market, as well our passion for technology, we provide jobseekers
                                               the best opportunities and deliver exceptional value to employers. With information about the salary differences, and more, you can 
                                                   build your own path based on what is meaningful to you.</h5>")
                                        ),
                                        column(3)
                                    ),
                                    
                                    fluidRow(
                                        
                                        style = "height:50px;")
                                    
                           ), #tabPanel(), Home
                   
                   tabPanel("Job Finder", value = "careerPF",
                            
                            sidebarLayout( 
                                
                                sidebarPanel( width = 3,
                                              introjsUI(),
                                              
                                              tags$div(
                                                  actionButton("help", "Take a Quick Tour"),
                                                  style = "height:50px;"
                                              ),
                                              useShinyjs(),
                                              
                                              tags$div(
                                                  style = "height:2000px;",
                                                  introBox(
                                                      tags$div(
                                                          style = "height:50px;",
                                                          actionLink("settings", "Settings", 
                                                                     icon = icon("sliders", class = "fa-2x"))),
                                                      data.step = 6,
                                                      data.intro = "Settings is where you can set options that affect the graph and career statistics."
                                                  ),
                                                  radioButtons("selectData", 
                                                               label = "Which data do you want to include?",
                                                               choices = c("Job Street",
                                                                           "Mau Kerja"),
                                                               inline = TRUE,
                                                               width = "100%"
                                                  ),
                                                  selectizeInput("changeAvatar", "Change Icon:",
                                                                 choices = c(
                                                                     "Map Marker" = "map-marker", 
                                                                     "Rocket" = "rocket"),
                                                                 selected = "map-marker"
                                                  ),
                                                  textInput("userName", "Add your name:", value = ""),
                                                  
                                                  # CV form
                                                  shinyjs::useShinyjs(),
                                                  shinyjs::inlineCSS(appCSS),
                                                  shiny::HTML("<h6>Fill up this form as your Curriculum Vitae</h6>"),
                                                  div(
                                                    id = "form",
                                                    
                                                    textInput("name", labelMandatory("Name"), ""),
                                                    textInput("address", "Address"),
                                                    textInput("email", labelMandatory("Email"), ""),
                                                    textInput("contactnum", labelMandatory("Phone number"), ""),
                                                    checkboxInput("jobseeker", "I'm looking for a job/internship", FALSE),
                                                    selectInput("highestedu", "Highest Education Level",
                                                                c("",  "Certificate/Associate", "Bachelors", "Doctorate", "Masters")),
                                                    sliderInput("cgpa", "Current CGPA", min = 0, max = 4, value = 0.5, step = 0.1 , ticks = FALSE),
                                                    actionButton("submit", "Submit", class = "btn-primary"),
                                                    shinyjs::hidden(
                                                      span(id = "submit_msg", "Submitting..."),
                                                      div(id = "error",
                                                          div(br(), tags$b("Error: "), span(id = "error_msg"))
                                                      )
                                                    )
                                                  ),
                                                  shinyjs::hidden(
                                                    div(
                                                      id = "thankyou_msg",
                                                      h3("Thanks, your CV was submitted successfully!"),
                                                      actionLink("submit_another", "Submit another response")
                                                    )
                                                  ),
                                                  
                                                  tags$div(
                                                      style = "height:100px;",
                                                      
                                                      uiOutput("printInput1"),
                                                      uiOutput("printInput2"),
                                                      uiOutput("printInput3"),
                                                      uiOutput("printInput4"),
                                                      uiOutput("printInput5")
                                                  )
                                              )
                                ),  # Closes sidebarPanel
                                mainPanel( width = 8,
                                           fluidRow(
                                               
                                               tags$style(type="text/css",
                                                          ".shiny-output-error { visibility: hidden; }",
                                                          ".shiny-output-error:before { visibility: hidden; }"
                                               ),
                                               introBox(
                                                   panel_div(class_type = "default",
                                                             content = tags$div(
                                                                 uiOutput("displayName"),
                                                                 visNetwork::visNetworkOutput("visTest", height = "200px")
                                                             )
                                                   ),
                                                   data.step = 4,
                                                   data.intro = "Your selections will be displayed here in a graph."
                                               )
                                           ),
                                           fluidRow(
                                               div(class="panel panel-default",
                                                   div(class="panel-body",  width = "600px",
                                                       tags$div(class = "wrap",
                                                                div(class = "left", 
                                                                    style="display: inline-block;vertical-align:top; width: 150px;",
                                                                    uiOutput("stepNo")
                                                                ),
                                                               
                                                                div(class = "center",
                                                                    style="display: inline-block;vertical-align:top; width: 150px;",
                                                                    introBox(
                                                                        actionButton("goBack", 
                                                                                     label = "Back", 
                                                                                     icon = icon("arrow-circle-left", class = "fa-2x"),
                                                                                     width= "100px", height= "40px"),
                                                                        data.step = 3,
                                                                        data.intro = "Go back a step to edit your selection anytime."
                                                                    )
                                                                ),
                                                                   
                                                                div(class = "center",
                                                                    style="display: inline-block;vertical-align:top; width: 150px;",
                                                                    introBox(
                                                                        actionButton("btn1", 
                                                                                     label = "Add", 
                                                                                     icon = icon("arrow-circle-right", class = "fa-2x"),
                                                                                     width= "100px", height= "40px"),
                                                                        data.step = 2,
                                                                        data.intro = "Confirm your selection by clicking here."
                                                                    )
                                                                )
                                                       ),
                                                       # Insert Table Output
                                                       introBox(
                                                           uiOutput("btns"),
                                                           data.step = 1, 
                                                           data.intro = "Start by selecting current job offered."
                                                       )
                                                   )
                                               ),
                                               plotOutput("myplot")
                                           )
                                )  # Closes the mainPanel
                            )  # Closes the sidebarLayout
                   ),  
                   
                   tabPanel("About Us", value = "about",
                            
                            fluidRow(
                                shiny::HTML("<br><center> 
                                            <h1>About IT Job Finder</h1>
                                            </center>"),
                                style = "height:100px;"),
    
                            # About Us
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h3>About the team</h3> </center><br>"),
                                       shiny::HTML("<h4>Our team analyzed over 500 employee records on computer science major from many popular job finder website and transformed them into the information 
                                                   you will see in the IT Job Finder.</h4>")
                                ),
                                column(3)
                            ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            fluidRow(
                                column(2),
                                
                                # Anis
                                column(2,
                                       div(class="panel panel-default", 
                                           div(class="panel-body",  width = "600px",
                                               align = "center",
                                               div(
                                                   tags$img(src = "anis.jpg", 
                                                            width = "80px", height = "140px")
                                               ),
                                               div(
                                                   tags$h5("Anis Zulaikha"),
                                                   tags$h6( tags$i("Project Lead"))
                                               )
                                            
                                           )
                                       )
                                ),
                                # Mahirah
                                column(2,
                                       div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px", 
                                               align = "center",
                                               div(
                                                   tags$img(src = "mahirah.jpeg", 
                                                            width = "80px", height = "130px")
                                               ),
                                               div(
                                                   tags$h5("Nur Mahirah"),
                                                   tags$h6( tags$i("Executive Officer"))
                                               )
                                               
                                           )
                                       )
                                ),
                                # Ain
                                column(2,
                                       div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px", 
                                               align = "center",
                                               div(
                                                   tags$img(src = "ain.jpg", 
                                                            width = "80px", height = "130px")
                                                   ),
                                               div(
                                                   tags$h5("Ain Safirah"),
                                                   tags$h6( tags$i("Chief Technology Officer"))
                                               )
                                              
                                           )
                                       )
                                ),
                                
                                # Amira
                                column(2,
                                       div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px", 
                                               align = "center",
                                               div(
                                                   tags$img(src = "amira.jpeg", 
                                                            width = "80px", height = "130px")
                                                   ),
                                               div(
                                                   tags$h5("Amira Hanee"),
                                                   tags$h6( tags$i("Chief Product Officer"))
                                               )
                                              
                                           )
                                       )
                                ),
                                column(3)
                                
                            ),
                            fluidRow(style = "height:150px;")
                   )  # Closes About tab
                   
),

)
