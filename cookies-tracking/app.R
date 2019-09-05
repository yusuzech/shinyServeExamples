library(shiny)
library(shinyjs)

#user info
user <- data.frame(
    user = c("user1","user2"),
    password = c("pass1","pass2"),
    cookies = NA_character_,
    stringsAsFactors = FALSE
)

# example cookie
genCookie <- function(){
    paste0(sample(as.character(0:9),size = 10,replace = TRUE),collapse = "")
}

# UI for logged in page
ui_loggedin <- function(){
    tagList(
        tags$h5("You are logged in as:"),
        textOutput("username"),
        tags$h5("Your cookie is:"),
        textOutput("usercookie"),
        actionButton("logout","Click to log out")
    )
}

# UI for login page
ui_login <- function(){
    tagList(
        tags$div(
            style = "width: 20%; margin: 0 auto;",
            wellPanel(
                textInput("login","Username",width = "200px"),
                passwordInput("password","Password",width = "200px"),
                actionButton("submit","Login")
            ),
            tableOutput("userinfo")
        )
    )
}

# main UI
ui <- fluidPage(
    tags$head(
        tags$script(src = "js/js.cookie.js")
    ),
    useShinyjs(),
    # js functions to manage cookies
    extendShinyjs(script = "extendJS/cookies.js"),
    htmlOutput(outputId = "page")
)

server <- function(input, output, session) {
    # variable to track session
    sessionStatus <- reactiveValues(
        userIndex = NULL, # row number for the user
        cookie = NULL, # user's cookie
        status = "out", # login in status, can be in/out
        user = NULL, # user name
        page = NULL # current UI(page)
    )
    
    # detect if user has cookie and send different UI.
    observe({
        js$getcookie()
        req(sessionStatus$status == "out")
        req(!is.null(input$jscookie))
        isolate({
            userCookieIndex <- which(input$jscookie == user$cookies)
            if (length(userCookieIndex) > 0) {
                # send login UI
                sessionStatus$userIndex <- userCookieIndex
                sessionStatus$page <- "loggedin"
            } else {
                sessionStatus$page <- "login"
            }
        })
    })
    
    
    # when login page is triggered
    observe({
        req(sessionStatus$page == "login")
        isolate({
            output$page <- renderUI(ui_login())
            output$userinfo <- renderTable(user)
        })
    })
    
    
    # when loggedin page is triggered
    observe({
        req(sessionStatus$page == "loggedin")
        isolate({
            userIndex <- sessionStatus$userIndex
            sessionStatus$cookie <- user$cookies[userIndex]
            sessionStatus$user <- user$user[userIndex]
            sessionStatus$status <- "in"
            js$setcookie(sessionStatus$cookie) # refresh cookie
            output$page <- renderUI(ui_loggedin())
            output$username <- renderText(sessionStatus$user)
            output$usercookie <- renderText(sessionStatus$cookie)
        })
    })
    
    
    # login page ----
    observeEvent(input$submit,{
        userIndex <- which(input$login == user$user)
        passwordIndex <- which(input$password == user$password)
        if(length(userIndex) > 0 & length(passwordIndex) > 0){
            if(userIndex == passwordIndex){
                currentCookie <- genCookie()
                js$setcookie(currentCookie)
                user[[userIndex,"cookies"]] <<- currentCookie
                # send login UI'
                sessionStatus$userIndex <- userIndex
                sessionStatus$page <- "loggedin"
            } else {
                shinyjs::alert("wrong username or password")
            }
        } else {
            shinyjs::alert("wrong username or password")
        }
    })
    
    # logout
    observeEvent(input$logout,{
        js$rmcookie()
        sessionStatus$page <- "login"
    })
    
    
    
}

shinyApp(ui, server)