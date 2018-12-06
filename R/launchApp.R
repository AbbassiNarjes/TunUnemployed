#' @title  launchApp
#'
#' @description  this function launch an R shiny application to help you explor a dataset about the Unemployed population in Tunisia
#'
#'
#' @example
#'
#' launchApp()
#'
#' @export launchApp
library(shiny)
launchApp=function(){
  library(shiny)

  shinyApp( ui=ui <-tagList(
    #shinythemes::themeSelector(),
    navbarPage(
      theme = shinythemes::shinytheme("cerulean"),
      "the Unemployed population in Tunisia ",
      tabPanel("Exploring Dataset  ",

               # Show a plot of the generated distribution
               mainPanel(
                 tableOutput("tab")
               )),
      tabPanel("variable correlation  ",
               sidebarPanel(
                 tableOutput("tabl")
               ),
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("corril")
               ))
      ,
      tabPanel("Exploring variables ",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("checkGroup", label = h3("choose your variables "),
                                      choices = c(colnames(unem[,-c(1,2)])),
                                      selected = c(colnames(unem[,c(5,6,7)]))
                   )
                 ),

                 # Show a plot of the generated distribution
                 mainPanel(
                   htmlOutput("page1plot")
                 ))
      ),
      tabPanel("crosing  variables ",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("var1", label = h3("choose your variables "),
                                      choices = c(colnames(baseG[,-c(1,2,3,ncol(baseG))])),
                                      selected = c(colnames(baseG[,c(2,13)])))
                 )
                 ,

                 # Show a plot of the generated distribution
                 mainPanel(
                   plotOutput("croi")
                 ))
      )
    )
  ),server = server <- function(input, output) {
    library(shiny)
    library(shinythemes)

    library(googleVis)
    library(corrplot)



    x=unem[,-c(1,2)]



    colnames(x)=as.character(1:40)
    m=cor(x)
    output$tab=renderTable(unem)
    output$page1plot=renderGvis({ gvisLineChart(unem,xvar="Regions",yvar =input$checkGroup,options=list(width=1250, height=700,titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}", bar="{groupWidth:'100%'}", hAxis="{format:'#,##%'}"))
    })
    output$corril=renderPlot({corrplot(m, order = "hclust",addrect = 3)})
    k=1:40
    k1=colnames(unem[,-c(1,2)])
    c =cbind(k,k1)
    colnames(c)=c("number","variable ")
    output$tabl=renderTable(c)
    library(GGally)
    output$croi=renderPlot({ggpairs(data=baseG,
                                    columns=input$var1,
                                    upper = list(continuous = "density"),
                                    lower = list(combo = "facetdensity")
    )})

  }





  )



}
