library(jsonlite)
library(plyr)
library(dplyr)

editionsInfo_url <- "http://mtgjson.com/json/AllSetsArray.json"
editionsInfo_localFile <- "editions.JSON"
cardsInfo_url <- "http://mtgjson.com/json/AllSets.json"
cardsInfo_localFile <- "cards.JSON"


if (!any(list.files()==editionsInfo_localFile)) download.file(editionsInfo_url, editions_localFile)
if (!any(list.files()==cardsInfo_localFile)) download.file(cardsInfo_url, cards_localFile)


editions <- fromJSON(editionsInfo_localFile)
cards <<- fromJSON(cardsInfo_localFile)

blocks <- unique(editions[,14]); blocks <- c("Core editions","First expansions",blocks[!is.na(blocks)])
blocks <- data.frame(blocks,rep(0,length(blocks))); names(blocks) <- c("block","show")
blocks$block <- as.character(blocks$block)
blocks <<- blocks

cores <- editions[editions$type=="core",c(1:2,7,14)]
cores <- cores[!is.na(cores[,1]),]
cores[,5] <- 0; names(cores)[5] <- "show"
cores <<- cores

expansions <- editions[editions$type=="expansion",c(1:2,7,14)]
expansions <- expansions[!is.na(expansions[,1]),]
expansions[is.na(expansions[,4]),4] <- "First expansions"
expansions[,5] <- 0; names(expansions)[5] <- "show"
expansions <<- expansions

sets <<- NULL

shinyUI(
    
    navbarPage("Magic The gathering explorer",
        
        tabPanel("Sets Selection",
            sidebarLayout(
                    sidebarPanel(
                        checkboxGroupInput("blocks", "Select Blocks", as.character(blocks[,1]), selected = NULL, inline = FALSE, width = NULL)

                    ),                        
                    mainPanel(
                        wellPanel(
                            checkboxGroupInput("cores", "Select Core sets", cores[,2], selected = NULL, inline = TRUE  , width = NULL),
                            checkboxGroupInput("expansions", "Select Expansions", expansions[,2], selected = NULL, inline = TRUE  , width = NULL)
                        ),
                        "Current Selection",
                            verbatimTextOutput("editionsDetails")
                        ## plotOutput("plot")
                    )
           )
        ),
        
        tabPanel("Sets comparison",
                fluidPage(    
                    fluidRow(
                        column(4, wellPanel(
                            selectInput("Characteristic", "Choose Characteristic", choices=c('Converted Mana Cost','Power', 'Toughness'), selected = 'Converted Mana Cost', multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
                        )),
                        column(4, wellPanel(                    
                            selectInput("Types", "Choose types", choices=c('ALL','Creatures','Enchantment', 'Instant', 'Sorcery', 'non-creatures'), selected = 'ALL', multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
                        )),
                        column(4, wellPanel(   
                            selectInput("Color", "Choose color", choices=c('ALL','White','Blue', 'Black', 'Red', 'Green', 'None'), selected = 'ALL', multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
                        ))
                    )
                ),
                    plotOutput("plot"),
                    verbatimTextOutput("summary")
        ),        
        ##navbarMenu("More",
            
            ##tabPanel("Table",
            ##                        DT::dataTableOutput("table")
            ##),
            
            tabPanel("Instructions",
                h1("What is this application about ?"),
                p("This application helps you to compare sets characteristics of Magic The Gathering expansions."),
                "Magic the Gathering is a card game with a deep play game. For more information, you can browse ",
                a(href="http://magic.wizards.com/en/content/new-magic-1","magic website"),
                h1("How to use this application ?"),
                p("First, you need to select the sets you want to be compared. To do so, you need to :"),
                p("- click on the tab panel ",tags$b("Sets selection")),
                p("- then select either expansions or core sets (on the right side) by clicking checkboxes", 
                "You can also select Blocks. All expansions linked to the selected blocked will be selected"),
                p(" "),
                p("Once sets are selected, click on the tab panel ",tags$b("Sets comparison")),
                p("You can then select characteristics you want to compare"),
                h2("Credits"),    
                p("This application is the result of a Course project that you can find on coursera.org : ", a(href="https://www.coursera.org/learn/data-products/home/welcome", "Developing Data Products")),
                p("Code Source can be found at : xxxx")
            )
        ##)
    )
)    