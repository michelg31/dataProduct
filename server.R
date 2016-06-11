setsCards <- function (sets){
    sel <- NULL; as.data.frame(sel)
    for (i in 1:length(sets)){
        sel <- rbind(sel,cbind(sets[i],cards[[sets[i]]]$cards[,c('name','types','cmc', 'colorIdentity', 'power','toughness')]))
    }
    sel$white=0;sel$blue=0; sel$black=0; sel$red=0;sel$green=0; sel$artifact=0

    for (i in 1:nrow(sel)) {
        if(grepl('W', sel$colorIdentity[i])) sel$white[i]=1
        if(grepl('U', sel$colorIdentity[i]) & sel$colorIdentity[i]!='NULL') sel$blue[i]=1
        if(grepl('B', sel$colorIdentity[i])) sel$black[i]=1
        if(grepl('R', sel$colorIdentity[i])) sel$red[i]=1
        if(grepl('G', sel$colorIdentity[i])) sel$green[i]=1
        if(sel$types[i]=='Artifact') sel$artifact[i]=1
    }
    names(sel)[1] <- "set"
    sel <- sel[!is.na(sel$cmc),]
    return(sel)
}

updateSelectedSet <- function(){
    sets <<- c(cores[cores[,5]==1,2],expansions[expansions[,5]==1,2])
}

coreChange <- function(selection){
    sel <- NULL

    for (i in 1:length(cores[,2])){
        
        if((cores[i,5]==0) & any(selection==cores[i,2],na.rm=TRUE))   cores[i,5] <<- 1   
        if((cores[i,5]==1) & !any(selection==cores[i,2],na.rm=TRUE))   cores[i,5] <<- 0   
        
        if(any(selection==cores[i,2],na.rm=TRUE)) {
            sel <- rbind(sel, cores[i,])
            cores[i,5] <<- 1   
        }        
        
    }
    updateSelectedSet()
    if(length(sel)==0) return()
    sel[,4] <- "core"
    print(sel[,c(4,2,1)])
    
}

expansionChange <- function(selection){
        
        sel <- NULL
        for (i in 1:length(expansions[,2])){
        
        if((expansions[i,5]==0) & any(selection==expansions[i,2],na.rm=TRUE))   expansions[i,5] <<- 1   
        if((expansions[i,5]==1) & !any(selection==expansions[i,2],na.rm=TRUE))   expansions[i,5] <<- 0   

        if(any(selection==expansions[i,2],na.rm=TRUE)) {
            sel <- rbind(sel, expansions[i,])
            expansions[i,5] <<- 1   
        }        
         
    }
    
    updateSelectedSet()
    if(length(sel)==0) return('---------------------------------------')
    print('---------------------------------------')
    print(sel[,c(4,2,1)])
    


}    

blockChange <- function(selection){
 
    for (i in 2:length(blocks[,1])){
        if(blocks[i,2]==0){
            if(any(selection==blocks[i,1],na.rm=TRUE)) {
                blocks[i,2] <<- 1
                for (j in 1:length(expansions[,4])){
                    if(expansions[j,4]==blocks[i,1]) {
                        expansions[j,5] <<- 1
                    }
                }
            }    
        } else {
            if(!any(selection==blocks[i,1],na.rm=TRUE)) {
                blocks[i,2] <<- 0
                for (j in 1:length(expansions[,4])){
                    if(expansions[j,4]==blocks[i,1]) {
                        expansions[j,5] <<- 0
                    }
                }
            }            
        }    
    }
    expansions[expansions[,5]==1,2]
    
    
}

coreSetsChange <- function(selection){
    
    if(blocks[1,2]==0 & any(selection==blocks[1,1],na.rm=TRUE)) {
        cores[,5] <<- 1
        blocks[1,2] <<-1
    } 
    if(blocks[1,2]==1 & !any(selection==blocks[1,1],na.rm=TRUE)) {
        cores[,5] <<- 0
        blocks[1,2] <<-0
    }

    cores[cores[,5]==1,2]
    
}

shinyServer(function(input, output, session) {
    
                 
    
    observe({
        updateCheckboxGroupInput(session, "expansions", label = "Select expansions", choices = expansions[,2], selected = blockChange(input$blocks), inline = TRUE)        
        updateCheckboxGroupInput(session, "cores", label = "Select Core sets", choices = cores[,2], selected = coreSetsChange(input$blocks), inline = TRUE)        
        ##updateSelectInput(session, "char",choices = c('Converted Mana Cost','Power', 'Thoughness'))
        
        
        
        })
    
    output$editionsDetails <- renderPrint({
        
        coreChange(input$cores)
        expansionChange(input$expansions)
        ## sets
        updateSelectedSet()


        output$summary <- renderPrint({
            sel <- setsCards(sets)
            
            if(input$Color=="White") sel <- sel[sel$white==1,]
            if(input$Color=="Blue") sel <- sel[sel$blue==1,]
            if(input$Color=="Black") sel <- sel[sel$black==1,]
            if(input$Color=="Red") sel <- sel[sel$red==1,]
            if(input$Color=="Green") sel <- sel[sel$green==1,]            
            if(input$Color=="None") sel <- sel[sel$white==0 & sel$blue==0 & sel$black==0 & sel$red==0 & sel$green==0,]             
            
            if(input$Types=="Creatures") sel <- sel[grepl('Creature', sel$types),]
            if(input$Types=="Enchantment") sel <- sel[grepl('Enchantment', sel$types),]
            if(input$Types=="Instant") sel <- sel[grepl('Instant', sel$types),]
            if(input$Types=="Sorcery") sel <- sel[grepl('Sorcery', sel$types),]
            if(input$Types=="non-creatures") sel <- sel[!grepl('Creature', sel$types),]
            
            
            output$plot <- renderPlot({
                if(input$Characteristic=='Converted Mana Cost') boxplot(sel$cmc ~ sel$set, ylim=c(1,10))
                if(input$Characteristic=='Power') plot(as.numeric(sel[!is.na(as.numeric(sel$power)),]$power) ~ sel[!is.na(as.numeric(sel$power)),]$set)
                if(input$Characteristic=='Toughness') plot(as.numeric(sel[!is.na(as.numeric(sel$toughness)),]$toughness) ~ sel[!is.na(as.numeric(sel$toughness)),]$set)
            })
            print(sel[,1:10])
        })
        
    })
    
    
##    output$summary <- renderPrint({    
##        updateSelectInput(session, "selectedSet",choices = sets, selected = NULL)
        ##summarize(group_by(sel,'set'), count(cmc))
        ##print(setsCards(sets))
##    })
    
    
    output$table <- DT::renderDataTable({
        DT::datatable(cores)
    })
})