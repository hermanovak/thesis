#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

######Load necessary packages first: ##############
library(shiny)
library(RNifti)
library(shinythemes)
library(shinyWidgets)
library(neurobase)
#library(stringr)
library(tidyverse)
#library(ggplot2)
#library(plotly)
library(sortable)
library(readxl)
#library(shinyjs)
library(rowr)
library(shinycssloaders)
library(shinybusy)
library(ggplot2)

#####Limits######
max_file_size = 30
options(shiny.maxRequestSize = max_file_size*1024^2) #allow max _ * 1024^2 MB/files
counter <- 0


####Functions####
get_full_ext <- function(path) {
    str_extract(path,"(?<=\\.).*")
}


get_nif_path <- function(datapath) {
    #this function changes extension of temp file from .gz to .nii.gz    
    
    if (get_full_ext(datapath)=="gz"){
        
        newdatapath <- sub("gz$", "nii.gz", datapath)
        file.copy(datapath, newdatapath)
        datapath <- newdatapath
        
    } 
    nif <- RNifti::readNifti(datapath)
    
}

get_csv <- function(datapath) {
    #this function determines whether to open an excel or csv file
    if (get_full_ext(datapath)=='xlsx' || get_full_ext(datapath)=='xls') {
        file <- read_excel(datapath)}
    else {file <- read.csv(datapath, fileEncoding="UTF-8-BOM")}
}

rndr_nif_slice <- function(path,slice) {
    outfile <- tempfile(fileext='.png')
    png(outfile, width = 600, height = 500)
    img <- RNifti::readNifti(path)
    img <- dropEmptyImageDimensions(img)
    image(img, z=slice, plot.type = "single")
    dev.off()
    list(src=outfile)
}


calc_vol <- function(img) {
    sm <- sum(img!=0)
    pix <- slot(img, "pixdim")
    vol <- (sm*pix[1]*pix[2]*pix[3])/1000
    
    return(vol)
    
}

calc_reg <- function(img) {
    
    if(max(img)>1){
        nc <- sum(img==1)/1000
        edema <- sum(img==2)/1000
        ne <- sum(img==3)/1000
        ec <- sum(img==4)/1000
    }
    else {
        nc <- NA
        edema <- NA
        ne <- NA
        ec <- NA
    }
    
    list(nc=nc, edema=edema, ne=ne, ec=ec)
}

calc_dims <- function(arr) {
    pix <- slot(arr, "pixdim")
    d <- dim(arr)
    
    z_dim = 0
    for (row in 1:d[1]) {
        for (col in 1:d[2]){
            idx = which(arr[row,col,]!=0)
            if (length(idx)==0) {rw=0}
            else {rw = idx[length(idx)] - idx[1]}
            if (rw > z_dim) {z_dim <- rw}}}
    z_dim = z_dim*pix[3]
    
    y_dim=0
    for (row in 1:d[1]) {
        for (slice in 1:d[3]){
            idx = which(arr[row,,slice]!=0)
            if (length(idx)==0) {rw=0}
            else {rw = idx[length(idx)] - idx[1]}
            if (rw > y_dim) {y_dim <- rw}}}
    y_dim = y_dim*pix[2]
    
    x_dim=0
    for (col in 1:d[2]) {
        for (slice in 1:d[3]){
            idx = which(arr[,col,slice]!=0)
            if (length(idx)==0) {rw=0}
            else {rw = idx[length(idx)] - idx[1]}
            if (rw > x_dim) {x_dim <- rw}}}
    x_dim = x_dim*pix[1]
    
    
    list(x_dim=x_dim, y_dim=y_dim, z_dim=z_dim)
}





############ Define UI for application ##################
ui <- fluidPage(theme = shinytheme("darkly"),
                #useShinyjs(),  # Set up shinyjs
                add_busy_bar(color = "#FF0000"),
                #add_busy_spinner(color= "#112446"),
                titlePanel(title="Quantitative Brain Lesion Characteristic Exploration"),
                sidebarLayout(
                    sidebarPanel(width=3,
                                 
                                 fileInput("segin", "Upload segmentations as .nii or .nii.gz:", multiple=TRUE, placeholder = "default data"),
                                 helpText("The maximum file upload size is", max_file_size, "MB."),
                                 br(),
                                 radioButtons("defdata","Or choose a default data set",choices=c("Longitudinal","Cross-sectional"),selected="Longitudinal"),
                                 br(),
                                 
                                 fileInput("csvin", "Upload additional metadata (e.g. segmentation scores) as csv or excel file. The data must be organized in columns with the first row containing labels of each column.", multiple = FALSE),
                                 #helpText(''),
                                 hr(),
                                 
                                 actionButton("calc", "Calculate"),
                                 br(),
                                 br(),
                                 h5("Mean Segmentation # of pixels:"),
                                 withSpinner(verbatimTextOutput("pix", placeholder=TRUE), type=4, size=0.5, proxy.height = 50),
                                 
                                 h5("Mean Segmentation volume [cm^3]:"), #make the mm3 look nicer
                                 withSpinner(verbatimTextOutput("vol",placeholder=TRUE), type=5, size=0.5, proxy.height = 50),
                                 hr(),
                                 
                                 
                                 textInput("downloadname", "Save table as csv file:", value = "seg_data_analysis",placeholder=TRUE),
                                 downloadButton('downloadData', 'Download table summary')
                                 
                                 
                    ), #sidebarPanel
                    
                    mainPanel(
                        tabsetPanel(type='tab',
                                    
                                    tabPanel("Segmentation analysis",
                                             fluidPage(
                                                 
                                                 column(4,htmlOutput("ranklist"),
                                                        
                                                        tags$style(
                                                            HTML("
                                                            .rank-list-container.custom-sortable {
                                                              background-color: black;
                                                            }
                                                            .custom-sortable .rank-list-item {
                                                              background-color: teal;
                                                            }
                                                          ") #html
                                                        ) #tags$style
                                                 ),
                                                 
                                                 column(8, 
                                                        column(2, 
                                                               br(),
                                                               actionButton("loplotit", "Plot")),
                                                        column(3, selectInput("loplottype", "Select plot type", choices = c("Scatter", "Bar plot"), selected = "Scatter")),
                                                        column(3, selectInput("lodatatype", "Select data type", choices = c("Raw data", "Z score"), selected = "Raw data")),
                                                        column(3, selectInput("loplotvar", "Select variable", choices = c("Volume", "Volume change", "Max dimensions", "Tumor components", "Product"), selected = "Mean volume")),
                                                        
                                                        uiOutput("trui"),
                                                        plotOutput("rankedplot"), 
                                                        hr(),
                                                        
                                                 ),
                                                 
                                                 column(12,
                                                        br(),
                                                        br(),
                                                        helpText("The following data was calculated from the uploaded files:"),
                                                        tableOutput("table")
                                                 ),
                                             )),
                                    
                                    tabPanel("Segmentation quality", 
                                             fluidPage(
                                                 #tableOutput("table"),
                                                 #hr(),
                                                 column(3, 
                                                        uiOutput("myui")
                                                 ),
                                                 column(3, 
                                                        uiOutput("myui2")
                                                 ),
                                                 
                                                 #column(2, selectInput("plottype", "Select plot type", choices = c("Scatter", "Boxplot", "Bar plot"), selected = "Scatter")),
                                                 #column(2, selectInput("datatype", "Select data type", choices = c("Raw data", "Z score"), selected = "Raw data")),
                                                 #column(2, selectInput("plotvar", "Select variable", choices = c("Volume", "Max dimensions"), selected = "Mean volume")),
                                                 
                                                 plotOutput("myplot", width = "100%"), 
                                                 
                                             )), #tabPanel + fluidPage  
                                    
                                    
                                    
                                    # tabPanel("Data summary & Download",
                                    #         fluidPage(
                                    #             column(12, 
                                    #                    br(),
                                    #                    helpText("The following data was calculated from the uploaded files:")),
                                    #                    #tableOutput("table")),         
                                    #             # column(3,
                                    #             #        textInput("downloadname", "Name of file to be saved:", value = "seg_data_analysis",placeholder=TRUE),
                                    #             #        downloadButton('downloadData', 'Download table summary')
                                    #             #        ),
                                    # 
                                    #         )),
                                    
                                    tabPanel('About this App',
                                             includeMarkdown('About.Rmd'))
                        ) #tabsetPanel
                    ) #mainPanel
                ) #sidebarLayout
) #fluidPage   


############## Define server logic #######################
server <- function(input, output) {
    
    shiny::addResourcePath('www', here::here("www"))
    #shinyjs::hide("table")
    
    csvdatapath <- reactive({
        input$csvin$datapath
    })
    
    data <- reactive({ 
        #counter <- counter+1
        #data()$counter+1
        #req(input$segin)
        datapath <- input$segin$datapath
        titles <- c("Filename", "#Pixels","Volume[cm^3]", "X_dim", "Y_dim", "Z_dim", "Necrotic core", "Enhancing core", "Non-enhancing core", "Edema", "Dummy_score")
        
        
        if (is.null(datapath)) {
            if (input$defdata=="Cross-sectional")
            {images <- readRDS(file="./images2.Rda")
            labels <- list("brats_tcia_pat153_0002_seg.nii.gz", "brats_tcia_pat171_0001_seg.nii.gz", "brats_tcia_pat222_0122_seg.nii.gz", "brats_tcia_pat230_0199_seg.nii.gz", "brats_tcia_pat260_0001_seg.nii.gz", "brats_tcia_pat290_0305_seg.nii.gz", "brats_tcia_pat309_0001_seg.nii.gz")}
            
            if (input$defdata=="Longitudinal")
            {images <- readRDS(file="./loimages3.Rda")
            labels <- list("brats_tcia_pat153_0002_seg.nii", "brats_tcia_pat153_0109_seg.nii", "brats_tcia_pat153_0165_seg.nii", "brats_tcia_pat153_0181_seg.nii", "brats_tcia_pat153_0277_seg.nii", "brats_tcia_pat153_0294_seg.nii")}
        }
        
        else {
            
            labels <- strsplit(input$segin$name, " ")
            num_images=length(datapath)
            #images <- vector(mode = "list", length = num_images)
            images <- matrix(nrow=num_images, ncol=9)
            #titles <- c("Filename", "#Pixels","Volume[cm^3]", "X_dim", "Y_dim", "Z_dim", "Necrotic core", "Enhancing core", "Non-enhancing core", "Edema")
            
            
            for (i in 1:num_images) {
                nif <- get_nif_path(datapath[i])
                #nam <- paste0("seg", i)
                #images[[i]] <- assign(nam,nif)
                
                dims <- calc_dims(nif)
                reg <- calc_reg(nif)
                
                images[i,1] <- sum(nif!=0)
                images[i,2] <- calc_vol(nif)
                images[i,3] <- dims$x_dim
                images[i,4] <- dims$y_dim
                images[i,5] <- dims$z_dim
                images[i,6] <- reg$nc
                images[i,7] <- reg$ec
                images[i,8] <- reg$ne
                images[i,9] <- reg$edema
                
            }
            
            csvdatapath <- input$csvin$datapath
            titles <- c("Filename", "#Pixels","Volume[cm^3]", "X_dim", "Y_dim", "Z_dim", "Necrotic core", "Enhancing core", "Non-enhancing core", "Edema")
            
            if ((!is.null(csvdatapath))) {
                scores <- get_csv(input$csvin$datapath)
                images <- cbind.fill(images, scores, fill=NA)
                #images <- cbind(images, scores)
                titles <- c(titles[1:length(titles)],str_sub(colnames(scores),1,11))
            }}
        
        
        list(imgData=as.matrix(images), path=datapath, labels=labels, titles=titles)
        
    })
    # 
    # observeEvent(input$csvin$datapath, {
    #         scores <- get_csv(input$csvin$datapath)
    #         images <- cbind.fill(data()$imgData, scores, fill=NA)
    #         #images <- cbind(images, scores)
    #         titles <- c(data()$titles,colnames(scores))
    #       
    #     })
    
    
    ###### sidebar #############
    
    observeEvent(input$calc, ignoreNULL = FALSE, {
        output$pix <- renderText({
            req(input$calc)
            info <- data()
            images <- info$imgData
            mpix <- as.integer(mean(images[,1],na.rm=TRUE))
        })
    })
    
    observeEvent(input$segin, { #becomes empty when new files are uploaded
        output$pix <- renderText({
        })
    })
    
    observeEvent(input$csvin, ignoreNULL = FALSE, { #becomes empty when new files are uploaded
        info <- csvdatapath()
        info <- data()
    })
    
    # observeEvent(input$cvsin, { #this needs work
    #   input$csvin$datapath <- NULL
    #   print(input$csvin$datapath)
    #   print(is.null(input$csvin$datapath))
    # })
    
    observeEvent(input$calc, {
        output$vol <- renderText({
            req(input$calc)
            info <- data()
            images <- info$imgData
            mvol <- as.integer(mean(images[,2],na.rm=TRUE))
        })
    })
    
    observeEvent(input$segin, ignoreNULL = FALSE,{
        output$vol <- renderText({
        })
    })    
    
    
    
    ###### Segmentation quality ###########    
    output$myui <- renderUI({
        #req(input$csvin, input$segin)
        info <- data()
        vars <- info$titles[2:length(info$titles)]
        selectInput("x_choice", "Select x to correlate", unique(vars) , selected="Volume[cm^3]")
        
    })
    
    output$myui2 <- renderUI({
        #req(input$csvin, input$segin)
        info <- data()
        vars <- info$titles[2:length(info$titles)]
        selectInput("y_choice", "Select y variable to correlate", unique(vars), selected="X_dim")
        
    })
    
    output$myplot <- renderPlot({
        # if (is.null(input$csvin$datapath)) {
        #   plot(1,1,col="white")
        #   text(1,1,"No data uploaded")
        # }
        # else {
        req(input$x_choice, input$y_choice)
        
        info <- data()
        df <- data.frame(cbind(info$labels,info$imgData)) 
        df <- setNames(df, info$titles) #%>% 
        chosenx <- as.character(input$x_choice)
        xvar <- unlist(df[chosenx])
        choseny <- as.character(input$y_choice)
        yvar <- unlist(df[choseny])
        
        #yvar <- setNames(data.frame(scores), colnames(scores))
        
        title <- "Segmentation quality" 
        
        # 
        #             if(input$datatype == "Z score") {
        #               idx <- sapply(yvar, class)=="numeric"
        #               yvar[, idx] <- lapply(yvar[, idx], function(x) (x-mean(x))/sd(x))
        #               ylab <- "z score"
        #               title <- paste0("Z score of ", title)}
        
        #plot(y=yvar,x=xvar, main=title, ylab=input$y_choice, xlab = input$x_choice) 
        #ylim=c(min(yvar)-0.05*min(yvar),max(yvar)+0.1*max(yvar)))
        
        #ggplot and Bland-altman
        df <- data.frame(xvar,yvar)
        df$avg <- rowMeans(df) #create new column for average measurement
        df$diff <- df$x - df$y #create new column for difference in measurements
        mean_diff <- mean(df$diff)
        lower <- mean_diff - 1.96*sd(df$diff)
        upper <- mean_diff + 1.96*sd(df$diff)
        
        ggplot(df, aes(x = avg, y = diff)) +
            geom_point(size=2) +
            geom_hline(yintercept = mean_diff) +
            geom_hline(yintercept = lower, color = "red", linetype="dashed") +
            geom_hline(yintercept = upper, color = "red", linetype="dashed") +
            ggtitle("Bland-Altman Plot") +
            ylab("Difference Between Measurements") +
            xlab("Average Measurement")
        
        
    })
    ##### Segmentation analysis ############   
    table_data <- reactive({
        info <- data()
        df <- data.frame(cbind(info$labels,info$imgData)) 
        df <- setNames(df, info$titles) #%>% 
        #formatRound(c("#Pixels","Volume[cm^3]", "X_dim", "Y_dim", "Z_dim"), digits=2)
        #df <- apply(df,2,as.character)
        #table_order <- ranklist_data()
    })
    
    rv <- reactiveValues(data = data.frame())
    
    observe({rv$data <- data()$labels})
    
    
    output$ranklist <- renderUI({
        
        ranklist <- rank_list(
            text = "Organize data points in time by dragging",
            labels = rv$data,
            input_id = "ranklist",
            class = c("default-sortable", "custom-sortable"),
        )
        
    })
    
    # toListen <- reactive({
    #   list(input$ranklist,data())
    # })
    
    observeEvent(input$ranklist, {
        rnk <- rank(input$ranklist)
        rv$orgdata <- table_data()[rnk,1:ncol(table_data())]
    })
    
    # observeEvent(data(), {
    #   rnk <- rank(input$ranklist)
    #   rv$orgdata <- table_data()[rnk,1:ncol(table_data())]
    # })
    
    #   observe({
    #   if(any(is.na(rv$orgdata))==TRUE) {
    #     hide("rankedPlot")
    #   } else {
    #     show("rankedPlot")
    #   }
    # })    
    
    observe({
        if(input$loplotvar == "Tumor components") {
            
            output$trui <- renderUI({
                info <- data()
                #checkboxGroupInput("trx", label = "Choose whether to hide any variables", choices = info$titles[7:10], inline=TRUE)
                checkboxGroupButtons("trx", label = "Choose whether to hide any variables", choices = info$titles[7:10], selected = info$titles[7:10])
            })
        }
    })
    
    
    output$rankedplot <- renderPlot({
        req(input$loplotit)
        
        info <- data()
        df <- rv$orgdata
        xvar <- 1:nrow(df)
        
        if(input$loplotvar == "Max dimensions") {
            
            
            yvar <- df[4:6]
            title <- "Maximum dimensions of segmentations"
            ylab <- "dimension [mm]"
            xdim <- as.numeric(df$X_dim)
            ydim <- as.numeric(df$Y_dim)
            zdim <- as.numeric(df$Z_dim)
            yvar <- data.frame(xdim,ydim,zdim)
            #product <- apply(yvar, 1, function(x) prod(max(x), max(x[-which(x == max(x))[1]])))
            
            if(input$lodatatype == "Z score") {
                
                xdim <- (xdim - mean(xdim))/sd(xdim)
                ydim <- (ydim - mean(ydim))/sd(ydim)
                zdim <- (zdim - mean(zdim))/sd(zdim)
                yvar <- data.frame(xdim,ydim,zdim)
                ylab <- "z score"
                title <- paste0("Z score of ", title)}
            
            plot(y=xdim,x=xvar,  col="red", main=title, ylab=ylab, xlab = "Segmentation no.", ylim=c(min(yvar)-0.05*min(yvar),max(yvar)+0.1*max(yvar)))
            points(y=ydim, x=xvar, col="green")
            points(y=zdim, x=xvar, col="blue")
            legend("topright", legend = c("xdim", "ydim", "zdim"), pch=1, col=c("red", "green", "blue"))
            axis(1, xvar)
            
            if(input$loplottype=="Bar plot") {p <- barplot(t(as.matrix(yvar)),beside=TRUE,legend.text=TRUE, col=c("red","green","blue"),names.arg=1:nrow(df), main=title,
                                                           xlab="Segmentation no.", ylab=ylab)} 
        }
        
        else if(input$loplotvar == "Tumor components") {
            
            
            
            if (any(is.na(df$Edema))==FALSE) {
                
                if(is.null(input$trx)) {yvar <- df[7:10]}
                else {yvar <- df[input$trx]}
                #yvar <- df[7:10]
                title <- "Tumor region volumes"
                ylab <- "volume [cm^3]"
                #nc <- as.numeric(df$`Necrotic core`)
                #edema <- as.numeric(df$Edema)
                #ne <- as.numeric(df$`Non-enhancing core`)
                #ec <- as.numeric(df$`Enhancing core`)
                
                #yvar <- data.frame(nc, edema, ne, ec)
                
                if(input$lodatatype == "Z score") {
                    
                    # nc <- (nc - mean(nc))/sd(nc)
                    # edema <- (edema - mean(edema))/sd(edema)
                    # ne <- (ne - mean(ne))/sd(ne)
                    # ec <- (ec - mean(ec))/sd(ec)
                    # 
                    # yvar <- data.frame(nc, edema, ne, ec)
                    
                    l <- lapply(yvar,as.numeric)
                    yvar <- data.frame(lapply(l, function(x) (x-mean(x))/sd(x)))
                    
                    ylab <- "z score"
                    title <- paste0("Z score of ", title)}
                
                #for ( c in yvar ) plot( c, type="l" )
                #colnames(yvar) <- NULL
                #plot(y=unlist(yvar[1]),x=xvar,  col="red", main=title, ylab=ylab, xlab = "Segmentation no.", ylim=c(min(yvar)-0.05*min(yvar),max(yvar)+0.1*max(yvar)))
                #col <- c("red","green", "blue", "brown")
                #for (i in 2:ncol(yvar)) {
                #  points(y=unlist(yvar[i]), x=xvar, col=col[i])}
                #points(y=ne, x=xvar, col="blue")
                #points(y=ec, x=xvar, col="brown")
                matplot(x=xvar,y=yvar,pch=1,col=1:4,main=title,ylab=ylab,xlab="Segmentation no.")
                legend("topleft", legend = colnames(yvar), col = 1:4, fill = 1:4)
                axis(1, xvar)
                
                if(input$loplottype=="Bar plot") {p <- barplot(t(as.matrix(yvar)),beside=FALSE,legend.text=TRUE, col=1:4,names.arg=1:nrow(df), main=title,
                                                               xlab="Segmentation no.", ylab=ylab)} 
            }
            
            else {plot(1,1,col="white")
                text(1,1,"Volumes of tumor components could not be calculated")}
            
        }
        
        else if (input$loplotvar == "Volume change") {
            
            vols <- as.numeric(df$`Volume[cm^3]`)
            minvol <- min(vols)
            yvar <- 100*(vols - minvol)/minvol
            title <- "Volume change from nadir [%]"
            ylab <- "Volume change [%]"
            
            
            if(input$lodatatype == "Z score") {
                
                yvar <- (yvar - mean(yvar))/sd(yvar)
                ylab <- "z score"
                title <- paste0("Z score of ", title)}
            
            
            plot(y = yvar, x=xvar, main=title,xlab="Segmentation no.", ylab=ylab)
            axis(1, xvar)
            
            if(input$loplottype=="Bar plot") {p <-  barplot(yvar, main=title,names.arg=1:nrow(df),
                                                            xlab="Segmentation no.", ylab=ylab)} 
        }
        
        else if (input$loplotvar == "Product") {
            
            yvar <- df[4:6]
            title <- "Product of two largest diameters"
            ylab <- "product of two largest dimensions [cm^2]"
            xdim <- as.numeric(df$X_dim)
            ydim <- as.numeric(df$Y_dim)
            zdim <- as.numeric(df$Z_dim)
            yvar <- data.frame(xdim,ydim,zdim)
            yvar <- apply(yvar, 1, function(x) prod(max(x), max(x[-which(x == max(x))[1]])))/100
            
            if(input$lodatatype == "Z score") {
                
                yvar <- (yvar - mean(yvar))/sd(yvar)
                ylab <- "z score"
                title <- paste0("Z score of ", title)}
            
            plot(y=yvar,x=xvar,  col="red", main=title, ylab=ylab, xlab = "Segmentation no.", ylim=c(min(yvar)-0.05*min(yvar),max(yvar)+0.1*max(yvar)))
            
            axis(1, xvar)
            
            if(input$loplottype=="Bar plot") {p <- barplot(t(as.matrix(yvar)),beside=TRUE,legend.text=TRUE, col=c("red"),names.arg=1:nrow(df), main=title,
                                                           xlab="Segmentation no.", ylab=ylab)} 
        }
        
        else {
            yvar <- as.numeric(df$`Volume[cm^3]`)
            title <- "Segmentation volumes"
            ylab <- "vol [cm^3]"
            
            if(input$lodatatype == "Z score") {
                yvar <- (yvar - mean(yvar))/sd(yvar)
                ylab <- "z score"
                title <- paste0("Z score of ", title)}
            
            # p <- ggplot(data = df) + geom_point(mapping = aes(x=1:nrow(df), y=yvar)) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + xlab("segmentation idx")+ylab(ylab)+ 
            #     scale_alpha(guide = 'none')
            
            plot(y = yvar, x=xvar, main=title,xlab="Segmentation no.", ylab=ylab)
            axis(1, xvar)
            
            # if(input$loplottype=="Boxplot") {p  <- boxplot(yvar, main=title,
            #                                              ylab=ylab)}
            if(input$loplottype=="Bar plot") {p <-  barplot(yvar, main=title,names.arg=1:nrow(df),
                                                            xlab="Segmentation no.", ylab=ylab)} 
        }
        
    }) #renderPlot close
    
    
    ##### Data summary & Download ###############    
    
    
    output$table <- renderTable({
        return(rv$orgdata)
    })
    
    # output$table <- renderDT(
    # 
    #   input$rank_list_basic,
    #   #expr <-  rank_list_basic,
    #   expr <- table_data(),
    #   #expr <- apply(df,2,as.numeric),
    #   style = "bootstrap",
    #   selection = "single",
    # 
    #   options = list(
    #     pageLength=12,
    #     searching = FALSE,
    #     info = TRUE
    #   )
    # )
    
    output$downloadData <- downloadHandler(
        
        filename = function() {
            paste(input$downloadname,'.csv', sep='')
        },
        content = function(file) {
            write.csv2(apply(rv$orgdata,2,as.character), file)
            #write_csv2(table_data(),file)
        }
    )
    
    # 
    # output$text <- renderUI({
    #   req(input$submit)
    #   nifImg <- data()
    #   l <- vector(mode = "list", length = input$numpts)
    #   for(n in 1:input$numpts)
    #     {nif <- get_nif_path(nifImg$path[n])
    #     dims <- calc_dims(nif)
    #     nam <- paste0("pt", n)
    #     p <- paste("Pt", n, ": ", as.integer(dims$x_dim), "in x,", as.integer(dims$y_dim), "in y,", as.integer(dims$z_dim), "in z")
    #     l[[n]] <- assign(nam,p)}
    #     HTML(paste(c(l[1:n]), sep = '<br/>'))
    #     })
    
    
}#server

########## Run the application ############
shinyApp(ui = ui, server = server)
