#' Run GUI for malia package
#'
#'@description Provide graphical and numerical summaries.
#'
#'
#' @export
#'


 shinymalia<-function(){

# ui_syns -----------------------------------------------------------------


   tabItem_syns<-
     tabItem(tabName = "tab_syns",
             titlePanel(strong("Aritificial and Natural products")),
             fluidRow(
               box(title = "legDTable_syns",DT::dataTableOutput("legDTable_syns"),
                   width = 6,solidHeader = T,status = "primary"),
               box(title = "gridDTable_syns",DT::dataTableOutput("gridDTable_syns"),
                   width = 6,solidHeader = T,status = "primary")
             ),
             hr(),
             fluidRow(
               column(3,offset = 2,
                      downloadButton("downloadlegAn", "legD_AN_Table")
               ),
               column(3,offset = 3,
                      downloadButton("downloadgridAn", "gridD_AN_Table")
               )),
             hr(),
             fluidRow(
               box(title = "areaDTable_syns",DT::dataTableOutput("areaDTable_syns"),
                   width = 6,solidHeader = T,status = "primary"),
               box(title = "leg Density of Artificial Product",imageOutput("leg_Ar"),
                   width = 6,solidHeader = T,status = "primary")
             ),
             hr(),
             fluidRow(
               column(3,offset = 2,
                      downloadButton("downloadareaAn", "areaD_AN_Table")
               ),
               column(3,offset = 3,
                      downloadButton("downloadlegDArPlot", "legD_Artificial_Plot")
               )),
             hr(),
             fluidRow(
               box(title = "leg Density of Natural Product",imageOutput("leg_Nt"),
                   width = 6,solidHeader = T,status = "primary"),
               box(title = "grid Density of Artificial Product",imageOutput("grid_Ar"),
                   width = 6,solidHeader = T,status = "primary")
             ),
             hr(),
             fluidRow(
               column(3,offset = 2,
                      downloadButton("downloadlegDNtPlot", "legD_Natural_Plot")
               ),
               column(3,offset = 3,
                      downloadButton("downloadgridDArPlot", "gridD_Artificial_Plot")
               )),
             hr(),
             fluidRow(
               box(title = "grid Density of Natural Product",imageOutput("grid_Nt"),
                   width = 6,solidHeader = T,status = "primary")
             ),
             fluidRow(
               column(3,offset = 2,
                      downloadButton("downloadgridDNtPlot", "gridD_Natural_Plot")
               )
             )
     )


# ui_density --------------------------------------------------------------


   tabItem_density<-
     tabItem(tabName = "tab_density",
             titlePanel(strong("Results of single debris")),
             hr(),
             fluidRow(
               box(title = "leg Density",DT::dataTableOutput("legDTable"),
                   width = 6,solidHeader = T,status = "primary"),
               box(title = "leg Density",imageOutput("leg"),
                   width = 6,solidHeader = T,status = "primary")
             ),
             hr(),
             fluidRow(column(3,offset = 2,
                             downloadButton("downloadlegD", "legDTable")
             ),
             column(3,offset = 3,
                    downloadButton("downloadlegDPlot", "legDPlot")
             )),
             hr(),
             fluidRow(
               box(title = "grid Density",DT::dataTableOutput("gridDTable"),
                   width = 6,solidHeader = T,
                   status = "primary"),
               box(title = "grid Density",imageOutput("grid"),
                   width = 6,solidHeader = T,status = "primary")
             ),
             hr(),
             fluidRow(column(3,offset = 2,
                             downloadButton("downloadgridD", "gridDTable")
             ),
             column(3,offset = 3,
                    downloadButton("downloadgridDPlot", "gridDPlot")
             )
             ),
             hr(),
             fluidRow(
               box(title = "area Density",DT::dataTableOutput("areaDTable"),
                   width = 6,solidHeader = T,status = "primary")
             ),
             hr(),
             fluidRow(column(3,offset = 2,
                             downloadButton("downloadareaD", "areaDTable")
             )
             )
     )


# ui_info -----------------------------------------------------------------


   tabItem_Info<-
     tabItem(tabName = "tab_Info",
             titlePanel(strong("Marine litter analysis")),
             p("Analysis and visualization GUI for malia package"),
             br(),
             h2("Introduction"),
             h4("This shiny app provides visual and numerical summaries for
            malia package.\n"),
             h4("This app was built by laboratory of fish population analysis at TUMSAT."),


             h2("Prerequisite"),
             h4("Already created results of malia package are needed."),
             h4("They should be put in the working directory."),
             h4("In order to handle the results, it is needed to put sighing and effort data\n
             of the survey in the `data` folder at current directory.")

     )


# ui_servey ---------------------------------------------------------------

   tabItem_survey<-
     tabItem(tabName = "tab_survey",
             fluidRow(
               titlePanel(strong("Survey Information")),
               column(3,
                      wellPanel(
                        selectInput("select1", "Voyage",
                                    choices =list.files(getwd(),".obj"),
                                    size = NULL,
                                    selectize = TRUE,width = "200px",
                                    multiple = T),
                        downloadButton("downloadsurvey", "Survey.plot"),
                        downloadButton("downloaddebris", "debrisTable"),
                        helpText("Select a range"),
                        sliderInput("slider2", "longitude",
                                    min = 100, max = 180, value = c(120, 160),step = 5),
                        sliderInput("slider3", "latitude",
                                    min = 10, max = 80, value = c(25, 60),step = 5)
                      )),
               column(9,
                      box(title = "Survey leg",status = "primary",
                          imageOutput("survey"),width = 9,solidHeader = T)),
               hr(),
               fluidRow(
                 column(12,
                        wellPanel(
                          tableOutput("surveyinf"),
                          tableOutput("debrisinf")))
               )
             )
     )


# server ------------------------------------------------------------------



   ppi<-200

   server<- function(input, output) {

     readobj <- reactive({

       name.list<-strsplit(input$select1,".result")

       best.list<-vector("list",length = length(input$select1))
       for(i in 1:length(input$select1)){
         tmp<-readRDS(input$select1[i])
         aic.mat<-aic.summary(tmp)
         best.list[[i]]<-model.extract(aic.mat = aic.mat,tmp)
       }

       df.plot<-data.frame()
       for(i in 1:length(best.list)){
         tmp<-best.list[[i]]$best.list[[input$select2]]$leg.D.obs$leg.result
         if(is.null(tmp)){
           next
         }else{
           tmp<-cbind(data.frame(voyage_name=name.list[[i]][1]),tmp)
           df.plot<-rbind(df.plot,tmp)
         }
       }

       gridD<-grid.D(df.plot)
       areaD<-area.D(df.plot)


       # Ar and Nt ---------------------------------------------------------------

       Df<-data.frame()

       for(i in 1:length(best.list)){

         tmp2<-leg.D.table(best.list[[i]]$best.list)
         tmp3<-ls.(tmp2)
         Df<-rbind(Df,tmp3)

       }

       colnames(Df)[c(9,11)]<-"Density"

       gridAr<-grid.D(Df[,c(1:9)])
       gridNt<-grid.D(Df[,c(1:7,10,11)])

       areaAr<-area.D(Df[,c(1:9)])
       areaNt<-area.D(Df[,c(1:7,10,11)])


       return(list(best.list=best.list,df.plot=df.plot,
                   areaD=areaD,
                   gridD=gridD,
                   Ar.df=Df[,c(1:9)],
                   Nt.df=Df[,c(1:7,10,11)],
                   gridAr=gridAr,
                   gridNt=gridNt,
                   areaAr=areaAr,
                   areaNt=areaNt))
     })

     readsurvey<-reactive({

       df.survey<-df.debris<-df.inf<-data.frame()
       name.list<-strsplit(input$select1,".result")
       for(i in 1:length(input$select1)){
         tmp<-read.csv(paste0(getwd(),"/data/",name.list[[i]][1],".effort.csv"))
         tmp.debris<-read.csv(paste0(getwd(),"/data/",name.list[[i]][1],".debris.csv"))
         tmp$voyage_name<-name.list[[i]][1]
         df.tmp<-survey.inf2(tmp.debris,tmp)
         df.inf<-rbind(df.inf,cbind(data.frame(voyage_name=name.list[[i]][1]),
                                    df.tmp$voyage.inf))
         df.debris<-rbind(df.debris,cbind(data.frame(voyage_name=name.list[[i]][1]),
                                          t(df.tmp$debris.inf)))

         df.survey<-rbind(df.survey,tmp)
       }


       return(list(df.survey=df.survey,df.inf=df.inf,df.debris=df.debris))

     })


     output$survey<-renderPlot({
       tmp<-readsurvey()$df.survey
       survey.plot(tmp,multi = T,
                   xl=c(input$slider2[1],input$slider2[2]),
                   yl=c(input$slider3[1],input$slider3[2]))
     })

     output$surveyinf<-renderTable(
       readsurvey()$df.inf
     )


     output$debrisinf<-renderTable(
       readsurvey()$df.debris
     )


     output$legDTable<-renderDataTable({
       tmp<-readobj()$df.plot
     },options=list(scroller = TRUE,
                    scrollX = TRUE))

     output$legDTable_syns<-renderDataTable({
       tmp<-readobj()$Ar.df
       tmp2<-readobj()$Nt.df
       tmp3<-cbind(tmp,tmp2[,c(8,9)])
       colnames(tmp3)[8:11]<-c("Artificial_Number","Artificial_Density","Natural_Number","Natural_Density")
       tmp3
     },options=list(scroller = TRUE,
                    scrollX = TRUE)
     )

     output$gridDTable<-renderDataTable(
       readobj()$gridD,options=list(scroller = TRUE,
                                    scrollX = TRUE))

     output$gridDTable_syns<-renderDataTable({
       tmp<-readobj()$gridAr
       tmp2<-readobj()$gridNt
       tmp3<-cbind(tmp[,1:5],tmp2[,c(5,6)])
       colnames(tmp3)[5:6]<-c("Artificial_Density","Natural_Density")
       tmp3
     },options=list(scroller = TRUE,
                    scrollX = TRUE))


     output$areaDTable<-renderDataTable(
       readobj()$areaD$area.density,options=list(scroller = TRUE,
                                                 scrollX = TRUE))

     output$areaDTable_syns<-renderDataTable({
       tmp<-readobj()$areaAr$area.density
       tmp2<-readobj()$areaNt$area.density
       tmp3<-cbind(tmp[,1:2],tmp2[,c(2,3)])
       colnames(tmp3)[2:3]<-c("Artificial_Density","Natural_Density")
       tmp3
     },options=list(scroller = TRUE,
                    scrollX = TRUE)
     )

     output$leg<-renderPlot({
       tmp<-readobj()$df.plot
       leg.D.plot(tmp,Type = input$select2,
                  xl=c(input$slider4[1],input$slider4[2]),
                  yl=c(input$slider5[1],input$slider5[2]))
     })

     output$leg_Ar<-renderPlot({
       tmp<-readobj()$Ar.df
       leg.D.plot(tmp,Type = "Artificial",
                  xl=c(input$slider4[1],input$slider4[2]),
                  yl=c(input$slider5[1],input$slider5[2]))
     })

     output$leg_Nt<-renderPlot({
       tmp<-readobj()$Nt.df
       leg.D.plot(tmp,Type = "Natural",
                  xl=c(input$slider4[1],input$slider4[2]),
                  yl=c(input$slider5[1],input$slider5[2]))
     })


     output$grid<-renderPlot({
       tmp<-readobj()$gridD
       grid.D.plot(tmp,Type=input$select2,
                   xl=c(input$slider4[1],input$slider4[2]),
                   yl=c(input$slider5[1],input$slider5[2]))
     })

     output$grid_Ar<-renderPlot({
       tmp<-readobj()$gridAr
       grid.D.plot(tmp,Type="Artificial",
                   xl=c(input$slider4[1],input$slider4[2]),
                   yl=c(input$slider5[1],input$slider5[2]))
     })

     output$grid_Nt<-renderPlot({
       tmp<-readobj()$gridNt
       grid.D.plot(tmp,Type="Natural",
                   xl=c(input$slider4[1],input$slider4[2]),
                   yl=c(input$slider5[1],input$slider5[2]))
     })


     output$downloadlegDPlot <- downloadHandler(

       filename = paste(getwd(),"/leg-",input$select2, Sys.Date(), ".png", sep=""),

       content = function(file) {
         df.plot<-readobj()$df.plot
         png(file,width=6*ppi,height=4*ppi,res=ppi)
         leg.D.plot(df.plot,Type=input$select2,
                    xl=c(input$slider4[1],input$slider4[2]),
                    yl=c(input$slider5[1],input$slider5[2]))
         dev.off()
       },

       contentType="image/png"
     )

     output$downloadgridDPlot <- downloadHandler(

       filename = paste(getwd(),"/grid-",input$select2, Sys.Date(), ".png", sep=""),

       content = function(file) {
         tmp<-readobj()$gridD
         df.plot<-readobj()$df.plot
         png(file,width=6*ppi,height=4*ppi,res=ppi)
         grid.D.plot(tmp,Type=input$select2,
                     xl=c(input$slider4[1],input$slider4[2]),
                     yl=c(input$slider5[1],input$slider5[2]))
         dev.off()
       },

       contentType="image/png"
     )


     output$downloadlegD <- downloadHandler(

       filename = paste(getwd(),"/legTable-",input$select2, Sys.Date(), ".csv", sep=""),

       content = function(file) {

         readobj()$df.plot %>%
           write.csv(file)

       }

     )

     output$downloadgridD <- downloadHandler(

       filename = paste(getwd(),"/gridTable-",input$select2, Sys.Date(), ".csv", sep=""),

       content = function(file) {

         readobj()$gridD %>%
           write.csv(file)

       }

     )

     output$downloadareaD <- downloadHandler(

       filename = paste(getwd(),"/areaTable-",input$select2, Sys.Date(), ".csv", sep=""),

       content = function(file) {

         readobj()$areaD$area.density %>%
           write.csv(file)

       }

     )
     output$downloadsurvey <- downloadHandler(

       filename = paste(getwd(),"/survey-", Sys.Date(), ".png", sep=""),

       content = function(file) {
         tmp<-readsurvey()$df.survey
         png(file,width=6*ppi,height=4*ppi,res=ppi)
         survey.plot(tmp,multi = T,
                     xl=c(input$slider2[1],input$slider2[2]),
                     yl=c(input$slider3[1],input$slider3[2]))#+theme(legend.title = element_blank())
         dev.off()
       },

       contentType="image/png"

     )

     output$downloaddebris <- downloadHandler(

       filename = paste(getwd(),"/debrisTable-", Sys.Date(), ".csv", sep=""),

       content = function(file) {

         readsurvey()$df.debris %>%
           write.csv(file,row.names = F)

       }

     )


     # Synthesis ---------------------------------------------------------------


     output$downloadlegAn <- downloadHandler(

       filename = paste(getwd(),"/legTable_AN", Sys.Date(), ".csv", sep=""),

       content = function(file) {

         tmp<-readobj()$Ar.df
         tmp2<-readobj()$Nt.df
         tmp3<-cbind(tmp,tmp2[,c(8,9)])
         colnames(tmp3)[8:11]<-c("Artificial_Number","Artificial_Density","Natural_Number","Natural_Density")
         tmp3 %>%
           write.csv(file)

       }

     )

     output$downloadgridAn <- downloadHandler(

       filename = paste(getwd(),"/gridTable_AN", Sys.Date(), ".csv", sep=""),

       content = function(file) {

         tmp<-readobj()$gridAr
         tmp2<-readobj()$gridNt
         tmp3<-cbind(tmp[,1:5],tmp2[,c(5,6)])
         colnames(tmp3)[5:6]<-c("Artificial_Density","Natural_Density")
         tmp3 %>%
           write.csv(file)

       }

     )

     output$downloadareaAn <- downloadHandler(

       filename = paste(getwd(),"/areaTable_AN", Sys.Date(), ".csv", sep=""),

       content = function(file) {

         tmp<-readobj()$areaAr$area.density
         tmp2<-readobj()$areaNt$area.density
         tmp3<-cbind(tmp[,1:2],tmp2[,c(2,3)])
         colnames(tmp3)[2:3]<-c("Artificial_Density","Natural_Density")
         tmp3 %>%
           write.csv(file)

       }

     )

     output$downloadlegDArPlot <- downloadHandler(

       filename = paste(getwd(),"/leg_Artificial_", Sys.Date(), ".png", sep=""),

       content = function(file) {
         tmp<-readobj()$Ar.df
         png(file,width=6*ppi,height=4*ppi,res=ppi)
         leg.D.plot(tmp,Type = "Artificial",
                    xl=c(input$slider4[1],input$slider4[2]),
                    yl=c(input$slider5[1],input$slider5[2]))
         dev.off()
       },

       contentType="image/png"
     )

     output$downloadgridDArPlot <- downloadHandler(

       filename = paste(getwd(),"/grid_Artificial_", Sys.Date(), ".png", sep=""),

       content = function(file) {
         tmp<-readobj()$gridAr
         png(file,width=6*ppi,height=4*ppi,res=ppi)
         grid.D.plot(tmp,Type="Artificial",
                     xl=c(input$slider4[1],input$slider4[2]),
                     yl=c(input$slider5[1],input$slider5[2]))
         dev.off()
       },

       contentType="image/png"
     )

     output$downloadlegDNtPlot <- downloadHandler(

       filename = paste(getwd(),"/leg_Natural_", Sys.Date(), ".png", sep=""),

       content = function(file) {
         tmp<-readobj()$Nt.df
         png(file,width=6*ppi,height=4*ppi,res=ppi)
         leg.D.plot(tmp,Type = "Natural",
                    xl=c(input$slider4[1],input$slider4[2]),
                    yl=c(input$slider5[1],input$slider5[2]))
         dev.off()
       },

       contentType="image/png"
     )

     output$downloadgridDNtPlot <- downloadHandler(

       filename = paste(getwd(),"/grid_Natural_", Sys.Date(), ".png", sep=""),

       content = function(file) {
         tmp<-readobj()$gridNt
         png(file,width=6*ppi,height=4*ppi,res=ppi)
         grid.D.plot(tmp,Type="Natural",
                     xl=c(input$slider4[1],input$slider4[2]),
                     yl=c(input$slider5[1],input$slider5[2]))
         dev.off()
       },

       contentType="image/png"
     )


   }


# help func ---------------------------------------------------------------


   survey.inf2<-function(Sight.Data,Effort.Data, Artificial = c("FGN",
                                                                "FGF", "FGO", "EPS", "PBA", "PBO",
                                                                "FP", "PC", "G", "M", "W",
                                                                "UO"), Natural = c("SW", "DW", "NO"))
   {
     leg.number <- length(Effort.Data$leg.number)
     leg.length <- sum(Effort.Data$leg.length)
     n.debris <- Sight.Data$number
     n.debris[n.debris >= 20] <- 20
     sight.debris <- sum(n.debris, na.rm = T)

     Type<-c(Artificial,Natural,"UK")

     voyage.inf <- data.frame(Number_of_legs = leg.number, Total_leg_length = leg.length,
                              Number_of_debris = sight.debris)
     tmp.df <- data.frame(n = n.debris, type = Sight.Data$type)
     summary.df <- data.frame(matrix(NA, length(Type) +
                                       2, 1, dimnames = list(c(Type, "Artificial",
                                                               "Natural"), c("Number"))))
     for (i in 1:length(Type)) {
       summary.df[i, 1] <- sum(tmp.df[tmp.df$type == Type[i],
                                      ]$n, na.rm = T)
     }
     summary.df["Artificial", ] <- sum(tmp.df[tmp.df$type %in%
                                                Artificial, ]$n, na.rm = T)
     summary.df["Natural", ] <- sum(tmp.df[tmp.df$type %in%
                                             Natural, ]$n, na.rm = T)
     return(list(voyage.inf = voyage.inf, debris.inf = summary.df))
   }


# dashboard ---------------------------------------------------------------


   ui <- dashboardPage(
     dashboardHeader(title = "shinyMALIA"),
     dashboardSidebar(
       sidebarMenu(id="menu1",
                   menuItem("Information", icon=icon("info"), tabName = 'tab_Info'
                   ),
                   menuItem("Perf Analytics", icon=icon("line-chart"),
                            menuSubItem("Survey", tabName = "tab_survey"),
                            menuSubItem("Single debris", tabName = "tab_density"),
                            menuSubItem("Synthesis", tabName = "tab_syns"))
       ),
       conditionalPanel(condition = "input.menu1 == 'tab_density'",
                        selectInput("select2", "Debris type",
                                    list("Artificial Product" = c("FGF","FGN","FGO","EPS","PBA","PBO","FP","PC","G","M",
                                                                  "W","UO"),
                                         "Natural Product" = c("NO","SW","DW"),
                                         "UnKnown" = c("","UK")),
                                    size = NULL,
                                    selectize = TRUE,width = "200px")),
       conditionalPanel(condition = "input.menu1 == 'tab_syns'",
                        sliderInput("slider4", "longitude",
                                    min = 100, max = 180, value = c(120, 160),step = 5),
                        sliderInput("slider5", "latitude",
                                    min = 10, max = 80, value = c(25, 60),step = 5)),
       conditionalPanel(condition = "input.menu1 == 'tab_density'",
                        sliderInput("slider4", "longitude",
                                    min = 100, max = 180, value = c(120, 160),step = 5),
                        sliderInput("slider5", "latitude",
                                    min = 10, max = 80, value = c(25, 60),step = 5))
     ),
     dashboardBody(
       tabItems(
         tabItem_Info,
         tabItem_survey,
         tabItem_density,
         tabItem_syns
       )
     ),
     skin="blue"
   )

   shinyApp(ui=ui, server=server)

 }#shinymalia

 shinymalia()
