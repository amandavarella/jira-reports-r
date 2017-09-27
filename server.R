library(shiny)
library(ggplot2)
library(graphics)
library(scales)
library(grid)
library(MASS)
library(car)
library(data.table)
library(httr)
library(jsonlite)
library(curl)
library(RCurl)
library(stringr)
library(stats)
library(zoo)
library(lubridate)
library(DT)
library(plyr)

source("getleadtime.R")
source("monteCarlo.R")

# Initialize
# Para acrescentar projetos, é necessário atualizar as variáveis abaixo
###################################################
jiraAddress = ""

print("iniciando")

vProj <- c("ATLAS","SKYWALKER", "ROGUEONE", "ATOMS", "iB2B")


vTotalProj <- length(vProj)


vIniDate <- c("2017-03-01", "2017-07-01", "2017-04-01", "2017-04-01", "2017-04-01")


vTypeClosed <- c(
               "'Story','Task','Bug'",
               "'Story','Task','Bug'",
               "'Story','Task','Bug'",
               "'Story','Task','Bug'",
               "'Story','Task','Bug'"
               )


rView <- c(116, 586, 1691, 119, 719)



ccColumns <- list(
                     c(3794,603,3788,3784),
                     c(3478,3479,3480,5551),
                     c(12008,12009,12010,12011,12012,12007),
                     c(1781,1782,1785,612,1768),
                     c(5004,6115,5003)
                   )



ccSwimLaneId <- list(
                       c(169),
                       c(895,2545,2547),
                       c(3347),
                       c(172),
                       c(1161)
                     )


ccQuickFilterId <- c(1875, 4954, 4957, 5079, 5080)


cfColumns <- list(
                    c(602,3794,603,3788,3784,604),
                    c(3478,3479,5551,3480,3481),
                    c(12006,12007,12008,12009,12010,12011,12012,12013),
                    c(611,1768,612,1781,1782,1785,1789),
                    c(5002,5003,6115,5004,6118)
                  )


cfSwimLane <- list(
                     c(169),
                     c(895,2545,2547),
                     c(3347),
                     c(172),
                     c(1161)
                   )

testVectorSizes <- c(length(vProj), length(vIniDate), length(vTypeClosed), length(rView), length(ccColumns), length(ccSwimLaneId), length(ccQuickFilterId), length(cfColumns), length(cfSwimLane))

if(length(unique(testVectorSizes)) > 1){
  print("Tamanhos de vetores diferentes")
  print(testVectorSizes)
  stopApp(returnValue=NULL)
}


############################################################################################
##No arquivo ui.R, alterar tabPanel("Projeto", para incluir o projeto desejado na interface
###########################################################################################

vLTSP <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

# server logic required to draw the selected graphic
shinyServer(function(input, output, session) {
      print("vTotalProj")    
      print(vTotalProj)    
  
    # Session global variables
      listProjClosed <<- vector("list", vTotalProj) #mudei aqui de 3 para vTotalProj
      listProjLT <<- vector("list", vTotalProj) #mudei aqui de 3 para vTotalProj
      listProjMeanLT <<- vector("list", vTotalProj) #mudei aqui de 3 para vTotalProj
      listProjThroughput <<- vector("list", vTotalProj) #mudei aqui de 3 para vTotalProj

      indProj <<- 0
      vQ <<- vector("numeric", 0)
      vLQ <<- vector("character", 0)
      avg <<- 0
      md <<- 0
      sh <<- 0
      sc <<- 0
      aut <<- FALSE
      
      print("antes de ler o TP")
    # Throughput
      output$plotThroughput <- renderPlot({
            shiny::validate(
                  need(aut, "Entre com chave e senha!"),
                  need(input$selProj != "SEL", "Selecione Projeto!")
            )

            proj <- input$selProj
            
           
            if(jiraAddress !=""){
                closedProj <- listProjClosed[[indProj]]
                if (is.null(closedProj)) {
                  return()
                }
              
                closed <- prepareThroughputData(closedProj)
                if (is.null(closed)) {
                      return()
                }
                closed$Data <- as.yearmon(closed$Data, "%Y-%m-%d")
                
                #writes the closed data into a csv file
                write.csv(closed, file = paste("./data/",proj,"-closed",".csv",sep=""))
            }
            else{
              closed <- read.csv(paste("./data/",proj,"-closed",".csv",sep=""), header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
            }
                
          
     

          # Set complete range of Dates
            primeDate <- as.Date(vIniDate[indProj])
            finalDate <- Sys.Date()
            numMon <- floor(((as.yearmon(strptime(finalDate, format="%Y-%m-%d")) -
                         as.yearmon(strptime(primeDate, format="%Y-%m-%d"))) * 12) + 0.5)
            vDate <- c(primeDate, primeDate %m+% months(1:(numMon-1)))

            dfTP <- data.frame(Data=double(numMon), Quant=integer(numMon), row.names=NULL)
            dfTP$Data <- as.yearmon(vDate, "%Y-%m-%d")
            dfTP$Quant <- rep(0, numMon)


          # Generate throughput data for stories and melhorias
            stories <- closed[closed$Tipo.de.Pendência %in% c("Story", "Melhoria"), ]
            storyThroughput <- aggregate(stories$Tipo.de.Pendência, list(stories$Data), length)
            colnames(storyThroughput) <- c("Data", "Quantidade")
            
            
            if(jiraAddress ==""){
              storyThroughput$Data <- paste("01", storyThroughput$Data, sep = " ")
              storyThroughput$Data <- as.yearmon(storyThroughput$Data, "%d %b %Y")
            }
            

          # Complete throughput data with missing months
            storyThroughput <- merge(storyThroughput, dfTP, by="Data", all=TRUE)
            storyThroughput$Quantidade[is.na(storyThroughput$Quantidade)] <- 0
            if (ncol(storyThroughput) > 2)  {
                  storyThroughput <- storyThroughput[, 1:2]
            }
            storyThroughput$Data <- as.Date(storyThroughput$Data)

          # Generate throughput data for all types of issues
            tt <- aggregate(closed,
                  list(closed$Data, closed$Tipo.de.Pendência), length)
            if (ncol(tt) > 3)  {
                  tt <- tt[, 1:3]
            }
            
            colnames(tt) <- c("Data", "Tipo.de.Pendência", "Quantidade")
            
            if(jiraAddress ==""){
              tt$Data <- paste("01", tt$Data, sep = " ")
              tt$Data <- as.yearmon(tt$Data, "%d %b %Y")
            }
              

          # Complete all types throughput data with missing months
            typeThroughput <- data.frame(Data=double(0),
                  Tipo.de.Pendência=character(0), Quantidade=integer(0), row.names=NULL)
            vtl <- levels(as.factor(tt$Tipo.de.Pendência))
            vt <- vtl[!vtl %in% c("Story", "Melhoria", "Bug")]
            nvt <- length(vt)

            vtt <- "Bug"
            for (i in 1:nvt) {
                  vtt <- c(vtt, vt[i])
            }
            if ("Melhoria" %in% vtl) {
                  vtt <- c(vtt, "Melhoria")
            }
            vt <- c(vtt, "Story")
            nvt <- length(vt)
            for (i in 1:nvt) {
                  mtt <- tt[tt$Tipo.de.Pendência==vt[i], ]
                  mtt <- merge(mtt, dfTP, by="Data", all=TRUE)
                  mtt$Tipo.de.Pendência[is.na(mtt$Tipo.de.Pendência)] <- vt[i]
                  typeThroughput <- rbind(typeThroughput, mtt)
            }
            typeThroughput$Quantidade[is.na(typeThroughput$Quantidade)] <- 0
            if (ncol(typeThroughput) > 3)  {
                  typeThroughput <- typeThroughput[, 1:3]
            }

          # Order typeThroughput by date
            orderTT <- order(typeThroughput$Data)
            typeThroughput <- typeThroughput[orderTT, ]
            typeThroughput$Data <- as.Date(typeThroughput$Data)

          # Generate graph
            dfTP$Data <- as.Date(dfTP$Data)
            par(mar=c(5, 4, 4, 2) + 0.1)
            if (input$porTipo == TRUE) {
                  g <- ggplot(typeThroughput, aes(x=Data, y=Quantidade, group=Tipo.de.Pendência),
                              environment=environment()) +
                        labs(title=proj) +
                        theme(plot.title=element_text(vjust=2, face="bold")) +
                        theme(axis.title.x=element_text(vjust=-0.5, face="bold")) +
                        theme(axis.title.y=element_text(vjust=1.5, face="bold")) +
                        theme(axis.text.x=element_text(vjust=0.5, angle=45)) +
                        theme(plot.margin=unit(c(1, 3, 1, 1), "lines")) +
                        labs(x="Data de Resolução", y="Quantidade") +
                        scale_x_date(breaks=dfTP$Data, labels=date_format("%b-%Y")) ##+
                        ##scale_y_discrete(breaks=vLTSP, labels=vLTSP)

                  g <- g + geom_line(aes(colour=Tipo.de.Pendência), size=1.2)

            } else {
                  yt <- floor(max(storyThroughput$Quantidade) + 0.5)
                  yt <- floor(yt/10 + 0.5)
                  vy <- c(0, (1:yt)*10)

                  g <- ggplot(storyThroughput, aes(x=Data, y=Quantidade),
                        environment=environment()) + labs(title=proj) +
                        theme(plot.title=element_text(vjust=2, face="bold")) +
                        theme(axis.title.x=element_text(vjust=-0.5, face="bold")) +
                        theme(axis.title.y=element_text(vjust=1.5, face="bold")) +
                        theme(axis.text.x=element_text(vjust=0.5, angle=45)) +
                        theme(plot.margin=unit(c(1, 3, 1, 1), "lines")) +
                        labs(x="Data de Resolução", y="Quantidade - Story") +
                        scale_x_date(breaks=storyThroughput$Data, labels=date_format("%b-%Y")) ##+
                        ##scale_y_discrete(breaks=vy, labels=vy)
                  ##Comentando escala

                  g <- g + geom_line(colour="midnightblue", size=1.2)
            }

            gt <- ggplot_gtable(ggplot_build(g))
            gt$layout$clip[gt$layout$name=="panel"] <- "off"
            grid.draw(gt)
      })

    # Average Lead Time
      output$plotLT <- renderPlot({
            shiny::validate(
                  need(aut, "Entre com chave e senha!"),
                  need(input$selProj != "SEL", "Selecione Projeto!")
            )

            ltProj <- listProjLT[[indProj]]
            if (is.null(ltProj)) {
                  return()
            }

            meanLT <- prepareMeanLTData(ltProj)
            if (is.null(meanLT)) {
                  return()
            }
            

            proj <- input$selProj
            
            #writes the meanLT data into a csv file
            write.csv(meanLT, file = paste("./data/",proj,"-meanLT",".csv",sep=""))

            mLT <- aggregate(meanLT$Lead.Time, list(meanLT$Data), mean)
            colnames(mLT) <- c("Data", "LT.Médio")
            sdLT <- aggregate(meanLT$Lead.Time, list(meanLT$Data), sd)
            mLT <- cbind(mLT, sdLT[, 2])
            q25LT <- aggregate(meanLT$Lead.Time, list(meanLT$Data), FUN=quantile, probs=0.25)
            mLT <- cbind(mLT, q25LT[, 2])
            q75LT <- aggregate(meanLT$Lead.Time, list(meanLT$Data), FUN=quantile, probs=0.75)
            mLT <- cbind(mLT, q75LT[, 2])

            minLT <- aggregate(meanLT$Lead.Time, list(meanLT$Data), min)
            mLT <- cbind(mLT, minLT[, 2])
            maxLT <- aggregate(meanLT$Lead.Time, list(meanLT$Data), max)
            mLT <- cbind(mLT, maxLT[, 2])
            colnames(mLT) <- c("Data", "LT.Médio", "LT.Std", "LT.PQuantil", "LT.TQuantil", "LT.Mínimo", "LT.Máximo")

            yhi <- floor(min(mLT$LT.PQuantil) / 20)
            yhf <- floor(max(mLT$LT.TQuantil) + 0.5)
            yhf <- floor(yhf/20 + 0.5)
            vy <- c((yhi:yhf)*20)

            par(mar=c(5, 4, 4, 2) + 0.1)
            g <- ggplot(mLT, aes(x=Data, y=LT.Médio),
                  environment=environment()) + labs(title=proj) +
                  theme(plot.title=element_text(vjust=2, face="bold")) +
                  theme(axis.title.x=element_text(vjust=-0.5, face="bold")) +
                  theme(axis.title.y=element_text(vjust=1.5, face="bold")) +
                  theme(axis.text.x=element_text(vjust=0.5, angle=45)) +
                  theme(plot.margin=unit(c(1, 3, 1, 1), "lines")) +
                  labs(x="Data de Resolução", y="Lead Time Médio - Story") +
                  scale_x_date(breaks=mLT$Data, labels=date_format("%b-%Y")) ##+
                  ##scale_y_discrete(breaks=vy, labels=vy)

            g <- g + geom_line(colour="midnightblue", size=1.2) +
                  geom_line(aes(Data, LT.PQuantil), colour="steelblue", size=1) +
                  geom_line(aes(Data, LT.TQuantil), colour="steelblue", size=1) +
                  geom_ribbon(aes(ymin=LT.PQuantil, ymax=LT.TQuantil),
                        colour="steelblue", fill="steelblue", alpha=0.3) +
                  geom_point(colour="midnightblue", size=3.5)

            gt <- ggplot_gtable(ggplot_build(g))
            gt$layout$clip[gt$layout$name=="panel"] <- "off"
            grid.draw(gt)
      })

    # Scatter Plot chart
      output$plotSP <- renderPlot({
        
       
        shiny::validate(
              need(aut, "Entre com chave e senha!"),
              need(input$selProj != "SEL", "Selecione Projeto!")
        )
        proj <- input$selProj
        
        if(jiraAddress !=""){

            ltProj <- listProjLT[[indProj]]
            if (is.null(ltProj)) {
                  return()
            }
            
            #writes the ltProj data into a csv file
            write.csv(ltProj, file = paste("./data/",proj,"-ltProj",".csv",sep=""))
            
        }else{
          ltProj <- read.csv(paste("./data/",proj,"-ltProj",".csv",sep=""), header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
          ltProj$Data.de.Resolução <- as.Date(ltProj$Data.de.Resolução)
        }

            lt <- ltProj$Lead.Time
            avg <<- mean(lt)
            md <<- median(lt)
            vQ <<- quantile(lt, probs=c(0.3, 0.5, 0.7, 0.85, 0.95))
            vLQ <<- names(vQ)
            vQ <<- as.vector(vQ)

            par(mar=c(5, 4, 4, 2) + 0.1)
            g <- ggplot(ltProj, environment=environment()) + labs(title=proj) +
                  theme(plot.title=element_text(vjust=2, face="bold")) +
                  theme(axis.title.y=element_text(vjust=1.5, face="bold")) +
                  theme(axis.title.x=element_text(vjust=-1, face="bold")) +
                  theme(axis.text.x=element_text(vjust=0.5, angle=45)) +
                  theme(plot.margin=unit(c(1, 3, 1, 1), "lines")) +
                  labs(x="Data de Resolução", y="Lead Time - Story") +
                  geom_hline(yintercept=vQ[1], color="royalblue4", size=1, linetype=2) +
                  geom_hline(yintercept=vQ[2], color="royalblue1", size=1, linetype=2) +
                  geom_hline(yintercept=vQ[3], color="yellow", size=1, linetype=2) +
                  geom_hline(yintercept=vQ[4], color="orange", size=1, linetype=2) +
                  geom_hline(yintercept=vQ[5], color="red", size=1, linetype=2) +
                  geom_hline(yintercept=avg, color="steelblue1", size=1.5, linetype=1) +
                  geom_point(aes(Data.de.Resolução, Lead.Time), colour="gray45", alpha=0.7, size=6)

            df <- as.data.frame(table(as.data.frame(ggplot_build(g)$data[[1]][, 1:2])))
            counts <- df[df$Freq>1, ]
            counts$x <- as.double(as.character(counts$x))
            vx <- as.Date(counts$x, origin="1970-01-01 UTC")
            counts$Freq <- as.character(counts$Freq)

            xh <- floor(max(lt) + 0.5)
            xh <- floor(xh/20 + 0.5)
            vx <- c(0, (1:xh)*20)

            g <- g + scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%b-%Y")) +
                  ##scale_y_discrete(breaks=vx, labels=vx) +
                  annotate("text", x=as.Date(Inf, origin="1970-01-01"), y=vQ,
                           hjust=0, label=vLQ, size=3.5)

            gt <- ggplot_gtable(ggplot_build(g))
            gt$layout$clip[gt$layout$name=="panel"] <- "off"
            grid.draw(gt)
      })

    # Histogram
      output$plotHist <- renderPlot({
            shiny::validate(
                  need(aut, ""),
                  need(input$selProj != "SEL", "")
            )
            proj <- input$selProj
            
            if(jiraAddress !=""){
                  ltProj <- listProjLT[[indProj]]
                  if (is.null(ltProj)) {
                        return()
                  }
            }else{
              ltProj <- read.csv(paste("./data/",proj,"-ltProj",".csv",sep=""), header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
              ltProj$Data.de.Resolução <- as.Date(ltProj$Data.de.Resolução)
            }
            

            lt <- ltProj$Lead.Time
            
            avg <<- mean(lt)
            md <<- median(lt)
            vQ <<- quantile(lt, probs=c(0.3, 0.5, 0.7, 0.85, 0.95))
            vLQ <<- names(vQ)
            vQ <<- as.vector(vQ)

            par(mar=c(5, 2, 4, 2) + 0.1)
            g <- ggplot(ltProj, environment=environment()) + labs(title=proj) +
                  theme(plot.title=element_text(vjust=2, face="bold")) +
                  theme(axis.title.x=element_text(vjust=-0.5, face="bold")) +
                  theme(axis.title.y=element_text(vjust=1.5, face="bold")) +
                  labs(x="Lead Time - Story", y="Itens Feitos") +
                  geom_histogram(aes(x=Lead.Time), color="black", fill="gray", binwidth=1)
            

            xh <- floor(max(lt) + 0.5)
            xh <- floor(xh/20 + 0.5)
            vx <- c(0, (1:xh)*20)
            yh <- max(ggplot_build(g)$data[[1]]$count)
            yh <- yh / 2
            vy <- c(0, (1:yh)*2)

            g <- g + 
                  ##scale_x_discrete(breaks=vx, labels=vx) +
                  ##scale_y_discrete(breaks=vy, expand=waiver()) +
                  geom_vline(xintercept=vQ[1], color="royalblue4", size=1, linetype=2) +
                  geom_vline(xintercept=vQ[2], color="royalblue1", size=1, linetype=2) +
                  geom_vline(xintercept=vQ[3], color="yellow", size=1, linetype=2) +
                  geom_vline(xintercept=vQ[4], color="orange", size=1, linetype=2) +
                  geom_vline(xintercept=vQ[5], color="red", size=1, linetype=2) +
                  geom_vline(xintercept=avg, color="steelblue1", size=1.5, linetype=1) +
                  annotate("text", x=vQ, y=Inf, vjust=-0.5, label=vLQ, size=4)

            gt <- ggplot_gtable(ggplot_build(g))
            gt$layout$clip[gt$layout$name=="panel"] <- "off"
            grid.draw(gt)
      })

    # Weibull curves
    #https://connected-knowledge.com/2014/09/08/how-to-match-to-weibull-distribution-without-excel/
      
      output$plotWeibull <- renderPlot({
            shiny::validate(
                  need(aut, ""),
                  need(input$selProj != "SEL", "")
            )

            #proj <- input$selProj
            proj <- "ACT"

            ltProj <- listProjLT[[indProj]]
            if (is.null(ltProj)) {
                  return()
            }

            lt <- ltProj$Lead.Time
            avg <<- mean(lt)
            md <<- median(lt)

            fd <- fitdistr(lt, densfun="weibull", lower=0)
            sh <<- as.numeric(fd$estimate[1])
            sc <<- as.numeric(fd$estimate[2])
            mode <- as.numeric(NA)
            if ( sh < 0.87) {
                  vQ <<- c(avg*0.5152, avg, avg*0.8399, avg*1.298, avg*1.584, avg*2.349,
                        avg*3.041, avg*4.319, avg*6.164, avg*7.662, avg*12.40)
                  vLQ <<- c("50%", "68%", "63%", "75%", "80%", "85%", "90%", "95%", "98%", "99%", "99.865%")
            } else if (sh < 1.12) {
                  vQ <<- c(avg*0.6931, avg, avg, avg*1.386, avg*1.609, avg*1.897,
                        avg*2.303, avg*3.000, avg*3.912, avg*4.605, avg*6.608)
                  vLQ <<- c("50%", "63%", "63%", "75%", "80%", "85%", "90%", "95%", "98%", "99%", "99.865%")
            } else if (sh < 1.37) {
                  mode <- avg * 0.2963
                  vQ <<- c(avg*0.8008, avg, avg*1.074, avg*1.394, avg*1.571, avg*1.792,
                        avg*2.092, avg*2.583, avg*3.197, avg*3.643, avg*4.863)
                  vLQ <<- c("50%", "60%", "63%", "75%", "80%", "85%", "90%", "95%", "98%", "99%", "99.865%")
            } else if (sh < 1.62) {
                  mode <- avg * 0.5325
                  vQ <<- c(avg*0.8676, avg, avg*1.108, avg*1.377, avg*1.521, avg*1.698,
                        avg*1.932, avg*2.302, avg*2.752, avg*3.067, avg*3.901)
                  vLQ <<- c("50%", "58%", "63%", "75%", "80%", "85%", "90%", "95%", "98%", "99%", "99.865%")
            } else if (sh < 1.87) {
                  mode <- avg * 0.7979
                  vQ <<- c(avg*0.9394, avg, avg*1.128, avg*1.329, avg*1.432, avg*1.554,
                        avg*1.712, avg*1.953, avg*2.232, avg*2.421, avg*2.901)
                  vLQ <<- c("50%", "54%", "63%", "75%", "80%", "85%", "90%", "95%", "98%", "99%", "99.865%")
            } else {
                  mode <- avg * 0.9783
                  vQ <<- c(avg*0.9911, avg, avg*1.120, avg*1.249, avg*1.312, avg*1.386,
                        avg*1.479, avg*1.614, avg*1.765, avg*1.863, avg*2.101)
                  vLQ <<- c("50%", "51%", "63%", "75%", "80%", "85%", "90%", "95%", "98%", "99%", "99.865%")
            }

            par(mar=c(4, 2, 4, 2) + 0.1)
            xmax <- round(max(vQ))
            lx <- round(xmax/10)
            dw <- curve(dweibull(x, scale=sc, shape=sh), from=0, to=xmax,
                  main=labs(title=proj), xlab="Lead Time - Story", ylab=" ",
                  font.main=2, font.lab=2,
                  lab=c(lx, 5, 7), yaxt="n", las=1, lwd=2, cex.axis=0.8)

            if (sh >= 1.25) {
                  abline(v=mode, col="magenta", lwd=2.5, lty=1)
            }
            abline(v=md, col="royalblue1", lwd=2.5, lty=1)
            abline(v=vQ[1], col="royalblue1", lwd=2, lty=2)
            abline(v=vQ[2], col="steelblue1", lwd=2.5, lty=1)
            abline(v=vQ[3], col="cyan", lwd=2.5, lty=1)
            abline(v=vQ[4], col="seagreen4", lwd=2, lty=2)
            abline(v=vQ[5], col="yellowgreen", lwd=2, lty=2)
            abline(v=vQ[6], col="orange", lwd=2, lty=2)
            abline(v=vQ[7], col="sienna1", lwd=2, lty=2)
            abline(v=vQ[8], col="sienna3", lwd=2, lty=2)
            abline(v=vQ[9], col="tomato", lwd=2, lty=2)
            abline(v=vQ[10], col="red", lwd=2, lty=2)
            abline(v=vQ[11], col="red3", lwd=2, lty=2)
            mtext(vLQ, side=3, at=vQ, cex=0.9)
      })

    # Scatter Plot Statistics
      output$tableSP <- DT::renderDataTable({
            shiny::validate(
                  need(input$selProj != "SEL", "")
            )

            if (is.null(listProjLT[[indProj]])) {
                  return()
            }

            tLTNames <- c("Average", "Median", vLQ)
            avg <<- as.numeric(format(avg, digits=2, nsmall=2))
            md <<- as.numeric(format(md, digits=2, nsmall=2))
            vQ <<- as.numeric(format(vQ, digits=2, nsmall=2))
            tLTValues <- c(avg, md, vQ)
            mLT <- matrix(tLTValues, nrow=1)
            colnames(mLT) <- tLTNames
            mLT
      }, options=list(paging=FALSE, info=FALSE, searching=FALSE, ordering=FALSE))

    # Histogram Statistics
      output$tableHist <- DT::renderDataTable({
        shiny::validate(
          need(input$selProj != "SEL", "")
        )
        
        if (is.null(listProjLT[[indProj]])) {
          return()
        }
        
        tLTNames <- c("Average", "Median", vLQ)
        avg <<- as.numeric(format(avg, digits=2, nsmall=2))
        md <<- as.numeric(format(md, digits=2, nsmall=2))
        vQ <<- as.numeric(format(vQ, digits=2, nsmall=2))
        tLTValues <- c(avg, md, vQ)
        mLT <- matrix(tLTValues, nrow=1)
        colnames(mLT) <- tLTNames
        mLT
      }, options=list(paging=FALSE, info=FALSE, searching=FALSE, ordering=FALSE))

    # Weibull Parameters
      output$tableWeibullParam <- DT::renderDataTable({
            shiny::validate(
                  need(input$selProj != "SEL", "")
            )

            if (is.null(listProjLT[[indProj]])) {
                  return()
            }

            tLTNames <- c("Shape", "Scale")
            sh <<- as.numeric(format(sh, digits=2, nsmall=2))
            sc <<- as.numeric(format(sc, digits=2, nsmall=2))
            tLTValues <- c(sh, sc)
            mLT <- matrix(tLTValues, nrow=1)
            colnames(mLT) <- tLTNames
            mLT
      }, options=list(paging=FALSE, info=FALSE, searching=FALSE, ordering=FALSE))

    # Weibull Statistics
      output$tableWeibullValues <- DT::renderDataTable({
            shiny::validate(
                  need(input$selProj != "SEL", "")
            )

            if (is.null(listProjLT[[indProj]])) {
                  return()
            }

            tLTNames <- c("Average", "Median", vLQ)
            avg <<- as.numeric(format(avg, digits=2, nsmall=2))
            md <<- as.numeric(format(md, digits=2, nsmall=2))
            vQ <<- as.numeric(format(vQ, digits=2, nsmall=2))
            tLTValues <- c(avg, md, vQ)
            mLT <- matrix(tLTValues, nrow=1)
            colnames(mLT) <- tLTNames
            mLT
      }, options=list(paging=FALSE, info=FALSE, searching=FALSE, ordering=FALSE))
      
      
      # PI Panel Epics
      output$piPanelEpics <- renderPlot({
        shiny::validate(
          need(aut, ""),
          need(input$selProj != "SEL", "")
        )

        gt = getBurndownPI("Epic", jiraAddress)
        plot(gt)

      })
      
      # PI Panel Stories
      output$piPanelStories <- renderPlot({
        shiny::validate(
          need(aut, ""),
          need(input$selProj != "SEL", "")
        )
       
        gt = getBurndownPI("Story", jiraAddress)
        plot(gt)
        
      })
      
      
      # Monte Carlo Panel
      output$plotMonteCarlo <- renderPlot({
        shiny::validate(
          need(aut, ""),
          need(input$selProj != "SEL", "")
        )
        
        p <- monteCarlo()
        
        #gt <- ggplot_gtable(ggplot_build(g))
        #gt$layout$clip[gt$layout$name=="panel"] <- "off"
        grid.draw(p)
        
        #####
        
      })
      

    # Observe for authentication
      observeEvent(input$okAut, {
            

            user <- input$chave
            pass <- input$senha
            
            if (jiraAddress!=""){
            aut <- jiraAuthentication(jiraAddress, user, pass)
            }
            else{
              aut <<- TRUE
            }
            
            if (!aut) {
                  updateTextInput(session, "chave", value="")
                  updateTextInput(session, "senha", value="")
            }
            else {
                  updateTabsetPanel(session, "tabset", selected = "Projeto")
            }
      })

    # Observe for project selection
      observeEvent(input$selProj, {
            
            if (input$selProj != "SEL") {
                  proj <- input$selProj

                  
                  indProj <<- grep(paste("^",proj,"$",sep = ""), vProj)
                  
                  #Refatorar
                  proj <- "ACT"
                  print(input$selProj)
                  
                  if(input$selProj=="ATLAS"){
                    team = 90
                  }else if(input$selProj=="SKYWALKER"){
                    team = 85
                  }else if(input$selProj=="ROGUEONE"){
                    team = 91
                  }
                  else if(input$selProj=="ATOMS"){
                    team = 93
                  }
                  else if(input$selProj=="iB2B"){
                    team = 89
                  }

                  if (jiraAddress != ""){
                        if (is.null(listProjLT[[indProj]])) {
                              ltProj <- getProjectLT(proj, team, jiraAddress, rView, ccSwimLaneId, ccQuickFilterId, ccColumns, vTypeClosed, vIniDate)
                              if (is.null(ltProj)) {
                                    return()
                              }
                        }
                  }

                # Update Lead Time date range
                  ip <- as.Date(vIniDate[indProj]) %m+% months(2)
                  fp <- Sys.Date() %m-% months(1)
                  updateDateRangeInput(session, "periodo",
                        label="Período:", start=ip, end=fp)
            }
      })

    # Observe for tab selection
      observeEvent(input$tabset, {
            if (!aut) {
                  updateTabsetPanel(session, "tabset", selected = "Login")
            } else if (input$selProj == "SEL") {
                  updateTabsetPanel(session, "tabset", selected = "Projeto")
            }
      })

    # Observe for quit button
      observeEvent(input$sair, {
            stopApp(returnValue=NULL)
      })

    # Reactive for average lead time period
      outLT <- eventReactive(input$okLT, {

      })

})


prepareMeanLTData <- function(ltProj) {

    # If not already prepared, prepare project mean lead time data
      if (is.null(listProjMeanLT[[indProj]])) {
            iniDate <- as.Date(vIniDate[indProj])
            primeDate <- as.Date(iniDate) %m+% months(2)
            finalDate <- Sys.Date()

            numMon <- as.integer(ceiling((as.yearmon(strptime(finalDate, format="%Y-%m-%d")) -
                       as.yearmon(strptime(primeDate, format="%Y-%m-%d"))) * 12))

            numMon <- floor(((as.yearmon(strptime(finalDate, format="%Y-%m-%d")) -
                                as.yearmon(strptime(primeDate, format="%Y-%m-%d"))) * 12) + 0.5)

            meanLT <- data.frame(Data=as.Date(character(0)), Lead.Time=integer(0), row.names=NULL)

            pd <- iniDate %m-% months(1)
            fd <- pd %m+% months(2)
            for (i in 1:numMon) {
                  pd <- pd %m+% months(1)
                  fd <- fd %m+% months(1)
                  partLT <- subset(ltProj, ltProj$Data.de.Resolução>=pd & ltProj$Data.de.Resolução<=fd,
                                    select=Lead.Time)
                  nRow <- nrow(partLT)
                  if (nRow > 0) {
                        data <- rep(fd, nRow)
                        meanLT <- rbind(meanLT, cbind(data, partLT))
                  }
                  else {
                        data <- rep(fd, 1)
                        partLT <- rbind(partLT, 0)
                        colnames(partLT) <- "Lead.Time"
                        meanLT <- rbind(meanLT, cbind(data, partLT))
                  }
            }

            if (nrow(meanLT) == 0) {
                  return(NULL)
            }

            colnames(meanLT) <- c("Data", "Lead.Time")

          # Update Lead Time list
            listProjMeanLT[[indProj]] <<- meanLT
      }
      return(listProjMeanLT[[indProj]])
}

prepareThroughputData <- function(closedProj) {

    # If not already prepared, prepare project mean lead time data
      if (is.null(listProjThroughput[[indProj]])) {
            nRow <- nrow(closedProj)
            closed <- data.frame(Data=as.Date(nRow),
                     Tipo.de.Pendência=character(nRow), row.names=NULL)

            closed$Data <- as.Date(closedProj$Data.de.Resolução, "%d/%m/%Y", tz="")
            closed$Tipo.de.Pendência <- closedProj$Tipo.de.Pendência

            if (nrow(closed) == 0) {
                  return(NULL)
            }

            orderClosed <- order(closed$Data, closed$Tipo.de.Pendência)
            closed <- closed[orderClosed, ]

          # Update throughput list
            listProjThroughput[[indProj]] <<- closed
      }

      return(listProjThroughput[[indProj]])
}
