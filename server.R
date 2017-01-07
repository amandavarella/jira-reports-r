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

# Initialize
# Para acrescentar projetos, é necessário atualizar as variáveis abaixo
###################################################


vProj <- c("SAG", "SGA", "SIRR", "VRT", "SIPLEX", "VRTCR", "GIPCR")
vTotalProj <- length(vProj)


vIniDate <- c("2013-01-01", "2013-01-01", "2013-01-01", "2015-01-01", "2014-07-01", "2015-10-01", "2015-10-01")


vTypeClosed <- c("'Story','Suporte','Débito Técnico','Reunião','Defeito'",
               "'Story','Suporte','Consultoria','Reunião','Defeito'",
               "'Story','Melhoria','Débito Técnico','Technical Support','Defeito'",
               "'Story','Melhoria','Débito Técnico','Suporte','Defeito','Epic','Tarefa'",
               "'Story','Melhoria','Débito Técnico','Suporte','Defeito','Epic','Tarefa'",
               "'Story','Melhoria','Atividade de Atendimento de Desenvolvimento','Manutenção Corretiva Produção','Manutenção Corretiva Emergencial Produção'",
               "'Story','Melhoria','Atividade de Atendimento de Desenvolvimento','Epic','Manutenção Corretiva Produção'")


rView <- c(23, 1022, 711, 981, 682, 5299, 5473)



ccColumns <- list(
                     c(67, 66, 59, 2667, 69, 65, 68, 3757),
                     c(2269, 2658, 2271, 2272, 7252, 7255),
                     c(1366, 1373, 1374, 1378, 2917, 6482, 6792),
                     c(13903,	2136,	2137,	2394,	2395),
                     c(1566, 2179, 7180, 7181, 1223),
                     c(13587,13588,13589,13590,13591,13592,13593,13594,13595,13596,13597,13598,13599),
                     c(14268,14269,14270,14271,14272,14273,14274,14275,14276,14277,14278,14279,14267)
                   )




ccSwimLaneId <- list(
                       c(1407, 1209),
                       c(1435, 1436, 1437, 1438, 1439, 1433),
                       c(888, 986, 1253, 7191, 1712, 894, 889),
                       c(1363, 1486, 1487, 1488, 1489, 1490, 1524, 1362),
                       c(2585, 3718, 4993, 6292, 6461, 6904, 8195, 826,	827, 828, 829, 831, 832, 833, 835, 9045, 825),
                       c(9981,8818,8819,8820),
                       c(12399,	12429, 12430,	12431,	9026,	9740, 9025)
                     )


ccQuickFilterId <- c(1173, 1669, 923, 9455, 9624, 10301, 11880)


cfColumns <- list(
                    c(58, 67, 66, 59, 2667, 69, 65, 68, 60),
                    c(2268, 2269, 2658, 2271, 2272, 2273, 7252, 7255),
                    c(1365, 1373, 1366, 2917, 1378, 6482, 1374, 6792, 1367),
                    c(2393,	2136,	2395,	2394,	13903,	2137),
                    c(6794, 1223, 1566, 7181, 2179, 7180, 1224),
                    c(13586,13587,13588,13589,13590,13591,13592,13593,13594,13595,13596,13597,13598,13599,13600),
                    c(14266, 14267,	14268,	14269,	14270,	14271,	14272,	14273,	14274,	14275,	14276,	14277,	14278,	14279,	14280)
                  )


cfSwimLane <- list(
                     c(1306, 1407, 1209, 1208, 27, 28),
                     c(1435, 1436, 1437, 1438, 1439, 1433),
                     c(1253, 1712, 7191, 888, 894, 986, 889),
                     c(1362,	1487,	1524,	1489,	1490,	1486,	1488,	1363),
                     c(825, 4993, 6292, 8195, 3718, 831, 835, 833, 2585, 6904, 832, 829, 828, 827, 6461, 9045, 826),
                     c(9981,8818,8819,8820),
                     c(9025,	12399,	12429,	12430,	12431,	9740,	9026)
                   )


############################################################################################
##No arquivo ui.R, alterar tabPanel("Projeto", para incluir o projeto desejado na interface
###########################################################################################

vLTSP <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

# server logic required to draw the selected graphic
shinyServer(function(input, output, session) {

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

    # Throughput
      output$plotThroughput <- renderPlot({
            shiny::validate(
                  need(aut, "Entre com chave e senha!"),
                  need(input$selProj != "SEL", "Selecione Projeto!")
            )

            proj <- input$selProj

            closedProj <- listProjClosed[[indProj]]
            if (is.null(closedProj)) {
                  return()
            }

            closed <- prepareThroughputData(closedProj)
            if (is.null(closed)) {
                  return()
            }
            closed$Data <- as.yearmon(closed$Data, "%Y-%m-%d")

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

          # Complete throughput data with missing months
            storyThroughput <- merge(storyThroughput, dfTP, by="Data", all.y=TRUE)
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
#            tt$Data <- as.yearmon(tt$Data, "%Y-%m-%d")

          # Complete all types throughput data with missing months
            typeThroughput <- data.frame(Data=double(0),
                  Tipo.de.Pendência=character(0), Quantidade=integer(0), row.names=NULL)
            vtl <- levels(as.factor(tt$Tipo.de.Pendência))
            vt <- vtl[!vtl %in% c("Story", "Melhoria", "Defeito")]
            nvt <- length(vt)

            vtt <- "Defeito"
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
                  mtt <- merge(mtt, dfTP, by="Data", all.y=TRUE)
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
                        scale_x_date(breaks=dfTP$Data, labels=date_format("%b-%Y")) +
                        scale_y_discrete(breaks=vLTSP, labels=vLTSP)

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
                        scale_x_date(breaks=storyThroughput$Data, labels=date_format("%b-%Y")) +
                        scale_y_discrete(breaks=vy, labels=vy)

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
                  scale_x_date(breaks=mLT$Data, labels=date_format("%b-%Y")) +
                  scale_y_discrete(breaks=vy, labels=vy)

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

            ltProj <- listProjLT[[indProj]]
            if (is.null(ltProj)) {
                  return()
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
                  scale_y_discrete(breaks=vx, labels=vx) +
#                     geom_text(aes(label=counts$Freq, x=vx, y=counts$y, vjust=-1, size=3)) +
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

            ltProj <- listProjLT[[indProj]]
            if (is.null(ltProj)) {
                  return()
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

            g <- g + scale_x_discrete(breaks=vx, labels=vx) +
                  scale_y_discrete(breaks=vy, expand=waiver()) +
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
      output$plotWeibull <- renderPlot({
            shiny::validate(
                  need(aut, ""),
                  need(input$selProj != "SEL", "")
            )

            proj <- input$selProj

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

    # Observe for authentication
      observeEvent(input$okAut, {
            j1 <- "https://jira.com.br:8443/rest/auth/1/session"

            user <- input$chave
            pass <- input$senha
            if (user != "" & pass != "") {
                  r1 <- HEAD(j1,
                        config(ssl_verifypeer=FALSE),
                        body=FALSE,
                        add_headers("Accept"="application/json",
                                    "Content-Type"="application/json"),
                        authenticate(user, pass), verbose())

                  if (status_code(r1) == 200) {
                        aut <<- TRUE
                        scookie <<- paste("JSESSIONID=", cookies(r1)$JSESSIONID, "; Path=/Jira", sep="")
                  }
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


                  if (is.null(listProjLT[[indProj]])) {
                        ltProj <- getProjectLT(proj)
                        if (is.null(ltProj)) {
                              return()
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

# Get project Lead Time data
getProjectLT <- function(proj) {
      withProgress(message='Lendo dados do projeto', value=0.1, {

          # Read Cycle Time data
            h1 <- "https://jira.com.br:8443/rest/greenhopper/1.0/rapid/charts/controlchart?"

            param <- paste("rapidViewId=", rView[indProj], sep="")
            v <- as.vector(ccSwimLaneId[[indProj]])
            len <- length(v)
            for (i in 1:len) {
                  param <- paste(param, "&swimlaneId=", v[i], sep="")
            }
            if (!is.na(ccQuickFilterId[indProj])) {
                  param <- paste(param, "&quickfilterId=", ccQuickFilterId[indProj], sep="")
            }

            j1 <- paste(h1, param, "&maxResults=2000", sep="")

            incProgress(0.1)

            r1 <- GET(j1,
                  config(ssl_verifypeer=FALSE),
                  body=FALSE,
                  add_headers("Accept"="application/json",
                        "Content-Type"="application/json",
#                        "Set-Cookie"= scookie))
                        "Set-Cookie"= scookie), verbose())
            if (status_code(r1) != 200 ) {
                  return(NULL)
            }

            incProgress(0.15)

            con <- content(r1, "text")
            dat <- fromJSON(con)
            lIssues <- dat$issues
            nIssues <- length(lIssues$key)
            if (nIssues == 0) {
                  return(NULL)
            }

            lColumns <- dat$columns
            vColumns <- unlist(ccColumns[indProj])
            vShow <- lColumns$id %in% vColumns
            vNames <- lColumns$name[vShow]
            namesCT <- c("Issue", vNames)
            nc = length(namesCT)
            dfCT = data.frame(matrix(NA, nrow=nIssues, ncol=nc))
            names(dfCT) <- namesCT

            dfCT$Issue <- unlist(lIssues$key)
            for (i in 1:nIssues) {
                  dfCT[i, 2:nc] <- round(as.double(unlist(lIssues$totalTime[[i]])[vShow] / 1000 / 3600 / 24), 2)
            }

#            print(dfCT$Issue[order(dfCT$Issue)])
            fCT <- paste("./data/CT_", proj, ".csv", sep="")
            write.csv(dfCT[order(dfCT$Issue), ], file=fCT)

            incProgress(0.15, detail="Consulta ao Jira pode demorar...")

          # Read closed data
            h2 <- "https://jira.com.br:8443/rest/api/2/search?jql="
            param <- paste("project=", proj,
                  " and issuetype in (", vTypeClosed[indProj], ") and status in ('Fechada', 'Em Produção')",
                  " and resolutiondate>=", vIniDate[indProj], sep="")
            j2 <- paste(h2, param, "&maxResults=2000", sep="")

            r2 <- GET(j2,
                  config(ssl_verifypeer=FALSE),
                  body=FALSE,
                  add_headers("Accept"="application/json",
                        "Content-Type"="application/json",
                        "Set-Cookie"= scookie), verbose())
            if (status_code(r2) != 200) {
                  return(NULL)
            }
            incProgress(0.5, detail="")

            con <- content(r2, "text")
            dat <- fromJSON(con)
            lIssues <- dat$issues
            nIssues <- length(lIssues$key)
            if (nIssues == 0) {
                  return(NULL)
            }

            lFields <- dat$issues$fields
            namesClosed <- c("Tipo.de.Pendência", "Issue", "Situação", "Data.de.Resolução" )
            nc = length(namesClosed)
            dfClosed = data.frame(matrix(NA, nrow=nIssues, ncol=nc))
            names(dfClosed) <- namesClosed
            dfClosed$Tipo.de.Pendência <- unlist(lFields$issuetype$name)
            dfClosed$Issue <- unlist(lIssues$key)
            dfClosed$Situação <- unlist(lFields$status$name)
            data <- unlist(lFields$resolutiondate)
            d <- substr(data, 9, 10)
            m <- substr(data, 6, 7)
            y <- substr(data, 1, 4)
            date <- paste(d, "/", m, "/", y, sep="")
            dfClosed$Data.de.Resolução <- date
            listProjClosed[[indProj]] <<- dfClosed
#            cat("  nrow de dfClosed= ", nrow(dfClosed), "\n")
#            print(dfClosed)

            incProgress(0.25)

          # Prepare Lead Time data
            nct <- ncol(dfCT)
            issueType <- c("Story", "Melhoria")
            iniType <- as.character(dfClosed$Tipo.de.Pendência)
            closed <- subset( dfClosed, iniType %in% issueType)
#            cat("  nrow de closed= ", nrow(closed), "\n")
            fclosed <- paste("./data/closed_", proj, ".csv", sep="")
            write.csv(closed[order(closed$Issue), ], file=fclosed)

#            print(closed)
            LT <- merge(dfCT, closed, by="Issue")
            nRow <- nrow(LT)
#            cat("  nrow de LT resultado do merge= ", nRow, "\n")
            if (nRow == 0) {
                  return(NULL)
            }

            for (i in 2:nct) LT[, i] <- as.numeric(LT[, i])
            LT$Lead.Time <- rowSums(LT[, 2:nct])
            LT <- subset(LT, LT$Lead.Time>0 & LT$Lead.Time<=1500)
            if (nrow(LT) == 0) {
                  return(NULL)
            }

          # Sort Lead Time data by date and update Lead Time file
            vDate <- as.Date(LT$Data.de.Resolução, "%d/%m/%Y", tz="")
            ordem <- order(vDate, LT$Issue)
            LT <- LT[ordem, ]
            fLT <- paste("./data/DadosLT_", proj, ".csv", sep="")
            write.csv(LT, file=fLT)

          # Update date format
            LT$Data.de.Resolução <- vDate[ordem]

          # Update Lead Time list
#            cat("  nrow de LT final= ", nrow(LT), "\n")
            listProjLT[[indProj]] <<- LT

            setProgress(1)
            return(LT)
      })
}

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
