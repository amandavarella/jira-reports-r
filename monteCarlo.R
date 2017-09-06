
monteCarlo <- function(x){
  df <- read.csv("closed.csv", header = TRUE, sep = ",", quote = "\"",dec = ",", fill = TRUE, comment.char = "", stringsAsFactors = FALSE)
  df <- df[order(df$Data.de.Resolução),]
  df.tp.por.dia<- data.frame(table(df$Data.de.Resolução))
  
  colnames(df.tp.por.dia) <- c("Date", "Tp")
  
  df.tp.por.dia$Date <- as.Date(df.tp.por.dia$Date)
  
  vetorDatas <- seq(min(df.tp.por.dia[,1]), max(df.tp.por.dia[,1]), by = "day")
  vetorTPVazio <- vector("integer", length(vetorDatas))
  
  matriz = cbind(vetorDatas, vetorTPVazio)
  df.new = data.frame(matriz)
  colnames(df.new) <- c("Date", "Tp")
  df.new$Date <- as.Date(df.new$Date)
  
  df.tp.por.dia <- rbind(df.tp.por.dia, df.new)
  
  df.tp.por.dia <- aggregate(df.tp.por.dia$Tp, by=list(Date=df.tp.por.dia$Date), FUN=sum)
  colnames(df.tp.por.dia) <- c("Date", "Tp")
  
  #Sampling for TP in 30 days
  #I want to know the likelihood of delivering X amount of items in 30 days
  vetorTP <- vector("integer", length=0)
  for (i in 1:5000){
    soma <- sum(sample(df.tp.por.dia$Tp, 30, replace=TRUE))
    vetorTP <- append(vetorTP, soma)
  }

  df.tp.final <- data.frame(vetorTP)
  colnames(df.tp.final) <- c("Tp")
  
  ###descomentar isso aqui
  #return(df.tp.final)
  
  ###########
  lt <- df.tp.final$Tp
  avg <<- mean(lt)
  md <<- median(lt)
  vQ <<- quantile(lt, probs=c(0.3, 0.5, 0.7, 0.85, 0.95))
  vLQ <<- names(vQ)
  vQ <<- as.vector(vQ)
  
  par(mar=c(5, 2, 4, 2) + 0.1)
  
  g<- NULL
  g <- ggplot(df.tp.final) 
  p <- g + geom_histogram(aes(x=df.tp.final$Tp), color="black", fill="gray", binwidth=1) + labs(x="Amount of items in 30 days", y="Freq")
  print(p)
    #+ labs(title=proj) +
    #theme(plot.title=element_text(vjust=2, face="bold")) +
    #theme(axis.title.x=element_text(vjust=-0.5, face="bold")) +
    #theme(axis.title.y=element_text(vjust=1.5, face="bold")) +
    #labs(x="Lead Time - Story", y="Itens Feitos") +
    #geom_histogram(aes(x=df.tp.final$Tp), color="black", fill="gray", binwidth=1)
  
  #xh <- floor(max(lt) + 0.5)
  #xh <- floor(xh/20 + 0.5)
  #vx <- c(0, (1:xh)*20)
  #yh <- max(ggplot_build(g)$data[[1]]$count)
  #yh <- yh / 2
  #vy <- c(0, (1:yh)*2)
  
  #g <- g + 
  #  geom_vline(xintercept=vQ[1], color="royalblue4", size=1, linetype=2) +
  #  geom_vline(xintercept=vQ[2], color="royalblue1", size=1, linetype=2) +
  #  geom_vline(xintercept=vQ[3], color="yellow", size=1, linetype=2) +
  #  geom_vline(xintercept=vQ[4], color="orange", size=1, linetype=2) +
  #  geom_vline(xintercept=vQ[5], color="red", size=1, linetype=2) +
  #  geom_vline(xintercept=avg, color="steelblue1", size=1.5, linetype=1) +
  #  annotate("text", x=vQ, y=Inf, vjust=-0.5, label=vLQ, size=4)
  
  #gt <- ggplot_gtable(ggplot_build(g))
  #gt$layout$clip[gt$layout$name=="panel"] <- "off"
  #grid.draw(gt)
  
  
  #######
  return(p)
  
}





