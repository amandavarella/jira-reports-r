source("jira.R")
library(plan)

# Get project Lead Time data
getProjectLT <- function(proj, team, jiraAddress, rView, ccSwimLaneId, ccQuickFilterId, ccColumns, vTypeClosed, vIniDate) {
  withProgress(message='Lendo dados do projeto', value=0.1, {
    
    # Read Cycle Time data
    h1 <- paste(jiraAddress,"/rest/greenhopper/1.0/rapid/charts/controlchart?", sep="")
    
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
    print(j1)
    
    incProgress(0.1)
    
    r1 <- getJiraContent(j1)
    
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
    
    #COMENTANDO ESCRITA EM ARQUIVO
    #fCT <- paste("./data/CT_", proj, ".csv", sep="")
    #write.csv(dfCT[order(dfCT$Issue), ], file=fCT)
    #DESCOMENTANDO ESCRTA EM ARQUIVO
    
    incProgress(0.15, detail="Consulta ao Jira pode demorar...")
    
    
    
    # Read closed data
    h2 <- paste(jiraAddress,"/rest/api/2/search?jql=",sep="")
    param <- paste("project=", proj,
                   " and issuetype in (", vTypeClosed[indProj], ") and status in (Closed, Done)",
                   " and team=",team,
                   " and resolutiondate>=", vIniDate[indProj], sep="")
    j2 <- paste(h2, param, "&maxResults=2000", sep="")

    
    r2 <- getJiraContent(j2)
    
    
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
    
    ##COMENTANDO ESCRITA DE ARQUIVO
    ##fclosed <- paste("./data/closed_", proj, ".csv", sep="")
    ##write.csv(closed[order(closed$Issue), ], file=fclosed)
    ##Descomentando escrita de arquivo
    
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
    
    ##COMENTANDO ESCRITA DE ARQUIVO
    ##fLT <- paste("./data/DadosLT_", proj, ".csv", sep="")
    ##write.csv(LT, file=fLT)
    ##DESCOMENTANDO ESCRITA DE ARQUIVO
    
    # Update date format
    LT$Data.de.Resolução <- vDate[ordem]
    
    # Update Lead Time list
    #            cat("  nrow de LT final= ", nrow(LT), "\n")
    listProjLT[[indProj]] <<- LT
    
    setProgress(1)
    return(LT)
  })
}


getBurndownPI<-function(issueType, jiraAddress){

  
  h2 <- paste(jiraAddress,"/rest/api/2/search?jql=",sep="")
  
  if(issueType=="Epic"){
    param <- "project = ACT and labels = 18Q1 and issuetype = Epic"
    print("Epic")
  } else {
    param <- "issueFunction in issuesInEpics('project=ACT and labels=18Q1') and issuetype in (Story, Task, Bug)" 
    print("Story")
  }
  print(param)
  
  #param <- "project = Billing and issuetype = Epic and fixVersion in (FY18Q1)"
  
  j2 <- paste(h2, param, "&maxResults=2000", sep="")
  
  aut <- jiraAuthentication(jiraAddress,"amandavarella", "NBN-lab245")
  
  
  r2 <- getJiraContent(j2)
  con <- content(r2, "text")
  
  dat <- fromJSON(con)
  lIssues <- dat$issues
  nIssues <- length(lIssues$key)
  
  
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
  date <- paste(y, "-", m, "-", d, " 00:00:00",sep="")
  dfClosed$Data.de.Resolução <- date
  
  fn <- "pi.dat"
  if(file.exists(fn)){file.remove(fn)}
  file.create(fn)
  
  line="Start,2017-07-13 00:00:00"
  write(line,file="pi.dat",append=TRUE)
  line="Deadline,2017-10-09 00:00:00"
  write(line,file="pi.dat",append=TRUE)
  line="Key,Description,Effort"
  write(line,file="pi.dat",append=TRUE)
  line=paste("1,",issueType,",",nIssues)
  write(line,file="pi.dat",append=TRUE)
  line="Key,Done,Time"
  write(line,file="pi.dat",append=TRUE)
  
  dfClosed <- dfClosed[dfClosed$Situação=="Done",]
  
  if(nrow(dfClosed)==0)
  {
    line="1,0,2017-07-14 00:00:00"
    write(line,file="pi.dat",append=TRUE)
  }else{
    for(i in 1:nrow(dfClosed)) 
    {
      row <- paste("1,",i,",",dfClosed$Data.de.Resolução)
      write(row,file="pi.dat",append=TRUE)
    }
  }
  
  b <- read.burndown("pi.dat")
  
  
  return(b)
}


