#Receive as parameter the full query in Jira
getJiraContent<-function(j1){
  r1 <- GET(j1,
            config(ssl_verifypeer=FALSE),
            body=FALSE,
            add_headers("Accept"="application/json",
                        "Content-Type"="application/json",
                        #                        "Set-Cookie"= scookie))
                        "Set-Cookie"= scookie), verbose())
  if (status_code(r1) != 200 ) {
    return(NULL)
  }else
    return(r1)
}


jiraAuthentication <- function (jiraAddress, user, pass){
  if (user != "" & pass != "") {
    j1 <- paste(jiraAddress,"/rest/auth/1/session", sep="")
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
    
    return(aut)
  }
}