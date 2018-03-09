library(sqldf)

# Function distribute weights --------------------

distributeweights<-function(datain,varLevel,varID,varParentID,varWeight,varActive,levelPrefix="",WeightName="Weight"){
  
  dataout<-datain
  
  if(missing(varWeight)){
    #print("No weighting")
    bAddvarWeight = TRUE
    varWeight<-"WeightX"
    datain[,varWeight]<-1
  }else{
    bAddvarWeight = FALSE
  }
  if(missing(varActive)){
    #print("No Active")
    bAddvarActive = TRUE
    varActive<-"Active"
    datain[,varActive]<-1
  }else{
    bAddvarActive = FALSE
  }
  
  n0<-min(datain[,varLevel])
  n1<-max(datain[,varLevel])
  
  datain[,paste0('P',n0)]<-datain[,varParentID]
  for (i in (n0+1):(n1-1)){
    datain[,paste0('P',i)] <- sqldf(paste0("SELECT t2.",varParentID," as P2 
                                           FROM datain t1 LEFT JOIN datain t2 ON t1.P",i-1,"=t2.", varID))
  }
  
  datain[,'a']<-ifelse(is.na(datain[,varWeight]),ifelse(datain[,varLevel]==n0,1,datain[,varWeight]),datain[,varWeight])
  
  for (i in (n0+1):(n1-1)){
    datain[,paste0('a',i)]<-sqldf(paste0("SELECT t2.",varWeight," as a2 FROM datain t1 LEFT JOIN datain t2 ON t1.P",i-1,"=t2.", varID))
    datain$a<-ifelse(is.na(datain$a), datain[,paste0('a',i)] ,datain$a)
    datain[,paste0('a',i)]<-NULL
  }
  
  for (i in n1:n0){
    datain[,paste0('L',i)]<-ifelse(datain[,varLevel]==i,datain[,varID],NA)
  }
  
  for (i in (n1-1):(n0)){
    for (j in n0:(n1-i)){
      #print(paste0("i=",i," j=",j))
      datain[,paste0('L',i)]<-ifelse(datain[,varLevel]==1+i+j-n0,datain[,paste0('P',j)],datain[,paste0('L',i)])
    }
  } 
  
  #count of indicators using sqldf
  count <- sqldf(paste0("SELECT t1.",varID,", SUM(t2.",varActive,") AS Count 
                        FROM datain t1 LEFT JOIN datain t2 ON t1.",varID," = t2.L", n0 ,
                        " GROUP BY t1.",varID," ORDER BY t1.",varID))
  
  for (i in (n0+1):n1){
    count1 <- sqldf(paste0("SELECT t1.",varID,", SUM(t2.",varActive,") AS Count 
                           FROM datain t1 LEFT JOIN datain t2 ON t1.",varID," = t2.L", i ,
                           " GROUP BY t1.",varID," ORDER BY t1.",varID))
    
    count <- sqldf("SELECT * FROM count UNION ALL SELECT * FROM count1")
  }
  
  count<-sqldf(paste0("SELECT t1.",varID,", SUM(t1.count) AS Count FROM count t1 GROUP BY t1.",varID," ORDER BY t1.",varID))
  
  datain$count_ind_child<-count$Count
  
  datain$m<-ifelse(datain$count_ind>0,1,0)
  datain$n<-ifelse(datain$count_ind_child>0,1,0)
  datain$am<-datain$a*datain$m
  datain$an<-datain$a*datain$n
  datain[,'amp']<-sqldf(paste0("SELECT t2.am as P2 FROM datain t1 LEFT JOIN datain t2 ON t1.",varParentID,"=t2.",varID))
  
  n<-nrow(datain)
  
  nbasin0<-nrow(datain[datain[,varLevel]==n0,])
  
  datain[,'denom']<-sqldf(paste0("SELECT SUM(t2.an) FROM datain t1 LEFT JOIN datain t2 ON t1.",varID,"=t2.",varParentID," GROUP BY t1.",varID))
  
  datain$denom<-ifelse(is.na(datain$denom),0,datain$denom)+ifelse(is.na(datain$am),0,datain$am)
  datain$denom<-ifelse(datain$denom==0,NA,datain$denom)
  
  datain[,'denomp']<-sqldf(paste0("SELECT t2.denom as P2 FROM datain t1 LEFT JOIN datain t2 ON t1.",varParentID,"=t2.",varID))
  
  i=n0
  
  datain[,paste0('v',i)]<-ifelse(datain[,varLevel]==i,(1.0/nbasin0),NA) #0
  datain[,paste0('w',i)]<-ifelse(datain[,varLevel]==i,datain[,paste0('v',i)]*datain$am/(datain$denom),NA) #0
  
  n<-nrow(datain)
  
  datain$ww<-ifelse(is.na(datain[,paste0('w',i)]),0,datain[,paste0('w',i)])
  
  for(i in (n0+1):n1){
    datain[,paste0('vp',i)]<-NA
    for (j in 1:n) {
      pid<-datain[j,varParentID]
      if(is.na(pid)){
        #   
      }else{
        newv<-datain[datain[,varID]==pid,paste0('v',i-1)]
        datain[j,paste0('vp',i)] <- newv
      }
    }
    datain[,paste0('v',i)]<-datain[,paste0('vp',i)]*datain$an/(datain$denomp)
    datain[,paste0('w',i)]<-ifelse(datain[,varLevel]==i,datain[,paste0('v',i)]*datain$am/(datain$denom),NA)
    datain$ww<-datain$ww+ifelse(is.na(datain[,paste0('w',i)]),0,datain[,paste0('w',i)])
  }
  
  if (bAddvarWeight){
    dataout[,'Value']<-datain[,'WeightX']
  }
  if (bAddvarActive){
    dataout[,'Active']<-datain[,'Active']
  }
  for(i in n0:n1){
    dataout[,paste0(levelPrefix,'L',i)]<-datain[,paste0('L',i)]
  } 
  dataout[,WeightName]<-datain[,'ww']
  
  return(dataout)
  
  }

# Functions to Calculate EQR --------------------------------------------------------------------
EQR<-function(indData,Type="IndType",Obs,Bad,BadPoor="BP",PoorMod="PM",ModGood,GoodHigh="GH",High,GoodHigh2,ModGood2,PoorMod2,BadPoor2,Bad2){
  if (missing(Obs))
    stop("Need to specify variable 'Obs' containing the observed indicator value")
  if (missing(Bad))
    stop("Need to specify variable 'Bad' containing the indicator value corresponding to EQR=0.0 \n  (The worst possible value the indicator can take)")
  if (missing(ModGood))
    stop("Need to specify variable 'ModGood' containing the indicator value corresponding to EQR=0.6 \n   (The GES or 'Moderate/Good' boundary)  ")
  if (missing(High))
    stop("Need to specify variable 'High' containing the indicator value corresponding to EQR=1.0 \n  (The best possible value the indicator can take).")
  
  if ((High %in% names(indData))==FALSE) stop(paste0("Variable not found: High='",High,"'"))
  if ((Bad %in% names(indData))==FALSE) stop(paste0("Variable not found: Bad='",Bad,"'"))
  if ((ModGood %in% names(indData))==FALSE) stop(paste0("Variable not found: ModGood='",ModGood,"'"))
  
  temp<-indData
  if ((Type %in% names(indData))==FALSE) print("Type missing")
  if ((Type %in% names(indData))==FALSE) temp[,Type]<-NA
  temp[,Type]<-ifelse(is.na(temp[,Type]),1,temp[,Type])
  
  if ((BadPoor %in% names(indData))==FALSE)  temp[,BadPoor]<-NA
  if ((PoorMod %in% names(indData))==FALSE)  temp[,PoorMod]<-NA
  if ((GoodHigh %in% names(indData))==FALSE)  temp[,GoodHigh]<-NA
  
  
  if (nrow(indData[indData[,Type]==2,]) > 0) {
    if (missing(ModGood2))
      stop("Need to specify variable 'ModGood2' containing the second indicator value corresponding to EQR=0.6 \n   (The second GES or 'Moderate/Good' boundary for optimum range indicators)  ")
    if (missing(Bad2))
      stop("Need to specify variable 'Bad2' containing the second indicator value corresponding to EQR=0.0 \n   (The second 'extreme' value of the indicator)  ")
    if ((Bad2 %in% names(indData))==FALSE) stop(paste0("Variable not found: Bad2='",Bad2,"'"))
    if ((ModGood2 %in% names(indData))==FALSE) stop(paste0("Variable not found: ModGood2='",ModGood2,"'"))
    if (missing(PoorMod2)){
      PoorMod2<-"PM2"
      temp[,PoorMod2]<-NA
    }
    if (missing(BadPoor2)){
      BadPoor2<-"BP2"
      temp[,BadPoor2]<-NA
    }
    if (missing(GoodHigh2)){
      GoodHigh2<-"GH2"
      temp[,GoodHigh2]<-NA
    }
  }
  
  # Type 1 - most common indicator type - increasing monotonically
  # Type 2 - optimum range
  temp$Err<-""
  temp$OK<-0
  temp$Sign<-0
  temp$EQR<-NA
  
  for (i in 1:nrow(temp)){
    
    #if(is.na(temp[i,Bad])||is.na(temp[i,ModGood])||is.na(temp[i,High])
    #   ||(is.na(temp[i,ModGood2])&&temp[i,Type]==2)||(is.na(temp[i,Bad2])&&temp[i,Type]==2)
    if(is.na(temp[i,Bad])||is.na(temp[i,ModGood])||is.na(temp[i,High])
    ){
      #Missing a key value
      temp[i,'Err']<-"Missing  boundary value"
    }
    else{
      if(is.na(temp[i,BadPoor])&&is.na(temp[i,PoorMod])){
        temp[i,BadPoor]=(2*temp[i,Bad]+temp[i,ModGood])/3
        temp[i,PoorMod]=(temp[i,Bad]+2*temp[i,ModGood])/3
      }
      if(is.na(temp[i,BadPoor])){
        temp[i,BadPoor]=(temp[i,Bad]+temp[i,PoorMod])/2
      }      
      if(is.na(temp[i,PoorMod])){
        temp[i,PoorMod]=(temp[i,BadPoor]+temp[i,ModGood])/2
      }      
      if(is.na(temp[i,GoodHigh])){
        temp[i,GoodHigh]=(temp[i,ModGood]+temp[i,High])/2
      }      
      
      if(temp[i,Type]==2&&is.na(temp[i,BadPoor2])&&is.na(temp[i,PoorMod2])){
        temp[i,BadPoor2]=(2*temp[i,Bad2]+temp[i,ModGood2])/3
        temp[i,PoorMod2]=(temp[i,Bad2]+2*temp[i,ModGood2])/3
      }
      if(temp[i,Type]==2&&is.na(temp[i,BadPoor2])){
        temp[i,BadPoor2]=(temp[i,Bad2]+temp[i,PoorMod2])/2
      }
      if(temp[i,Type]==2&&is.na(temp[i,PoorMod2])){
        temp[i,PoorMod2]=(temp[i,BadPoor2]+temp[i,ModGood2])/2
      }
      if(temp[i,Type]==2&&is.na(temp[i,GoodHigh2])){
        temp[i,GoodHigh2]=(temp[i,High]+temp[i,ModGood2])/2
      }
      
      
      if(temp[i,Type]==1) {
        #Check if values are increasing monotonically
        if (temp[i,Bad]<temp[i,BadPoor]&&temp[i,BadPoor]<temp[i,PoorMod]&&temp[i,PoorMod]<temp[i,ModGood]&&temp[i,ModGood]<temp[i,GoodHigh]&&temp[i,GoodHigh]<temp[i,High]){
          temp[i,'OK']<- 1
          temp[i,'Sign']<- 1
        }
        #Check if values are decreasing monotonically
        if (temp[i,Bad]>temp[i,BadPoor]&&temp[i,BadPoor]>temp[i,PoorMod]&&temp[i,PoorMod]>temp[i,ModGood]&&temp[i,ModGood]>temp[i,GoodHigh]&&temp[i,GoodHigh]>temp[i,High]){
          temp[i,'OK']<- 1
          temp[i,'Sign']<- -1
        }
        if(temp[i,'OK']==0){
          temp[i,'Err']<-"Boundary values not monotonic"
        }else{
          temp[i,"EQR"]<-EQR1(temp[i,'Sign'],temp[i,Obs],temp[i,Bad],temp[i,BadPoor],
                              temp[i,PoorMod],temp[i,ModGood],temp[i,GoodHigh],
                              temp[i,High])
          
        }
        
      }
      if(temp[i,Type]==2) {
        if (temp[i,Bad]<temp[i,BadPoor]&&temp[i,BadPoor]<temp[i,PoorMod]
            &&temp[i,PoorMod]<temp[i,ModGood]&&temp[i,ModGood]<temp[i,GoodHigh]
            &&temp[i,GoodHigh]<temp[i,High]&&temp[i,High]<temp[i,GoodHigh2]
            &&temp[i,GoodHigh2]<temp[i,ModGood2]&&temp[i,ModGood2]<temp[i,PoorMod2]
            &&temp[i,PoorMod2]<temp[i,BadPoor2]&&temp[i,BadPoor2]<temp[i,Bad2]
        ){
          temp[i,'OK']<- 1
          temp[i,'Sign']<- 1
          temp[i,"EQR"]<-EQR2(temp[i,Obs],temp[i,Bad],temp[i,BadPoor],
                              temp[i,PoorMod],temp[i,ModGood],temp[i,GoodHigh],
                              temp[i,High],temp[i,GoodHigh2],temp[i,ModGood2],
                              temp[i,PoorMod2],temp[i,BadPoor2],temp[i,Bad2])
        }
        
        if(temp[i,'OK']==0)  temp[i,'Err']<-"Boundary values not monotonic"
        
      }
    }
  }
  
  
  return(temp)
}

EQR1<-function(sgn,obs,b0,b02,b04,b06,b08,b1){
  if(sgn>0){
    if(obs<b0) {n=0}
    else if (obs<b02){n=0.2*((obs-b0)/(b02-b0))}
    else if (obs<b04){n=0.2+0.2*((obs-b02)/(b04-b02))}
    else if (obs<b06){n=0.4+0.2*((obs-b04)/(b06-b04))}
    else if (obs<b08){n=0.6+0.2*((obs-b06)/(b08-b06))}
    else if (obs<b1){n=0.8+0.2*((obs-b08)/(b1-b08))}
    else{n=1}
  }else{
    if(obs>b0) {n=0}
    else if (obs>b02){n=0.2*((obs-b0)/(b02-b0))}
    else if (obs>b04){n=0.2+0.2*((obs-b02)/(b04-b02))}
    else if (obs>b06){n=0.4+0.2*((obs-b04)/(b06-b04))}
    else if (obs>b08){n=0.6+0.2*((obs-b06)/(b08-b06))}
    else if (obs>b1){n=0.8+0.2*((obs-b08)/(b1-b08))}
    else{n=1}
  }
  return(n)
}

EQR2<-function(obs,b0,b02,b04,b06,b08,b1,b208,b206,b204,b202,b200){
  if(obs<b0) {n=0}
  else if (obs<b02){n=0.2*((obs-b0)/(b02-b0))}
  else if (obs<b04){n=0.2+0.2*((obs-b02)/(b04-b02))}
  else if (obs<b06){n=0.4+0.2*((obs-b04)/(b06-b04))}
  else if (obs<b08){n=0.6+0.2*((obs-b06)/(b08-b06))}
  else if (obs<b1){n=0.8+0.2*((obs-b08)/(b1-b08))}
  else if (obs<b208){n=1-0.2*((obs-b1)/(b208-b1))}
  else if (obs<b206){n=0.8-0.2*((obs-b208)/(b206-b208))}
  else if (obs<b204){n=0.6-0.2*((obs-b206)/(b204-b206))}
  else if (obs<b202){n=0.4-0.2*((obs-b204)/(b202-b204))}
  else if (obs<b200){n=0.2-0.2*((obs-b202)/(b200-b202))}
  else{n=0}  
  return(n)
}

# Function to calculate numeric confidence from string --------------------------------------------------------------------
# The function will always return a numeric value between 0 and 1, depending on the argument sConf
# Given a numeric argument between 0 and 1, the function returns the same value
# e.g. ConfValue(0.37) returns a value of 0.37
# Passing a numeric argument less than 0, the function returns a value of 0
# Passing a numeric argument greater than 1, the function returns a value of 1
# The function recognizes the following words and returns the respective values:
#    High = 1.0
#    Intermediate = 0.5
#    Medium = 0.5
#    Moderate = 0.5
#    Low = 0.0
# The function is case-insensitive.
# Starting from the leftmost characte, the function recognizes any part of the key words
# e.g. "H", "hi", "hig" will all result in a value of 1.0
#      "med", "m", "int", "I", "in" will all return values of 0.5
#      "lo", "l" will all give a value of 0.0
#
# Any other argument passed to the function will give a result equal to the argument NAvalue (default=0)

ConfValue<-function(sConf,NAvalue=NA){
  if(is.numeric(sConf)){
    return(sConf)
  }else{
    sConf<-tolower(sConf)
    l<-nchar(sConf)
    if(l<1){
      return(NAvalue)
    }else{
      desc<-c("low          ","intermediate","medium          ","moderate          ","high          ")
      value<-c(0,0.5,0.5,0.5,1)
      df<-data.frame(desc,value)
      df$desc<-substr(df$desc, 1, l)
      if(sConf %in% df$desc){
        n<-df$value[df$desc==sConf][1]
        return(n)
      }else{
        n<-suppressWarnings((as.numeric(sConf)))
        if(is.na(n)){
          n = NAvalue
        }else{
          n=min(1,max(0,n))
        }
        return(n)
      }
    }
  }
}


ConfidenceArray<-function(arrayConf){
  arrayConf<-sapply(arrayConf, ConfValue)
  return(arrayConf)
}


Confidence<-function(datain,varObs,varGES,varStdErr,varConfTemp,varConfSpat,varConfAcc,varConfMeth){
  # if any of the Temporal, Spatial or Methodological confidence components are missing then they are assigned a value of 0
  if(missing(varConfTemp)){
    varConfTemp<-"ConfTemp"
    datain[,varConfTemp]<-0
    bDropConfTemp<-TRUE
  }else{
    bDropConfTemp<-FALSE
  }
  if(missing(varConfSpat)){
    varConfSpat<-"ConfSpat"
    datain[,varConfSpat]<-0
    bDropConfSpat<-TRUE
  }else{
    bDropConfSpat<-FALSE
  }
  if(missing(varConfMeth)){
    varConfSpat<-"ConfMeth"
    datain[,varConfMeth]<-0
    bDropConfMeth<-TRUE
  }else{
    bDropConfMeth<-FALSE
  }
  
  if(missing(varConfAcc)){
    varConfAcc<-"ConfAcc"
    datain[,varConfAcc]<-NA
    bDropConfAcc<-TRUE
  }else{
    bDropConfAcc<-FALSE
  }
  
  if(missing(varGES)){
    varGES<-"GES"
    datain[,varGES]<-NA
    bDropGES<-TRUE
  }else{
    bDropGES<-FALSE
  }
  if(missing(varObs)){
    varObs<-"Obs"
    datain[,varObs]<-NA
    bDropObs<-TRUE
  }else{
    bDropObs<-FALSE
  }
  if(missing(varStdErr)){
    varStdErr<-"StdErr"
    datain[,varStdErr]<-NA
    bDropStdErr<-TRUE
  }else{
    bDropStdErr<-FALSE
  }
  
  datain$cTemp<-ConfidenceArray(datain[,varConfTemp])
  datain$cSpat<-ConfidenceArray(datain[,varConfSpat])
  datain$cMeth<-ConfidenceArray(datain[,varConfMeth])
  
  datain$cAcc1<-ConfidenceArray(datain[,varConfAcc])
  datain$pGES<-pnorm(q=datain[,varObs],mean=datain[,varGES],sd=datain[,varStdErr], lower.tail = TRUE)
  
  datain$cAcc<-ifelse(is.na(datain$cAcc1),datain$pGES,datain$cAcc1)
  
  datain$Conf<-0.25*((ifelse(is.na(datain$cTemp),0,datain$cTemp))+(ifelse(is.na(datain$cSpat),0,datain$cSpat))+
                       (ifelse(is.na(datain$cAcc),0,datain$cAcc))+(ifelse(is.na(datain$cMeth),0,datain$cMeth)))
  
  #datain$cTemp <- NULL
  #datain$cSpat <- NULL
  #datain$cMeth <- NULL
  #datain$cAcc1 <- NULL
  #datain$pGES <- NULL
  #datain$cAcc <- NULL
  
  drops<-c("cTemp","cSpat","cMeth","cAcc1","cAcc")
  datain<-datain[,!(names(datain) %in% drops)]
  
  if(bDropConfSpat){datain[,varConfSpat]<-NULL}
  if(bDropConfTemp){datain[,varConfTemp]<-NULL}
  if(bDropConfMeth){datain[,varConfMeth]<-NULL}
  if(bDropConfAcc){datain[,varConfAcc]<-NULL}
  if(bDropObs){datain[,varObs]<-NULL}
  if(bDropStdErr){datain[,varStdErr]<-NULL}
  if(bDropGES){datain[,varGES]<-NULL}
  if(nrow(subset(datain, !is.na(pGES)))<1){
    datain$pGES<-NULL
  }
  return(datain)
  
}


