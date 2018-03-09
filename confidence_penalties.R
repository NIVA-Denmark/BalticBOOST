cat("Calculating Confidence penalties\n")
library(dplyr)

#---------------- Copy indicator data and add information about level 2 SAU and level 2 EC (Mammal,Bird,etc.)
# We apply the confidence penalties on SAUs at level 2 (Baltic Basins)
ind2<-ind
ind2$ConfMultiply<-1

ind2<-left_join(ind2,select(ECall,ECID,EC_L2,EC_L4),by=c("ECID"="ECID"))
ind2<-left_join(ind2,select(sau,SAUID,SAU_L2),by=c("SAUID"="SAUID"))
ind2$SAU_L2<-ifelse(is.na(ind2$SAU_L2),-99,ind2$SAU_L2) # Level 1 SAU (Baltic Sea) does not have a lvel 2 SAU specified

# Get list of Level 2 SAUs
SAU_L2<-sau %>%
  distinct(SAU_L2)
SAU_L2$SAU_L2<-ifelse(is.na(SAU_L2$SAU_L2),-99,SAU_L2$SAU_L2)
  #filter(!is.na(SAU_L2)) %>%

# ---------------- cross join help function -------------------------------------- 
cross_join<-function(dfa,dfb){
  dfa$k<-1
  dfb$k<-1
  dfa<-inner_join(dfa,dfb, by='k')
  dfa$k<-NULL
  return(dfa)
}

ApplyPenalties=c(T,T,T,T,T,T,T)
#ApplyPenalties=c(F,F,F,T,F,F,F)

CriteriaList<-c("D1C1","D1C2","D1C3","D1C4","D1C5","D1C6")


PenaltiesECCriteria<-function(df,CriteriaSelect,SelectedEC){
  #CriteriaSelect<-c("D1C1","D1C2","D1C3","D1C4","D1C5","D1C6")
  #SelectedEC<-c(3) # Mammals
  
  dfCriteriaSelect<-data.frame(CriteriaSelect,stringsAsFactors=FALSE)
  nrequired<-length(CriteriaSelect)
  
  for(i in 1:length(CriteriaSelect)){
    temp<-indcat[,names(indcat) %in% c("IndID","EcosystemComponentID",CriteriaSelect[i])]
    names(temp)[3]<-"Criteria"
    temp<-temp %>%
      filter(Criteria==1) %>%
      select(IndID) %>% 
      mutate(Criteria=CriteriaSelect[i])
    temp<-inner_join(ind2,temp,by=c("IndicatorID"="IndID"))
    
    if(i==1){
      indcat2<-temp
    }else{
      indcat2 <- rbind(indcat2,temp)
    }
  }
  
  #Count by descriptor and SAU - counts for SAU at level 1 count for SAU level 2 and vice versa  
  count <- indcat2 %>%
    filter(EC_L2 %in% SelectedEC) %>% #Filter for selected ECID
    group_by(SAU_L2,Criteria) %>%
    summarise(count=n()) %>%
    ungroup()
  
  count_L1<-count %>%
    filter(SAU_L2!=-99) %>%
    mutate(SAU_L2=-99)
  
  count_L2<-count %>%
    filter(SAU_L2==-99)
  if(nrow(count_L2)>0){
    count_L2<-cross_join(filter(SAU_L2,SAU_L2!=-99),select(count_L2,Criteria,count))
    count<-rbind(count,count_L1,count_L2)
  }else{
    count<-rbind(count,count_L1)
  }
  
  count <- count %>%
    group_by(SAU_L2,Criteria) %>%
    summarise(count=sum(count)) %>%
    ungroup() %>%
    mutate(CritOK=1)
  
  # Get all combinations of Level 2 SAUs and Criteria
  Crit_SAU_L2<-cross_join(SAU_L2,dfCriteriaSelect)
  
  ind_ok<-left_join(Crit_SAU_L2,count,by=c("SAU_L2"="SAU_L2","CriteriaSelect"="Criteria"))
  
  sau_multiplier<-ind_ok %>%
    group_by(SAU_L2) %>%
    summarise(count=sum(CritOK,na.rm=TRUE)) %>%
    ungroup()
  
  sau_multiplier$multiplier<-ifelse(sau_multiplier$count<nrequired,0.75,1)
  sau_multiplier$count<-NULL
  
  for(i in 1:length(SelectedEC)){
    temp<-sau_multiplier
    temp$EC_L2<-SelectedEC[i]
    if(i==1){
      new<-temp
    }else{
      new<-rbind(new,temp)
    }
  }
  sau_multiplier<-new
  sau_multiplier$count<-NULL
  
  df<-left_join(df,sau_multiplier,by=c("SAU_L2"="SAU_L2","EC_L2"="EC_L2"))
  df$ConfMultiply<-ifelse(is.na(df$multiplier),df$ConfMultiply,df$ConfMultiply*df$multiplier)
  df$multiplier<-NULL  
  
  return(df)
}

#---------------- (1) Mammals - all HD annex II species covered ------------------
# Harbour porpoise (ECID=60), Grey Seal (61), Harbour seal (62), Ringed seal (63)
# Otter?
if(ApplyPenalties[1]==TRUE){
  cat("Applying confidence penalties for coverage of HD annex II mammal species\n")
  
  ECID<-c(60,61,62,63)
  nrequired<-length(ECID)
  ECID<-data.frame(ECID)
  
  # Get all combinations of Level 2 SAUs and annex II ECs
  EC_SAU_L2<-cross_join(SAU_L2,ECID)
  
  #Get counts of indicators grouped by SAU level 2 and EC level 4
  count <- ind2 %>%
    group_by(SAU_L2,EC_L4) %>%
    summarise(count=n()) %>%
    ungroup()
  
  #indicators at SAU level 1 (SAU_L2=-99) count for all level 2 SAUs(and vice versa)
  count_L1<-count %>%
    filter(SAU_L2!=-99) %>%
    mutate(SAU_L2=-99)
  
  count_L2<-count %>%
    filter(SAU_L2==-99)  
  
  if(nrow(count_L2)>0){
    count_L2<-cross_join(filter(SAU_L2,SAU_L2!=-99),select(count_L2,EC_L4,count))
    count<-rbind(count,count_L1,count_L2)
  }else{
    count<-rbind(count,count_L1)
  }
  
  count <- count %>%
    group_by(SAU_L2,EC_L4) %>%
    summarise(count=sum(count)) %>%
    ungroup() %>%
    mutate(OK=1)
  
  ind_ok<-left_join(EC_SAU_L2,count,by=c("SAU_L2"="SAU_L2","ECID"="EC_L4"))
  
  sau_multiplier<-ind_ok %>%
    group_by(SAU_L2) %>%
    summarise(count=sum(OK,na.rm=TRUE)) %>%
    ungroup()
  
  sau_multiplier$multiplier<-ifelse(sau_multiplier$count<nrequired,0.75,1)
  sau_multiplier$EC_L2<-3
  sau_multiplier$count<-NULL
  
  ind2<-left_join(ind2,sau_multiplier,by=c("SAU_L2"="SAU_L2","EC_L2"="EC_L2"))
  ind2$ConfMultiply<-ifelse(is.na(ind2$multiplier),ind2$ConfMultiply,ind2$ConfMultiply*ind2$multiplier)
  ind2$multiplier<-NULL
  
  #rm(list=c("nrequired","ECID","sau_multiplier","count","ind_ok","EC_SAU_L2"))
}
#---------------- (2) Mammals - all primary criteria covered ----------------
if(ApplyPenalties[2]==TRUE){
  cat("Applying confidence penalties for coverage of all primary criteria for mammals\n")
  ind2<-PenaltiesECCriteria(ind2,CriteriaList,c(3))
}

#---------------- (3) Fish/birds - all primary criteria covered ----------------
if(ApplyPenalties[3]==TRUE){
  cat("Applying confidence penalties for coverage of all primary criteria for Fish/Birds\n")
  ind2<-PenaltiesECCriteria(ind2,CriteriaList,c(2,4))
}

#---------------- (4) Broad habitat types - all primary criteria covered ----------------
if(ApplyPenalties[4]==TRUE){
  cat("Applying confidence penalties for coverage of all primary criteria for Broad habitat types\n")
  ind2<-PenaltiesECCriteria(ind2,CriteriaList,c(5,6))
}

#---------------- (5) Ecosystem components - all primary criteria covered ----------------
if(ApplyPenalties[5]==TRUE){
  cat("Applying confidence penalties for coverage of all primary criteria for Ecosystem components\n")
  
}

#---------------- (6) Ecosystem components - all species broad habitat types covered ----------------
if(ApplyPenalties[6]==TRUE){
  cat("Applying confidence penalties for coverage of all species groups/broad habitat types for Ecosystem components\n")
  
  ECID<-c(2,3,4,5,6)
  nrequired<-length(ECID)
  ECID<-data.frame(ECID)
  
  # Get all combinations of Level 2 SAUs and annex II ECs
  EC_SAU_L2<-cross_join(SAU_L2,ECID)
  
  #Get counts of indicators grouped by SAU level 2 and EC level 4
  count <- ind2 %>%
    group_by(SAU_L2,EC_L2) %>%
    summarise(count=n()) %>%
    ungroup()
  
  #indicators at SAU level 1 (SAU_L2=-99) count for all level 2 SAUs(and vice versa)
  count_L1<-count %>%
    filter(SAU_L2!=-99) %>%
    mutate(SAU_L2=-99)
  
  count_L2<-count %>%
    filter(SAU_L2==-99)  
  
  if(nrow(count_L2)>0){
    count_L2<-cross_join(filter(SAU_L2,SAU_L2!=-99),select(count_L2,EC_L2,count))
    count<-rbind(count,count_L1,count_L2)
  }else{
    count<-rbind(count,count_L1)
  }
  
  count <- count %>%
    group_by(SAU_L2,EC_L2) %>%
    summarise(count=sum(count)) %>%
    ungroup() %>%
    mutate(OK=1)
  
  ind_ok<-left_join(EC_SAU_L2,count,by=c("SAU_L2"="SAU_L2","ECID"="EC_L2"))
  
  sau_multiplier<-ind_ok %>%
    group_by(SAU_L2) %>%
    summarise(count=sum(OK,na.rm=TRUE)) %>%
    ungroup()
  
  sau_multiplier$multiplier<-ifelse(sau_multiplier$count<nrequired,0.75,1)
  sau_multiplier$count<-NULL
  
  ind2<-left_join(ind2,sau_multiplier,by=c("SAU_L2"="SAU_L2"))
  ind2$ConfMultiply<-ifelse(is.na(ind2$multiplier),ind2$ConfMultiply,ind2$ConfMultiply*ind2$multiplier)
  ind2$multiplier<-NULL
  
  rm(list=c("nrequired","ECID","sau_multiplier","count","ind_ok","EC_SAU_L2"))
}

#---------------- (7) Criteria for Ecosystem components - all species broad habitat types covered ----------------
if(ApplyPenalties[6]==TRUE){
  cat("Applying confidence penalties for coverage of all species groups/broad habitat types for criteria for Ecosystem components\n")
  
}

# ---------------- Tidy up --------------------------------------------------------------------

ind2$Conf<-ind2$Conf*ind2$ConfMultiply
ind2$EC_L2<-NULL
ind2$EC_L4<-NULL
ind2$SAU_L2<-NULL
ind2$ConfMultiply<-NULL

ind<-ind2

