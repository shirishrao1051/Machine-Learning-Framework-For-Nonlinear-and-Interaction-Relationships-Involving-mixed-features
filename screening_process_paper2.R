library(DoE.base)
library(randtoolbox)
library(MaxPro)
library(SLHD)
library(caret)
library(gbm)
library(MASS)
library(MOLHD)
library(earth)
library(MLmetrics)
library(rio)
library(plyr)
library(glinternet)
library(dummies)
library(gglasso)
library(stringr)
library(paramtest)
library(fastDummies)











f<-nl1_modified_ver2(60,2,0.5,3)
func<-as.character(f[1])

type<-"MartinezSLHD"
dim<-12
size<-2
split<-2
imp<-0.5
nl<-3
resptype<-2



f<-nl1_modified_ver2(dim,split,imp,resptype)
func<-as.character(f[1])
f


#### Alternate screening process - with ALL terms added tp terms from MARS and then fed to Group LASSO


screening_with_all_terms_s2<- function(iter,type,dim,size,split,imp,nl,resptype){
  
  #### Pre steps 
  
  
  ## Generate experimental design 
  
  if(type=="Kung"){
    
    design_matrix<- design_kung(dim,size,split)[[1]]
  }   else if (type=="KungMP"){
    
    
    design_matrix<- design_kungMP(dim,size,split)[[1]]
    
  } else if (type=="KungSLHD"){
    
    design_matrix<- design_kungSLHD(dim,size,split)[[1]]
    
  } else if (type=="Martinez"){
    
    
    design_matrix<- design_martinez(dim,size,split,nfour=3)[[1]]
    
    
  } else if (type=="MartinezMP"){
    
    design_matrix<- design_martinezMP(dim,size,split,nfour=3)[[1]]
    
  } else if (type=="MartinezSLHD"){
    
    design_matrix<- design_martinezSLHD(dim,size,split,nfour=3)[[1]]
    
  } else if(type=="MaxproQQ") {
    
    design_matrix<- design_mpqq(dim,size,split,nfour=3)[[1]]
    
  } else if (type=="CoC"){
    
    design_matrix<-design_coc(dim,size,split)
    
  }  else if (type=="MC_unif"){
    
    design_matrix<-design_mc_unif(dim,size,split)[[1]]
    
  } else if (type=="MC_beta"){
    
    design_matrix<-design_mc_beta(dim,size,split)[[1]]
    
  } else (print("Wrong design type entered"))
  
  
  colnames(design_matrix)<-paste0('x',1:ncol(design_matrix))
  
  
  
  ##### Splitting cont and categ features of data matrix
  num_col<-c()   # Numeric columns
  
  for (i in 1:ncol(design_matrix)){
    
    num_col[i]<- is.numeric(design_matrix[,i])
    
  }
  cont_col<- design_matrix[,num_col]
  
  fact_col<-c()  # Factor columns
  
  for (i in 1:ncol(design_matrix)){
    
    fact_col[i]<- is.factor(design_matrix[,i])
    
  }
  
  categ_col<- design_matrix[,fact_col]
  
  
  #Changing to 0,1 level (in accordance to glinternet)
  
  for (i in 1:ncol(categ_col)){
    
    if(nlevels(categ_col[,i])==2){
      
      levels(categ_col[,i])<-c(0,1)
    } else{levels(categ_col[,i])<-c(0,1,2,3)}
  }
  
  dm2<- cbind(cont_col,categ_col)    #Design matrix changed to 0,1 factor level
  
  ###### Response simulation
  #f<-resp_gen_p2(dim,split,imp,nl,resptype)
  #f<-nl1_modified_ver2(12,2,0.5,4)
  
  #func<-as.character(f[1])
  
  # f<-nl1_modified_ver2(60,2,0.5,3)
  #f<-nl1_modified_ver2(dim,split,imp,resptype)
  
  # if(nl==1){
  # 
  # f<-nl1_modified_ver2(dim,split,imp,resptype)
  # 
  # } else if(nl==2){  
  # 
  # f<-nl2_modified_ver2(dim,split,imp,resptype)
  # 
  # } else {f<-nl3_modified_ver2(dim,split,imp,resptype)}
  
  
  
  if(nl==1){

  f<-nl1_dim12(dim,split,imp,resptype)

  } else if(nl==2){

  f<-nl2_dim12(dim,split,imp,resptype)

  } else {f<-nl3_dim12(dim,split,imp,resptype)}
  

  
  func<-as.character(f[1])
  
  resp_value<- with(dm2,eval(parse(text=func))) #Calculating response values from the func
  tm<-f[[2]]
  
  
  
  
  #Generating noise
  # noise<- rnorm(nrow(design_matrix),0,0.1)
  # snr<-5
  # sigma <- sqrt(var(resp_value)/snr)
  # 
  # 
  # y<-resp_value+rnorm(nrow(design_matrix),0,sigma)
  # 
  
  y<-resp_value
  
  ######## Step 1 - Fitting MARS on cont features  #################################
  ########                                         ###############################
  
  
  mars<- earth::earth(cont_col,y,degree=2,penalty=-1,pmethod="none",nk=1000)
  
  #all basis fns from mars
  at<- mars$bx  
  at<- at[,-1]#basis functions values
  drs<- mars$dirs[mars$selected.terms,]
  drs<-drs[-1,]#basis fns features selected
  
  cn<-list()
  
  for (i in 1:nrow(drs)){
    
    cn[[i]]<- which(drs[i,]!=0)
    
  }
  
  bs_names<-list()  #list of basis function selected and the features involved with it
  rn<-rownames(drs)
  for (i in 1:length(cn)){
    bs_names[[i]]<- c(rn[i],names(cn[[i]]))
    
  }
  
  
  #list of features selected by MARS
  fs_mars<- list()
  for (i in 1:length(bs_names)){
    
    fs_mars[[i]]<- bs_names[[i]][-1]
    
  }
  
  fs_mars<-unlist(fs_mars)
  fs_mars<- unique(fs_mars)
  
  #### Grouping scheme for cont variables
  grp_cont<-list()
  for (i in 1:length(bs_names)){
    grp_cont[[i]]<- bs_names[[i]][-1]
    
    
  }
  
  #grp_number<-c()
  
  
  ln<-c()  #getting length of grp_cont
  for (i in 1:length(grp_cont)){
    
    ln[i]<- length(grp_cont[[i]])
    
  }
  
  
  
  #######   group numbers for main effects ########
  
  me<- which(ln==1)
  
  at_me<- at[,me]   #all terms , main effects
  
  #subsetting only main effects in grp_cont
  grp_main<- grp_cont[which(ln==1)]
  
  
  grp_number_main<-c()
  grp_number_main[1]<-1
  ug<- unlist(grp_main)
  
  for (i in 1:(length(ug)-1)){
    
    if(any(ug[i+1]==ug[1:i])==TRUE){
      
      tmp<- which(ug[i+1]==ug[1:i])[1]
      grp_number_main[i+1]<-grp_number_main[tmp]
      
    } else{
      grp_number_main[i+1]<-max(grp_number_main) +1
      
    }
    
  }
  
  ######  group numbers for interaction effects
  at_int<-subset(at,select=-me)
  grp_int<- grp_cont[which(ln==2)]
  
  
  if (length(grp_int)>0){
    
    un<-unique(grp_int)
    
    sq<- seq(max(grp_number_main)+1, max(grp_number_main)+length(un))
    
    l1<-do.call(rbind.data.frame,grp_int)
    l2<-do.call(rbind.data.frame,un)
    
    colnames(l1)<-c(paste0('v',1:ncol(l1)))
    colnames(l2)<-c(paste0('v',1:ncol(l2)))
    tn<- seq(max(grp_number_main)+1,max(grp_number_main)+length(un))
    l2$v3<- tn
    
    l1$id<-1:nrow(l1)
    
    ref<- merge(l1,l2)
    ref<-ref[order(ref$id),]
    
    
    #tmp<- merge(l1,ref)
    #tmp<- tmp[order(tmp$v3),]
    
    grp_number_int<- c(ref[,4]) 
  } else {grp_number_int<-NULL}
  
  gln<- c(grp_number_main,grp_number_int)   #grp number indices to group Lasso
  atn<- cbind(at_me,at_int)  #the basis fn matrix 
  
  
  
  ################### Step 2 - Merging cont and cat , and feeding it to group lasso   ##################
  ###################                                                                 #################
  
  fd<- cbind(atn,categ_col)
  
  
  
  
  # group indices for categorical features
  ncat<- (max(gln)+1):((max(gln)+1)+(ncol(categ_col)-1))
  grp_cat<-list()
  
  for (i in ncat){
    
    grp_cat[[i]]<- rep(i,nlevels(categ_col[,i -max(gln)]))
    
  }
  
  un_grp_cat<- unlist(grp_cat)
  
  
  suppressWarnings(ct3<-dummy.data.frame(categ_col))   #dummy encoding
  
  ct3<-dummy_cols(categ_col)
  ct3<- ct3[,-(1:ncol(categ_col))]
  
  #ct3[,1:ncol(ct3)]<-lapply(ct3[,1:ncol(ct3)],as.factor)
  
  fd2<- cbind(atn,ct3) #Full data matrix including cont and categorical features
  gp<- c(gln,un_grp_cat)  #Group indices for GL
  
  
  key<-cbind(colnames(at_me),grp_number_main)
  
  names<-paste0('x',1:ncol(cont_col))
  names2<-paste0('\\b',names,'\\b')
  
  
  n2<-list()
  n3<-list()
  n4<-list()
  for (i in 1:length(names2)){
    
    n2[[i]]<-paste0('x',i)
    n3[[i]]<-unique(key[grepl(names2[i],colnames(at_me)),2])
    n4[[i]]<-list(n2[[i]],n3[[i]])
    
  }
  
  
  key_df<- as.data.frame(do.call(rbind,n4))
  
  #main features selected by mars
  fs_mars<-unlist(key_df[which(key_df[,2]>=1),1])   
  
  # Features excluded by mars
  fs_not_in_mars<- unlist(key_df[key_df[,2]=="character(0)",1])
  
  
  #main features selected by mars and its group number
  fs_mars_key<-as.data.frame(cbind((key_df[which(key_df[,2]>=1),1]),key_df[which(key_df[,2]>=1),2])) 
  gp_mars_cont<-unlist(fs_mars_key[,2])  # group number for in mars main feature
  
  if (length(fs_not_in_mars)>0){
  gp_not_in_mars<-(max(gp)+1):(max(gp)+length(fs_not_in_mars)) #gn for not in mars main feature
  } else{gp_not_in_mars<-NULL}
  
  cont_in_mars<-subset(dm2,select=fs_mars)
  cont_not_in_mars<-subset(dm2,select=fs_not_in_mars)
  
  fd3<-cbind(fd2,cont_in_mars,cont_not_in_mars)
  gp_final<-c(gp,gp_mars_cont,gp_not_in_mars)
  
  gp_final<-as.data.frame(t(gp_final))
  
  colnames(gp_final)<-colnames(fd3)
  
  before_sorting<- rbind(gp_final,fd3)
  
  trying<-lapply(before_sorting[1,],as.numeric)

  
  trying2<-do.call(rbind,trying)
  trying3<- as.data.frame(sort(trying2[,1]))
  
  trying4<-fd3[,rownames(trying3)]
  
  gp_final2<-unlist(trying3[,1])
  
  optimized_parameters<-hp_opt3(type,dim,size,split,dm2,func,y)
  
  
  ##### Fitting group lasso to fd3
  fd3<-trying4
  
  fd3<- as.matrix(fd3)
  
  glas<- gglasso(fd3,y,gp_final2,loss="ls",lambda = optimized_parameters[[1]])
  
  cof<- as.data.frame(coef(glas)) #coef of 1SE model
  
  t5<-which(cof[,]!=0)  # Row number of non zero groups
  
  nzr_names<-rownames(cof)[t5]  #Rownames of non zero group
  nzr_names<- nzr_names[-1] 
  
  codenames<-c()
 
  for (i in 1:length(nzr_names))
    
    if(nchar(nzr_names[i])==4 |nchar(nzr_names[i])==5){
      
      codenames[i]<-sub("\\_.*", "", nzr_names[i])
      
    } else{codenames[i]<-variable_finder(nzr_names[i])}
      
      
  stage2_sf<-unique(unlist(codenames))    # names of features selected by group lasso
      
  stage2_sf<-stage2_sf[nchar(stage2_sf)<=3]
   sum(tm %in% unlist(stage2_sf))
  
  codenames2<-c()
  
  for (i in 1:length(nzr_names))
    
    if(nchar(nzr_names[i])==5 |nchar(nzr_names[i])==4 ){
      
      codenames2[i]<-sub("\\_.*", "", nzr_names[i])
      
    } else{codenames2[i]<-nzr_names[i]}
  
  
  codenames2<-unique(unlist(codenames2))    # names of columns selected by group lasso
  
  
    
   ##### FS calculation
   cont_features_names<- colnames(cont_col)         # Names of all cont features
   categ_features_names<- colnames(categ_col)       # Names of all categ features
   
   true_cont<- cont_features_names[cont_features_names %in% tm] # True model important cont
   true_categ<- categ_features_names[categ_features_names %in% tm] # True model important categ
   tms_cont <- setdiff(cont_features_names,true_cont) # True model spurious - cont
   tms_categ <- setdiff(categ_features_names,true_categ) # True model spurious - categ
   
   
   
   
   stage2_sf_cont<-stage2_sf[stage2_sf %in% true_cont]         #selected FS cont at stage2
   stage2_sf_categ<-stage2_sf[stage2_sf %in% true_categ]     # selected FS categ at stage 2
   
   
   spurious_stage2_sf_cont<-setdiff(cont_features_names,stage2_sf_cont)
   spurious_stage2_sf_categ<-setdiff(categ_features_names,stage2_sf_categ)
   
   
   
   
   
   
   
   # Sensitivity and specificity - stage 2 
   acont_s2<- length(intersect(tms_cont,spurious_stage2_sf_cont))
   dcont_s2<- length(intersect(true_cont,stage2_sf_cont))
   
   sens_cont_s2<- dcont_s2/length(true_cont)
   spec_cont_s2<- acont_s2/length(tms_cont)
   gmean_cont_s2<-sqrt(sens_cont_s2*spec_cont_s2)
   
   
   acateg_s2<- length(intersect(tms_categ,spurious_stage2_sf_categ))
   dcateg_s2<- length(intersect(true_categ,stage2_sf_categ))
   
   sens_categ_s2<- dcateg_s2/length(true_categ)
   spec_categ_s2<- acateg_s2/length(tms_categ)
   gmean_categ_s2<-sqrt(sens_categ_s2*spec_categ_s2)
   
   
   ##################################################
   #### Step 3a - Fitting Boosted Trees
   ##################################################
   #fd_step3<- fd[,codenames2]
   
   fd_new<-cbind(atn,cont_col,categ_col)
   
   fd_step3<-subset(fd_new,select=codenames2)
   
   cnfd3<-colnames(fd_step3)  #column names of input data matrix fed to glinternet
   
   # 
   # cont_cfd3_mars<-cnfd3[which(nchar(cnfd3)>4)]
   # 
   # cont_cfd3_raw<-cont_features_names[cont_features_names %in% cnfd3]
   # 
   # 
   # cont_cfd3<-c(cont_cfd3_mars,cont_cfd3_raw)
   #categ_cfd3<- cnfd3[which(nchar(cnfd3)<4)]
   categ_cfd3<-categ_features_names[categ_features_names %in% cnfd3]
   
   cont_cfd3<-setdiff(cnfd3,categ_cfd3)
   
   fd_step3<- fd_step3[,c(cont_cfd3,categ_cfd3)]
   
   # for (i in 1:ncol(fd_step3)){
   # 
   #   if(is.numeric(fd_step3[,i])){
   # 
   #     colnames(fd_step3)[i]<-paste0('n',i)
   #   }
   # 
   # 
   # }
   
   # key<-cbind(codenames2,colnames(fd_step3))
   
   
   
   best_parameters_gbm<-optimized_parameters[[3]]
   
   # gbmGrid3 <-  expand.grid(n.trees = best_parameters_gbm[1,1],interaction.depth=best_parameters_gbm[1,2],
   #                          shrinkage = best_parameters_gbm[1,3],
   #                          n.minobsinnode=best_parameters_gbm[1,4])
   
   
   
   
   
   fulldata<-as.data.frame(cbind(fd_step3,y))
   
   tuned_gbm2<- gbm(y~., distribution = "gaussian",data=fulldata,n.trees = best_parameters_gbm[1,1],interaction.depth=best_parameters_gbm[1,2],
                    shrinkage = best_parameters_gbm[1,3],n.minobsinnode=best_parameters_gbm[1,4] )
   
   
   # tuned_gbm2<-train(fd_step3,y,method = "gbm",tuneGrid=gbmGrid3,
   #                   verbose=FALSE)
   
   ### Calculating metrics
   
   #fd_step3<- fd[,codenames2]
   
   
   
   
   td2<-get_td(dim,size,split)
   
   if(length(cont_cfd3)==0){              #subsetting only cont variables
     
     tbf2<-NULL
     
   } else {      
     # To clean up the expr 
     # clean<- function(expr){
     #   t1<- gsub("h","",expr)
     #   return(t1)
     # }
     
     
     
     #lst<-lapply(cont_cfd3,clean)
     lst<-unlist(lapply(cont_cfd3,clean))
     #lst<-unlist(lapply(cnfd3,clean))
     
     
     lst2<-list()
     
     for (i in 1:length(lst)){
       lst2[[i]]<- with(td2,eval(parse(text=lst[[i]]))) 
       
     }
     
     
     tbf<-lapply(lst2,hinge_max)  # test data matrix with hinge funcs evaluated
     tbf2<-do.call(cbind,(tbf))  # as data frame
   }
   # Modified test data set 
   
   if (length(cont_cfd3)==0){
     td_new<- subset(td2,select=categ_cfd3)
     
     
     
   } else {td_new<- cbind(tbf2,td2[,categ_cfd3])}
   
   td_new<- as.data.frame(td_new)
   
   colnames(td_new)<-colnames(fd_step3)
   
   
   
   y_td<- with(td2,eval(parse(text=func)))  # Test data responses
   
   
   #### Prediction MAE metric - proposed model
   
   mae_pm_bt<-MAE(predict(tuned_gbm2,td_new,n.trees=best_parameters_gbm[1,1]),y_td)     #MAE 
   
    cont_features_names<- colnames(cont_col)         # Names of all cont features
    categ_features_names<- colnames(categ_col)       # Names of all categ features
   # 
    true_cont<- cont_features_names[cont_features_names %in% tm] # True model important cont
    true_categ<- categ_features_names[categ_features_names %in% tm] # True model important categ
    tms_cont <- setdiff(cont_features_names,true_cont) # True model spurious - cont
    tms_categ <- setdiff(categ_features_names,true_categ) # True model spurious - categ
    
    tms<-c(tms_cont,tms_categ)
   # 
   # # ####### Common calculation - interactions ###################
   # #   combns <- t(combn(paste0("x", 1:12), m = 2))
   # #   
   # #   all_interactions<- paste0(combns[, 1L], "*", combns[, 2L])
   # #   
   # #   true_interactions_tm<-unlist(f[[3]])
   # #   spurious_interactions<- setdiff(all_interactions,true_interactions_tm)
   # 
   # 
   # ##############################################
   # 
   # 
   # ############################### Main features calculation ##########################
   # ####################################################################################
   # 
    fbt<- as.data.frame(summary(tuned_gbm2,plotit=FALSE))     ### Var imp of BT from 3a
    rn_fbt<- rownames(fbt)[which(fbt[,2]>0)]             ## names of features with >1 rel.inf
   # 
    #fs_categ_pm1<- rn_fbt[which(nchar(rn_fbt)<6)]
    fs_categ_pm1<- rn_fbt[rn_fbt %in% categ_features_names]                 # categorical features selected by pm_bt
   # 
    #fs1<- rn_fbt[which(nchar(rn_fbt) > 5)]   
    fs1<- c(rn_fbt[rn_fbt %in% cont_features_names],rn_fbt[which(nchar(rn_fbt)>4)])# cont features selected by pm_bt
   # 
   # 
   # ### Cleaning selected features
   # 
    fs1_list<- lapply(fs1,clean)  
    fs2<- lapply(fs1_list,loc)
   # # 
    #fs_cont_pm1<- unique(unlist(fs2))               # # continuous features selected by pm_bt
   # 
   # 
    fs_cont_pm1<- unique(unlist(lapply(fs1, c2)))
   # 
   # 
    spurious_cont_pm1<- setdiff(cont_features_names,fs_cont_pm1)           # Spurious cont features - PM1
    spurious_categ_pm1<- setdiff(categ_features_names,fs_categ_pm1)        # Spurious categ features - PM1
   # 
   # 
   # #########
   # # Sensitivity and specificity - Main effects -PM1 
    acont_pm1<- length(intersect(tms_cont,spurious_cont_pm1))
    dcont_pm1<- length(intersect(true_cont,fs_cont_pm1))
   # 
    sens_cont_pm1<- dcont_pm1/length(true_cont)
    spec_cont_pm1<- acont_pm1/length(tms_cont)
    gmean_cont_pm1<-sqrt(sens_cont_pm1*spec_cont_pm1)
   # 
   # 
   acateg_pm1<- length(intersect(tms_categ,spurious_categ_pm1))
    dcateg_pm1<- length(intersect(true_categ,fs_categ_pm1))
   # 
    sens_categ_pm1<- dcateg_pm1/length(true_categ)
    spec_categ_pm1<- acateg_pm1/length(tms_categ)
    gmean_categ_pm1<-sqrt(sens_categ_pm1*spec_categ_pm1)
    
    
    ############ FDR calculations
    fs_pm1<-c(fs_cont_pm1,fs_categ_pm1)
    
    fdr_pm1<-length(intersect(fs_pm1,tms))/length(fs_pm1)
    
    # fdr_cont_pm1<-length(intersect(fs_cont_pm1,tms_cont))/length(fs_cont_pm1)
    # fdr_categ_pm1<-length(intersect(fs_categ_pm1,tms_categ))/length(fs_categ_pm1)
    
    
   # 
   # 
   # ############################### Interactions calculation ##########################
   # ####################################################################################
   # 
   # ####### Common calculations - interactions
   #  combns <- t(combn(paste0("x", 1:ncol(dm2)), m = 2))
   # # 
   #  all_interactions<- paste0(combns[, 1L], "*", combns[, 2L])
   # # 
   #  true_interactions<- unlist(f[[3]])
   #  true_interactions<- unname(true_interactions)
   #  spurious_interactions<- setdiff(all_interactions,true_interactions)
   # # 
   # # #######################
   # # 
   # colnames(fd_step3)<- tuned_gbm2$var.names
   # # 
   #  tuned_gbm2$var.names<-str_remove_all(tuned_gbm2$var.names,'`')
   #  rn_fbt<-str_remove_all(rn_fbt,'`')
   # # 
   # # #combn2 <- t(combn(tuned_gbm2$var.names, m = 2))
   # # 
   #  combn2 <- t(combn(tuned_gbm2$var.names[tuned_gbm2$var.names %in% rn_fbt], m = 2))
   # # 
   # # 
   # # 
   #  all_interactions2<- paste0(combn2[, 1L], "*", combn2[, 2L])
   # # 
   # # 
   # # 
   # # ### Method1 - hstatistic
   #  int.h2 <- numeric(nrow(combn2))
   # # 
   #  for (i in 1:nrow(combn2)) {
   # #   
   #    skip_to_next<- FALSE  
   # #   
   # #   
   # #   #print(paste("iter", i, "of", nrow(combn2)))
   # #   
   #    tryCatch({ int.h2[i] <- interact.gbm(tuned_gbm2, data = fd_step3, i.var = combn2[i, ],
   #                                         n.trees = tuned_gbm2$n.trees)},
   #             error=function(e){skip_to_next<<-TRUE})
   # #   
   #    if (skip_to_next) {next}
   # #   
   #  }
   #  int.h2 <- data.frame(x = paste0(combn2[, 1L], "*", combn2[, 2L]), y = int.h2)
    #int.h2<- int.h2[order(int.h2$y, decreasing = TRUE), ]
   # #
   # 
   #  if(nrow(int.h2)>10){
   # #   
   #    int_ranked_screeningbt_method1<- int.h2[1:floor(nrow(combn2)/4),1]
   #    
   #  } else {int_ranked_screeningbt_method1<- int.h2[1,1]}
   # 
   # 
   # 
   # #int_selected_screeningbt_method1<- unique(unlist(lapply(int_ranked_screeningbt_method1,id_int))) 
   # 
  #int_selected_screeningbt_method1<- unique(unlist(lapply(int_ranked_screeningbt_method1,new_id_int))) 
   # 
   # #int method1
   # spurious_screeningbt_method1<- setdiff(all_interactions,int_selected_screeningbt_method1)
   
    
   #  int_selected_screeningbt_method1<-int.h2[which(int.h2[,2]>0),1]
   #  #int_selected_screeningbt_method1<- unique(unlist(lapply(int_ranked_screeningbt_method1,new_id_int))) 
   #  
   #  spurious_screeningbt_method1<- setdiff(all_interactions,int_selected_screeningbt_method1)
   #  
   # # ##### Calculations
   # # 
   #  a_screeningbt_method1<- length(intersect(spurious_interactions,spurious_screeningbt_method1))
   #  d_screeningbt_method1<- length(intersect(true_interactions,int_selected_screeningbt_method1))
   # # 
   #  sensitivity_int_screeningbt_method1<- d_screeningbt_method1/length(true_interactions)
   #  specificity_int_screeningbt_method1<- a_screeningbt_method1/length(spurious_interactions)
   # 
   # # a_screeningbt_method2<- length(intersect(spurious_interactions,spurious_screeningbt_method2))
   # # d_screeningbt_method2<- length(intersect(true_interactions,int_selected_screeningbt_method2))
   # # 
   # # sensitivity_int_screeningbt_method2<- d_screeningbt_method2/length(true_interactions)
   # # specificity_int_screeningbt_method2<- a_screeningbt_method2/length(spurious_interactions)
   # 
   # 
   # ##################################################
   # #### Step 3b - Fitting Glinternet
   # ##################################################
   
   numlevels<-c()  # numlevel for glinternet
   
   for (i in 1:ncol(fd_step3)){
     if (class(fd_step3[,i])=="numeric"){
       
       numlevels[i]<- 1
       
     } else {numlevels[i]<- nlevels(fd_step3[,i])}
     
   }
   
   
   # Converting to glinternet input form
   for(i in 1:ncol(fd_step3)){
     
     fd_step3[,i]<- as.numeric(as.character(fd_step3[,i]))
     
   }
   
   
   ## Fitting glinternet
   glnet<- glinternet(fd_step3,y,numlevels,lambda =optimized_parameters[[2]] )
   
   # Prediction Metric - PM2
   mae_pm_gln <- MAE(predict(glnet,td_new,lambda = optimized_parameters[[2]]),y_td)
   
   
   # Co-ef of glinternet model
    coef_1std<-coef(glnet)[[2]]
   # 
   # #Separating cont and categ features in input given to glinternet.
   # 
    #fd_step3_cont_names<- codenames2[which(nchar(codenames2)>4)]
    #fd_step3_categ_names<-codenames2[which(nchar(codenames2)<4)]
   
    
    fd_step3_cont_names<-c(codenames2[codenames2 %in% cont_features_names],codenames2[which(nchar(codenames2)>4)])
    fd_step3_categ_names<-codenames2[codenames2 %in% categ_features_names]
    
    
   # #Selected features from glinternet
    cont_selected_glnet<- fd_step3_cont_names[unlist(coef_1std$mainEffects[2])]
    categ_selected_glnet<- fd_step3_categ_names[unlist(coef_1std$mainEffects[1])]
   # 
    test<-list()
   # 
    for (i in 1:length(cont_features_names)){
   #   
      test[[i]]<-any(grepl(cont_features_names[i],cont_selected_glnet))
   #   
    }
   # 
    fs_cont_pm2<- which(unlist(test))                          # Selected cont features by glinternet
   # 
    for (i in 1:length(fs_cont_pm2)){
   #   
      fs_cont_pm2[i]<- paste0("x",fs_cont_pm2[i])
   #   
    }
   # 
    test2<-list()
   # 
    for (i in 1:length(categ_features_names)){
   #   
      test2[[i]]<-any(grepl(categ_features_names[i],categ_selected_glnet))
   #   
    }
   # 
    fs_categ_pm2<- which(unlist(test2))   
   # 
    fs_categ_pm2<- fs_categ_pm2 + length(cont_features_names)    # Adding 6 (number of cont features to subset off the index)
   # 
    for (i in 1:length(fs_categ_pm2)){
   #   
      fs_categ_pm2[i]<- paste0("x",fs_categ_pm2[i])         # Selected categ features by glinternet
   #   
    }
   # 
   # 
    spurious_cont_pm2<- setdiff(cont_features_names,fs_cont_pm2)           # Spurious cont features - PM2
    spurious_categ_pm2<- setdiff(categ_features_names,fs_categ_pm2)        # Spurious categ features - PM2
   # 
   # 
   # 
   # #########
   # # Sensitivity and specificity - Main effects- PM2
   # 
    acont_pm2<- length(intersect(tms_cont,spurious_cont_pm2))
    dcont_pm2<- length(intersect(true_cont,fs_cont_pm2))
   # 
    sens_cont_pm2<- dcont_pm2/length(true_cont)
    spec_cont_pm2<- acont_pm2/length(tms_cont)
    gmean_cont_pm2<-sqrt(sens_cont_pm2*spec_cont_pm2)
   # 
    acateg_pm2<- length(intersect(tms_categ,spurious_categ_pm2))
    dcateg_pm2<- length(intersect(true_categ,fs_categ_pm2))
   # 
    sens_categ_pm2<- dcateg_pm2/length(true_categ)
    spec_categ_pm2<- acateg_pm2/length(tms_categ)
    gmean_categ_pm2<-sqrt(sens_categ_pm2*spec_categ_pm2)
   
    
    ############ FDR calculations
    
    fs_pm2<-c(fs_cont_pm2,fs_categ_pm2)
    
    fdr_pm2<-length(intersect(fs_pm2,tms))/length(fs_pm2)
    
    
    
    # fdr_cont_pm2<-length(intersect(fs_cont_pm2,tms_cont))/length(fs_cont_pm2)
    # fdr_categ_pm2<-length(intersect(fs_categ_pm2,tms_categ))/length(fs_categ_pm2)
    
    
    
    
   # 
   # ########################## Interactions
   # ################################################################
   # int_glnet<-coef_1std$interactions
   # cont_fd_step3<-subset(fd_step3,select = which(numlevels==1))
   # categ_fd_step3<-subset(fd_step3,select = which(numlevels!=1))
   # 
   # cnames_cont_fd_step3<-colnames(cont_fd_step3)
   # cnames_categ_fd_step3<-colnames(categ_fd_step3)
   # 
   # # cat-cat interaction
   # catcat_int_indices<- coef_1std$interactions$catcat
   # 
   # if(length(catcat_int_indices)> 0) {
   #   catcat_int_screeningglnet<- list()
   #   
   #   
   #   for (i in 1:nrow(catcat_int_indices)){
   #     
   #     catcat_int_screeningglnet[[i]]<- c(cnames_categ_fd_step3[catcat_int_indices[i,1]],cnames_categ_fd_step3[catcat_int_indices[i,2]])
   #   }
   #   
   #   
   #   catcat_int_screeningglnet<-lapply(catcat_int_screeningglnet,mult)
   # } else {catcat_int_screeningglnet<- NULL}
   # 
   # # contcont interaction
   # contcont_int_indices<- coef_1std$interactions$contcont
   # 
   # if (length(contcont_int_indices)>0){
   #   contcont_int_screeningglnet<- list()
   #   for (i in 1:nrow(contcont_int_indices)){
   #     
   #     contcont_int_screeningglnet[[i]]<- c(cnames_cont_fd_step3[contcont_int_indices[i,1]],cnames_cont_fd_step3[contcont_int_indices[i,2]])
   #   }
   #   
   #   
   #   contcont_int_screeningglnet<-lapply(contcont_int_screeningglnet,mult)
   #   
   # } else (contcont_int_screeningglnet<-NULL)
   # 
   # 
   # # catcont interaction
   # catcont_int_indices<- coef_1std$interactions$catcont
   # 
   # if(length(catcont_int_indices)>0){
   #   catcont_int_screeningglnet<- list()
   #   
   #   for (i in 1:nrow(catcont_int_indices)){
   #     
   #     catcont_int_screeningglnet[[i]]<- c(cnames_categ_fd_step3[catcont_int_indices[i,1]],cnames_cont_fd_step3[catcont_int_indices[i,2]])
   #   }
   #   
   #   catcont_int_screeningglnet<-lapply(catcont_int_screeningglnet,mult)
   #   
   # } else {catcont_int_screeningglnet<- NULL}
   # 
   # combine_int_screeningglnet<-c(catcat_int_screeningglnet,contcont_int_screeningglnet,catcont_int_screeningglnet)
   # 
   # 
   # 
   # #int_selected_screeningglnet<-unlist(unique(lapply(combine_int_screeningglnet,new_id_int)))  # interactions selected screening-glnet
   # 
   # #int_selected_screeningglnet<-unlist(unique(lapply(combine_int_screeningglnet,new_id_int)))  # interactions selected screening-glnet
   # if(length(combine_int_screeningglnet)==0){
   #   
   #   int_selected_screeningglnet<-NULL  
   #   
   # } else{
   #   temp<-list()
   #   
   #   for (i in 1:length(combine_int_screeningglnet)){
   #     
   #     temp[[i]]<- new_id_int(combine_int_screeningglnet[i])
   #     
   #     
   #   }
   #   
   #   
   #   int_selected_screeningglnet<-unique(unlist(temp))
   # }
   # spurious_int_screeningglnet<- setdiff(all_interactions,int_selected_screeningglnet)          # spurious interactions
   # 
   # 
   # a_int_screeningglnet<- length(intersect(spurious_interactions,spurious_int_screeningglnet))
   # d_int_screeningglnet<- length(intersect(true_interactions,int_selected_screeningglnet))
   # 
   # sensitivity_int_screeningglnet<- d_int_screeningglnet/length(true_interactions)
   # specificity_int_screeningglnet<- a_int_screeningglnet/length(spurious_interactions)
   # 
   
   
   ###################################################
   ###### With boosted trees #########################
   ###################################################
   
   best_parameters_gbm<-optimized_parameters[[3]]
   
   gbmGrid2 <-  expand.grid(n.trees = best_parameters_gbm[1,1],interaction.depth=best_parameters_gbm[1,2],
                            shrinkage = best_parameters_gbm[1,3],
                            n.minobsinnode=best_parameters_gbm[1,4])
   
   
   fm<-cbind(dm2,y)
   tuned_gbm<- gbm(y~.,distribution = "gaussian",data=fm,n.trees = best_parameters_gbm[1,1],
                   interaction.depth=best_parameters_gbm[1,2],
                   shrinkage = best_parameters_gbm[1,3],
                   n.minobsinnode=best_parameters_gbm[1,4])
   
   # tuned_gbm<-train(dm2,y,method = "gbm",tuneGrid=gbmGrid2,
   #                  verbose=FALSE)
   
   
   ### Prediction metrics
   mae_bt<-MAE(predict(tuned_gbm,td2),y_td)     #MAE
   
   
   # ###### Feature selection metric - main effects ################
   # ##############################################################
    var_name_bt<- as.data.frame(summary(tuned_gbm,plotit=FALSE))
   # 
    fs_bt<- rownames(var_name_bt)[which(var_name_bt[,2] > 0)]
   # 
    fs_cont_bt<-fs_bt[fs_bt %in% cont_features_names]              # Selected cont features
    fs_categ_bt<-fs_bt[fs_bt %in% categ_features_names]            # Selected categ features
   # 
    spurious_cont_bt<- setdiff(cont_features_names,fs_cont_bt)           # Spurious cont features - BT
    spurious_categ_bt<- setdiff(categ_features_names,fs_categ_bt)        # Spurious categ features - BT
   # 
    acont_bt<- length(intersect(tms_cont,spurious_cont_bt))
    dcont_bt<- length(intersect(true_cont,fs_cont_bt))
   # 
    sens_cont_bt<- dcont_bt/length(true_cont)
    spec_cont_bt<- acont_bt/length(tms_cont)
    gmean_cont_bt<-sqrt(sens_cont_bt*spec_cont_bt)
   # 
    acateg_bt<- length(intersect(tms_categ,spurious_categ_bt))
    dcateg_bt<- length(intersect(true_categ,fs_categ_bt))
   # 
    sens_categ_bt<- dcateg_bt/length(true_categ)
    spec_categ_bt<- acateg_bt/length(tms_categ)
    gmean_categ_bt<-sqrt(sens_categ_bt*spec_categ_bt)
   
    
    ############ FDR calculations
    fs_bt<-c(fs_cont_bt,fs_categ_bt)
    
    fdr_bt<-length(intersect(fs_bt,tms))/length(fs_bt)
    
    # fdr_cont_bt<-length(intersect(fs_cont_bt,tms_cont))/length(fs_cont_bt)
    # fdr_categ_bt<-length(intersect(fs_categ_bt,tms_categ))/length(fs_categ_bt)
    
    
    
   # 
   # ####################### Interactions #############################
   # 
   #  combns3 <- t(combn(paste0("x", 1:60), m = 2))
   # # 
   # # 
   #  combns3<- t(combn(tuned_gbm$var.names[tuned_gbm$var.names %in% fs_bt], m = 2))
   #  
   #  all_interactions3<- paste0(combns3[, 1L], "*", combns3[, 2L])
   # # #
   # # 
   # # # #### Method 1 - H statistic ##################
   #  int.h3 <- numeric(nrow(combns3))
   #  for (i in 1:nrow(combns3)) {
   # #   
   #    skip_to_next<- FALSE  
   # #   
   # #   #print(paste("iter", i, "of", nrow(combns3)))
   # #   
   #    tryCatch({ int.h3[i] <- interact.gbm(tuned_gbm, data = dm2, i.var = combns3[i, ],
   #                                         n.trees = tuned_gbm$n.trees)},
   #             error=function(e){skip_to_next<<-TRUE})
   #    
   #    if (skip_to_next) {next}
   #    
   #    
   #  }
   # # 
   # int.h3 <- data.frame(x = paste0(combns3[, 1L], "*", combns3[, 2L]), y = int.h3)
   # int.h3<- int.h3[order(int.h3$y, decreasing = TRUE), ]
   # 
   # int_ranked_directbt_method1<- int.h3[1:floor(nrow(combns3)/4),1]
   # 
   # int_selected_directbt_method1<- unique(unlist(lapply(int_ranked_directbt_method1,new_id_int)))           #int method1
   # spurious_directbt_method1<- setdiff(all_interactions,int_selected_directbt_method1)
   # #
   # #
   # a_directbt_method1<- length(intersect(spurious_interactions,spurious_directbt_method1))
   # d_directbt_method1<- length(intersect(true_interactions,int_selected_directbt_method1))
   # #
   # sensitivity_int_directbt_method1<- d_directbt_method1/length(true_interactions)
   # specificity_int_directbt_method1<- a_directbt_method1/length(spurious_interactions)
   # 
   # 
   # #### Method2 - pdp
   # 
   # #colnames(fm)[1:(length(fm)-1)]<-tuned_gbm$var.names
   # 
   # 
   # 
   # # int.i2 <- vint(
   # #   object = tuned_gbm,                    # fitted model object
   # #   feature_names = colnames(fm)[-(length(colnames(fm)))],  # features for which to compute pairwise interactions statistics
   # #   n.trees = tuned_gbm$n.trees,                 # needed if object is of class "gbm"
   # #   parallel = FALSE
   # # )
   # # 
   # # int.i2<-as.data.frame(int.i2)
   # 
   # 
   # 
   # # int.i2 <- vint2(
   # #   object = tuned_gbm,                    # fitted model object
   # #   feature_names = colnames(fm)[-(length(colnames(fm)))],  # features for which to compute pairwise interactions statistics
   # #   n.trees = tuned_gbm$n.trees,                 # needed if object is of class "gbm"
   # #   parallel = FALSE
   # # )
   # # 
   # # int.i2<-as.data.frame(int.i2)
   # 
   # 
   # 
   # 
   # 
   # 
   # 
   # # int_ranked_directbt_method2<- int.i2[1:6,1]
   # # int_selected_directbt_method2<- unique(unlist(lapply(int_ranked_directbt_method2,id_int)))           #int method1
   # # spurious_directbt_method2<- setdiff(all_interactions,int_selected_directbt_method2)
   # # 
   # # 
   # # a_directbt_method2<- length(intersect(spurious_interactions,spurious_directbt_method2))
   # # d_directbt_method2<- length(intersect(true_interactions,int_selected_directbt_method2))
   # # 
   # # sensitivity_int_directbt_method2<- d_directbt_method2/length(true_interactions)
   # # specificity_int_directbt_method2<- a_directbt_method2/length(spurious_interactions)
   # 
   # # 
   # 
   # 
   #########                       ######################
   ######### With direct glinternet ####################
   dnl<-c()
   
   for (i in 1:ncol(dm2)){
     if (class(dm2[,i])=="numeric"){
       
       dnl[i]<- 1
       
     } else {dnl[i]<- nlevels(dm2[,i])}
     
   }
   
   
   
   for(i in 1:ncol(dm2)){
     
     dm2[,i]<- as.numeric(as.character(dm2[,i]))
     
   }
   direct_gln<-glinternet(dm2,y,numLevels = dnl,lambda = optimized_parameters[[2]])
   #direct_gln<-glinternet(dm2,y,numLevels = dnl,lambda = lambda_value_glnet)
   
   # Prediction metric
   mae_direct_glnet<-MAE(predict(direct_gln,td2,lambda =optimized_parameters[[2]]),y_td) #MAE
   
   ########################### Feature selection metric - main effects #####
   ################################################################################
   
    coef_direct_gln<-coef(direct_gln)[[2]]   # Getting coefficients of 1SE
   # 
   # #Selected features - direct glinternet
    fs_cont_directglnet<-cont_features_names[unlist(coef_direct_gln$mainEffects[2])]   #Selected features cont direct gln
    fs_categ_directglnet<-categ_features_names[unlist(coef_direct_gln$mainEffects[1])] # Selected features categ direct gln
   # 
    spurious_cont_directgln<- setdiff(cont_features_names,fs_cont_directglnet)           # Spurious cont features - BT
    spurious_categ_directgln<- setdiff(categ_features_names,fs_categ_directglnet)        # Spurious categ features - BT
   # 
    acont_directgln<- length(intersect(tms_cont,spurious_cont_directgln))
    dcont_directgln<- length(intersect(true_cont,fs_cont_directglnet))
   # 
    sens_cont_directgln<- dcont_directgln/length(true_cont)
    spec_cont_directgln<- acont_directgln/length(tms_cont)
    gmean_cont_directgln<-sqrt(sens_cont_directgln*spec_cont_directgln)
   # 
    acateg_directgln<- length(intersect(tms_categ,spurious_categ_directgln))
    dcateg_directgln<- length(intersect(true_categ,fs_categ_directglnet))
   # 
    sens_categ_directgln<- dcateg_directgln/length(true_categ)
    spec_categ_directgln<- acateg_directgln/length(tms_categ)
    gmean_categ_directgln<-sqrt(sens_categ_directgln*spec_categ_directgln)
   
    ############ FDR calculations
    fs_gln<-c(fs_cont_directglnet,fs_categ_directglnet)
    
    fdr_gln<-length(intersect(fs_gln,tms))/length(fs_gln)
    
    # fdr_cont_gln<-length(intersect(fs_cont_directglnet,tms_cont))/length(fs_cont_directglnet)
    # fdr_categ_gln<-length(intersect(fs_categ_directglnet,tms_categ))/length(fs_categ_directglnet)
    
    
    
     
   # 
   # ####################### Interaction effects ############
   # int_directglnet<-coef_direct_gln$interactions
   # # cont_directgln<-dm2[,c(which(dnl==1))]
   # # categ_directgln<-dm2[,c(which(dnl!=1))]
   # 
   # 
   # cont_directgln<-subset(dm2,select=which(dnl==1))
   # categ_directgln<-subset(dm2,select=which(dnl!=1))
   # 
   # 
   # cnames_cont_dm2<-colnames(cont_directgln)
   # cnames_categ_dm2<-colnames(categ_directgln)
   # 
   # # cat-cat interaction
   # catcat_int_indices_direct<- int_directglnet$catcat
   # 
   # if(length(catcat_int_indices_direct)>0){
   #   catcat_int_directglnet<- list()
   #   
   #   
   #   for (i in 1:nrow(catcat_int_indices_direct)){
   #     
   #     catcat_int_directglnet[[i]]<- c(cnames_categ_dm2[catcat_int_indices_direct[i,1]],cnames_categ_dm2[catcat_int_indices_direct[i,2]])
   #   }
   #   
   #   
   #   catcat_int_directglnet<-lapply(catcat_int_directglnet,mult)
   #   
   # } else {catcat_int_directglnet<- NULL}
   # # contcont interaction
   # contcont_int_indices_direct<- int_directglnet$contcont
   # 
   # if(length(contcont_int_indices_direct)>0){
   #   contcont_int_directglnet<- list()
   #   for (i in 1:nrow(contcont_int_indices_direct)){
   #     
   #     contcont_int_directglnet[[i]]<- c(cnames_cont_dm2[contcont_int_indices_direct[i,1]],cnames_cont_dm2[contcont_int_indices_direct[i,2]])
   #   }
   #   
   #   
   #   contcont_int_directglnet<-lapply(contcont_int_directglnet,mult)
   # } else {contcont_int_directglnet<-NULL}
   # 
   # # catcont interaction
   # catcont_int_indices_direct<- int_directglnet$catcont
   # 
   # if(length(catcont_int_indices_direct)>0){
   #   catcont_int_directglnet<- list()
   #   
   #   for (i in 1:nrow(catcont_int_indices_direct)){
   #     
   #     catcont_int_directglnet[[i]]<- c(cnames_categ_dm2[catcont_int_indices_direct[i,1]],cnames_cont_dm2[catcont_int_indices_direct[i,2]])
   #   }
   #   
   #   catcont_int_directglnet<-lapply(catcont_int_directglnet,mult)
   #   
   # } else{catcont_int_directglnet<- NULL}
   # 
   # combine_int_directglnet<-c(catcat_int_directglnet,contcont_int_directglnet,catcont_int_directglnet)
   # 
   # if(length(combine_int_directglnet)==0){
   #   int_selected_directglnet<- NULL
   #   
   # } else{
   #   
   #   int_selected_directglnet<-unlist(unique(lapply(combine_int_directglnet,new_id_int)))
   # }
   # # interactions selected screening-glnet
   # spurious_int_directglnet<- setdiff(all_interactions,int_selected_directglnet)          # spurious interactions
   # 
   # 
   # a_int_directglnet<- length(intersect(spurious_interactions,spurious_int_directglnet))
   # d_int_directglnet<- length(intersect(true_interactions,int_selected_directglnet))
   # 
   # sensitivity_int_directglnet<- d_int_directglnet/length(true_interactions)
   # specificity_int_directglnet<- a_int_directglnet/length(spurious_interactions)
   # 
   # 
   # 
   # ################# 
   # 
   # 
   # 
   # 
   # 
   # # mae_pm_bt
   # # mae_pm_gln
   # # mae_bt
   # # mae_direct_glnet
   # # # 
   # # sensitivity_int_screeningbt_method1
   # #sensitivity_int_screeningbt_method2
   # #sensitivity_int_screeningglnet
   # # sensitivity_int_directbt_method1
   # #sensitivity_int_directbt_method2
   # #sensitivity_int_directglnet
   # # 
   # #specificity__int_screeningbt_method1
   # # specificity_int_screeningbt_method2
   # # specificity__int_screeningglnet
   # # #specificity__int_directbt_method1
   # # specificity__int_directbt_method2
   # # specificity__int_directglnet
   # 
   # 
   # # mae_pm_bt
   # # mae_pm_gln
   # # mae_bt
   # # mae_direct_glnet
   # # spec_cont_pm1
   # # spec_cont_pm2
   # # spec_cont_bt
   # # spec_cont_directgln
   # # spec_categ_pm1
   # # spec_categ_pm2
   # # spec_categ_bt
   # # spec_categ_directgln
   # 
   
   # op1<- list(mae_pm_bt,mae_bt,mae_pm_gln,mae_direct_glnet)
   # op2<- list(sens_cont_pm1,sens_cont_pm2,sens_cont_bt,sens_cont_directgln)
   # op3<-list(spec_cont_pm1,spec_cont_pm2,spec_cont_bt,spec_cont_directgln)
   # op4<- list(sens_categ_pm1,sens_categ_pm2,sens_categ_bt,sens_categ_directgln)
   # op5<- list(spec_categ_pm1,spec_categ_pm2,spec_categ_bt,spec_categ_directgln)
   #
   
   op<-list()
   op[[1]]<- c(mae_pm_bt,mae_bt,mae_pm_gln,mae_direct_glnet)
   # #
  op[[2]]<-c(sens_cont_pm1,sens_cont_pm2,sens_cont_bt,sens_cont_directgln,
               spec_cont_pm1,spec_cont_pm2,spec_cont_bt,spec_cont_directgln,
               sens_categ_pm1,sens_categ_pm2,sens_categ_bt,sens_categ_directgln,
               spec_categ_pm1,spec_categ_pm2,spec_categ_bt,spec_categ_directgln)
    
   # 
   # op[[3]]<- c(sensitivity_int_screeningbt_method1,sensitivity_int_screeningglnet,
   #             sensitivity_int_directbt_method1,sensitivity_int_directglnet,
   #             specificity_int_screeningbt_method1,specificity_int_screeningglnet,
   #             specificity_int_directbt_method1,specificity_int_directglnet)
   # 
   # 
    # op[[3]]<-c(sens_cont_s2,sens_cont_bt,sens_cont_directgln,
    #            spec_cont_s2,spec_cont_bt,spec_cont_directgln,
    #            sens_categ_s2,sens_categ_bt,sens_categ_directgln,
    #            spec_categ_s2,spec_categ_bt,spec_categ_directgln)
    
    op[[3]]<-c(sens_cont_s2,spec_cont_s2,sens_categ_s2,spec_categ_s2)
    
    
    # op[[4]]<-c(fdr_cont_pm1,fdr_cont_pm2,fdr_cont_bt,fdr_cont_gln,
    #            fdr_categ_pm1,fdr_categ_pm2,fdr_categ_bt,fdr_categ_gln)
    op[[4]]<-c(fdr_pm1,fdr_pm2,fdr_bt,fdr_gln)

    
   
  names(op[[1]])<- c("mae_pm_bt","mae_bt","mae_pm_gln","mae_direct_glnet")
   
   
  names(op[[2]])<- c("sens_cont_pm1","sens_cont_pm2","sens_cont_bt","sens_cont_directgln",
                       "spec_cont_pm1","spec_cont_pm2","spec_cont_bt","spec_cont_directgln",
                       "sens_categ_pm1","sens_categ_pm2","sens_categ_bt","sens_categ_directgln",
                       "spec_categ_pm1","spec_categ_pm2","spec_categ_bt","spec_categ_directgln" )
   
   # 
   # 
   # 
   # names(op[[3]])<- c("sensitivity_int_screeningbt_method1","sensitivity_int_screeningglnet",
   #                    "sensitivity_int_directbt_method1","sensitivity_int_directglnet",
   #                    "specificity_int_screeningbt_method1","specificity_int_screeningglnet",
   #                    "specificity_int_directbt_method1","specificity_int_directglnet")
   # 
   # 
  names(op[[3]])<- c("sens_cont_s2","spec_cont_s2","sens_categ_s2","spec_categ_s2")
    
    
    # names(op[[4]])<- c("fdr_cont_screening_bt","fdr_cont_screeningGLN","fdr_cont_bt","fdr_cont_GLN",
    #                    "fdr_categ_screeningBT","fdr_categ_screeningGLN","fdr_categ_BT","fdr_categ_GLN")
    
  names(op[[4]])<- c("fdr_screening_bt","fdr_screeningGLN","fdr_bt","fdr_GLN")

   # 
   # #op_pred<-unlist(op)
   # #op_pred
   
   #op
   # sens_cont_s2
   # spec_cont_s2
   # sens_categ_s2
   # spec_categ_s2
   
   
   return(op)
   
  
}
  

variable_finder<-function(expr){
  
  names<-paste0('x',1:60)
  names2<-paste0('\\b',names,'\\b')
  
  n<-list()
  
  for (i in 1:length(names2)){
    
    n[[i]]<-grepl(names2[i],expr)
  
  }
  
  cn<-colnames(dm2)[unlist(n)]
  
  if(length(cn)>1){
    
    op<-mult(cn)
    
  } else{op<-cn}
  
  return(op)
  
  
  
}

#sub("\\|.*", "", str1)





mr<- grid_search(screening_with_all_terms_s2,params=list(split=c(1,2)),type="MartinezSLHD",dim=12,size=2,
                              imp=0.25,resptype=4,nl=1, n.iter = 20,output = "list")

#### Prediction MAE
pe<-list()
#for (i in 1:length(mr)){

for (i in 1:40){ 
  pe[[i]]<-mr$results[[i]][[1]]
  
}

colMeans(t(do.call(cbind,pe)))

boxplot(t(do.call(cbind,pe)))





fs<-list()
#for (i in 1:length(mr)){

for (i in 1:40){ 
  fs[[i]]<-mr$results[[i]][[2]]
  
}

colMeans(t(do.call(cbind,fs)))

boxplot(t(do.call(cbind,fs))[,13:16])

  

fs_s2<-list()
#for (i in 1:length(mr)){

for (i in 1:40){ 
  fs_s2[[i]]<-mr$results[[i]][[3]]
  
}

colMeans(t(do.call(cbind,fs_s2)))

boxplot(t(do.call(cbind,fs_s2)))




fdr<-list()
#for (i in 1:length(mr)){

for (i in 1:40){ 
  fdr[[i]]<-mr$results[[i]][[4]]
  
}

colMeans(t(do.call(cbind,fdr)))

boxplot(t(do.call(cbind,fdr)))



