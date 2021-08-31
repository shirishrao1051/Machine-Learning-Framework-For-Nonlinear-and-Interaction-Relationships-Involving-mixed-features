##### Function to fit MARS and GBM model. The outputs are testing errors and % of right features selected. 
# Input : design_matrix - Full design matrix 
#         y - response values
#         td - test data set (from MaxproQQ design)
#         y_td - actual response values of test data set
#         tm - indexes of true features (for calculating % features picked)
#         int - 1(additive) , 2-(interaction)

modeling<-function(design_matrix,dim,split,imp,nl,resptype){
  
  library(earth)
  library(MLmetrics)
  
  colnames(design_matrix)<-paste0('x',1:ncol(design_matrix))
  
  #Response calculation
   f<-resp_gen_new(dim,split,imp,nl,resptype)
  func<-as.character(f[1])
  resp_value<- with(design_matrix,eval(parse(text=func))) #Calculating response values from the func
   tm<-f[[2]]
  
  
  
  #Generating noise
    #noise<- rnorm(nrow(design_matrix),0,0.1)
     snr<-25
     sigma <- sqrt(var(resp_value)/snr)


     y<-resp_value+rnorm(nrow(design_matrix),0,sigma)
  #y<- resp_value 
 
  #Getting test dataset
  test_data<-td(dim,split)
  
  #Test data responses
  
  y_td<- with(test_data,eval(parse(text=func)))
  sigmatd <- sqrt(var(y_td)/snr)
  test_data_y<-y_td + rnorm(nrow(test_data),0,sigmatd)
  
  
  
  
  ############# Creating testing data set for tuning the parameters

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
  c4<-categ_col[,(ncol(categ_col)-2):ncol(categ_col)]
  categ_col<-categ_col[,1:(ncol(categ_col)-3)]


  cont_col2<-cont_col[,sample(ncol(cont_col))] #Randomizing numeric columns
  categ_col2<-categ_col[,sample(ncol(categ_col))] #Randomizing factor columns

  test_design_parameters<- cbind(cont_col2,categ_col2,c4)
  colnames(test_design_parameters)<-paste0('x',1:ncol(test_design_parameters))

  #### Response values for testing data set (for parameters)

  resp_value_parameters<- with(test_design_parameters,eval(parse(text=func)))

  sigma2 <- sqrt(var(resp_value_parameters)/snr)


  y_tuning<-resp_value_parameters+rnorm(nrow(design_matrix),0,sigma2)

  #
  #
  # # ############### Tuning hyperparameter for MARS using a test data set (another experimental design)
  # grid<- expand.grid(degree=c(1,2),nprune=c(seq(10,100,10)))
  # start_time1 <- Sys.time()
  # tuned_mars <- train(
  #   x = test_design_parameters,
  #   y = y_tuning,
  #   method = "earth",
  #   metric = "RMSE",
  #   tuneGrid = grid
  # )
  # 
  # 
  # best_parameters_mars<- tuned_mars$bestTune
  # 
  # prune<-best_parameters_mars[1,1]
  # deg<-best_parameters_mars[1,2]
  # 
  # grid2<- expand.grid(degree= deg,nprune=prune)
  # #
  # #
  # #
  # # ##### Fitting MARS model on design
  # # start_time1 <- Sys.time()
  # #
  # mars <- train(
  #   x = design_matrix,
  #   y = y,
  #   method = "earth",
  #   metric = "RMSE",
  #   tuneGrid = grid2
  # )
  # 
  # end_time1 <- Sys.time()
  # #mars<- earth(x=design_matrix,y=y,degree=int)
  # 
  #  
  #  time_mars<-(end_time1)-(start_time1)
  # 
  # pred_mars<-predict(mars,test_data)
  # testing_error_MARS<- MAE(pred_mars,test_data_y)
  # 
  # 
  #  
  
  ############### Tuning hyperparameter for GBM using a test data set (another experimental design)

   gbmGrid <-  expand.grid(n.trees =c(100,500),interaction.depth=c(1,2), shrinkage = c(0.01,0.1),n.minobsinnode=10)
   start_time2 <- Sys.time()
  #
   tuned_gbm<-train(test_design_parameters,y_tuning,method = "gbm",tuneGrid=gbmGrid,verbose=FALSE)
  #
   best_parameters_gbm<- tuned_gbm$bestTune
  #
  #
  #
  # ### GBM model
   gbmGrid2 <-  expand.grid(n.trees = best_parameters_gbm[1,1],interaction.depth=best_parameters_gbm[1,2],
                            shrinkage = best_parameters_gbm[1,3],
                            n.minobsinnode=best_parameters_gbm[1,4])
  #
   gbmfit<-train(design_matrix,y,method = "gbm",tuneGrid=gbmGrid2,verbose=FALSE)
  #
  #
  #
  #
   gbmfit<- gbm.fit(design_matrix,y,distribution = "gaussian")
   end_time2<- Sys.time()
   time_gbm<-(end_time2)-(start_time2)
  
   pred_gbm<-predict(gbmfit,test_data)
   testing_error_gbm<- MAE(pred_gbm,test_data_y)

  
  
  
  ### ---------------------------------------------- ###
  ##          Group Lasso for g-mean                 ###

 
  suppressWarnings(dummy_design_matrix<- dummy.data.frame(design_matrix))
  
  suppressWarnings(dummy_test_design_parameters<- dummy.data.frame(test_design_parameters))
  

  
  dummy_design_matrix<-as.matrix(dummy_design_matrix) # design matrix
  dummy_test_design_parameters<-as.matrix(dummy_test_design_parameters) # Test Design matrix


  #Grouping index for group lasso

  cont<- sapply(test_design_parameters,is.numeric)    # Counting number of continuous features

  nc<- sum(cont)



  cat_group<- list()   # List to store grouping index for categorical variables


  ncat<- (nc+1):ncol(test_design_parameters)

  for(i in ncat){

    cat_group[[i]]<- rep(i,nlevels(test_design_parameters[,i]))
  }


  u<- unlist(cat_group)
  group<- c(1:nc,u)


  # Fitting group lasso
  glas<- gglasso(dummy_test_design_parameters,y_tuning,group = group,loss="ls")
  t<-glas$beta
  err<-c()
  
  pr1<- predict(glas,dummy_design_matrix,type = "link")
  

  for (i in 1:ncol(pr1)){

    err[i]<- MSE(pr1[,i],y)

  }


  mincvlambda<- which.min(err)  #index of lambda with minimum CV

  threshold<- err[mincvlambda]+std.error(err)  # treshold for 1SE

  lv<- err<threshold     #Comparing err with treshold value

  oneSElambda<- min(which(lv==TRUE))   # index of optimum lambda from 1se rule

  #sf<- as.data.frame(t[,oneSElambda])

  ri<-which (t[,oneSElambda]!=0,arr.ind=T)
  rn<- rownames(t[ri,])

  # Cleaning up selected features
  sf_grl<-c()


  if (length(rn)==0){
    sens_cont<-0
    spec_cont<-0
    gmean_cont<-0
    sens_categ<-0
    spec_categ<-0
    gmean_categ<- 0

  } else {




  for(i in 1:length(rn)) {

    if (ncol(design_matrix)==12 & nchar(rn[i])==3){

      sf_grl[i]<- substr(rn[i],1,2)

    } else if (nchar(rn[i])==4){

      sf_grl[i]<-substr(rn[i],1,3)
    } else (sf_grl[i]<- rn[i])


  }

  sf_grl<-unique(sf_grl)    # Vector containing features selected by group lasso

   #fn<-colnames(design_matrix)
  #  tmt<-tm                  # True model true features
  #  tms<-setdiff(fn,tm)      # True model spurious features

   
   if (dim==12 & split==1){
     contf<- 1:3
     categf<-4:12
     allcont<-c()
          for (i in 1:length(contf)){
       allcont[i]<-paste0('x',contf[i])
       
     }
     allcateg<-c()
     for (i in 1:length(categf)){
       allcateg[i]<-paste0('x',categf[i])
       
     }
   } else if (dim==12 & split==2){
     contf<- 1:6
     categf<-7:12
     allcont<-c()
     for (i in 1:length(contf)){
       allcont[i]<-paste0('x',contf[i])
       
     }
     allcateg<-c()
     for (i in 1:length(categf)){
       allcateg[i]<-paste0('x',categf[i])
       
     }
   } else if (dim==12 & split==3){
     contf<- 1:9
     categf<-10:12
     allcont<-c()
     for (i in 1:length(contf)){
       allcont[i]<-paste0('x',contf[i])
       
     }
     allcateg<-c()
     for (i in 1:length(categf)){
       allcateg[i]<-paste0('x',categf[i])
       
     }
   } else if (dim==60 & split==1){
     contf<- 1:15
     categf<-16:60
     allcont<-c()
     for (i in 1:length(contf)){
       allcont[i]<-paste0('x',contf[i])
       
     }
     allcateg<-c()
     for (i in 1:length(categf)){
       allcateg[i]<-paste0('x',categf[i])
       
     }
   } else if (dim==60 & split==2){
     contf<- 1:30
     categf<-31:60
     allcont<-c()
     for (i in 1:length(contf)){
       allcont[i]<-paste0('x',contf[i])
       
     }
     allcateg<-c()
     for (i in 1:length(categf)){
       allcateg[i]<-paste0('x',categf[i])
       
     }
   } else (print("Wrong"))
     
   
   truecont<- allcont[allcont %in% tm]    #true cont features
   truecateg<- allcateg[allcateg %in% tm] #true categorical features
   tms_cont <- setdiff(allcont,truecont) # True model spurious - cont
   tms_categ <- setdiff(allcateg,truecateg) # True model spurious - categ
   
   
   
    sf_cont<- sf_grl[sf_grl %in% allcont]  # selected cont features by group lasso
    sf_categ<- sf_grl[sf_grl %in% allcateg] # selected categ features by group lasso
   # 
   # sf_true_cont<-  sf_cont[sf_cont %in% truecont] #True selected continuous features by GL
   # sf_true_categ<- sf_categ[sf_categ %in% truecateg] # True selected categ features by GL
   # 
    sf_spurious_cont <- setdiff(allcont,sf_cont) #GL spurious - cont
    sf_spurious_categ<- setdiff(allcateg , sf_categ) #GL spurious - categ
   # 
    acont<- length(intersect(tms_cont,sf_spurious_cont))
   # bcont<- length(intersect(tms_cont,sf_cont))
   # ccont<- length(intersect(truecont,sf_spurious_cont))
    dcont<- length(intersect(truecont,sf_cont))
   
    sens_cont<- dcont/length(truecont)
    spec_cont<- acont/length(tms_cont)
   gmean_cont<-sqrt(sens_cont*spec_cont)
   
    acateg<- length(intersect(tms_categ,sf_spurious_categ))
   # bcateg<- length(intersect(tms_categ,sf_categ))
   # ccateg<- length(intersect(truecateg,sf_spurious_categ))
    dcateg<- length(intersect(truecateg,sf_categ))
   # 
    sens_categ<-dcateg/length(truecateg)
   spec_categ<-acateg/length(tms_categ)
    gmean_categ<-sqrt(sens_categ*spec_categ)
   
   
   
   # pmt_grl<-sf_grl          # Lasso model true features selected
   # pms_grl<-setdiff(fn,sf_grl) # Lasso model spurious features
   # 
   # a<-length(intersect(tms,pms_grl))
   # b<-length(intersect(tms,pmt_grl))
   # c<-length(intersect(tmt,pms_grl))
   # d<-length(intersect(tmt,pmt_grl))
   # 
   # sens<- d/(c+d)
   # spec<- a/(a+b)
   # g_mean<-sqrt(sens*spec)
   
  
   
   
   
   
   }

  #output<-c(testing_error_MARS,testing_error_gbm,sensitivity,specificity,g_mean,time_mars,time_gbm)
  output<-c(testing_error_gbm,sens_cont,spec_cont,gmean_cont,sens_categ,spec_categ,gmean_categ,time_gbm)
  
  return(output)
 
   
} 
  
  
  
  
  
  
  
  
  
# #####Calculating gmean of MARS #####
# sf_mars<-rownames(evimp(mars))
# 
# 
# # Cleaning up to remove indicator in the variable name
# 
# if (length(sf_mars)==0){
# 
#   right_picked_features_MARS<-0
#   
# } else {
# 
# for(i in 1:length(sf_mars)) {
#   
#   if (ncol(design_matrix)==12 & nchar(sf_mars[i])==3){
#     
#     sf_mars[i]<- substr(sf_mars[i],1,2)
#     
#   } else if (nchar(sf_mars[i])==4){
#     
#     sf_mars[i]<-substr(sf_mars[i],1,3)
#   } else (sf_mars[i]<- sf_mars[i])
#   
#   
#  }
#   
# sf_mars<-unique(sf_mars)
# 
# # Calculating % of rightly picked features from MARS modesl (tm is the vector containing indexes of true features) 
# 
# right_picked_features_MARS<- (sum (tm %in% sf_mars)/length(tm))*100
# }  
# 
  

# pmt_mars<-sf_mars
# pms_mars<-setdiff(fn,sf_mars)
# 
# a<-length(intersect(tms,pms_mars))
# b<-length(intersect(tms,pmt_mars))
# c<-length(intersect(tmt,pms_mars))
# d<-length(intersect(tmt,pmt_mars))
# 
# sensitivity_mars<- d/(c+d)
# specificity_mars<- a/(a+b)
# g_mean_mars<-sqrt(sensitivity_mars*specificity_mars)
###################################  




  
  
# ##### Calculating gmean of GBM ##### 
# #Getting variable importance
# i3<-(varImp(gbmfit)$importance)
# 
# #Getting non-zero imp variables
# sf_gbm<-rownames(which(i3!=0,arr.ind = T))
# 
# right_picked_features_gbm<- (sum (tm %in% sf_gbm)/length(tm))*100
# 
# pmt_gbm<-sf_gbm
# pms_gbm<-setdiff(fn,sf_gbm)
# 
# a2<-length(intersect(tms,pms_gbm))
# b2<-length(intersect(tms,pmt_gbm))
# c2<-length(intersect(tmt,pms_gbm))
# d2<-length(intersect(tmt,pmt_gbm))
# 
# sensitivity_gbm<- d2/(c2+d2)
# specificity_gbm<- a2/(a2+b2)
# g_mean_gbm<-sqrt(sensitivity_gbm*specificity_gbm)  
  
  
  
# ##################### Glinternet model for feature selection
# numLevels<-vector()
# for(i in 1:ncol(design_matrix)){
#   
#   if (class(design_matrix[,i])=="numeric"){
#     
#     numLevels[i]<-1
#     
#   } else {numLevels[i]<-nlevels(design_matrix[,i])}
#   
# }
# 
# ######### to store names of cont and categorical columns
# 
# cont_names<-colnames(design_matrix[,sapply(design_matrix, is.numeric)])
# cat_names<-colnames(design_matrix[,sapply(design_matrix, is.factor)])
# 
# 
# ########## Changing class from factors to numeric
# for(i in 1:ncol(design_matrix)){
#   
#   design_matrix[,i]<- as.numeric(as.character(design_matrix[,i]))
#   
# }
# 
# #Fitting glinternet model
# gl<-glinternet(design_matrix,y,numLevels)
# 
# # Getting model for a specific lambda
# k<-coef(gl,lambdaIndex = which.min(gl$objValue))
# #k<-coef(gl,lambdaIndex = 2)
# 
# gl_cat<-unlist(k[[1]][[1]][1])
# gl_cont<-unlist(k[[1]][[1]][2])
# 
# 
# #### Features selected by glinternet
# sf_gl_cont<-cont_names[gl_cont]
# sf_gl_cat<- cat_names[gl_cat]
# 
# sf_gl<- c(sf_gl_cont,sf_gl_cat)
# 
# 
# fn<-colnames(design_matrix)
# tmt<-tm
# tms<-setdiff(fn,tm)
# 
# pmt_gl<-sf_gl
# pms_gl<-setdiff(fn,sf_gl)
# 
# a<-length(intersect(tms,pms_gl))
# b<-length(intersect(tms,pmt_gl))
# c<-length(intersect(tmt,pms_gl))
# d<-length(intersect(tmt,pmt_gl))
# 
# sensitivity<- d/(c+d)
# specificity<- a/(a+b)
# g_mean<-sqrt(sensitivity*specificity)  
#   
#   
#   
#   
#   
#   
  
  
  
  
  
  
  
