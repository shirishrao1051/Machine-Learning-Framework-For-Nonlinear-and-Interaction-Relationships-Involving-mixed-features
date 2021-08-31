# Functions to generate different types of designs 


#Kung design
design_kung <- function(dim,size,split){
  categ<- get_oa(dim,size,split)
  
  if (dim==12 & split==1){
    ncont<-3
    ntwo<- dim-ncont-3
    
  } else if (dim==12 & split==2){
    
    ncont<-6
    ntwo<- dim-ncont-3
  } else if (dim==12 & split==3){
    
    ncont<-9
    ntwo<-0
  } else if (dim==36 & split==1){
    
    ncont<-9
    ntwo<- dim-ncont-3
  } else if (dim==36 & split ==2){
    
    ncont<-18
    ntwo<- dim-ncont-3
  } else if (dim==36 & split==3){
    
    ncont<-27
    ntwo<- dim-ncont-3
  } else if (dim==60 & split ==1){
    
    ncont<-15
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==2){
    
    ncont<-30
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==3){
    
    ncont<-45
    ntwo<- dim-ncont-3
  } else (print("Wrong input"))
  
  start_time <- Sys.time()
  cont<- as.data.frame(sobol(nrow(categ),ncont))
  
  #lhd<-as.data.frame(LHDesign(nrow(categ),2))
  lhd<-as.data.frame(maximinSLHD(1,nrow(categ),2)[[1]])
  contk<- cont[lhd[,1],]
  categk<- categ[lhd[,2],]
  
  kung<- cbind(contk,categk)
  end_time <- Sys.time()
  time<-(end_time)-(start_time)
  op<-list(kung,time)
  return(op)
  
}

# Kung MP design 
design_kungMP<- function(dim,size,split){
  
  categ<-categ<- get_oa(dim,size,split)
  
  if (dim==12 & split==1){
    ncont<-3
    ntwo<- dim-ncont-3
    
  } else if (dim==12 & split==2){
    
    ncont<-6
    ntwo<- dim-ncont-3
  } else if (dim==12 & split==3){
    
    ncont<-9
    ntwo<-0
  } else if (dim==36 & split==1){
    
    ncont<-9
    ntwo<- dim-ncont-3
  } else if (dim==36 & split ==2){
    
    ncont<-18
    ntwo<- dim-ncont-3
  } else if (dim==36 & split==3){
    
    ncont<-27
    ntwo<- dim-ncont-3
  } else if (dim==60 & split ==1){
    
    ncont<-15
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==2){
    
    ncont<-30
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==3){
    
    ncont<-45
    ntwo<- dim-ncont-3
  } else (print("Wrong input"))
  
  start_time <- Sys.time()
  cont<-as.data.frame(MaxProLHD(nrow(categ),ncont)[[1]])
  
  #lhd<-as.data.frame(LHDesign(nrow(categ),2))
  lhd<-as.data.frame(maximinSLHD(1,nrow(categ),2)[[1]])
  kungMP<- cbind(cont[lhd[,1],],categ[lhd[,2],])
  end_time <- Sys.time()
  time<-(end_time)-(start_time)
  op<-list(kungMP,time)
  return(op)
  
}

#Kung SLHD design
design_kungSLHD<- function(dim,size,split){
  
  categ<- get_oa(dim,size,split)
  
  if (dim==12 & split==1){
    ncont<-3
    ntwo<- dim-ncont-3
    
  } else if (dim==12 & split==2){
    
    ncont<-6
    ntwo<- dim-ncont-3
  } else if (dim==12 & split==3){
    
    ncont<-9
    ntwo<-0
  } else if (dim==36 & split==1){
    
    ncont<-9
    ntwo<- dim-ncont-3
  } else if (dim==36 & split ==2){
    
    ncont<-18
    ntwo<- dim-ncont-3
  } else if (dim==36 & split==3){
    
    ncont<-27
    ntwo<- dim-ncont-3
  } else if (dim==60 & split ==1){
    
    ncont<-15
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==2){
    
    ncont<-30
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==3){
    
    ncont<-45
    ntwo<- dim-ncont-3
  } else (print("Wrong input"))
  
  
  start_time <- Sys.time()
  cont<- as.data.frame(maximinSLHD(2,nrow(categ)/2,ncont)[[3]])
  cont<-cont[,-1]
  
  #lhd<-as.data.frame(LHDesign(nrow(categ),2))
  lhd<-as.data.frame(maximinSLHD(1,nrow(categ),2)[[1]])
  kungSLHD<- cbind(cont[lhd[,1],],categ[lhd[,2],])
  end_time <- Sys.time()
  time<-(end_time)-(start_time)
  op<-list(kungSLHD,time)
  return(op)

}

# Martinez design
design_martinez<- function(dim,size,split,nfour=3){
  
  if (dim==12 & split==1){
    ncont<-3
    ntwo<- dim-ncont-3
    
  } else if (dim==12 & split==2){
    
    ncont<-6
    ntwo<- dim-ncont-3
  } else if (dim==12 & split==3){
    
    ncont<-9
    ntwo<-0
  } else if (dim==36 & split==1){
    
    ncont<-9
    ntwo<- dim-ncont-3
  } else if (dim==36 & split ==2){
    
    ncont<-18
    ntwo<- dim-ncont-3
  } else if (dim==36 & split==3){
    
    ncont<-27
    ntwo<- dim-ncont-3
  } else if (dim==60 & split ==1){
    
    ncont<-15
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==2){
    
    ncont<-30
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==3){
    
    ncont<-45
    ntwo<- dim-ncont-3
} else (print("Wrong input"))
    
  start_time <- Sys.time()
  totalcol<-ncont+ntwo+3*nfour
  rcol<-ntwo+3*nfour
  
  oa<- get_oa(dim,size,split)
  cont<- sobol(nrow(oa),totalcol)
  
  cont<-cont[,sample(1:ncol(cont),ncol(cont))]
  martinez_rounding<-rm_modified(cont[,seq(ncol(cont)-rcol+1,ncol(cont))],ntwo,nfour)
  martinez_rounding[,1:ncol(martinez_rounding)]<-lapply(martinez_rounding[,1:ncol(martinez_rounding)],factor)
  
  martinez<-cbind(cont[,1:ncont],martinez_rounding)
  end_time <- Sys.time()
  time<-(end_time)-(start_time)
  op<-list(martinez,time)
  return(op)
}

#Martinez MP design
design_martinezMP<- function(dim,size,split,nfour=3){
  
  if (dim==12 & split==1){
    ncont<-3
    ntwo<- dim-ncont-3
    
  } else if (dim==12 & split==2){
    
    ncont<-6
    ntwo<- dim-ncont-3
  } else if (dim==12 & split==3){
    
    ncont<-9
    ntwo<-0
  } else if (dim==36 & split==1){
    
    ncont<-9
    ntwo<- dim-ncont-3
  } else if (dim==36 & split ==2){
    
    ncont<-18
    ntwo<- dim-ncont-3
  } else if (dim==36 & split==3){
    
    ncont<-27
    ntwo<- dim-ncont-3
  } else if (dim==60 & split ==1){
    
    ncont<-15
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==2){
    
    ncont<-30
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==3){
    
    ncont<-45
    ntwo<- dim-ncont-3
  } else (print("Wrong input"))
  
  start_time <- Sys.time()
  totalcol<-ncont+ntwo+3*nfour
  rcol<-ntwo+3*nfour
  
  oa<- get_oa(dim,size,split)
  
  cont<-as.data.frame(MaxProLHD(nrow(oa),totalcol)[[1]])
  cont<-cont[,sample(1:ncol(cont),ncol(cont))]
  rounding<-rm_modified(cont[,seq(ncol(cont)-rcol+1,ncol(cont))],ntwo,nfour)
  rounding[,1:ncol(rounding)]<-lapply(rounding[,1:ncol(rounding)],factor)
  martinezMP<-cbind(cont[,1:ncont],rounding)
  
  end_time <- Sys.time()
  time<-(end_time)-(start_time)
  op<-list(martinezMP,time)
  return(op)
}
  
#Martinez SLHD
design_martinezSLHD<- function(dim,size,split,nfour=3){
  
  if (dim==12 & split==1){
    ncont<-3
    ntwo<- dim-ncont-3
    
  } else if (dim==12 & split==2){
    
    ncont<-6
    ntwo<- dim-ncont-3
  } else if (dim==12 & split==3){
    
    ncont<-9
    ntwo<-0
  } else if (dim==36 & split==1){
    
    ncont<-9
    ntwo<- dim-ncont-3
  } else if (dim==36 & split ==2){
    
    ncont<-18
    ntwo<- dim-ncont-3
  } else if (dim==36 & split==3){
    
    ncont<-27
    ntwo<- dim-ncont-3
  } else if (dim==60 & split ==1){
    
    ncont<-15
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==2){
    
    ncont<-30
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==3){
    
    ncont<-45
    ntwo<- dim-ncont-3
  } else (print("Wrong input"))
  
  start_time <- Sys.time()
  totalcol<-ncont+ntwo+3*nfour
  rcol<-ntwo+3*nfour
  
  oa<- get_oa(dim,size,split)
  
  cont<-as.data.frame(maximinSLHD(2,nrow(oa)/2,totalcol)[[3]])
  cont<-cont[,-1]
  
  cont<-cont[,sample(1:ncol(cont),ncol(cont))]
  
  rounding<-rm_modified(cont[,seq(ncol(cont)-rcol+1,ncol(cont))],ntwo,nfour)  
  rounding[,1:ncol(rounding)]<-lapply(rounding[,1:ncol(rounding)],factor)
  martinezSLHD<-cbind(cont[,1:ncont],rounding)

  end_time <- Sys.time()
  time<-(end_time)-(start_time)
  op<-list(martinezSLHD,time)
  return(op)
  
  
  
}

#MaxproQQ
design_mpqq<- function(dim,size,split,nfour=3){
  categ<- get_oa(dim,size,split)
  
  if (dim==12 & split==1){
    ncont<-3
    ntwo<- dim-ncont-3
    
  } else if (dim==12 & split==2){
    
    ncont<-6
    ntwo<- dim-ncont-3
  } else if (dim==12 & split==3){
    
    ncont<-9
    ntwo<-0
  } else if (dim==36 & split==1){
    
    ncont<-9
    ntwo<- dim-ncont-3
  } else if (dim==36 & split ==2){
    
    ncont<-18
    ntwo<- dim-ncont-3
  } else if (dim==36 & split==3){
    
    ncont<-27
    ntwo<- dim-ncont-3
  } else if (dim==60 & split ==1){
    
    ncont<-15
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==2){
    
    ncont<-30
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==3){
    
    ncont<-45
    ntwo<- dim-ncont-3
  } else (print("Wrong input"))
  
  start_time <- Sys.time()
  cont<-LHD(nrow(categ),ncont)[[2]]
  cmb<- cbind(cont,categ)
  
  ncat<-ntwo+nfour
  
  mpd<-as.data.frame(MaxProQQ(cmb,p_nom = ncat)[[1]])
  mpd[,(ncont+1):ncol(mpd)]<-lapply(mpd[,(ncont+1):ncol(mpd)],factor)
  
  end_time <- Sys.time()
  time<-(end_time)-(start_time)
  op<-list(mpd,time)
  return(op)
  

  }
  
  
#To read multiple excel files to a list 
library(rio)
data_list<-import_list("12small_1.xlsx",which = c(1:10))

#Coc
design_coc<-function(dim,size,split){
  
  if (dim==12 & size==1 & split==1){
    
    dm<-import_list("12small_1.xlsx",which = c(1:10))
    dm<-lapply(dm, "[", -grep(c("Y"), names(dm[[1]])))
    
    for(i in 1:length(dm)){
      
      dm[[i]][,4:12]<- lapply(dm[[i]][,4:12],factor)
      
      
    }
    
  } else if (dim==12 & size==1 & split==2){
    
    dm<-import_list("12small_2.xlsx",which = c(1:10))
    dm<-lapply(dm, "[", -grep(c("Y"), names(dm[[1]])))
    
    for(i in 1:length(dm)){
      
      dm[[i]][,7:12]<- lapply(dm[[i]][,7:12],factor)
      
      
    }
  } else if (dim==12 & size==1 & split==3){
    
    dm<-import_list("12small_3.xlsx",which = c(1:10))
    dm<-lapply(dm, "[", -grep(c("Y"), names(dm[[1]])))
    
    for(i in 1:length(dm)){
      
      dm[[i]][,10:12]<- lapply(dm[[i]][,10:12],factor)
      
      
    }
  } else if (dim==12 & size==2 & split==1){
    
    dm<-import_list("12big_1.xlsx",which = c(1:10))
    dm<-lapply(dm, "[", -grep(c("Y"), names(dm[[1]])))
    
    for(i in 1:length(dm)){
      
      dm[[i]][,4:12]<- lapply(dm[[i]][,4:12],factor)
      
      
    }
    
  } else if (dim==12 & size==2 & split==2){
    
    dm<-import_list("12big_2.xlsx",which = c(1:10))
    dm<-lapply(dm, "[", -grep(c("Y"), names(dm[[1]])))
    
    for(i in 1:length(dm)){
      
      dm[[i]][,7:12]<- lapply(dm[[i]][,7:12],factor)
      
      
    }
  } else if(dim==12 & size==2 & split==3) { 
    
    dm<-import_list("12big_3.xlsx",which = c(1:10))
    
   
  
  
  dm<-lapply(dm, "[", -grep(c("Y"), names(dm[[1]])))
 
  
  for(i in 1:length(dm)){
    
    dm[[i]][,10:12]<- lapply(dm[[i]][,10:12],factor)
    
    
  } 
  
  ############## Dim 60 ##################################
  
  } else if (dim==60 & size==1 & split==1){
    
    dm<-import_list("60small_1.xlsx",which = c(1:10))
    dm<-lapply(dm, "[", -grep(c("Y"), names(dm[[1]])))
    
    for(i in 1:length(dm)){
      
      dm[[i]][,16:60]<- lapply(dm[[i]][,16:60],factor)
      
      
    }
    
  } else if (dim==60 & size==1 & split==2){
    
    dm<-import_list("60small_2.xlsx",which = c(1:10))
    dm<-lapply(dm, "[", -grep(c("Y"), names(dm[[1]])))
    
    for(i in 1:length(dm)){
      
      dm[[i]][,31:60]<- lapply(dm[[i]][,31:60],factor)
      
      
    }
  } else if (dim==60 & size==1 & split==3){
    
    dm<-import_list("60small_3.xlsx",which = c(1:10))
    dm<-lapply(dm, "[", -grep(c("Y"), names(dm[[1]])))
    
    for(i in 1:length(dm)){
      
      dm[[i]][,10:12]<- lapply(dm[[i]][,10:12],factor)
      
      
    }
  } else if (dim==60 & size==2 & split==1){
    
    dm<-import_list("60big_1.xlsx",which = c(1:10))
    dm<-lapply(dm, "[", -grep(c("Y"), names(dm[[1]])))
    
    for(i in 1:length(dm)){
      
      dm[[i]][,16:60]<- lapply(dm[[i]][,16:60],factor)
      
      
    }
    
  } else if (dim==60 & size==2 & split==2){
    
    dm<-import_list("60big_2.xlsx",which = c(1:10))
    dm<-lapply(dm, "[", -grep(c("Y"), names(dm[[1]])))
    
    for(i in 1:length(dm)){
      
      dm[[i]][,31:60]<- lapply(dm[[i]][,31:60],factor)
      
      
    }
  } else if(dim==60 & size==2 & split==3) { 
    
    dm<-import_list("60big_3.xlsx",which = c(1:10))
    
    
    
    
    dm<-lapply(dm, "[", -grep(c("Y"), names(dm[[1]])))
    
    
    for(i in 1:length(dm)){
      
      dm[[i]][,10:12]<- lapply(dm[[i]][,10:12],factor)
      
      
    } 
    
    ############## Dim 60 ##################################
    
  } else (print("Wrong input parameters"))
  
  
  return(dm)
}  
  



#Function to design MC with uniform distribution

design_mc_unif<-function(dim,size,split){
  categ<- get_oa(dim,size,split)
  
  if (dim==12 & split==1){
    ncont<-3
    ntwo<- dim-ncont-3
    
  } else if (dim==12 & split==2){
    
    ncont<-6
    ntwo<- dim-ncont-3
  } else if (dim==12 & split==3){
    
    ncont<-9
    ntwo<-0
  } else if (dim==36 & split==1){
    
    ncont<-9
    ntwo<- dim-ncont-3
  } else if (dim==36 & split ==2){
    
    ncont<-18
    ntwo<- dim-ncont-3
  } else if (dim==36 & split==3){
    
    ncont<-27
    ntwo<- dim-ncont-3
  } else if (dim==60 & split ==1){
    
    ncont<-15
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==2){
    
    ncont<-30
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==3){
    
    ncont<-45
    ntwo<- dim-ncont-3
  } else (print("Wrong input"))
  
  
  nfour<-3
  start_time <- Sys.time()
  rt<-runif(nrow(categ)*ncont)
  
  cont = matrix(ncol = ncont,nrow = nrow(categ))
  for(col in 1:ncont) {
    
    for(i in 1:nrow(categ)){
      cont[i,col] = sample(rt,1)
    }
  }
  
  if (dim==12 & split==3){
    cat_levels4 <- c(1,2,3,4)
    
    sims4 <- sample(cat_levels4, size = nrow(categ), replace = TRUE)
    
    xcat4<-matrix(ncol=nfour,nrow=nrow(categ))
    
    for(col in 1:nfour) {
      
      for(i in 1:nrow(categ)){
        xcat4[i,col] = sample(sims4,1)
      }
    }
    
    fd<-as.data.frame(cbind(cont,xcat4))
    
  } else { cat_levels2<- c(1,2)
  
  sims2<- sample(cat_levels2, size = nrow(categ), replace = TRUE)
  
  xcat2<-matrix(ncol=ntwo,nrow=nrow(categ))
  
  for(col in 1:ntwo) {
    
    for(i in 1:nrow(categ)){
      xcat2[i,col] = sample(sims2,1)
    }
  }
  
  
  cat_levels4 <- c(1,2,3,4)
  
  sims4 <- sample(cat_levels4, size = nrow(categ), replace = TRUE)
  
  xcat4<-matrix(ncol=nfour,nrow=nrow(categ))
  
  for(col in 1:nfour) {
    
    for(i in 1:nrow(categ)){
      xcat4[i,col] = sample(sims4,1)
    }
  }
  
  fd<-as.data.frame(cbind(cont,xcat2,xcat4))
  }
  
  fd[,(ncont+1):ncol(fd)]<-lapply(fd[,(ncont+1):ncol(fd)],factor)
  
  end_time <- Sys.time()
  time<-(end_time)-(start_time)
  
  op<-list(fd,time)
  
  
  
  return(op)
  
}


#Function to design MC with uniform distribution

design_mc_beta<-function(dim,size,split){
  categ<- get_oa(dim,size,split)
  
  if (dim==12 & split==1){
    ncont<-3
    ntwo<- dim-ncont-3
    
  } else if (dim==12 & split==2){
    
    ncont<-6
    ntwo<- dim-ncont-3
  } else if (dim==12 & split==3){
    
    ncont<-9
    ntwo<-0
  } else if (dim==36 & split==1){
    
    ncont<-9
    ntwo<- dim-ncont-3
  } else if (dim==36 & split ==2){
    
    ncont<-18
    ntwo<- dim-ncont-3
  } else if (dim==36 & split==3){
    
    ncont<-27
    ntwo<- dim-ncont-3
  } else if (dim==60 & split ==1){
    
    ncont<-15
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==2){
    
    ncont<-30
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==3){
    
    ncont<-45
    ntwo<- dim-ncont-3
  } else (print("Wrong input"))
  
  
  nfour<-3
  start_time <- Sys.time()
  rt<-rbeta(nrow(categ)*ncont,1,3)
  
  cont = matrix(ncol = ncont,nrow = nrow(categ))
  for(col in 1:ncont) {
    
    for(i in 1:nrow(categ)){
      cont[i,col] = sample(rt,1)
    }
  }
  
  if (dim==12 & split==3){
    cat_levels4 <- c(1,2,3,4)
    
    sims4 <- sample(cat_levels4, size = nrow(categ), replace = TRUE)
    
    xcat4<-matrix(ncol=nfour,nrow=nrow(categ))
    
    for(col in 1:nfour) {
      
      for(i in 1:nrow(categ)){
        xcat4[i,col] = sample(sims4,1)
      }
    }
    
    fd<-as.data.frame(cbind(cont,xcat4))
    
  } else { cat_levels2<- c(1,2)
  
  sims2<- sample(cat_levels2, size = nrow(categ), replace = TRUE)
  
  xcat2<-matrix(ncol=ntwo,nrow=nrow(categ))
  
  for(col in 1:ntwo) {
    
    for(i in 1:nrow(categ)){
      xcat2[i,col] = sample(sims2,1)
    }
  }
  
  
  cat_levels4 <- c(1,2,3,4)
  
  sims4 <- sample(cat_levels4, size = nrow(categ), replace = TRUE)
  
  xcat4<-matrix(ncol=nfour,nrow=nrow(categ))
  
  for(col in 1:nfour) {
    
    for(i in 1:nrow(categ)){
      xcat4[i,col] = sample(sims4,1)
    }
  }
  
  fd<-as.data.frame(cbind(cont,xcat2,xcat4))
  }
  
  fd[,(ncont+1):ncol(fd)]<-lapply(fd[,(ncont+1):ncol(fd)],factor)
  
  end_time <- Sys.time()
  time<-(end_time)-(start_time)
  
  op<-list(fd,time)
  
  
  
  return(op)
  
}

  

############# Design Halton


design_halton<- function(dim,size,split,nfour=3){
  
  if (dim==12 & split==1){
    ncont<-3
    ntwo<- dim-ncont-3
    
  } else if (dim==12 & split==2){
    
    ncont<-6
    ntwo<- dim-ncont-3
  } else if (dim==12 & split==3){
    
    ncont<-9
    ntwo<-0
  } else if (dim==36 & split==1){
    
    ncont<-9
    ntwo<- dim-ncont-3
  } else if (dim==36 & split ==2){
    
    ncont<-18
    ntwo<- dim-ncont-3
  } else if (dim==36 & split==3){
    
    ncont<-27
    ntwo<- dim-ncont-3
  } else if (dim==60 & split ==1){
    
    ncont<-15
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==2){
    
    ncont<-30
    ntwo<- dim-ncont-3
  } else if (dim==60 & split==3){
    
    ncont<-45
    ntwo<- dim-ncont-3
  } else (print("Wrong input"))
  
  start_time <- Sys.time()
  totalcol<-ncont+ntwo+3*nfour
  rcol<-ntwo+3*nfour
  
  oa<- get_oa(dim,size,split)
  cont<- halton(nrow(oa),totalcol)
  
  cont<-cont[,sample(1:ncol(cont),ncol(cont))]
  martinez_rounding<-rm_modified(cont[,seq(ncol(cont)-rcol+1,ncol(cont))],ntwo,nfour)
  martinez_rounding[,1:ncol(martinez_rounding)]<-lapply(martinez_rounding[,1:ncol(martinez_rounding)],factor)
  
  martinez<-cbind(cont[,1:ncont],martinez_rounding)
  end_time <- Sys.time()
  time<-(end_time)-(start_time)
  op<-list(martinez,time)
  return(op)
}




library(LHD)
t<-FastMmLHD(40,8,method = "euclidean")

t2<-apply(t, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
  
  
id<-MaxProLHD(40,8)[[1]]

fd<-MaxPro(id)[[1]]

  
