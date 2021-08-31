nl1_modified_ver2<- function(dim,split,imp,resptype){
  
  
  if (dim==12 & split==1){
    ncont<-3
    
    
  } else if (dim==12 & split==2){
    
    ncont<-6
    
  } else if (dim==12 & split==3){
    
    ncont<-9
    
  } else if (dim==36 & split==1){
    
    ncont<-9
    
  } else if (dim==36 & split ==2){
    
    ncont<-18
    
  } else if (dim==36 & split==3){
    
    ncont<-27
    
  } else if (dim==60 & split ==1){
    
    ncont<-15
    
  } else if (dim==60 & split==2){
    
    ncont<-30
    
  } else if (dim==60 & split==3){
    
    ncont<-45
    
  } else (print("Wrong input"))
  
  
  ncat<-dim-ncont
  
  spl<-dim*imp
  
  
  #impvar<-sample(1:dim,spl)
  if (split==3 & imp==0.75){
    impvar_cont<-sample(1:ncont,round(0.75*spl))
    impvar_categ<-sample((ncont+1):dim,spl-round(0.75*spl))
    
    
  } else if (split==1 & imp==0.75) {impvar_cont<-sample(1:ncont,spl-round(0.75*spl))
  impvar_categ<-sample((ncont+1):dim,round(0.75*spl))
  
  
  
  } else if(split==2 & imp==0.75 & dim==60){impvar_cont<- sample(1:ncont,20)
  
  impvar_categ<- sample((ncont+1):dim,25)
  
  } else if(split==2 & imp==0.75 & dim==12){impvar_cont<- sample(1:ncont,4)
  
  impvar_categ<- sample((ncont+1):dim,5)
  
  } else {impvar_cont<-sample(1:ncont,round(spl/3))
  impvar_categ<-sample((ncont+1):dim,spl-round(spl/3))
  }
  
  ###### Continuous terms
  terms_cont<-c()
  for (i in 1:length(impvar_cont)){
    terms_cont[i]<-paste0('x',impvar_cont[i])
    
  }
  
  
  #Generating polynomial terms
  poly_terms_cont2<-c()
  for(i in 1:length(terms_cont)){
    poly_terms_cont2[i]<-paste0(terms_cont[i],'^2')
    
  }
  
  poly_terms_cont3<-c()
  for(i in 1:length(terms_cont)){
    poly_terms_cont3[i]<-paste0(terms_cont[i],'^3')
    
  }
  
  poly_terms_cont4<-c()
  for(i in 1:length(terms_cont)){
    poly_terms_cont4[i]<-paste0(terms_cont[i],'^4')
    
  }
  
  
  # #Generating sin terms
  # sin_terms<-c()
  # for(i in 1:length(terms_cont)){
  #   sin_terms[i]<-paste0('sin(',terms_cont[i])
  #   
  # }
  
  #Closing paranthesis
  # for (i in 1:length(sin_terms)){
  #   sin_terms[i]<- paste0(sin_terms[i],')')
  # }
  
  
  #Generating interaction terms
  
  int_terms_cont<-unlist(lapply(1, function(i) combn(terms_cont, 2, paste, collapse = "*")))
  
  
  #All additive cont terms
  cont_add<-list(terms_cont,poly_terms_cont2,poly_terms_cont3,poly_terms_cont4)
  cont_add_df<-as.data.frame(do.call(rbind,cont_add))
  
  #All terms cont (as chr vector)
  #cont_all<-c(terms_cont,poly_terms_cont2,poly_terms_cont3)
  
  
  ######  Categorical terms
  # terms_categ<-c()
  # for (i in 1:length(impvar_categ)){
  #   terms_categ[i]<-paste0('x',impvar_categ[i])
  #   
  # }
  
  fourlevel<- impvar_categ > (dim-3)
  twolevel<- impvar_categ < (dim-2)
  
  fourlevel_cat<- impvar_categ[fourlevel]
  twolevel_cat<- impvar_categ[twolevel]
  
  
  # Two-level categorical variables
  if (length(twolevel_cat)==0){
    terms_categ_2l<-NULL
    
  } else {
    terms_categ_2l<-c()
    for (i in 1:length(twolevel_cat)){
      terms_categ_2l[i]<-paste0('x',twolevel_cat[i])
      
    }
  }
  # Four-level categorical variables
  
  if (length(fourlevel_cat)==0){
    
    terms_categ_4l<-NULL
    
  } else{
    
    terms_categ_4l<-c()
    for (i in 1:length(fourlevel_cat)){
      terms_categ_4l[i]<-paste0('x',fourlevel_cat[i])
      
    }
  }
  
  if(length(terms_categ_2l)==0){
    
    ol1<-NULL
    ol2<-NULL
    
    
  } else { ol1<-c()
  
  
  for( i in 1:length(terms_categ_2l)){
    
    
    ol1[i]<-paste0(terms_categ_2l[i],'==',0)
    
  }
  
  ol2<-c()
  for( i in 1:length(terms_categ_2l)){
    
    
    ol2[i]<-paste0(terms_categ_2l[i],'==',1)
    
  }
  }
  
  
  if (length(terms_categ_4l)==0){
    ol41<-NULL
    ol42<-NULL
    ol43<-NULL
    ol44<-NULL
    
    
  } else {
    
    ol41<-c()
    
    
    for( i in 1:length(terms_categ_4l)){
      
      
      ol41[i]<-paste0(terms_categ_4l[i],'==',0)
      
    }
    
    ol42<-c()
    for( i in 1:length(terms_categ_4l)){
      
      
      ol42[i]<-paste0(terms_categ_4l[i],'==',1)
      
    }
    
    ol43<-c()
    
    
    for( i in 1:length(terms_categ_4l)){
      
      
      ol43[i]<-paste0(terms_categ_4l[i],'==',2)
      
    }
    
    ol44<-c()
    for( i in 1:length(terms_categ_4l)){
      
      
      ol44[i]<-paste0(terms_categ_4l[i],'==',3)
      
    }
  }
  
  
  
  categ_add<- c(ol1,ol2,ol41,ol42,ol43,ol44)
  
  #Adding paranthesis
  for (i in 1:length(categ_add)){
    categ_add[i]<- paste0('(',categ_add[i],')')
  }
  
  
  
  #All additive categorical terms
  #categ_add<-list(ol1,ol2,ol41,ol42,ol43,ol44)
  #categ_add_df<-as.data.frame(do.call(rbind,categ_add))
  
  
  
  
  categ_add_2l<-list(ol1,ol2)
  categ_add_2l_df<-as.data.frame(do.call(rbind,categ_add_2l))
  
  categ_add_4l<-list(ol41,ol42,ol43,ol44)
  categ_add_4l_df<-as.data.frame(do.call(rbind,categ_add_4l))
  
  
  #Generating interaction terms
  # int_terms_categ<-unlist(lapply(1, function(i) combn(categ_add, 2, paste, collapse = "*")))
  
  impvar_terms<-c(terms_cont,terms_categ_2l,terms_categ_4l)
  
  ###### Generating equations
  if (resptype==1){    # Additive
    
    
    sterms<-c()
    
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1)
      
    }
    
    coef_cont<-round(runif(length(sterms),1,3),2)
    coef_cat<-round(runif(length(categ_add),1,3),2)
    
    for (i in 1:length(sterms)){
      sterms[i]<- paste0('(',sterms[i],')')
    }
    
    t_cont<-paste(coef_cont,sterms,sep='*')
    t_categ<-paste(coef_cat,categ_add,sep='*')
    
    resp1<- c(t_cont,t_categ)
    
    func<- paste(resp1,collapse = '+')
    
  }  else if (resptype==2){        # Interactions (cont-cont)
    
    sterms<-c()
    
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1)
      
    }
    
    for (i in 1:length(sterms)){
      sterms[i]<- paste0('(',sterms[i],')')
    }  
    
    
    int_terms_cont<-unlist(lapply(1, function(i) combn(sterms[1:4], 2, paste, collapse = "*")))
    
    int_terms_resp2<-int_terms_cont
    
    # if (length(int_terms_cont)< 3){
    #   
    #   int_terms_resp2<- sample(int_terms_cont)
    #   
    # } else {
    #   
    #   int_terms_resp2<- sample(int_terms_cont,3)                                              #Generating interaction terms
    # }
    
    
    coef_int<- round(runif(length(int_terms_resp2),5,10),2)                                   # Co-ef for interaction terms
    coef_cat<-round(runif(length(categ_add),10,15),2)                                        # Co-ef for categorical terms
    coef_cont<- round(runif(length(sterms),10,15),2) 
    
    t_categ<-paste(coef_cat,categ_add,sep='*')
    t_int<- paste(coef_int,int_terms_resp2,sep='*')
    t_cont<- paste(coef_cont,sterms,sep='*')
    
    resp1<- c(t_int,t_categ,t_cont)
    
    func<- paste(resp1,collapse = '+')
    
    int_variables_list<- lapply(int_terms_resp2,new_id_int)
    
    
  } else if (resptype==3){                                                                              # Interactions(cont-categ)
    
    
    sterms<-c()
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1) #sampling cont additive terms
      
    }
    
    if(length(categ_add_2l_df)==0){
      
      sterms2<-NULL
    } else {
      sterms2<-c()
      for (i in 1:ncol(categ_add_2l_df)){
        
        sterms2[i]<-sample(categ_add_2l_df[,i],1) #sampling 2 level categ terms
        
      }
    }
    
    if (length(categ_add_4l_df)==0){
      
      sterms3<-NULL
    } else {
      sterms3<-c()
      for (i in 1:ncol(categ_add_4l_df)){
        
        sterms3[i]<-sample(categ_add_4l_df[,i],1) #sampling 4 level categ terms
        
      }
    }
    
    
    if(length(sterms2)==0){        # Adding parenthesis 
      
      sterms2<- NULL
      
    } else {
      
      for (i in 1:length(sterms2)){
        sterms2[i]<- paste0('(',sterms2[i],')')
      }
    }  
    
    
    
    if(length(sterms3)==0){    # Adding paranthesis
      
      sterms3<- NULL
      
    } else {
      
      for (i in 1:length(sterms3)){
        sterms3[i]<- paste0('(',sterms3[i],')')
      }
    }  
    
    
    #ct<-c(sterms2,sterms3)
    
    ct<-categ_add
    
    # if (length(sterms)<length(ct)){
    #   
    #   ct2<- ct[1:length(sterms)]
    #   
    #   
    # } else {
    #   
    #   ct2<- ct
    #   
    # }
    # 
    
    tr<- c()    # Cont-categ interaction terms
    
    
    for (i in 1:4){
      
      tr[i]<- paste(sterms[i],ct[i],sep='*')
      
    } 
    
    
    
    # ct3<- ct[-(1:length(sterms))]
    # 
    # 
    # 
    # 
    # 
    # ca<-c()
    # for (i in 1:ncol(cont_add_df)){
    #   
    #   ca[i]<-sample(cont_add_df[,i],1) #sampling cont additive terms
    #   
    # }
    
    
    #terms_rem<-c(sterms[4:length(sterms)],ct[4:length(ct)])
    
    terms_rem_cont<-sterms[1:length(sterms)]
    terms_rem_categ<-ct[1:length(ct)]
    
    
    
    coef1<- round(runif(length(terms_rem_categ),10,15),2)
    coef2<- round(runif(length(tr),5,10),2)
    coef3<- round(runif(length(terms_rem_cont),10,15),2)
    
    # for (i in 1:length(resp1)){
    #   resp1[i]<- paste0('(',resp1[i],')')
    # }
    
    
    # for (i in 1:length(terms_rem_categ)){
    #   terms_rem_categ[i]<- paste0('(',terms_rem_categ[i],')')
    # }
    
    
    for (i in 1:length(tr)){
      tr[i]<- paste0('(',tr[i],')')
    }
    
    
    fm1<- paste(coef1,terms_rem_categ,sep='*')
    fm2<- paste(coef2,tr,sep='*')
    fm3<- paste(coef3,terms_rem_cont,sep='*')
    
    fm<-c(fm1,fm2,fm3)
    func<- paste(fm,collapse = '+')
    
    int_variables_list<- lapply(tr,new_id_int)
    
    
    
  } else if (resptype==4){
    
    sterms<-c()
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1) #sampling cont additive terms
      
    }
    
    
    if(length(categ_add_2l_df)==0){
      
      sterms2<-NULL
    } else {
      
      
      sterms2<-c()
      for (i in 1:ncol(categ_add_2l_df)){
        
        sterms2[i]<-sample(categ_add_2l_df[,i],1) #sampling 2 level categ terms
        
      }
    }  
    
    if(length(categ_add_4l_df)==0){
      
      sterms3<-NULL
    } else {
      
      sterms3<-c()
      for (i in 1:ncol(categ_add_4l_df)){
        
        sterms3[i]<-sample(categ_add_4l_df[,i],1) #sampling 4 level categ terms
        
      }
    }  
    
    
    
    
    for (i in 1:length(sterms)){
      sterms[i]<- paste0('(',sterms[i],')')
    }
    
    
    if(length(sterms2)==0){        # Adding parenthesis 
      
      sterms2<- NULL
      
    } else {
      
      for (i in 1:length(sterms2)){
        sterms2[i]<- paste0('(',sterms2[i],')')
      }
    }  
    
    
    
    if(length(sterms3)==0){    # Adding paranthesis
      
      sterms3<- NULL
      
    } else {
      
      for (i in 1:length(sterms3)){
        sterms3[i]<- paste0('(',sterms3[i],')')
      }
    }  
    
    
    
    
    
    ct<- c(sterms2,sterms3)
    
    int_terms_categ<-unlist(lapply(1, function(i) combn(ct[1:3], 2, paste, collapse = "*")))   # Interaction terms
    
    
    # if (length(int_terms_categ)< 3){
    #   
    #   int_terms_resp2<- sample(int_terms_categ)
    #   
    # } else {
    #   
    #   int_terms_resp2<- sample(int_terms_categ,3)                                              #Generating interaction terms
    # }
    # 
    
    int_terms_resp2<-int_terms_categ
    
    
    
    
    
    rem_terms_categ<- ct[1:length(ct)]
    rem_terms_categ<- categ_add
    
    
    coef1<-round(runif(length(rem_terms_categ),3,5),2)
    coef2<- round(runif(length(int_terms_resp2),3,5),2)
    coef3<- round(runif(length(sterms),10,15),2)
    
    fm1<- paste(coef1,rem_terms_categ,sep='*')
    fm2<- paste(coef2,int_terms_resp2,sep='*')
    fm3<- paste(coef3,sterms,sep='*')
    
    fm<-c(fm1,fm2,fm3)
    func<- paste(fm,collapse = '+')
    
    int_variables_list<- lapply(int_terms_resp2,new_id_int)
  } else (print("Wrong resp type"))
  
  op<- list(func,impvar_terms,int_variables_list)
  
  return(op)
  
}




