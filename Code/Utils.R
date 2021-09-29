#' Title : Genearte AUC
#'
#' @param pheno : data frame of the phenotype 
#' @param outcome : as.numeric , outcome to test 
#' @param covars_prs : covariates to adjust
#' @param seed : random seed number
#'
#' @return auc
#' @export
#'
#' @examples
generate_auc<-function(pheno,outcome, covars_prs, seed=NULL ){
  
  if(!is.null(seed)){
    set.seed(888)
    
  }else{
    set.seed(seed)
  }
  
  
  sample <- sample(nrow(pheno),floor(nrow(pheno)*0.8))
  
  train <- pheno[sample,]
  test <- pheno[-sample,]
  

  model_fit<- glm(as.formula(paste(outcome , "~ ", paste(covars_prs, collapse = "+"))), family = "binomial",data = train)
  
  pred_mod <- predict(model_fit, test, type="response")
  
  pred_mod<-data.frame(pred=pred_mod)
 
  test_dat<- cbind(test,pred_mod)
  

  auc_prs= ci.auc(roc(test_dat[,outcome] ~ test_dat$pred), conf.level=0.95, method=c("bootstrap") ,boot.n = 2000, boot.stratified = TRUE, reuse.auc=TRUE,
                 progress = getOption("pROCProgress")$name, parallel=FALSE)
  auc_prs_df<- c(auc_prs)
  names(auc_prs_df)<- c("auc_low","auc","auc_hi") 
  
  return(auc_prs_df)
  
}

#' Run association test
#'
#' @param pheno : data frame of the phenotype 
#' @param outcome : as.numeric , outcome to test 
#' @param covars_prs : covariates to adjust
#' @param group.var : 
#' @param covmat: specifies the covariance structures for the random effects included in the model 
#'
#' @return association test results in data frame
#' @export
#'
#' @examples
#' 
run_assoc_mixmodel<-function(pheno, outcome, covars_prs, covmat=NULL,group.var=NULL){
  
  
  if(all(pheno[,outcome] %in% c(0,1,NA))){
    family<-"binomial"
  }else{
    family="gaussian"
  }
  
  nullmod <- fitNullModel(pheno,
                          outcome = outcome,
                          covars = covars_prs,
                          family = family,
                          cov.mat=covmat,
                          group.var = group.var,
                          verbose=TRUE)
  
  
  
  
  nullmod_other_fixef<- nullmod$fixef
  nullmod_other_fixef<-nullmod_other_fixef[grepl("PRS",rownames(nullmod_other_fixef)),]
  sample_size<-length(nullmod$outcome)
  assoc_df<- data.frame(nullmod_other_fixef, sample_size)
  colnames(assoc_df)<- c("prs_effect","prs_se","prs_stat","pval_assoc_with_outcome","sample_size")
  assoc_df<- assoc_df %>% rownames_to_column(var="exposure")
  
  return(assoc_df)
  
}







