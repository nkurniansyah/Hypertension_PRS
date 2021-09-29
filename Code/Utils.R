


#' Title: Standardize PRS
#'
#' @param prs_score : vector of prs score
#' @param mean_prs :mean of the prs
#' @param sd_prs : sd of the prs
#'
#' @return std prs
#' @export
#'
#' @examples
standardize_prs<-function(prs_score,mean_prs, sd_prs){
  
  std_prs <- (prs_score-mean_prs)/sd_prs
  return(std_prs)
}





generate_auc<-function(pheno,outcome, unrels ){
  
  
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







