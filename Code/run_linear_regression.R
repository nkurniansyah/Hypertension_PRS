#' Title Association test using linear regression
#'
#' @param pheno : data frame phenotype
#' @param covars_prs : covariates to adjust
#' @param exposure : exposure
#' @param outcome : outcome to test
#'
#' @return (data frame of association test)
#' @export
#'
#' @examples
run_linear_regression<- function(pheno, covars_prs, exposure, outcome){
  
  
  outcome_type<- all(na.omit(phenotype_clean[,outcome]) %in% 0:1)
  
  
  if(outcome_type==TRUE){
    
    message(paste0(outcome,"is binomial"))
    
    
    assoc<- glm(as.formula(paste(outcome , "~", paste(covars_prs, collapse = "+"))), family = "binomial",data = pheno)
  }else{
    
    message(paste0(outcome,"is gaussian"))
    
    assoc<- (lm(as.formula(paste(outcome, "~", paste(covars_prs, collapse = "+" ))),data=pheno))
    
  }
  
  assoc_summary<- summary(assoc)$coef[exposure,]
  names(assoc_summary)<-c("beta","se","stat","pvalue")
  assoc_summary<-c(exposure=exposure, trait=outcome, assoc_summary)
  
  assoc_summary_df<- data.frame(t(assoc_summary))
  
  return(assoc_summary_df)
}




