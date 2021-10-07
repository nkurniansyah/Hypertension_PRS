

#' Title : Genearte AUC
#'
#' @param pheno : data frame of the phenotype 
#' @param outcome : as.numeric , outcome to test 
#' @param covars_prs : covariates to adjust
#' @param n : boostrap n
#' @param seed : random seed number
#'
#' @return auc
#' @export
#'
#' @examples
#' 
generate_auc<-function(pheno,outcome, covars_prs, seed=NULL,n= 2000){
  
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
  
  
  auc_prs= ci.auc(roc(test_dat[,outcome] ~ test_dat$pred), conf.level=0.95, method=c("bootstrap") ,boot.n = n, boot.stratified = TRUE, reuse.auc=TRUE,
                  progress = getOption("pROCProgress")$name, parallel=FALSE)
  auc_prs_df<- c(auc_prs)
  names(auc_prs_df)<- c("auc_low","auc","auc_hi") 
  
  return(auc_prs_df)
  
}