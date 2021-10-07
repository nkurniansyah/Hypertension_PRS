

#' Title : Split phenotype into 5 indipedent data set
#'
#' @param unrels_pheno  : data frame of the phenotype (only unrelated people)
#' @param covars_prs : Covariates to indclude in the analysis
#' @param outcome : outcome to test
#' @param seed : random seed number
#'
#' @return 5 list independent data frame of phenotype
#' @export
#'
#' @examples
split_phenotype<-function(unrels_pheno, covars_prs, outcome, seed=NULL ){
  if (!is.null(seed)) set.seed(888)
  
  phenotype<-unrels_pheno[,c("sample.id",covars_prs,outcome)]
  
  phenotype<-na.omit(phenotype)
  
  
  random_sample <- sample(1:5,size=nrow(phenotype),replace=TRUE,prob=c(0.2,0.2,0.2,0.2,0.2))
  out<-list()
  for(i in 1:5){
    split_pheno <- phenotype[random_sample==i,]
    
    out[[i]]<- split_pheno
  }
  
  
  return(out)
  
}





#' Title: Calculate CV
#'
#' @param list_unrel_pheno : 5 list of phenotype, only unrelated people
#' @param covars_prs : covars to adjust
#' @param exposure : exposure
#' @param outcome : outcome to test
#'
#' @return numeric cv
#' @export
#'
#' @examples
#' 
calculate_cv_effect<-function(list_unrel_pheno, covars_prs, exposure, outcome ){
  
  stopifnot(length(list_unrel_pheno)==5)
  out<-list()
  for(i in 1:5){
    
    pheno_cv<-list_unrel_pheno[[i]]
    
    assco_for_cv<-run_linear_regression(phenopheno_cv, covars_prs=covars_prs, exposure=exposure, outcome=outcome)
    out[[i]]<- assco_for_cv
    
  }
  
  
  combine_assoc<- do.call(rbind,out)
  
  sd_effect<- sd(combine_assoc$beta)
  mean_effect<- mean(combine_assoc$beta)
  
  cv_effect<- sd_effect/mean_effect
  
  return(cv_effect)
}


