


#' Title : Split data, and colapse in longitudinal data frame for Trajectories of hypertension risk
#'
#' @param phenotype
#'
#' @return data frame longitudinal for all visit
#' @export
#'
#' @examples
#' 
split_data<-function(pheno){
  
  # column that we need as it is at all time points
  same_cols <- c("sample.id", "prs", "race","sex",paste0("PC_",1:11))
  
  time_varying_cols <- c("age", "sbp", "dbp", "htn_med", "smoke", "bmi", "htn")
  
  
  time_points <- c(1, 2, 5, 7, 10, 15)
  list_by_time <- vector(mode = "list", length = length(time_points))
  
  for (i in 1:length(time_points)){
    cur_dat <- pheno[,c(same_cols, paste0(time_varying_cols, "_v", time_points[i]))]
    colnames(cur_dat) <- c(same_cols, time_varying_cols)
    list_by_time[[i]] <- cur_dat
  }
  
  ldat <- do.call(rbind, list_by_time)
  
  return(ldat)
  
}




#' Title : Compute Confident interval effect
#'
#' @param age_effect: age effect  
#' @param age_se :  age standard error  
#' @param age_sq_effect : age^2 effect
#' @param age_sq_se : age^ standard error
#' @param eff_cov : vcov age and age^2 
#' @param cur_age_17 : age (  real age - 17)
#'
#' @return effect and effect CI
#' @export
#'
#' @examples
compute_eff_CI <- function(age_effect, age_se, age_sq_effect, age_sq_se, eff_cov, cur_age_17){
    req_eff <- age_effect*cur_age_17 + age_sq_effect*(cur_age_17^2)
    cur_eff_se <- sqrt(age_se^2*cur_age_17^2 + 
                         age_sq_se^2*cur_age_17^4 +
                         2*cur_age_17^3*eff_cov)
    req_low <- req_eff - 1.96*cur_eff_se
    req_high <- req_eff + 1.96*cur_eff_se
    
    return(c(effect = req_eff, low = req_low, high = req_high))
    
}
  
  



### only unrelated individuals

clean_pheno<- split_data(phenotype)


# PRS quantile

prs_quantiles <- quantile(clean_pheno$prs, c(0, 0.1, 0.5, 0.9, 1), na.rm = TRUE)

## startatified phenotype based on prs quantile

clean_pheno$prs_q <- cut(clean_pheno$prs, 
                          breaks = prs_quantiles, 
                          include.lowest = TRUE, 
                          labels = c("bottom_10", "q_11_to_50", "q_51_to_90", "q_90_to_100"))
### add age^2
pheno_race$age_sq <- clean_pheno$age^2

  

  
# here we will save the OR of hypertension, relative to the youngest age in the sample (17)

clean_pheno$age_17 <- clean_pheno$age - 17
clean_pheno$age_sq_17 <- clean_pheno$age_17^2
age_seq <- seq(min(clean_pheno$age), max(clean_pheno$age))


res_list <- vector(mode = "list", length= length(unique(clean_pheno$prs_q)))
names(res_list) <- c("bottom_10", "q_11_to_50", "q_51_to_90", "q_90_to_100")
  
for (g in 1:length(res_list)){
  
  mod <- lmer(htn ~ age_17 + age_sq_17 + sex+ PC_1+PC_2+PC_3+PC_4+PC_5+PC_6+PC_7+
                PC_8+PC_9+PC_10+PC_11+ race+ smoke  + (1|sample.id), 
              data = pheno_race[which(pheno_race$prs_q == names(res_list)[g]),])
  
  res <- c()
  
  for (i in age_seq){
    cur_coef <- summary(mod)$coef[c("age_17", "age_sq_17"),]
    cur_res <- compute_eff_CI(age_effect = cur_coef[c("age_17"),"Estimate"], 
                              age_se = cur_coef[c("age_17"),"Std. Error"], 
                              age_sq_effect = cur_coef[c("age_sq_17"),"Estimate"], 
                              age_sq_se = cur_coef[c("age_sq_17"),"Std. Error"], 
                              eff_cov = summary(mod)$vcov["age_17","age_sq_17"], 
                              cur_age_17 = i -17)
    res <- rbind(res, cur_res)
  }
  
  res <- as.data.frame(res)
  res$age <- age_seq
  res$group <- names(res_list)[g]
  res_list[[g]] <- res
  
}

trejetories_age <- do.call(rbind, res_list)
  
head(trejetories_age)
  
  
trejetories_age$OR <- exp(trejetories_age$effect)
trejetories_age$low_OR <- exp(trejetories_age$low)
trejetories_age$high_OR <- exp(trejetories_age$high)
  

trejetories_age_df<- data.frame(trejetories_age)

