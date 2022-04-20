## Introduction

This repository provides information regarding the construction of a
polygenic risk score (PRS) for hypertension (HTN) that we developed
(HTN-PRS) in manuscript A multi-ethnic polygenic risk score is associated with hypertension prevalence and progression throughout adulthood (link to be added).

First, it provides instructions for constructing the HTN-PRS based on
summary statistics from GWAS. We provide the relevant summary statistics
(see folder “Summary\_Statistics\_for\_PRS\_construction”), as well as
code for using them to construct the PRS.

Second, this repository also provides code the we used for the analyses
in the manuscript (see folder “Code”).

## Required packages

We used [PRSice 2.3.1.e](https://www.prsice.info "PRSice 2.3.1.e") to
generate PRS. We provide example code that also uses PRSice to construct
PRS based on the provided summary statistics in folder
“Summary\_Statistics\_for\_PRS\_construction”.

Other software and packages that we used, but may not be necessary for
others to construct the PRS, are as follows:  
1. We performed the analysis using R version 4.0.2.  
2. We used the following packages from CRAN: dplyr, tidyverse,
data.table, purrr, pROC.  
3. We used the following packages from BioConductor: GENESIS,
GWASTools.  

    install.packages("dplyr")

## PRS construction

Our HTN-PRS is a sum of multiple trait-specific PRS (HTN, systolic blood
pressure (SBP) and diastolic blood pressure (DBP)). Summary statistics
to create the each of the trait-PRS are provided here in a
subfolder(./Summary\_Statistics\_for\_PRS\_construction/\*).

Specific GWAS used: hypertension “pan ancestry” GWAS from
[UKBB](https://pan.ukbb.broadinstitute.org), ([SBP GWAS from
MVP](https://pubmed.ncbi.nlm.nih.gov/30578418/)), and ([DBP GWAS from
MVP](https://pubmed.ncbi.nlm.nih.gov/30578418/)) with MVP standing for
Million Veteran Program.

The summary statistics provided were selected from
those in the complete GWAS based on clumping parameter below, where we
used the multi-ethnic TOPMed dataset used in the paper as an LD
reference panel. To select the specific tuning parameter (LD parameters,
p-value threshold) for each trait-specific PRS, we applied an approach
where we optimized the coefficient of variation (CV) computed over the
estimated effect sizes of the candidate PRS across 5 independent subset
of the training dataset. See manuscript for more detail.

The table below provides, for each trait-specific GWAS used, the
following information:  

1.  GWAS\_pop: GWAS population (which cohort/study the GWAS summary
    statistics are from?)  
2.  Trait (HTN, SBP, DBP)  
3.  Threshold: p-value threshold for selecting SNPs into the PRS  
4.  Distance: distance in kilo base-pairs used for clumping (SNPs were
    removed from consideration based on LD with other SNPs within a
    window of this distance)  
5.  R2: maximum LD for inclusion of SNPs within the distance-based
    window of another SNP that was already selected into the PRS.  
6.  TOPMed\_mean: the mean of the PRS after it was constructed in the
    multi-ethnic TOPMed population. That is, each of the TOPMed
    participants had a PRS value. This is the mean of these values.  
7.  TOPMed\_sd: the standard deviation (SD) of the PRS after it was
    constructed in the multi-ethnic TOPMed population. That is, each of
    the TOPMed participants had a PRS value. This is the SD of these
    values.

<!-- -->

    ##   GWAS_pop Trait Threshold Distance  R2 TOPMed_mean TOPMed_sd
    ## 1 Pan-UKBB   HTN      0.30    250kb 0.1   -8.57e-06  1.98e-05
    ## 2      MVP   DBP      0.10    250kb 0.3   -4.95e-04  2.63e-04
    ## 3      MVP   SBP      0.01   1000kb 0.2   -4.63e-03  1.11e-03

## PRSice command for PRS construction

This command is to construct PRS using the summary statistics that we
provide. No clumping is needed and no selection of SNPs. The summary
statistics are already based on the specific set of SNPs selected after
clumping and setting a p-value threshold. Note that genetic data files
need to be specified in the –target argument.



    Rscript ./PRSice.R \
     --dir ./PRS_Output \
     --prsice ./PRSice_linux/PRSice_linux \
     --base ./Summary_Statistics_for_PRS_construction/. \
     --target ./Genotype \
     --thread 2 \
     --chr Chromosome 
     --bp Position 
     --A1 Allele1 
     --A2 Allele2 
     --pvalue PValue \
     --bar-levels Threshold \
     --stat BETA 
     --all-score T \
     --out ./out_prs \
     --no-clump T
     --print-snp T \
     --ignore-fid T 
     --no-regress T 
     --fastscore T 
     --model add 
     --no-full T 
     --chr-id c:l:a:b

## Constructing PRSsum based on trait-specific PRS

After constructing trait-specific PRS, the HTN-PRS is obtained via the
PRSsum approach: as an unweighted of the scaled trait-specific PRS. For
scaling, we use the TOPMed mean and SD values of each trait-specific
PRS, and we also provide here the TOPMed mean and SD of the HTN-PRS for
final scaling. Using the same scaling throughout guarantees that effect
size estimates are similarly interpreted across all datasets and
individuals who use this PRS.

    ##   TOPMed_mean TOPMed_sd
    ## 1   -4.96e-16      2.69

See code below to construct PRSsum.

    library(data.table)
    library(dplyr)
    library(purrr)


    prs_traits <- c("SBP", "DBP","HTN")
    out<-list()
    for(trait in prs_traits){
      
      
      prs_output <-paste0("./", prs,".txt")
      prs_df <-fread(prs_output, data.table=F)
      prs_df <- prs_df %>% dplyr::select(-IID)
      colnames(prs_df)<- c("sample.id", trait)
      
      #standardize trait-prs using the mean and sd from TOPMed 
      cur_TOPMed_mean <- PRS_info$TOPMed_mean[which(PRS_info$Trait == trait)]
      cur_TOPMed_sd <- PRS_info$TOPMed_sd[which(PRS_info$Trait == trait)]
      
      prs_df[, trait]<- (prs_df[, trait] - cur_TOPMed_mean)/cur_TOPMed_sd
      out[[trait]]<- prs_df
      
      
    }

    combine_prs <- purrr::reduce(out, full_join , by="sample.id")



    prssum<- data.frame(sample.id=prssum$sample.id, 
                        PRSsum=apply(prssum[,], 1, sum))

    prssum[,"PRSsum"]<- (prssum[,"PRSsum"] - TOPMed_HTN_PRS_mean_sd$TOPMed_mean))/TOPMed_HTN_PRS_mean_sd$TOPMed_sd

## Example code for association analsis

We performed association analysis using mixed models implemented in the
GENESIS R package. Below is an example code. It uses function that we
provide in the folder “Code”.

    library(GENESIS)
    library(GWASTools)
    library(pROC)


    source("./Code/*")


    #phenotype

    pheno<- fread(phenotype_file, data.table=F)


    # merge PRSsum with phenotype

    pheno_df<-left_join(pheno,prssum, by="sample.id" )


    covarites_prs<- c("BMI","age","sex","site","race",paste0("PC_",1:11),"PRSsum")

    outcome<-"HTN"

    ## Kinship matrix

    covMatlist<-getobj(covMatlist)


    assoc_df<- run_assoc_mixmodel(pheno=pheno,
                                  outcome=outcome,
                                  covars_prs=covarites_prs, 
                                  covmat=covMatlist,
                                  group.var=NULL)


    # Perform AUC

    #only use unrelated people

    unrels<- getobj(unrels_people)

    pheno_unrels<- pheno[pheno_df$sample.id %in% unrels,]


    auc<- generate_auc(pheno=pheno_unrels,
                       outcome=outcome,
                       covars_prs=covarites_prs, seed=NULL,
                       n= 2000)



    final_assoc<-c(assoc_df,auc)
    
