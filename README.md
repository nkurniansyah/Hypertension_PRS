## Introduction

This is the instruction to construct generate and run the association
test for Polygenic Risk Score in Hypertension. This instruction is based
on the manuscript:…

## STEP 1: Installation and require packages

In this paper, We used [PRSice
2.3.1.e](https://www.prsice.info "PRSice 2.3.1.e") to generate PRS.

We performed the analysis using R programming and required few R
packages (dplyr, tidyverse, data.table, GENESIS)

## STEP 2: Construct PRS

We used hypertension “pan ancestry” GWAS from
[UKBB](https://pan.ukbb.broadinstitute.org), and systolic BP
([SBP](https://pubmed.ncbi.nlm.nih.gov/30578418/)), and diastolic BP
([DBP](https://pubmed.ncbi.nlm.nih.gov/30578418/)) from MVP(Million
Veteran Program) to construct PRS (See script below). Then we applied CV
(Coefecient Variation) to select best perfomance PRS, see manuscript for
more detail.

We provide summary statistics to crete HTN PRS in this
repistory(./Summary\_Statitcs/\*).



    Rscript ./PRSice.R --dir ./PRS_Output \
     --prsice ./PRSice_linux/PRSice_linux \
     --base ./Summary_Statistic/. \
     --target ./Genotype \
     --thread 2 \
     --chr Chromosome 
     --bp Position 
     --A1 Allele1 
     --A2 Allele2 
     --pvalue PValue \
     --bar-levels 5e-8,1e-7 \
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
     --chr-id c:L:a:B 

## STEP 3: Construct PRSsum

After run PRS for each summary statistics, then we combine PRS (PRSsum).

    library(data.table)
    library(dplyr)
    library(purrr)


    prs_trait<- c("SBP", "DBP","HTN")
    out<-list()
    for(prs in prs_trait){
      
      
      prs_output<-paste0("./", prs,".txt")
      prs_df<-fread(prs_output, data.table=F)
      prs_df<- prs_df %>% dplyr::select(-IID)
      colnames(prs_df)<- c("sample.id",prs)
      
      #standardize prs

      prs_df[, prs]<- (prs_df[,prs] - mean(prs_df[,prs]))/sd(prs_df[,prs])
      out[[prs]]<- prs_df
      
      
    }

    combine_prs<- purrr::reduce(out, full_join , by="sample.id")



    prssum<- data.frame(sample.id=prssum$sample.id, PRSsum=apply(prssum[,], 2, sum))

    prssum[,"PRSsum"]<- (prssum[,"PRSsum"] - mean(prssum[,"PRSsum"]))/sd(prssum[,"PRSsum"])

## STEP 3: Perform Association Analysis

Finally, we perormed association analysis using mix model.

    library(GENESIS)


    #phenotype


    pas
