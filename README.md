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

## STEP 3: Construct PRSsum

After run PRS for each summary statistics, then we combine PRS (PRSsum).

## STEP 3: Perform Association Analysis

Finally, we perormed association analysis using mix model.
