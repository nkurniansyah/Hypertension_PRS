## Introduction

This is the instruction to generate and run the analysis of Polygenic
Risk Score in Hypertension. This instruction is based on the manuscript:

## STEP 1: Installation and require packages

In this paper, We used [PRSice
2.3.1.e](https://www.prsice.info "PRSice 2.3.1.e") to generate PRS.

We performed the analysis using R programming and required few R
packages (dplyr, tidyverse, data.table, GENESIS)

## STEP 2: Construct PRS

For the main analysis, we used hypertension “pan ancestry” GWAS from
[UKBB](https://pan.ukbb.broadinstitute.org), and systolic BP
([SBP](https://pubmed.ncbi.nlm.nih.gov/30578418/)), and diastolic BP
([DBP](https://pubmed.ncbi.nlm.nih.gov/30578418/)) from MVP(Million
Veteran Program)
