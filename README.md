## Introduction

This is the instruction to generate and run the analysis of Polygenic
Risk Score in Hypertension. This instruction based on manuscript:

## STEP 1: Installation and require packages

In this paper, We used PRSice 2.3.1.e (<https://www.prsice.info>) to
generate PRS.

we performed the analysis using R programming and required few R
packages (dplyr, tidyverse, data.table, GENESIS)

## STEP 2: Construct PRS

For the main analysis, we used hypertension “pan ancestry” GWAS from
UKBB (<https://pan.ukbb.broadinstitute.org>), and systolic BP (SBP), and
diastolic BP (DBP) from MVP(<PMID:30578418>).
