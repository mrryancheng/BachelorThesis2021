#!/bin/bash
#$ -cwd
#$ -m abe
#$ -M ryan.cheng@student.unisg.ch
#$ -pe onenode 8
#$ -l m_mem_free=6G

R CMD BATCH --no-restore WRDS_HSC_Query.R