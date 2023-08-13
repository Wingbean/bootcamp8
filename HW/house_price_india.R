#load library
library(tidyverse)
library(caret)
library(mlbench)
library(readxl)

# Load data0
df1<-read_excel("hpi.xlsx", sheet = 1)
df2<-read_excel("hpi.xlsx", sheet = 2)