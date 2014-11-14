#################################################################################################
# Arcidiacono and Miller (2011) coding assignment Part 2
# Matt Beamer & Cheng-Yu Chan
# 11/08/2014
#################################################################################################

source('header.R')
data<- readRDS(paste0(varSave,"engines_data.rds"))
#################################################################################################
# Global parameters. Will not be passing into functions.
#################################################################################################
s_1 = 1
s_2 = 4
pi_s = .25
beta = .99

x_m = max(data$replace_Period) + 10

phat = matrix(rep(1, x_m*2), ncol = 2)
theta_1hat = 10
theta_2hat = 1