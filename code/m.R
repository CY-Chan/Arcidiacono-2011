#################################################################################################
# Arcidiacono and Miller (2011) coding assignment
# Matt Beamer & Cheng-Yu Chan
# 11/08/2014
#################################################################################################

source('header.R')

#################################################################################################
# Global parameters. Will not be passing into functions.
#################################################################################################
N = 10^6
theta_1 = 10
theta_2 = 1

s_1 = 1
s_2 = 4
pi_s = .25
beta = .99

eta = 10^-5 #For calculating lower bound on x
x_m = max(x_bound(eta = eta, s = s_1), x_bound(eta = eta, s = s_2))

u <- values()

#################################################################################################
# Initialize CCPs
# p is starting CCP estimate
# phat is empty but initialized for the while loop
#################################################################################################
p <- 
  matrix(rep(0.5, (x_m + 1) * 4), ncol = 4) %>%
  rbind(c(0, 0, 1, 1)) %>%
    as.data.frame
phat <- 
  matrix(rep(0, (x_m + 2) * 4), ncol = 4) %>%
  as.data.frame

#################################################################################################
# Apply the gamma operator to the CCP until phat converges to p.
# Convergence is defined as the greatest difference between the iterations being arbitrarily small.
#################################################################################################
while(max(abs(phat - p)) > 10^(-16)){
  phat <- p

  # Apply the F operator to get the difference in value between keeping the engine and replacing it
  vals <- Foperator(P = phat)

  # Apply the lambda operator to get the updated CCPs
  p <- lambda(v_bar = vals)
}

#################################################################################################
# For N engines, with pi_s * N of them of type s_1 and (1 - pi_s) of them of type s_2, find the time until each engine is 
# new again (replaced in the last period). Store the results in a data frame with three columns:
#   Column 1: engine number, from 1 to N
#   Column 2: type, 1 for s_1 and 2 for s_2
#   Column 3: mileage at replacement, x
#################################################################################################
engines <- data.frame(
  engine_number = 1:N,
  type = rbinom(N, 1, 1 - pi_s)
) %>%
  mutate(type = type + 1) %>%
  arrange(type)                                 # Sort by brand for easy merging of mileage data later

engine_count<- data.frame(table(engines[2]))    # Number of each brand from random draw.

history<- 
  cbind(todeath(engine_count[1, 1], engine_count[1, 2]),
        todeath(engine_count[2, 1], engine_count[2, 2]))

engines<-
  mutate(engines, replace_Period = apply(history, 2, which.min) - 1) %>%
  arrange(engine_number) %>%
  group_by(replace_Period) %>%
  summarise(Total_replaced = n())

saveRDS(engines,paste0(varSave,"engines_data.rds"))             # Save using saveRDS() for later use

