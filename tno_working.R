# Generate artificial T&O scenario using real data control parameters

library(tidyverse)
library(NHSRwaitinglist)


demand_phase1 = 416# 300
demand_phase2 = 434
demand_phase3 = demand_phase2 * 0.6  # 40 percent of demand taken out by CC
capacity_phase1 = 382 # 248
capacity_phase2 = 454
capacity_phase3 = capacity_phase2  # 40 percent of demand taken out by CC

phase_1_start = as.Date("2019-04-01")
phase_1_end = as.Date("2023-08-31")
phase_2_start = phase_1_end + 1 
phase_2_end = as.Date("2027-03-31")
phase_3_start = phase_2_end + 1 
phase_3_end = as.Date("2029-03-31")

# Phase 1:  pre-implementation, setting up the queue.  This may not be real, but should end at peak date

tno_sim1 <-
  wl_simulator(phase_1_start, phase_1_end, demand_phase1, capacity_phase1)

tno_queue1 <-
  wl_queue_size(tno_sim1)


ggplot(tno_queue1, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )


# Nicely guessed to meet peak of 6338 on list at Sept-2023
# Now we clear as per current rates

tno_sim2 <-
  wl_simulator(phase_2_start, phase_2_end, demand_phase2, capacity_phase2)

tno_queue2 <-
  wl_queue_size(tno_sim2)


ggplot(tno_queue2, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 2: Now until implementation \nCapacity = ", capacity_phase2, ", Demand=", demand_phase2),
    y = "Queue Size",
    x = "Month"
    
  )


# Waiting list with 40% off after this date
tno_sim3 <-
  wl_simulator(phase_3_start, phase_3_end, demand_phase3, capacity_phase3)

tno_queue3 <-
  wl_queue_size(tno_sim3)


ggplot(tno_queue3, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase T3: after implementation \nCapacity = ", capacity_phase3, ", Demand=", demand_phase3),
    y = "Queue Size",
    x = "Month"
  )




# Join waiting list

tno_wl2 <- wl_join(tno_sim1, tno_sim2)
tno_wl3 <- wl_join(tno_wl2, tno_sim3)


# Plot with implementation at March 27, then add 40% drop in demand

tno_combined_queue <- wl_queue_size(tno_wl3)

ggplot(tno_combined_queue, aes(dates, queue_size)) +
  geom_line() +
  geom_vline(xintercept = c(phase_2_start, phase_3_start), col=c("dodgerblue2", "red"), linetype = "dashed")+
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = "Full modelled period",
    y = "Queue Size",
    x = "Month"
  )


# Target queue size for current capacity
current_target_queue <- calc_target_queue_size(demand, 52, factor = 4)

# Future target queue size
current_target_capacity <- calc_target_capacity([demand], 52)


# How much resource could be freed?

# Calculate target capacity using Kingman/Marchal's Formula:
# capacity = demand + (cvd**2 + cvc**2) / waiting_time

# How long to meet 92% patients < 18 weeks?
# If we want the average wait to be within a target wait of 18 weeks
# For approx 2% chance of going over target
calc_target_mean_wait(52, factor = 4)

# Current target demand
current_target_capacity <- calc_target_capacity([demand], 52)

# Target capacity after difference
future_target_capacity <- calc_target_capacity([demand after 40% reduction], 52)
  
# capacity that release once implemented:
  
current_target_capacity - future_target_capacity




