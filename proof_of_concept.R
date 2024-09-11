library(tidyverse)
library(NHSRwaitinglist)


# Weekly added 
# Weekly remova

# Simulate WL with 70 entering and 65 exiting per week
a <- wl_simulator(as.Date('2023-04-01'), as.Date('2024-03-25'), 70, 65)

# Queue size
wl_queue_size(a)

# visualise queue
ggplot(wl_queue_size(a), aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "A growing waiting list"
  )

removal_stats <- wl_removal_stats(a)

# If we want the average wait to be within a target wait of 18 weeks
# For approx 2% chance of going over target
calc_target_mean_wait(18, factor = 4)


# Needs and average wait of 4.5 weeks


# Little's law: if Capacity > Demand, then Average Queue Size = Demand x Waiting Time
# In our case
calc_target_queue_size(70, 18)




# compute some waiting list statistics
overall_stats <- wl_stats(
  waiting_list = a,
  target_wait = 18 # standard NHS 18wk target
)


# review the waiting list statistics
overall_stats


# So we are over here.
# What is the relieve capacity required to reduce it in 6 months?
calc_relief_capacity(70, 239, 315, 4*6)


# Normal target capacity once WL is reduced to sustainable
calc_target_capacity(70, 4.5)

# 70.888