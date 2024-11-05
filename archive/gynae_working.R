# Generate artificial Gynae scenario using real data control parameters


# Connect to SQL Server.
library(tidyverse)
#library(DBI)
library(patchwork)
library(scales)
#library(zoo)
library(NHSRwaitinglist)
#library(officer)

# set ggplot theme
theme_set(
  theme_minimal()+
    theme(plot.title = element_text(size=12),
          plot.subtitle = element_text(size=9, face="italic")
    )
)


# 
demand_phase1 = 425# 300
demand_phase2 = 288
demand_phase3_0 = demand_phase2

capacity_phase1 = 390
capacity_phase2 = 314
capacity_phase3_0 = capacity_phase2

demand_phase3_20 = demand_phase2 * 0.8 # 20 percent of demand taken out by CC
capacity_phase3_20 = capacity_phase2 * 0.8 # 20 percent of demand taken out by CC

demand_phase3_40 = demand_phase2 * 0.6 # 40 percent of demand taken out by CC
capacity_phase3_40 = capacity_phase2 * 0.6   # 40 percent of demand taken out by CC




phase_1_start = as.Date("2020-10-01")
phase_1_end = as.Date("2023-03-12")
phase_2_start = phase_1_end + 1 
phase_2_end = as.Date("2026-11-30")
phase_3_start = phase_2_end + 1 
phase_3_end = as.Date("2029-03-31")

# Phase 1:  pre-implementation, setting up the queue.  This may not be real, but should end at peak date

gynae_sim1 <-
  wl_simulator(phase_1_start, phase_1_end, demand_phase1, capacity_phase1)


gynae_queue1 <-
  wl_queue_size(gynae_sim1)




ggplot(gynae_queue1, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "Gynae: First GP referral to first Outpatigynaes waiting list:",
    subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )+
  scale_y_continuous(labels = comma)


# Nicely guessed to meet peak of 6338 on list at Sept-2023
# Now we clear as per current rates

gynae_sim2 <-
  wl_simulator(phase_2_start, phase_2_end, demand_phase2, capacity_phase2
               , waiting_list = gynae_sim1)


gynae_queue2 <-
  wl_queue_size(gynae_sim2)


ggplot(gynae_queue2, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "Gynae: First GP referral to first Outpatigynaes waiting list:",
    subtitle = paste("Phase 2: Now until implemgynaeation \nCapacity = ", capacity_phase2, ", Demand=", demand_phase2),
    y = "Queue Size",
    x = "Month"
    
  ) +
  scale_y_continuous(labels = comma)


# Waiting list with 40% off after this date
gynae_sim3_0 <-
  wl_simulator(phase_3_start, phase_3_end, demand_phase3_0, capacity_phase3_0, 
                 waiting_list = gynae_sim2)

gynae_sim3_20 <-
  wl_simulator(phase_3_start, phase_3_end, demand_phase3_20, capacity_phase3_0, 
               waiting_list = gynae_sim2)

gynae_sim3_40 <-
  wl_simulator(phase_3_start, phase_3_end, demand_phase3_40, capacity_phase3_0, 
               waiting_list = gynae_sim2)


gynae_queue3_0 <-
  wl_queue_size(gynae_sim3_0) %>% 
  mutate(sim = "0% reduction")

gynae_queue3_20 <-
  wl_queue_size(gynae_sim3_20) %>% 
  mutate(sim = "20% reduction")

gynae_queue3_40 <-
  wl_queue_size(gynae_sim3_40) %>% 
  mutate(sim = "40% reduction")


gynae_queue3_0 %>% 
  union(gynae_queue3_20) %>% 
  union(gynae_queue3_40) %>% 

ggplot(aes(dates, queue_size, linetype = sim)) +
  geom_line() +
  #geom_line(data = gynae_queue3_20, linetype = "dashed") +
  #geom_line(data = gynae_queue3_40, linetype = "dotted") +
  labs(
    title = "Gynae: First GP referral to first Outpatigynaes waiting list:",
    subtitle = paste("Phase T3: after implemgynaeation: \nCapacity(0) = ", capacity_phase3_0, ", Demand (0)=", demand_phase3_0,
                     "\nCapacity(20) = ", capacity_phase3_20, ", Demand (20)=", demand_phase3_20,
                     "\nCapacity(40) = ", capacity_phase3_40, ", Demand (40)=", demand_phase3_40),
    y = "Queue Size",
    x = "Month"
  )



# Target queue size for currgynae and future queue
currgynae_target_queue <- calc_target_queue_size(demand_phase2, 18, factor = 3)
future_target_queue_0 <- calc_target_queue_size(demand_phase3_0, 18, factor = 3)
future_target_queue_20 <- calc_target_queue_size(demand_phase3_20, 18, factor = 3)
future_target_queue_40 <- calc_target_queue_size(demand_phase3_40, 18, factor = 3)

# 6 week average wait.
# 

# Phases 0
gynae_queue3_0 <-
  gynae_queue3_0 %>% 
  mutate(phase = case_when(dates <= phase_1_end ~ 1,
                           dates <= phase_2_end ~ 2,
                           dates <= phase_3_end ~ 3,
                           .default = 0)
         , phase2_target = ifelse(queue_size <= currgynae_target_queue & phase == 2, 1,0)
         , phase3_target = ifelse(queue_size <= future_target_queue_0 & phase > 1,1,0)
         )

# Phases20
gynae_queue3_20 <-
  gynae_queue3_20 %>% 
  mutate(phase = case_when(dates <= phase_1_end ~ 1,
                           dates <= phase_2_end ~ 2,
                           dates <= phase_3_end ~ 3,
                           .default = 0)
         , phase2_target = ifelse(queue_size <= currgynae_target_queue & phase == 2, 1,0)
         , phase3_target = ifelse(queue_size <= future_target_queue_20 & phase > 1,1,0)
  )


# Phases
gynae_queue3_40 <-
  gynae_queue3_40 %>% 
  mutate(phase = case_when(dates <= phase_1_end ~ 1,
                           dates <= phase_2_end ~ 2,
                           dates <= phase_3_end ~ 3,
                           .default = 0)
         , phase2_target = ifelse(queue_size <= currgynae_target_queue & phase == 2, 1,0)
         , phase3_target = ifelse(queue_size <= future_target_queue_40 & phase > 1,1,0)
  )


# Join waiting list

#gynae_wl2 <- wl_join(gynae_sim1, gynae_sim2)
#gynae_wl3 <- wl_join(gynae_wl2, gynae_sim3)

wl_stats(gynae_sim3)





# How much resource could be freed?

# Calculate target capacity using Kingman/Marchal's Formula:
# capacity = demand + (cvd**2 + cvc**2) / waiting_time

# How long to meet 92% patigynaes < 18 weeks?
# If we want the average wait to be within a target wait of 18 weeks
# For approx 2% chance of going over target
mean_wait6 <- calc_target_mean_wait(18, factor =6)
mean_wait4 <- calc_target_mean_wait(18, factor =4)
mean_wait3 <- calc_target_mean_wait(18, factor =3)

# Target capacity after difference
currgynae_target_capacity <- calc_target_capacity(demand_phase2, 18, factor = 3)
future_target_capacity_0 <- calc_target_capacity(demand_phase3_0, 18, factor = 3)
future_target_capacity_20 <- calc_target_capacity(demand_phase3_20, 18, factor = 3)
future_target_capacity_40 <- calc_target_capacity(demand_phase3_40, 18, factor = 3)
  
# capacity that release once implemgynaeed:
  
weekly_capacity_release_0 <- currgynae_target_capacity - future_target_capacity_0
weekly_capacity_release_20 <- currgynae_target_capacity - future_target_capacity_20
weekly_capacity_release_40 <- currgynae_target_capacity - future_target_capacity_40



#  How long does it take to get to queue < target length

# gynae_queue3 <-
#   gynae_queue3 %>% 





# Plot with implemgynaeation at March 27, then add 20% drop in demand

#gynae_combined_queue <- wl_queue_size(gynae_wl3)
  
phase_2_reach_0 <- 
  gynae_queue3_0 %>% 
  filter(phase2_target == 1) %>% 
  head(1) %>%
  mutate(sim = "Currgynae") %>% 
  select(dates, queue_size, sim) 

# phase_2_reach_20 <- 
#   gynae_queue3_20 %>% 
#   filter(phase2_target == 1) %>% 
#   head(1) %>%
#   mutate(sim = "20% reduction") %>% 
#   select(dates, queue_size, sim) 

# phase_2_reach_40 <- 
#   gynae_queue3_40 %>% 
#   filter(phase2_target == 1) %>% 
#   head(1) %>%
#   mutate(sim = "40% reduction") %>% 
#   select(dates, queue_size, sim) 
# 
# 
phase_3_reach_0 <- 
  gynae_queue3_0 %>% 
  filter(phase3_target == 1) %>% 
  head(1) %>%  
  mutate(sim = "0% reduction") %>% 
  select(dates, queue_size, sim) 

phase_3_reach_20 <- 
  gynae_queue3_20 %>% 
  filter(phase3_target == 1) %>% 
  head(1) %>% 
  mutate(sim = "20% reduction") %>% 
  select(dates, queue_size, sim) 

phase_3_reach_40 <- 
  gynae_queue3_40 %>% 
  filter(phase3_target == 1) %>% 
  head(1) %>%  
  mutate(sim = "40% reduction") %>% 
  select(dates, queue_size, sim) 


ggplot(gynae_queue3_0, aes(dates, queue_size)) +
  geom_line() +
  geom_vline(xintercept = c(phase_2_start, phase_3_start), col=c("dodgerblue2", "red"), linetype = "dashed")+
  #geom_hline(yintercept = currgynae_target_queue, linetype = "dashed")+
  #geom_hline(yintercept = future_target_queue, linetype = "dashed")+
  geom_point(data=phase_2_reach_0, shape=5, col = "dodgerblue2", size = 5, stroke = 2)+
  geom_point(data=phase_3_reach_0, shape=4, col = "red", size = 5, stroke = 2)+
  labs(
    title = "Gynae: First GP referral to first Outpatigynaes waiting list:",
    subtitle = "Full modelled period",
    y = "Queue Size",
    x = "Month"
  )+
  scale_y_continuous(labels = comma)

ggplot(gynae_queue3_20, aes(dates, queue_size)) +
  geom_line() +
  geom_vline(xintercept = c(phase_2_start, phase_3_start), col=c("dodgerblue2", "red"), linetype = "dashed")+
  #geom_hline(yintercept = currgynae_target_queue, linetype = "dashed")+
  #geom_hline(yintercept = future_target_queue, linetype = "dashed")+
  geom_point(data=phase_2_reach_0, shape=4, col = "dodgerblue2", size = 5, stroke = 2)+
  geom_point(data=phase_3_reach_20, shape=4, col = "red", size = 5, stroke = 2)+
  labs(
    title = "Gynae: First GP referral to first Outpatigynaes waiting list:",
    subtitle = "Full modelled period",
    y = "Queue Size",
    x = "Month"
  )+
  scale_y_continuous(labels = comma)


ggplot(gynae_queue3_40, aes(dates, queue_size)) +
  geom_line() +
  geom_vline(xintercept = c(phase_2_start, phase_3_start), col=c("dodgerblue2", "red"), linetype = "dashed")+
  #geom_hline(yintercept = currgynae_target_queue, linetype = "dashed")+
  #geom_hline(yintercept = future_target_queue, linetype = "dashed")+
  geom_point(data=phase_2_reach_0, shape=4, col = "dodgerblue2", size = 5, stroke = 2)+
  geom_point(data=phase_3_reach_0, shape=4, col = "red", size = 5, stroke = 2)+
  labs(
    title = "Gynae: First GP referral to first Outpatigynaes waiting list:",
    subtitle = "Full modelled period",
    y = "Queue Size",
    x = "Month"
  )+
  scale_y_continuous(labels = comma)


targets <-
  phase_2_reach_0 %>% 
  #union(phase_2_reach_20) %>% 
  #union(phase_2_reach_40) %>% 
  union(phase_3_reach_0) %>% 
  union(phase_3_reach_20) %>% 
  union(phase_3_reach_40) %>% 
  mutate(sim = ifelse(dates < phase_3_start, "Currgynae", sim))
  


gynae_queue3_0 %>% 
  union(gynae_queue3_20) %>% 
  union(gynae_queue3_40) %>% 
  mutate(sim = ifelse(dates < phase_3_start, "Currgynae", sim)) %>% 
  filter(year(dates)>2023) %>% 
  
  ggplot(aes(dates, queue_size, col = sim)) +  
  scale_colour_viridis_d()+
  geom_line() +
  geom_vline(xintercept = c(phase_2_start, phase_3_start), linetype = "dashed")+
  #geom_hline(yintercept = currgynae_target_queue, linetype = "dashed")+
  #geom_hline(yintercept = future_target_queue, linetype = "dashed")+
  #geom_point(data=targets, shape=4, size = 5, stroke = 2)
  geom_point(data=phase_2_reach_0, shape=4, size = 5, stroke = 2)+
  geom_point(data=phase_3_reach_0, shape=4, size = 5, stroke = 2) +
  geom_point(data=phase_3_reach_20, shape=4, size = 5, stroke = 2) +
  geom_point(data=phase_3_reach_40, shape=4, size = 5, stroke = 2) +
  #geom_point(data=phase_3_reach_0, shape=4, col = "red", size = 5, stroke = 2)+

  labs(
    title = "Gynae: First GP referral to first Outpatigynaes waiting list:",
    subtitle = "Crosses represgynae reaching the target queue size",
    y = "Queue Size",
    x = "Month",
    colour = "Simulation"
  )+
  scale_y_continuous(labels = comma)
  #geom_line(data = gynae_queue3_20, linetype = "dashed") +
  #geom_line(data = gynae_queue3_40, linetype = "dotted") +



#to_save <- list(gynae_sim1, gynae_sim2, gynae_sim3)
#lapply(to_save,function(x){saveRDS(x, file = paste0("/data/",x,".RDS"))})

#saveRDS(gynae_sim3_0, "./data/gynae_sim3_0.rds")
#saveRDS(gynae_sim3_20, "./data/gynae_sim3_20.rds")
#saveRDS(gynae_sim3_40, "./data/gynae_sim3_40.rds")
#saveRDS(gynae_sim2, "./data/gynae_sim2.rds")
#saveRDS(gynae_sim1, "./data/gynae_sim1.rds")

gynae_sim1 <- readRDS("./data/gynae_sim1.rds")
gynae_sim2 <- readRDS("./data/gynae_sim2.rds")
gynae_sim3_0 <- readRDS("./data/gynae_sim3_0.rds")
gynae_sim3_20 <- readRDS("./data/gynae_sim3_20.rds")
gynae_sim3_40 <- readRDS("./data/gynae_sim3_40.rds")
