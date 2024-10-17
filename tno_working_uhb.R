# Generate artificial T&O scenario using real data control parameters


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


demand_phase1 = 93# 300
demand_phase2 = 103
demand_phase3 = 67
demand_phaseT3_0 = demand_phase3

capacity_phase1 = 90 # 248
capacity_phase2 = 114
capacity_phase3 = 80
capacity_phaseT3_0 = capacity_phase3

demand_phaseT3_20 = demand_phase3 * 0.8 # 20 percent of demand taken out by CC
capacity_phaseT3_20 = capacity_phase3 * 0.8 # 20 percent of demand taken out by CC

demand_phaseT3_40 = demand_phase3 * 0.6 # 40 percent of demand taken out by CC
capacity_phaseT3_40 = capacity_phase3 * 0.6   # 40 percent of demand taken out by CC




phase_1_start = as.Date("2020-04-01")
phase_1_end = as.Date("2023-10-29")
phase_2_start = phase_1_end + 1 
phase_2_end = as.Date("2024-06-02")
phase_3_start = phase_2_end + 1 
phase_3_end = as.Date("2027-04-01")

phase_T3_start = phase_3_end + 1 
phase_T3_end = as.Date("2029-03-31")

# Phase 1:  pre-implementation, setting up the queue.  This may not be real, but should end at peak date

tno_uhb_sim1 <-
  wl_simulator(phase_1_start, phase_1_end, demand_phase1, capacity_phase1)


tno_queue1 <-
  wl_queue_size(tno_uhb_sim1)




ggplot(tno_queue1, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )+
  scale_y_continuous(labels = comma)


# Nicely guessed to meet peak of 6338 on list at Sept-2023
# Now we clear as per current rates

tno_uhb_sim2 <-
  wl_simulator(phase_2_start, phase_2_end, demand_phase2, capacity_phase2
               , waiting_list = tno_uhb_sim1)


tno_queue2 <-
  wl_queue_size(tno_uhb_sim2)


ggplot(tno_queue2, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 2: Now until implementation \nCapacity = ", capacity_phase2, ", Demand=", demand_phase2),
    y = "Queue Size",
    x = "Month"
    
  ) +
  scale_y_continuous(labels = comma)


# Additional t3 stage here

tno_uhb_sim3 <-
  wl_simulator(phase_3_start, phase_3_end, demand_phase3, capacity_phase3
               , waiting_list = tno_uhb_sim2)


tno_queue3 <-
  wl_queue_size(tno_uhb_sim3)


ggplot(tno_queue3, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 3: After peak \nCapacity = ", capacity_phase3, ", Demand=", demand_phase3),
    y = "Queue Size",
    x = "Month"
    
  ) +
  scale_y_continuous(labels = comma)


# Waiting list with 40% off after this date
tno_uhb_simT3_0 <-
  wl_simulator(phase_T3_start, phase_T3_end, demand_phaseT3_0, capacity_phaseT3_0, 
                 waiting_list = tno_uhb_sim3)

tno_uhb_simT3_20 <-
  wl_simulator(phase_T3_start, phase_T3_end, demand_phaseT3_20, capacity_phaseT3_0, 
               waiting_list = tno_uhb_sim3)

tno_uhb_simT3_40 <-
  wl_simulator(phase_T3_start, phase_T3_end, demand_phaseT3_40, capacity_phaseT3_0, 
               waiting_list = tno_uhb_sim3)


tno_queue3_0 <-
  wl_queue_size(tno_uhb_simT3_0) %>% 
  mutate(sim = "0% reduction")

tno_queue3_20 <-
  wl_queue_size(tno_uhb_simT3_20) %>% 
  mutate(sim = "20% reduction")

tno_queue3_40 <-
  wl_queue_size(tno_uhb_simT3_40) %>% 
  mutate(sim = "40% reduction")


tno_queue3_0 %>% 
  union(tno_queue3_20) %>% 
  union(tno_queue3_40) %>% 

ggplot(aes(dates, queue_size, linetype = sim)) +
  geom_line() +
  #geom_line(data = tno_queue3_20, linetype = "dashed") +
  #geom_line(data = tno_queue3_40, linetype = "dotted") +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase T3: after implementation: \nCapacity(0) = ", capacity_phaseT3_0, ", Demand (0)=", demand_phaseT3_0,
                     "\nCapacity(20) = ", capacity_phaseT3_20, ", Demand (20)=", demand_phaseT3_20,
                     "\nCapacity(40) = ", capacity_phaseT3_40, ", Demand (40)=", demand_phaseT3_40),
    y = "Queue Size",
    x = "Month"
  )



# Target queue size for current and future queue
current_target_queue <- calc_target_queue_size(demand_phase3, 18, factor = 3)
future_target_queue_0 <- calc_target_queue_size(demand_phaseT3_0, 18, factor = 3)
future_target_queue_20 <- calc_target_queue_size(demand_phaseT3_20, 18, factor = 3)
future_target_queue_40 <- calc_target_queue_size(demand_phaseT3_40, 18, factor = 3)

# 6 week average wait.
# 

# Phases 0
tno_queue3_0 <-
  tno_queue3_0 %>% 
  mutate(phase = case_when(dates <= phase_1_end ~ 1,
                           dates <= phase_2_end ~ 2,
                           dates <= phase_3_end ~ 3,
                           dates <= phase_T3_end ~ 4,
                           .default = 0)
         , phase3_target = ifelse(queue_size <= current_target_queue & phase == 3, 1,0)
         , phaseT3_target = ifelse(queue_size <= future_target_queue_0 & phase > 2,1,0)
         )

# Phases20
tno_queue3_20 <-
  tno_queue3_20 %>% 
  mutate(phase = case_when(dates <= phase_1_end ~ 1,
                           dates <= phase_2_end ~ 2,
                           dates <= phase_3_end ~ 3,
                           dates <= phase_T3_end ~ 4,
                           .default = 0)
         , phase3_target = ifelse(queue_size <= current_target_queue & phase == 3, 1,0)
         , phaseT3_target = ifelse(queue_size <= future_target_queue_20 & phase > 2,1,0)
  )


# Phases
tno_queue3_40 <-
  tno_queue3_40 %>% 
  mutate(phase = case_when(dates <= phase_1_end ~ 1,
                           dates <= phase_2_end ~ 2,
                           dates <= phase_3_end ~ 3,
                           dates <= phase_T3_end ~ 4,
                           .default = 0)
         , phase3_target = ifelse(queue_size <= current_target_queue & phase == 3, 1,0)
         , phaseT3_target = ifelse(queue_size <= future_target_queue_40 & phase > 2,1,0)
  )


# Join waiting list

#tno_wl2 <- wl_join(tno_sim1, tno_sim2)
#tno_wl3 <- wl_join(tno_wl2, tno_sim3)

wl_stats(tno_sim3)





# How much resource could be freed?

# Calculate target capacity using Kingman/Marchal's Formula:
# capacity = demand + (cvd**2 + cvc**2) / waiting_time

# How long to meet 92% patients < 18 weeks?
# If we want the average wait to be within a target wait of 18 weeks
# For approx 2% chance of going over target
mean_wait6 <- calc_target_mean_wait(18, factor =6)
mean_wait4 <- calc_target_mean_wait(18, factor =4)
mean_wait3 <- calc_target_mean_wait(18, factor =3)

# Target capacity after difference
current_target_capacity <- calc_target_capacity(demand_phase3, 18, factor = 3)
future_target_capacity_0 <- calc_target_capacity(demand_phaseT3_0, 18, factor = 3)
future_target_capacity_20 <- calc_target_capacity(demand_phaseT3_20, 18, factor = 3)
future_target_capacity_40 <- calc_target_capacity(demand_phaseT3_40, 18, factor = 3)
  
# capacity that release once implemented:
  
weekly_capacity_release_0 <- current_target_capacity - future_target_capacity_0
weekly_capacity_release_20 <- current_target_capacity - future_target_capacity_20
weekly_capacity_release_40 <- current_target_capacity - future_target_capacity_40



#  How long does it take to get to queue < target length

# tno_queue3 <-
#   tno_queue3 %>% 





# Plot with implementation at March 27, then add 20% drop in demand

#tno_combined_queue <- wl_queue_size(tno_wl3)
  
phase_2_reach_0 <- 
  tno_queue3_0 %>% 
  filter(phase3_target == 1) %>% 
  head(1) %>%
  mutate(sim = "Current") %>% 
  select(dates, queue_size, sim) 

# phase_2_reach_20 <- 
#   tno_queue3_20 %>% 
#   filter(phase2_target == 1) %>% 
#   head(1) %>%
#   mutate(sim = "20% reduction") %>% 
#   select(dates, queue_size, sim) 

# phase_2_reach_40 <- 
#   tno_queue3_40 %>% 
#   filter(phase2_target == 1) %>% 
#   head(1) %>%
#   mutate(sim = "40% reduction") %>% 
#   select(dates, queue_size, sim) 
# 
# 
phase_3_reach_0 <- 
  tno_queue3_0 %>% 
  filter(phaseT3_target == 1) %>% 
  head(1) %>%  
  mutate(sim = "0% reduction") %>% 
  select(dates, queue_size, sim) 

phase_3_reach_20 <- 
  tno_queue3_20 %>% 
  filter(phaseT3_target == 1) %>% 
  head(1) %>% 
  mutate(sim = "20% reduction") %>% 
  select(dates, queue_size, sim) 

phase_3_reach_40 <- 
  tno_queue3_40 %>% 
  filter(phaseT3_target == 1) %>% 
  head(1) %>%  
  mutate(sim = "40% reduction") %>% 
  select(dates, queue_size, sim) 


ggplot(tno_queue3_0, aes(dates, queue_size)) +
  geom_line() +
  geom_vline(xintercept = c(phase_2_start, phase_3_start), col=c("dodgerblue2", "red"), linetype = "dashed")+
  #geom_hline(yintercept = current_target_queue, linetype = "dashed")+
  #geom_hline(yintercept = future_target_queue, linetype = "dashed")+
  geom_point(data=phase_2_reach_0, shape=5, col = "dodgerblue2", size = 5, stroke = 2)+
  geom_point(data=phase_3_reach_0, shape=4, col = "red", size = 5, stroke = 2)+
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = "Full modelled period",
    y = "Queue Size",
    x = "Month"
  )+
  scale_y_continuous(labels = comma)

ggplot(tno_queue3_20, aes(dates, queue_size)) +
  geom_line() +
  geom_vline(xintercept = c(phase_2_start, phase_3_start), col=c("dodgerblue2", "red"), linetype = "dashed")+
  #geom_hline(yintercept = current_target_queue, linetype = "dashed")+
  #geom_hline(yintercept = future_target_queue, linetype = "dashed")+
  geom_point(data=phase_2_reach_0, shape=4, col = "dodgerblue2", size = 5, stroke = 2)+
  geom_point(data=phase_3_reach_20, shape=4, col = "red", size = 5, stroke = 2)+
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = "Full modelled period",
    y = "Queue Size",
    x = "Month"
  )+
  scale_y_continuous(labels = comma)


ggplot(tno_queue3_40, aes(dates, queue_size)) +
  geom_line() +
  geom_vline(xintercept = c(phase_2_start, phase_3_start), col=c("dodgerblue2", "red"), linetype = "dashed")+
  #geom_hline(yintercept = current_target_queue, linetype = "dashed")+
  #geom_hline(yintercept = future_target_queue, linetype = "dashed")+
  geom_point(data=phase_2_reach_0, shape=4, col = "dodgerblue2", size = 5, stroke = 2)+
  geom_point(data=phase_3_reach_0, shape=4, col = "red", size = 5, stroke = 2)+
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = "Full modelled period",
    y = "Queue Size",
    x = "Month"
  )+
  scale_y_continuous(labels = comma)


targets <-
  phase_2_reach_0 %>% 
  union(phase_3_reach_20) %>% 
  union(phase_3_reach_40) %>% 
  union(phase_3_reach_0) %>% 
  union(phase_3_reach_20) %>% 
  union(phase_3_reach_40) %>% 
  mutate(sim = ifelse(dates < phase_3_start, "Current", sim))
  


tno_queue3_0 %>% 
  union(tno_queue3_20) %>% 
  union(tno_queue3_40) %>% 
  mutate(sim = ifelse(dates < phase_3_start, "Current", sim)) %>% 
  filter(year(dates)>2023) %>% 
  
  ggplot(aes(dates, queue_size, col = sim)) +  
  scale_colour_viridis_d()+
  geom_line(alpha = 0.5) +
  geom_vline(xintercept = c(phase_2_start, phase_3_start), linetype = "dashed")+
  #geom_hline(yintercept = current_target_queue, linetype = "dashed")+
  #geom_hline(yintercept = future_target_queue, linetype = "dashed")+
  #geom_point(data=targets, shape=4, size = 5, stroke = 2)
  geom_point(data=phase_2_reach_0, shape=4, size = 5, stroke = 2)+
  geom_point(data=phase_3_reach_0, shape=4, size = 5, stroke = 2) +
  geom_point(data=phase_3_reach_20, shape=4, size = 5, stroke = 2) +
  geom_point(data=phase_3_reach_40, shape=4, size = 5, stroke = 2) +
  #geom_point(data=phase_3_reach_0, shape=4, col = "red", size = 5, stroke = 2)+

  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = "Crosses represent reaching the target queue size",
    y = "Queue Size",
    x = "Month",
    colour = "Simulation"
  )+
  scale_y_continuous(labels = comma)
  #geom_line(data = tno_queue3_20, linetype = "dashed") +
  #geom_line(data = tno_queue3_40, linetype = "dotted") +



#to_save <- list(tno_sim1, tno_sim2, tno_sim3)
#lapply(to_save,function(x){saveRDS(x, file = paste0("/data/",x,".RDS"))})

#saveRDS(tno_sim3_0, "./data/tno_uhb_sim3_0.rds")
#saveRDS(tno_sim3_20, "./data/tno_uhb_sim3_20.rds")
#saveRDS(tno_sim3_40, "./data/tno_uhb_sim3_40.rds")
#saveRDS(tno_sim2, "./data/tno_uhb_sim2.rds")
#saveRDS(tno_uhb_sim1, "./data/tno_uhb_sim1.rds")

tno_sim1 <- readRDS("./data/tno_sim1.rds")
tno_sim2 <- readRDS("./data/tno_sim2.rds")
tno_sim3_0 <- readRDS("./data/tno_sim3_0.rds")
tno_sim3_20 <- readRDS("./data/tno_sim3_20.rds")
tno_sim3_40 <- readRDS("./data/tno_sim3_40.rds")



############################################################################################

# Same thing for UHB and ROH

############################################################################################

