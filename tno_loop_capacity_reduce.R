#Generate artificial T&O scenario using real data control parameters

library(tidyverse)
library(NHSRwaitinglist)

# set ggplot theme
theme_set(
  theme_minimal()+
    theme(plot.title = element_text(size=12),
          plot.subtitle = element_text(size=9, face="italic")
    )
)

colours <- RColorBrewer::brewer.pal(n = 3, name="Dark2")


r_control_periods <- 
tibble::tribble(
        ~Start,         ~end, ~Period,   ~WL, ~Adds, ~Removes, ~Adds_20, ~Removes_20, ~Adds_40, ~Removes_40,
  "01/10/2022", "31/12/2022",      1L, 3903L,  608L,     300L,     608L,         300,     608L,         300,
  "01/01/2023", "31/03/2023",      2L, 4222L,  338L,     314L,     338L,         314,     338L,         314,
  "01/04/2023", "30/06/2023",      3L, 4442L,  332L,     323L,     332L,         323,     332L,         323,
  "01/07/2023", "30/09/2023",      4L, 4709L,  379L,     343L,     379L,         343,     379L,         343,
  "01/10/2023", "31/12/2023",      5L, 4814L,  336L,     354L,     336L,         354,     336L,         354,
  "01/01/2024", "31/03/2024",      6L, 4456L,  302L,     342L,     302L,         342,     302L,         342,
  "01/04/2024", "30/06/2024",      7L, 4007L,  308L,     323L,     308L,         323,     308L,         323,
  "01/07/2024", "30/09/2024",      8L, 4082L,  348L,     357L,     348L,         357,     348L,         357,
  "01/10/2024", "31/03/2026",      9L, 4082L,  348L,     357L,     348L,         357,     348L,         357,
  "01/04/2025", "31/03/2026",     10L,    NA,  351L,     372L,     351L,         372,     351L,         372,
  "01/04/2026", "31/12/2027",     11L,    NA,  355L,     372L,     355L,         372,     355L,         372,
  "01/01/2027", "31/01/2027",     12L,    NA,  358L,     372L,     347L,      333.87,     335L,      322.34,
  "01/02/2027", "28/02/2027",     13L,    NA,  358L,     372L,     335L,      322.34,     311L,      299.34,
  "01/03/2027", "31/03/2027",     14L,    NA,  358L,     372L,     323L,      310.84,     287L,       276.3,
  "01/04/2027", "30/04/2027",     15L,    NA,  358L,     372L,     311L,      299.34,     263L,      253.26,
  "01/05/2027", "31/05/2027",     16L,    NA,  358L,     372L,     299L,       287.8,     239L,      230.26,
  "01/06/2027", "30/06/2027",     17L,    NA,  358L,     372L,     287L,       276.3,     215L,      207.22,
  "01/07/2027", "31/03/2028",     18L,    NA,  362L,     372L,     290L,      281.69,     217L,      211.27,
  "01/04/2028", "31/03/2029",     19L,    NA,  365L,     372L,     292L,      287.13,     219L,      215.35
  )





# When system comes in December 27, T3
# When system comes full 18-month later, T4
# 

r_control_periods$Start <- as.Date(r_control_periods$Start, format = "%d/%m/%Y")
r_control_periods$end <- as.Date(r_control_periods$end, format = "%d/%m/%Y")




# version 1 with 5% referral growth
# set random number generation to defined start
set.seed(124)

r_tno_sim1 <-
  wl_simulator(r_control_periods$Start[1]
               , r_control_periods$end[1]
               , r_control_periods$Adds[1]
               , r_control_periods$Removes[1])

r_tno_queue1 <-
  wl_queue_size(r_tno_sim1)

ggplot(r_tno_queue1, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", r_control_periods$Removes[1], ", Demand=", r_control_periods$Adds[1]),
    y = "Queue Size",
    x = "Month"
  )

tail(r_tno_queue1)

r_tno_sim_20_1 <- r_tno_sim1
r_tno_sim_40_1 <-  r_tno_sim1


# Loop through and simulate each section
for(i in seq(2,19)){
  set.seed(125)  
  eval(
    call("<-"
         , as.name(paste0("r_tno_sim",as.character(i)))
         , wl_simulator(r_control_periods$Start[i]
                        , r_control_periods$end[i]
                        , r_control_periods$Adds[i]
                        , r_control_periods$Removes[i]
                        ,  waiting_list = get(paste0("r_tno_sim", as.character(i-1))))
    )
  )
  
}

# Loop through and simulate each section
for(i in seq(2,19)){
  set.seed(125)
  eval(
    call("<-"
         , as.name(paste0("r_tno_sim_20_",as.character(i)))
         , wl_simulator(r_control_periods$Start[i]
                        , r_control_periods$end[i]
                        , r_control_periods$Adds_20[i]
                        , r_control_periods$Removes_20[i]
                        ,  waiting_list = get(paste0("r_tno_sim_20_", as.character(i-1))))
    )
  )
  
}

# Loop through and simulate each section
for(i in seq(2,19)){
  set.seed(125)
  eval(
    call("<-"
         , as.name(paste0("r_tno_sim_40_",as.character(i)))
         , wl_simulator(r_control_periods$Start[i]
                        , r_control_periods$end[i]
                        , r_control_periods$Adds_40[i]
                        , r_control_periods$Removes_40[i]
                        ,  waiting_list = get(paste0("r_tno_sim_40_", as.character(i-1))))
    )
  )
  
}

# Phase 1:  pre-implementation, setting up the queue.  This may not be real, but should end at peak date

# tno_sim1 <-
#   wl_simulator(phase_1_start, phase_1_end, demand_phase1, capacity_phase1)

r_tno_queue19 <-
  wl_queue_size(r_tno_sim19)

r_tno_queue_20_19 <-
  wl_queue_size(r_tno_sim_20_19)

r_tno_queue_40_19 <-
  wl_queue_size(r_tno_sim_40_19)

ggplot(r_tno_queue19, aes(dates, queue_size)) +
  
  geom_line(col=colours[2], data=r_tno_queue_20_19) +
  geom_line(col=colours[3], data=r_tno_queue_40_19) +
  geom_line(col=colours[1]) +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    #subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )



# Target queue size for current and future queue
r_control_periods$target_queue_size <- calc_target_queue_size(r_control_periods$Adds, 6, factor = 1)
r_control_periods$target_queue_size_20 <- calc_target_queue_size(r_control_periods$Adds_20, 6, factor = 1)
r_control_periods$target_queue_size_40 <- calc_target_queue_size(r_control_periods$Adds_40, 6, factor = 1)

r_control_periods$wl_load <- calc_queue_load(r_control_periods$Adds, r_control_periods$Removes)
r_control_periods$wl_load_20 <- calc_queue_load(r_control_periods$Adds_20, r_control_periods$Removes)
r_control_periods$wl_load_40 <- calc_queue_load(r_control_periods$Adds_40, r_control_periods$Removes)

r_target_queue_size <- calc_target_queue_size(tail(r_control_periods,1)$Adds, 6, factor = 1)
r_target_queue_size_20 <- calc_target_queue_size(tail(r_control_periods,1)$Adds_20, 6, factor = 1)
r_target_queue_size_40 <- calc_target_queue_size(tail(r_control_periods,1)$Adds_40, 6, factor = 1)


r_with_popn_growth <-
  ggplot(r_tno_queue19, aes(dates, queue_size)) +
  
  geom_line(col=colours[2], data=r_tno_queue_20_19) +
  geom_line(col=colours[3], data=r_tno_queue_40_19) +
  geom_line(col=colours[1]) +
  
  geom_hline(yintercept = r_target_queue_size, col=colours[1], linetype="dashed")+
  geom_hline(yintercept = r_target_queue_size_20, col = colours[2], linetype="dashed")+
  geom_hline(yintercept = r_target_queue_size_40, col = colours[3], linetype="dashed")+
  
  scale_x_date(date_breaks = "3 month"
               , date_labels = "%b-%y"
               , limits = c(
                 as.Date("2023-01-01")
                 , as.Date("2029-04-01")
                 
               )
               , expand = c(0,0))+
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = "Dotted line represents target queue size \n
    Green = current demand projected forward, Orange = 20% demand reduced due to CC, Purple = 40% demand reduced due to CC",
    y = "Queue Size",
    x = "Month"
  )+
  theme(axis.text.x = element_text(angle=90))

r_with_popn_growth


#######
# Calculate stats


# How long to meet 92% patients < 18 weeks?
# If we want the average wait to be within a target wait of 18 weeks
# For approx 2% chance of going over target
mean_wait6 <- calc_target_mean_wait(6, factor = 1)
mean_wait4 <- calc_target_mean_wait(6, factor = 1)
mean_wait3 <- calc_target_mean_wait(6, factor = 1)

# Target capacity after difference
current_target_capacity <- calc_target_capacity(control_periods$Adds[9], 6, factor = 1)
future_target_capacity_0 <- calc_target_capacity(control_periods$Adds[19], 6, factor = 1)
future_target_capacity_20 <- calc_target_capacity(control_periods$Adds_20[19], 6, factor = 1)
future_target_capacity_40 <- calc_target_capacity(control_periods$Adds_40[19], 6, factor = 1)

# capacity that release once implemented:

weekly_capacity_release_0 <- current_target_capacity - future_target_capacity_0
weekly_capacity_release_20 <- current_target_capacity - future_target_capacity_20
weekly_capacity_release_40 <- current_target_capacity - future_target_capacity_40




# Phases 0
tno_queue19 <-
  tno_queue19 %>% 
  mutate( meet_target = ifelse((queue_size <= current_target_capacity) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_0 = ifelse((queue_size <= future_target_capacity_0) & (dates > as.Date('2023-01-01')), 1,0)
  )

# Phases 0
tno_queue_20_19 <-
  tno_queue_20_19 %>% 
  mutate( meet_target = ifelse((queue_size <= current_target_capacity) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_20 = ifelse((queue_size <= future_target_capacity_20) & (dates > as.Date('2023-01-01')), 1,0)
  )
# Phases 0
tno_queue_40_19 <-
  tno_queue_40_19 %>% 
  mutate( meet_target = ifelse((queue_size <= current_target_capacity) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_40 = ifelse((queue_size <= future_target_capacity_40) & (dates > as.Date('2023-01-01')), 1,0)
  )


# Meeting target on it's own
tno_queue19 %>% 
  filter(meet_target == 1) %>% 
  slice_head()

# Meeting target in future without intervention
tno_queue19 %>% 
  filter(meet_future_0 == 1) %>% 
  slice_head()

# Meeting target in future (20% reduction)
tno_queue_20_19 %>% 
  filter(meet_future_20 == 1) %>% 
  slice_head()

# Meeting target in future (40% reduction)
tno_queue_40_19 %>% 
  filter(meet_future_40 == 1) %>% 
  slice_head()

calc_target_capacity(365, target_wait = 6, factor = 1)

