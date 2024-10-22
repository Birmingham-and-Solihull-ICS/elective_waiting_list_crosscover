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


# programme timepoints
programme_dts<- 
  data.frame(
    dates = as.Date(c('01/01/2027', '01/07/2027'), '%d/%m/%Y'),
    descr = c("Implementation", "Full Benefits")
  )




############## Naive forecast - last point forward #####################################

control_periods <- 
tibble::tribble(
        ~Start,         ~end, ~Period,    ~WL, ~Adds, ~Removes, ~WL_pressure, ~Adds_20, ~Removes_20, ~Adds_40, ~Removes_40,
  "01/10/2022", "31/12/2022",      1L, 10024L, 1254L,     475L,         2.64,    1254L,        475L,    1254L,        475L,
  "01/01/2023", "31/03/2023",      2L,  9323L,  441L,     479L,         0.92,     441L,        479L,     441L,        479L,
  "01/04/2023", "30/06/2023",      3L,  9205L,  506L,     516L,         0.98,     506L,        516L,     506L,        516L,
  "01/07/2023", "30/09/2023",      4L,  8803L,  465L,     490L,         0.95,     465L,        490L,     465L,        490L,
  "01/10/2023", "31/12/2023",      5L,  7942L,  418L,     519L,         0.81,     418L,        519L,     418L,        519L,
  "01/01/2024", "31/03/2024",      6L,  6691L,  399L,     481L,         0.83,     399L,        481L,     399L,        481L,
  "01/04/2024", "30/06/2024",      7L,  5909L,  415L,     446L,         0.93,     415L,        446L,     415L,        446L,
  "01/07/2024", "30/09/2024",      8L,  5707L,  456L,     495L,         0.92,     456L,        495L,     456L,        495L,
  "01/10/2024", "31/03/2026",      9L,     NA,  456L,     495L,         0.92,     456L,        495L,     456L,        495L,
  "01/04/2025", "31/03/2026",     10L,     NA,  460L,     495L,         0.93,     460L,        495L,     460L,        495L,
  "01/04/2026", "31/12/2027",     11L,     NA,  465L,     495L,         0.94,     465L,        495L,     465L,        495L,
  "01/01/2027", "31/01/2027",     12L,     NA,  469L,     495L,         0.95,     454L,        495L,     438L,        495L,
  "01/02/2027", "28/02/2027",     13L,     NA,  469L,     495L,         0.95,     438L,        495L,     407L,        495L,
  "01/03/2027", "31/03/2027",     14L,     NA,  469L,     495L,         0.95,     423L,        495L,     376L,        495L,
  "01/04/2027", "30/04/2027",     15L,     NA,  469L,     495L,         0.95,     407L,        495L,     344L,        495L,
  "01/05/2027", "31/05/2027",     16L,     NA,  469L,     495L,         0.95,     391L,        495L,     313L,        495L,
  "01/06/2027", "30/06/2027",     17L,     NA,  469L,     495L,         0.95,     376L,        495L,     282L,        495L,
  "01/07/2027", "31/03/2028",     18L,     NA,  474L,     495L,         0.96,     379L,        495L,     284L,        495L,
  "01/04/2028", "31/03/2029",     19L,     NA,  479L,     495L,         0.97,     383L,        495L,     287L,        495L
  )





# When system comes in December 27, T3
# When system comes full 18-month later, T4
# 

control_periods$Start <- as.Date(control_periods$Start, format = "%d/%m/%Y")
control_periods$end <- as.Date(control_periods$end, format = "%d/%m/%Y")



# version 1 with 5% referral growth
# set random number generation to defined start
set.seed(124)

tno_sim1 <-
  wl_simulator(control_periods$Start[1]
               , control_periods$end[1]
               , control_periods$Adds[1]
               , control_periods$Removes[1])

tno_queue1 <-
  wl_queue_size(tno_sim1)

ggplot(tno_queue1, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", control_periods$Removes[1], ", Demand=", control_periods$Adds[1]),
    y = "Queue Size",
    x = "Month"
  )

tail(tno_queue1)

tno_sim_20_1 <- tno_sim1
tno_sim_40_1 <-  tno_sim1


# Loop through and simulate each section
for(i in seq(2,19)){
  set.seed(125)  
  eval(
    call("<-"
         , as.name(paste0("tno_sim",as.character(i)))
         , wl_simulator(control_periods$Start[i]
                        , control_periods$end[i]
                        , control_periods$Adds[i]
                        , control_periods$Removes[i]
                        ,  waiting_list = get(paste0("tno_sim", as.character(i-1))))
    )
  )
  
}

# Loop through and simulate each section
for(i in seq(2,19)){
  set.seed(125)
  eval(
    call("<-"
         , as.name(paste0("tno_sim_20_",as.character(i)))
         , wl_simulator(control_periods$Start[i]
                        , control_periods$end[i]
                        , control_periods$Adds_20[i]
                        , control_periods$Removes[i]
                        ,  waiting_list = get(paste0("tno_sim_20_", as.character(i-1))))
    )
  )
  
}

# Loop through and simulate each section
for(i in seq(2,19)){
  set.seed(125)
  eval(
    call("<-"
         , as.name(paste0("tno_sim_40_",as.character(i)))
         , wl_simulator(control_periods$Start[i]
                        , control_periods$end[i]
                        , control_periods$Adds_40[i]
                        , control_periods$Removes[i]
                        ,  waiting_list = get(paste0("tno_sim_40_", as.character(i-1))))
    )
  )
  
}

# Phase 1:  pre-implementation, setting up the queue.  This may not be real, but should end at peak date

# tno_sim1 <-
#   wl_simulator(phase_1_start, phase_1_end, demand_phase1, capacity_phase1)

tno_queue19 <-
  wl_queue_size(tno_sim19 )

tno_queue_20_19 <-
  wl_queue_size(tno_sim_20_19)

tno_queue_40_19 <-
  wl_queue_size(tno_sim_40_19)

ggplot(tno_queue19, aes(dates, queue_size)) +
  
  geom_line(col=colours[2], data=tno_queue_20_19) +
  geom_line(col=colours[3], data=tno_queue_40_19) +
  geom_line(col=colours[1]) +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    #subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )



# Target queue size for current and future queue
control_periods$target_queue_size <- calc_target_queue_size(control_periods$Adds, 6, factor = 1)
control_periods$target_queue_size_20 <- calc_target_queue_size(control_periods$Adds_20, 6, factor = 1)
control_periods$target_queue_size_40 <- calc_target_queue_size(control_periods$Adds_40, 6, factor = 1)

control_periods$wl_load <- calc_queue_load(control_periods$Adds, control_periods$Removes)
control_periods$wl_load_20 <- calc_queue_load(control_periods$Adds_20, control_periods$Removes)
control_periods$wl_load_40 <- calc_queue_load(control_periods$Adds_40, control_periods$Removes)

target_queue_size <- calc_target_queue_size(tail(control_periods,1)$Adds, 6, factor = 1)
target_queue_size_20 <- calc_target_queue_size(tail(control_periods,1)$Adds_20, 6, factor = 1)
target_queue_size_40 <- calc_target_queue_size(tail(control_periods,1)$Adds_40, 6, factor = 1)



#######
# Calculate stats


# How long to meet 92% patients < 18 weeks?
# If we want the average wait to be within a target wait of 18 weeks
# For approx 2% chance of going over target
mean_wait6 <- calc_target_mean_wait(6, factor = 1)
mean_wait4 <- calc_target_mean_wait(6, factor = 1)
mean_wait3 <- calc_target_mean_wait(6, factor = 1)

# Target capacity after difference
current_target_capacity <- calc_target_capacity(control_periods$Adds[8], 6, factor = 1)
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
  mutate( meet_target = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_0 = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
  )

# Phases 0
tno_queue_20_19 <-
  tno_queue_20_19 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_20 = ifelse((queue_size <= target_queue_size_20) & (dates > as.Date('2023-01-01')), 1,0)
  )
# Phases 0
tno_queue_40_19 <-
  tno_queue_40_19 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_40 = ifelse((queue_size <= target_queue_size_40) & (dates > as.Date('2023-01-01')), 1,0)
  )


# Meeting target on it's own
tno_queue19 %>% 
  filter(meet_target == 1) %>% 
  slice_head()

# Meeting target in future without intervention
target_date <-
  tno_queue19 %>% 
  filter(meet_future_0 == 1) %>% 
  slice_head()
target_date
# Meeting target in future (20% reduction)
target_20_date <-
  tno_queue_20_19 %>% 
  filter(meet_future_20 == 1) %>% 
  slice_head()

target_20_date

# Meeting target in future (40% reduction)
target_40_date <-
  tno_queue_40_19 %>% 
  filter(meet_future_40 == 1) %>% 
  slice_head()

target_40_date

calc_target_capacity(control_periods$Adds[19], target_wait = 6, factor = 1)
calc_target_capacity(control_periods$Adds_20[19], target_wait = 6, factor = 1)
calc_target_capacity(control_periods$Adds_40[19], target_wait = 6, factor = 1)

######
# Plot

with_popn_growth <-
  ggplot(tno_queue19, aes(dates, queue_size)) +
  geom_rect(xmin = programme_dts$dates[1], xmax=programme_dts$dates[2], ymin=0, ymax=Inf, alpha=0.02
            , fill="khaki2")+
  
  geom_line(col=colours[2], data=tno_queue_20_19) +
  geom_line(col=colours[3], data=tno_queue_40_19) +
  geom_line(col=colours[1]) +
  
  geom_hline(yintercept = target_queue_size, col=colours[1], linetype="dashed")+
  geom_hline(yintercept = target_queue_size_20, col = colours[2], linetype="dashed")+
  geom_hline(yintercept = target_queue_size_40, col = colours[3], linetype="dashed")+
  
  #geom_point(data=target_20_date, col = colours[2], linetype="dashed")+
  #geom_point(yintercept = target_20_date, col = colours[3], linetype="dashed")+
  geom_point(data=target_date, shape=4, col = colours[1], size = 5, stroke = 2)+
  geom_point(data=target_20_date, shape=4, col = colours[2], size = 5, stroke = 2)+
  geom_point(data=target_40_date, shape=4, col = colours[3], size = 5, stroke = 2)+
  
  #geom_vline(data = programme_dts, aes(xintercept = dates), col="red", linetype="dashed")+
  geom_text(x = as.Date("01-04-2027", format = "%d-%m-%Y")
            , label="T3 - T4"
            , y=4800
            , col="black"
            , size=3
            , family = "sans"
            #          , fontface = "bold"
  )+
  
  
  scale_x_date(date_breaks = "3 month"
               , date_labels = "%b-%y"
               , limits = c(
                 as.Date("2023-01-01")
                 , as.Date("2029-04-01")
                 
               )
               , expand = c(0,0))+
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list (capacity maintained at last data point):",
    subtitle = "Dotted line represents target queue size \n
    Green = current demand projected forward, Orange = 20% demand reduced, Purple = 40% demand reduced",
    y = "Queue Size",
    x = "Month"
  )+
  theme(axis.text.x = element_text(angle=90))

with_popn_growth



############## 2% clearance #######################################


control_periods_2 <-
tibble::tribble(
        ~Start,         ~end, ~Period,    ~WL, ~Adds, ~Removes, ~WL_pressure, ~Adds_20, ~Removes_20, ~Adds_40, ~Removes_40,
  "01/10/2022", "31/12/2022",      1L, 10024L, 1254L,     475L,         2.64,    1254L,        475L,    1254L,        475L,
  "01/01/2023", "31/03/2023",      2L,  9323L,  441L,     479L,         0.92,     441L,        479L,     441L,        479L,
  "01/04/2023", "30/06/2023",      3L,  9205L,  506L,     516L,         0.98,     506L,        516L,     506L,        516L,
  "01/07/2023", "30/09/2023",      4L,  8803L,  465L,     490L,         0.95,     465L,        490L,     465L,        490L,
  "01/10/2023", "31/12/2023",      5L,  7942L,  418L,     519L,         0.81,     418L,        519L,     418L,        519L,
  "01/01/2024", "31/03/2024",      6L,  6691L,  399L,     481L,         0.83,     399L,        481L,     399L,        481L,
  "01/04/2024", "30/06/2024",      7L,  5909L,  415L,     446L,         0.93,     415L,        446L,     415L,        446L,
  "01/07/2024", "30/09/2024",      8L,  5707L,  456L,     495L,         0.92,     456L,        495L,     456L,        495L,
  "01/10/2024", "31/03/2026",      9L,     NA,  456L,     465L,         0.98,     456L,        465L,     456L,        465L,
  "01/04/2025", "31/03/2026",     10L,     NA,  460L,     470L,         0.98,     460L,        470L,     460L,        470L,
  "01/04/2026", "31/12/2027",     11L,     NA,  465L,     474L,         0.98,     465L,        474L,     465L,        474L,
  "01/01/2027", "31/01/2027",     12L,     NA,  469L,     479L,         0.98,     454L,        463L,     438L,        447L,
  "01/02/2027", "28/02/2027",     13L,     NA,  469L,     479L,         0.98,     438L,        447L,     407L,        415L,
  "01/03/2027", "31/03/2027",     14L,     NA,  469L,     479L,         0.98,     423L,        431L,     376L,        383L,
  "01/04/2027", "30/04/2027",     15L,     NA,  456L,     465L,         0.98,     395L,        403L,     334L,        341L,
  "01/05/2027", "31/05/2027",     16L,     NA,  469L,     479L,         0.98,     391L,        399L,     313L,        319L,
  "01/06/2027", "30/06/2027",     17L,     NA,  469L,     479L,         0.98,     376L,        383L,     282L,        287L,
  "01/07/2027", "31/03/2028",     18L,     NA,  474L,     484L,         0.98,     379L,        387L,     284L,        290L,
  "01/04/2028", "31/03/2029",     19L,     NA,  479L,     488L,         0.98,     383L,        391L,     287L,        293L
  )



  

# When system comes in December 27, T3
# When system comes full 18-month later, T4
# 

control_periods_2$Start <- as.Date(control_periods_2$Start, format = "%d/%m/%Y")
control_periods_2$end <- as.Date(control_periods_2$end, format = "%d/%m/%Y")



# version 1 with 5% referral growth
# set random number generation to defined start
set.seed(124)

tno_sim1_2 <-
  wl_simulator(control_periods_2$Start[1]
               , control_periods_2$end[1]
               , control_periods_2$Adds[1]
               , control_periods_2$Removes[1])

tno_queue1_2 <-
  wl_queue_size(tno_sim1_2)

ggplot(tno_queue1_2, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", control_periods_2$Removes[1], ", Demand=", control_periods_2$Adds[1]),
    y = "Queue Size",
    x = "Month"
  )

tail(tno_queue1_2)

tno_sim_20_1_2 <- tno_sim1_2
tno_sim_40_1_2 <-  tno_sim1_2


# Loop through and simulate each section
for(i in seq(2,19)){
  set.seed(125)  
  eval(
    call("<-"
         , as.name(paste0("tno_sim",as.character(i), "_2"))
         , wl_simulator(control_periods_2$Start[i]
                        , control_periods_2$end[i]
                        , control_periods_2$Adds[i]
                        , control_periods_2$Removes[i]
                        ,  waiting_list = get(paste0("tno_sim", as.character(i-1), "_2")))
    )
  )
  
}

# Loop through and simulate each section
for(i in seq(2,19)){
  set.seed(125)  
  eval(
    call("<-"
         , as.name(paste0("tno_sim_20_",as.character(i), "_2"))
         , wl_simulator(control_periods_2$Start[i]
                        , control_periods_2$end[i]
                        , control_periods_2$Adds_20[i]
                        , control_periods_2$Removes[i]
                        ,  waiting_list = get(paste0("tno_sim_20_", as.character(i-1), "_2")))
    )
  )
}
  

# Loop through and simulate each section
for(i in seq(2,19)){
  set.seed(125)
  eval(
    call("<-"
         , as.name(paste0("tno_sim_40_",as.character(i), "_2"))
         , wl_simulator(control_periods_2$Start[i]
                        , control_periods_2$end[i]
                        , control_periods_2$Adds_40[i]
                        , control_periods_2$Removes[i]
                        ,  waiting_list = get(paste0("tno_sim_40_", as.character(i-1), "_2")))
    )
  )
  
}

# Phase 1:  pre-implementation, setting up the queue.  This may not be real, but should end at peak date

# tno_sim1 <-
#   wl_simulator(phase_1_start, phase_1_end, demand_phase1, capacity_phase1)

tno_queue19_2 <-
  wl_queue_size(tno_sim19_2 )

tno_queue_20_19_2 <-
  wl_queue_size(tno_sim_20_19_2)

tno_queue_40_19_2 <-
  wl_queue_size(tno_sim_40_19_2)

ggplot(tno_queue19_2, aes(dates, queue_size)) +
  
  geom_line(col=colours[2], data=tno_queue_20_19_2) +
  geom_line(col=colours[3], data=tno_queue_40_19_2) +
  geom_line(col=colours[1]) +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    #subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )



# Target queue size for current and future queue
control_periods_2$target_queue_size <- calc_target_queue_size(control_periods_2$Adds, 6, factor = 1)
control_periods_2$target_queue_size_20 <- calc_target_queue_size(control_periods_2$Adds_20, 6, factor = 1)
control_periods_2$target_queue_size_40 <- calc_target_queue_size(control_periods_2$Adds_40, 6, factor = 1)

control_periods_2$wl_load <- calc_queue_load(control_periods_2$Adds, control_periods_2$Removes)
control_periods_2$wl_load_20 <- calc_queue_load(control_periods_2$Adds_20, control_periods_2$Removes)
control_periods_2$wl_load_40 <- calc_queue_load(control_periods_2$Adds_40, control_periods_2$Removes)

target_queue_size_2 <- calc_target_queue_size(tail(control_periods_2,1)$Adds, 6, factor = 1)
target_queue_size_20_2 <- calc_target_queue_size(tail(control_periods_2,1)$Adds_20, 6, factor = 1)
target_queue_size_40_2 <- calc_target_queue_size(tail(control_periods_2,1)$Adds_40, 6, factor = 1)



#######
# Calculate stats


# How long to meet 92% patients < 18 weeks?
# If we want the average wait to be within a target wait of 18 weeks
# For approx 2% chance of going over target
mean_wait6 <- calc_target_mean_wait(6, factor = 1)
mean_wait4 <- calc_target_mean_wait(6, factor = 1)
mean_wait3 <- calc_target_mean_wait(6, factor = 1)

# Target capacity after difference
current_target_capacity_2 <- calc_target_capacity(control_periods_2$Adds[8], 6, factor = 1)
future_target_capacity_0_2 <- calc_target_capacity(control_periods_2$Adds[19], 6, factor = 1)
future_target_capacity_20_2 <- calc_target_capacity(control_periods_2$Adds_20[19], 6, factor = 1)
future_target_capacity_40_2 <- calc_target_capacity(control_periods_2$Adds_40[19], 6, factor = 1)

# capacity that release once implemented:

weekly_capacity_release_0_2 <- current_target_capacity_2 - future_target_capacity_0_2
weekly_capacity_release_20_2 <- current_target_capacity_2 - future_target_capacity_20_2
weekly_capacity_release_40_2 <- current_target_capacity_2 - future_target_capacity_40_2




# Phases 0
tno_queue19_2 <-
  tno_queue19_2 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_0 = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
  )

# Phases 0
tno_queue_20_19_2 <-
  tno_queue_20_19_2 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_20 = ifelse((queue_size <= target_queue_size_20) & (dates > as.Date('2023-01-01')), 1,0)
  )
# Phases 0
tno_queue_40_19_2 <-
  tno_queue_40_19_2 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_40 = ifelse((queue_size <= target_queue_size_40) & (dates > as.Date('2023-01-01')), 1,0)
  )


# Meeting target on it's own
tno_queue19_2 %>% 
  filter(meet_target == 1) %>% 
  slice_head()

# Meeting target in future without intervention
target_date_2 <-
  tno_queue19_2 %>% 
  filter(meet_future_0 == 1) %>% 
  slice_head()

target_date_2
# Meeting target in future (20% reduction)
target_20_date_2 <-
  tno_queue_20_19_2 %>% 
  filter(meet_future_20 == 1) %>% 
  slice_head()

target_20_date_2

# Meeting target in future (40% reduction)
target_40_date_2 <-
  tno_queue_40_19_2 %>% 
  filter(meet_future_40 == 1) %>% 
  slice_head()

target_40_date_2

calc_target_capacity(control_periods_2$Adds[19], target_wait = 6, factor = 1)
calc_target_capacity(control_periods_2$Adds_20[19], target_wait = 6, factor = 1)
calc_target_capacity(control_periods_2$Adds_40[19], target_wait = 6, factor = 1)

######
# Plot

with_popn_growth_2 <-
  ggplot(tno_queue19_2, aes(dates, queue_size)) +
  geom_rect(xmin = programme_dts$dates[1], xmax=programme_dts$dates[2], ymin=0, ymax=Inf, alpha=0.02
            , fill="khaki2")+
  
  geom_line(col=colours[2], data=tno_queue_20_19_2) +
  geom_line(col=colours[3], data=tno_queue_40_19_2) +
  geom_line(col=colours[1]) +
  
  geom_hline(yintercept = target_queue_size_2, col=colours[1], linetype="dashed")+
  geom_hline(yintercept = target_queue_size_20_2, col = colours[2], linetype="dashed")+
  geom_hline(yintercept = target_queue_size_40_2, col = colours[3], linetype="dashed")+
  
  #geom_point(data=target_20_date, col = colours[2], linetype="dashed")+
  #geom_point(yintercept = target_20_date, col = colours[3], linetype="dashed")+
  geom_point(data=target_date_2, shape=4, col = colours[1], size = 5, stroke = 2)+
  geom_point(data=target_20_date_2, shape=4, col = colours[2], size = 5, stroke = 2)+
  geom_point(data=target_40_date_2, shape=4, col = colours[3], size = 5, stroke = 2)+
  
  #geom_vline(data = programme_dts, aes(xintercept = dates), col="red", linetype="dashed")+
  geom_text(x = as.Date("01-04-2027", format = "%d-%m-%Y")
            , label="T3 - T4"
            , y=4800
            , col="black"
            , size=3
            , family = "sans"
            #          , fontface = "bold"
  )+
  
  
  scale_x_date(date_breaks = "3 month"
               , date_labels = "%b-%y"
               , limits = c(
                 as.Date("2023-01-01")
                 , as.Date("2029-04-01")
                 
               )
               , expand = c(0,0))+
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list (2% relief capacity maintained):",
    subtitle = "Dotted line represents target queue size \n
    Green = current demand projected forward, Orange = 20% demand reduced, Purple = 40% demand reduced",
    y = "Queue Size",
    x = "Month"
  )+
  theme(axis.text.x = element_text(angle=90))

with_popn_growth_2


############## 5% clearance #######################################


control_periods_5 <- 
tibble::tribble(
        ~Start,         ~end, ~Period,    ~WL, ~Adds, ~Removes, ~WL_pressure, ~Adds_20, ~Removes_20, ~Adds_40, ~Removes_40,
  "01/10/2022", "31/12/2022",      1L, 10024L, 1254L,     475L,         2.64,    1254L,        475L,    1254L,        475L,
  "01/01/2023", "31/03/2023",      2L,  9323L,  441L,     479L,         0.92,     441L,        479L,     441L,        479L,
  "01/04/2023", "30/06/2023",      3L,  9205L,  506L,     516L,         0.98,     506L,        516L,     506L,        516L,
  "01/07/2023", "30/09/2023",      4L,  8803L,  465L,     490L,         0.95,     465L,        490L,     465L,        490L,
  "01/10/2023", "31/12/2023",      5L,  7942L,  418L,     519L,         0.81,     418L,        519L,     418L,        519L,
  "01/01/2024", "31/03/2024",      6L,  6691L,  399L,     481L,         0.83,     399L,        481L,     399L,        481L,
  "01/04/2024", "30/06/2024",      7L,  5909L,  415L,     446L,         0.93,     415L,        446L,     415L,        446L,
  "01/07/2024", "30/09/2024",      8L,  5707L,  456L,     495L,         0.92,     456L,        495L,     456L,        495L,
  "01/10/2024", "31/03/2026",      9L,     NA,  456L,     480L,         0.95,     456L,        480L,     456L,        480L,
  "01/04/2025", "31/03/2026",     10L,     NA,  460L,     485L,         0.95,     460L,        485L,     460L,        485L,
  "01/04/2026", "31/12/2027",     11L,     NA,  465L,     489L,         0.95,     465L,        489L,     465L,        489L,
  "01/01/2027", "31/01/2027",     12L,     NA,  469L,     494L,         0.95,     454L,        478L,     438L,        461L,
  "01/02/2027", "28/02/2027",     13L,     NA,  469L,     494L,         0.95,     438L,        461L,     407L,        428L,
  "01/03/2027", "31/03/2027",     14L,     NA,  469L,     494L,         0.95,     423L,        445L,     376L,        395L,
  "01/04/2027", "30/04/2027",     15L,     NA,  469L,     494L,         0.95,     407L,        428L,     344L,        362L,
  "01/05/2027", "31/05/2027",     16L,     NA,  469L,     494L,         0.95,     391L,        412L,     313L,        329L,
  "01/06/2027", "30/06/2027",     17L,     NA,  469L,     494L,         0.95,     376L,        395L,     282L,        297L,
  "01/07/2027", "31/03/2028",     18L,     NA,  474L,     499L,         0.95,     379L,        399L,     284L,        299L,
  "01/04/2028", "31/03/2029",     19L,     NA,  479L,     504L,         0.95,     383L,        403L,     287L,        302L
  )



# When system comes in December 27, T3
# When system comes full 18-month later, T4
# 

control_periods_5$Start <- as.Date(control_periods_5$Start, format = "%d/%m/%Y")
control_periods_5$end <- as.Date(control_periods_5$end, format = "%d/%m/%Y")



# version 1 with 5% referral growth
# set random number generation to defined start
set.seed(124)

tno_sim1_5 <-
  wl_simulator(control_periods_5$Start[1]
               , control_periods_5$end[1]
               , control_periods_5$Adds[1]
               , control_periods_5$Removes[1])

tno_queue1_5 <-
  wl_queue_size(tno_sim1_5)

ggplot(tno_queue1_5, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", control_periods_5$Removes[1], ", Demand=", control_periods_5$Adds[1]),
    y = "Queue Size",
    x = "Month"
  )

tail(tno_queue1_5)

tno_sim_20_1_5 <- tno_sim1_5
tno_sim_40_1_5 <-  tno_sim1_5


# Loop through and simulate each section
for(i in seq(2,19)){
  set.seed(125)  
  eval(
    call("<-"
         , as.name(paste0("tno_sim",as.character(i), "_5"))
         , wl_simulator(control_periods_5$Start[i]
                        , control_periods_5$end[i]
                        , control_periods_5$Adds[i]
                        , control_periods_5$Removes[i]
                        ,  waiting_list = get(paste0("tno_sim", as.character(i-1), "_5")))
    )
  )
  
}

# Loop through and simulate each section
for(i in seq(2,19)){
  set.seed(125)  
  eval(
    call("<-"
         , as.name(paste0("tno_sim_20_",as.character(i), "_5"))
         , wl_simulator(control_periods_5$Start[i]
                        , control_periods_5$end[i]
                        , control_periods_5$Adds_20[i]
                        , control_periods_5$Removes[i]
                        ,  waiting_list = get(paste0("tno_sim_20_", as.character(i-1), "_5")))
    )
  )
}


# Loop through and simulate each section
for(i in seq(2,19)){
  set.seed(125)
  eval(
    call("<-"
         , as.name(paste0("tno_sim_40_",as.character(i), "_5"))
         , wl_simulator(control_periods_5$Start[i]
                        , control_periods_5$end[i]
                        , control_periods_5$Adds_40[i]
                        , control_periods_5$Removes[i]
                        ,  waiting_list = get(paste0("tno_sim_40_", as.character(i-1), "_5")))
    )
  )
  
}

# Phase 1:  pre-implementation, setting up the queue.  This may not be real, but should end at peak date

# tno_sim1 <-
#   wl_simulator(phase_1_start, phase_1_end, demand_phase1, capacity_phase1)

tno_queue19_5 <-
  wl_queue_size(tno_sim19_5 )

tno_queue_20_19_5 <-
  wl_queue_size(tno_sim_20_19_5)

tno_queue_40_19_5 <-
  wl_queue_size(tno_sim_40_19_5)

ggplot(tno_queue19_5, aes(dates, queue_size)) +
  
  geom_line(col=colours[2], data=tno_queue_20_19_5) +
  geom_line(col=colours[3], data=tno_queue_40_19_5) +
  geom_line(col=colours[1]) +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    #subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )



# Target queue size for current and future queue
control_periods_5$target_queue_size_5 <- calc_target_queue_size(control_periods_5$Adds, 6, factor = 1)
control_periods_5$target_queue_size_20_5 <- calc_target_queue_size(control_periods_5$Adds_20, 6, factor = 1)
control_periods_5$target_queue_size_40_5 <- calc_target_queue_size(control_periods_5$Adds_40, 6, factor = 1)

control_periods_5$wl_load <- calc_queue_load(control_periods_5$Adds, control_periods_5$Removes)
control_periods_5$wl_load_20 <- calc_queue_load(control_periods_5$Adds_20, control_periods_5$Removes)
control_periods_5$wl_load_40 <- calc_queue_load(control_periods_5$Adds_40, control_periods_5$Removes)

target_queue_size_5 <- calc_target_queue_size(tail(control_periods_5,1)$Adds, 6, factor = 1)
target_queue_size_20_5 <- calc_target_queue_size(tail(control_periods_5,1)$Adds_20, 6, factor = 1)
target_queue_size_40_5 <- calc_target_queue_size(tail(control_periods_5,1)$Adds_40, 6, factor = 1)



#######
# Calculate stats


# How long to meet 92% patients < 18 weeks?
# If we want the average wait to be within a target wait of 18 weeks
# For approx 2% chance of going over target
mean_wait6 <- calc_target_mean_wait(6, factor = 1)
mean_wait4 <- calc_target_mean_wait(6, factor = 1)
mean_wait3 <- calc_target_mean_wait(6, factor = 1)

# Target capacity after difference
current_target_capacity_5 <- calc_target_capacity(control_periods_5$Adds[8], 6, factor = 1)
future_target_capacity_0_5 <- calc_target_capacity(control_periods_5$Adds[19], 6, factor = 1)
future_target_capacity_20_5 <- calc_target_capacity(control_periods_5$Adds_20[19], 6, factor = 1)
future_target_capacity_40_5 <- calc_target_capacity(control_periods_5$Adds_40[19], 6, factor = 1)

# capacity that release once implemented:

weekly_capacity_release_0_5 <- current_target_capacity_5 - future_target_capacity_0_5
weekly_capacity_release_20_5 <- current_target_capacity_5 - future_target_capacity_20_5
weekly_capacity_release_40_5 <- current_target_capacity_5 - future_target_capacity_40_5




# Phases 0
tno_queue19_5 <-
  tno_queue19_5 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_0 = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
  )

# Phases 0
tno_queue_20_19_5 <-
  tno_queue_20_19_5 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_20 = ifelse((queue_size <= target_queue_size_20) & (dates > as.Date('2023-01-01')), 1,0)
  )
# Phases 0
tno_queue_40_19_5 <-
  tno_queue_40_19_5 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_40 = ifelse((queue_size <= target_queue_size_40) & (dates > as.Date('2023-01-01')), 1,0)
  )


# Meeting target on it's own
tno_queue19_5 %>% 
  filter(meet_target == 1) %>% 
  slice_head()

# Meeting target in future without intervention
target_date_5 <-
  tno_queue19_5 %>% 
  filter(meet_future_0 == 1) %>% 
  slice_head()
target_date_5
# Meeting target in future (20% reduction)
target_20_date_5 <-
  tno_queue_20_19_5 %>% 
  filter(meet_future_20 == 1) %>% 
  slice_head()

target_20_date_5

# Meeting target in future (40% reduction)
target_40_date_5 <-
  tno_queue_40_19_5 %>% 
  filter(meet_future_40 == 1) %>% 
  slice_head()

target_40_date_5

calc_target_capacity(control_periods_5$Adds[19], target_wait = 6, factor = 1)
calc_target_capacity(control_periods_5$Adds_20[19], target_wait = 6, factor = 1)
calc_target_capacity(control_periods_5$Adds_40[19], target_wait = 6, factor = 1)

######
# Plot

with_popn_growth_5 <-
  ggplot(tno_queue19_5, aes(dates, queue_size)) +
  geom_rect(xmin = programme_dts$dates[1], xmax=programme_dts$dates[2], ymin=0, ymax=Inf, alpha=0.02
            , fill="khaki2")+
  
  geom_line(col=colours[2], data=tno_queue_20_19_5) +
  geom_line(col=colours[3], data=tno_queue_40_19_5) +
  geom_line(col=colours[1]) +
  
  geom_hline(yintercept = target_queue_size_5, col=colours[1], linetype="dashed")+
  geom_hline(yintercept = target_queue_size_20_5, col = colours[2], linetype="dashed")+
  geom_hline(yintercept = target_queue_size_40_5, col = colours[3], linetype="dashed")+
  
  #geom_point(data=target_20_date, col = colours[2], linetype="dashed")+
  #geom_point(yintercept = target_20_date, col = colours[3], linetype="dashed")+
  geom_point(data=target_date_5, shape=4, col = colours[1], size = 5, stroke = 2)+
  geom_point(data=target_20_date_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  geom_point(data=target_40_date_5, shape=4, col = colours[3], size = 5, stroke = 2)+
  
  #geom_vline(data = programme_dts, aes(xintercept = dates), col="red", linetype="dashed")+
  geom_text(x = as.Date("01-04-2027", format = "%d-%m-%Y")
            , label="T3 - T4"
            , y=4800
            , col="black"
            , size=3
            , family = "sans"
            #          , fontface = "bold"
  )+
  
  
  scale_x_date(date_breaks = "3 month"
               , date_labels = "%b-%y"
               , limits = c(
                 as.Date("2023-01-01")
                 , as.Date("2029-04-01")
                 
               )
               , expand = c(0,0))+
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list (5% relief capacity maintained):",
    subtitle = "Dotted line represents target queue size \n
    Green = current demand projected forward, Orange = 20% demand reduced, Purple = 40% demand reduced",
    y = "Queue Size",
    x = "Month"
  )+
  theme(axis.text.x = element_text(angle=90))

with_popn_growth_5


