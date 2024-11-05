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




control_periods <- 
tibble::tribble(
        ~Start,         ~end, ~Period,   ~WL, ~Adds, ~Removes, ~Adds_20, ~Adds_40,
  "01/10/2022", "31/12/2022",      1L, 3903L,  608L,     300L,     608L,     608L,
  "01/01/2023", "31/03/2023",      2L, 4222L,  338L,     314L,     338L,     338L,
  "01/04/2023", "30/06/2023",      3L, 4442L,  332L,     323L,     332L,     332L,
  "01/07/2023", "30/09/2023",      4L, 4709L,  379L,     343L,     379L,     379L,
  "01/10/2023", "31/12/2023",      5L, 4814L,  336L,     354L,     336L,     336L,
  "01/01/2024", "31/03/2024",      6L, 4456L,  302L,     342L,     302L,     302L,
  "01/04/2024", "30/06/2024",      7L, 4007L,  308L,     323L,     308L,     308L,
  "01/07/2024", "30/09/2024",      8L, 4082L,  348L,     357L,     348L,     348L,
  "01/10/2024", "31/03/2026",      9L, 4082L,  348L,     357L,     348L,     348L,
  "01/04/2025", "31/03/2026",     10L,    NA,  351L,     372L,     351L,     351L,
  "01/04/2026", "31/12/2027",     11L,    NA,  355L,     372L,     355L,     355L,
  "01/01/2027", "31/01/2027",     12L,    NA,  358L,     372L,     347L,     335L,
  "01/02/2027", "28/02/2027",     13L,    NA,  358L,     372L,     335L,     311L,
  "01/03/2027", "31/03/2027",     14L,    NA,  358L,     372L,     323L,     287L,
  "01/04/2027", "30/04/2027",     15L,    NA,  358L,     372L,     311L,     263L,
  "01/05/2027", "31/05/2027",     16L,    NA,  358L,     372L,     299L,     239L,
  "01/06/2027", "30/06/2027",     17L,    NA,  358L,     372L,     287L,     215L,
  "01/07/2027", "31/03/2028",     18L,    NA,  362L,     372L,     290L,     217L,
  "01/04/2028", "31/03/2029",     19L,    NA,  365L,     372L,     292L,     219L
  )




# When system comes in December 27, T3
# When system comes full 18-month later, T4
# 

control_periods$Start <- as.Date(control_periods$Start, format = "%d/%m/%Y")
control_periods$end <- as.Date(control_periods$end, format = "%d/%m/%Y")

control_periods2 <- 
tibble::tribble(
        ~Start,         ~end, ~Period,   ~WL, ~Adds, ~Removes, ~Adds_20, ~Adds_40,
  "01/10/2022", "31/12/2022",      1L, 3903L,  608L,     300L,     608L,     608L,
  "01/01/2023", "31/03/2023",      2L, 4222L,  338L,     314L,     338L,     338L,
  "01/04/2023", "30/06/2023",      3L, 4442L,  332L,     323L,     332L,     332L,
  "01/07/2023", "30/09/2023",      4L, 4709L,  379L,     343L,     379L,     379L,
  "01/10/2023", "31/12/2023",      5L, 4814L,  336L,     354L,     336L,     336L,
  "01/01/2024", "31/03/2024",      6L, 4456L,  302L,     342L,     302L,     302L,
  "01/04/2024", "30/06/2024",      7L, 4007L,  308L,     323L,     308L,     308L,
  "01/07/2024", "30/09/2024",      8L, 4082L,  348L,     357L,     348L,     348L,
  "01/10/2024", "31/03/2026",      9L, 4082L,  348L,     357L,     348L,     348L,
  "01/04/2025", "31/03/2026",     10L,    NA,  349L,     372L,     349L,     349L,
  "01/04/2026", "31/12/2027",     11L,    NA,  349L,     372L,     349L,     349L,
  "01/01/2027", "31/01/2027",     12L,    NA,  349L,     372L,     337L,     326L,
  "01/02/2027", "28/02/2027",     13L,    NA,  349L,     372L,     326L,     322L,
  "01/03/2027", "31/03/2027",     14L,    NA,  349L,     372L,     314L,     279L,
  "01/04/2027", "30/04/2027",     15L,    NA,  349L,     372L,     302L,     256L,
  "01/05/2027", "31/05/2027",     16L,    NA,  349L,     372L,     291L,     248L,
  "01/06/2027", "31/03/2028",     17L,    NA,  349L,     372L,     279L,     209L,
  "01/07/2027", "31/03/2028",     18L,    NA,  349L,     372L,     279L,     209L,
  "01/04/2028", "31/03/2029",     18L,    NA,  349L,     372L,     279L,     209L
  )






control_periods2$Start <- as.Date(control_periods2$Start, format = "%d/%m/%Y")
control_periods2$end <- as.Date(control_periods2$end, format = "%d/%m/%Y")



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
  wl_queue_size(tno_sim19)

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
tno_queue19 %>% 
  filter(meet_future_0 == 1) %>% 
  slice_head()

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
  
  geom_point(data=target_20_date, shape=4, col = colours[2], size = 5, stroke = 2)+
  geom_point(data=target_40_date, shape=4, col = colours[3], size = 5, stroke = 2)+
  
  #geom_vline(data = programme_dts, aes(xintercept = dates), col="red", linetype="dashed")+
  geom_text(x = as.Date("01-04-2027", format = "%d-%m-%Y")
            , label="CC \nrollout"
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
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = "Dotted line represents target queue size \n
    Green = current demand projected forward, Orange = 20% demand reduced due to CC, Purple = 40% demand reduced due to CC",
    y = "Queue Size",
    x = "Month"
  )+
  theme(axis.text.x = element_text(angle=90))

with_popn_growth


##############################################

# without popn growth

set.seed(124)
tno_sim1 <-
  wl_simulator(control_periods2$Start[1]
               , control_periods2$end[1]
               , control_periods2$Adds[1]
               , control_periods2$Removes[1])

tno_queue1 <-
  wl_queue_size(tno_sim1)

ggplot(tno_queue1, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", control_periods2$Removes[1], ", Demand=", control_periods2$Adds[1]),
    y = "Queue Size",
    x = "Month"
  )

tno_sim_20_1 <- tno_sim1
tno_sim_40_1 <-  tno_sim1



# Loop through and simulate each section
for(i in seq(2,14)){
  set.seed(124)
  eval(
    call("<-"
         , as.name(paste0("tno_sim",as.character(i)))
         , wl_simulator(control_periods2$Start[i]
                        , control_periods2$end[i]
                        , control_periods2$Adds[i]
                        , control_periods2$Removes[i]
                        ,  waiting_list = get(paste0("tno_sim", as.character(i-1))))
    )
  )
  
}

# Loop through and simulate each section
for(i in seq(2,14)){
  set.seed(124)
  eval(
    call("<-"
         , as.name(paste0("tno_sim_20_",as.character(i)))
         , wl_simulator(control_periods2$Start[i]
                        , control_periods2$end[i]
                        , control_periods2$Adds_20[i]
                        , control_periods2$Removes[i]
                        ,  waiting_list = get(paste0("tno_sim_20_", as.character(i-1))))
    )
  )
  
}

# Loop through and simulate each section
for(i in seq(2,14)){
  set.seed(123)
  eval(
    call("<-"
         , as.name(paste0("tno_sim_40_",as.character(i)))
         , wl_simulator(control_periods2$Start[i]
                        , control_periods2$end[i]
                        , control_periods2$Adds_40[i]
                        , control_periods2$Removes[i]
                        ,  waiting_list = get(paste0("tno_sim_40_", as.character(i-1))))
    )
  )
  
}

# Phase 1:  pre-implementation, setting up the queue.  This may not be real, but should end at peak date

# tno_sim1 <-
#   wl_simulator(phase_1_start, phase_1_end, demand_phase1, capacity_phase1)

tno_queue13 <-
  wl_queue_size(tno_sim13)
tno_queue_20_13 <-
  wl_queue_size(tno_sim_20_13)
tno_queue_40_13 <-
  wl_queue_size(tno_sim_40_13)


ggplot(tno_queue13, aes(dates, queue_size)) +
  
  geom_line(col=colours[2], data=tno_queue_20_13) +
  geom_line(col=colours[3], data=tno_queue_40_13) +
  geom_line() +
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%y")+
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    #subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )+
  theme(axis.text.x = element_text(angle=90))



# Target queue size for current and future queue
control_periods2$target_queue_size <- calc_target_queue_size(control_periods2$Adds, 18, factor = 3)
control_periods2$target_queue_size_20 <- calc_target_queue_size(control_periods2$Adds_20, 18, factor = 3)
control_periods2$target_queue_size_40 <- calc_target_queue_size(control_periods2$Adds_40, 18, factor = 3)

target_queue_size <- calc_target_queue_size(tail(control_periods2,1)$Adds, 18, factor = 3)
target_queue_size_20 <- calc_target_queue_size(tail(control_periods2,1)$Adds_20, 18, factor = 3)
target_queue_size_40 <- calc_target_queue_size(tail(control_periods2,1)$Adds_40, 18, factor = 3)


without_popn_growth <-
ggplot(tno_queue13, aes(dates, queue_size)) +
  
  geom_line(col=colours[2], data=tno_queue_20_13) +
  geom_line(col=colours[3], data=tno_queue_40_13) +
  geom_line() +
  
  geom_hline(yintercept = target_queue_size, col = "black", linetype="dashed")+
  geom_hline(yintercept = target_queue_size_20, col = colours[2], linetype="dashed")+
  geom_hline(yintercept = target_queue_size_40, col = colours[3], linetype="dashed")+
  
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%y")+
  labs(
    title = "T&O: First GP referral to first Outpatients waiting list:",
    subtitle = "Dotted line represents target queue size \n
    Black = current demand projected forward, Blue = 20% demand reduced due to CC, Red = 20% demand reduced due to CC",
    y = "Queue Size",
    x = "Month"
  )+
  theme(axis.text.x = element_text(angle=90))


without_popn_growth


######

library(patchwork)

with_popn_growth + without_popn_growth




#############################


# How long to meet 92% patients < 18 weeks?
# If we want the average wait to be within a target wait of 18 weeks
# For approx 2% chance of going over target
mean_wait6 <- calc_target_mean_wait(6, factor = 2)
mean_wait4 <- calc_target_mean_wait(6, factor = 2)
mean_wait3 <- calc_target_mean_wait(6, factor = 2)

# Target capacity after difference
current_target_capacity <- calc_target_capacity(control_periods$Adds[9], 6, factor = 2)
future_target_capacity_0 <- calc_target_capacity(control_periods$Adds[18], 6, factor = 2)
future_target_capacity_20 <- calc_target_capacity(control_periods$Adds_20[18], 6, factor = 2)
future_target_capacity_40 <- calc_target_capacity(control_periods$Adds_40[18], 6, factor = 2)

# capacity that release once implemented:

weekly_capacity_release_0 <- current_target_capacity - future_target_capacity_0
weekly_capacity_release_20 <- current_target_capacity - future_target_capacity_20
weekly_capacity_release_40 <- current_target_capacity - future_target_capacity_40




# Phases 0
tno_queue19 <-
  tno_queue19 %>% 
  mutate( meet_target = ifelse(queue_size <= current_target_capacity, 1,0)
          , meet_future_0 = ifelse(queue_size <= future_target_capacity_0, 1,0)
          )


# Phases 0
tno_queue_20_19 <-
  tno_queue_20_19 %>% 
  mutate( meet_target = ifelse(queue_size <= current_target_capacity, 1,0)
         , meet_future_20 = ifelse(queue_size <= future_target_capacity_20, 1,0)
  )

# Phases 0
tno_queue_40_19 <-
  tno_queue_40_19 %>% 
  mutate( meet_target = ifelse(queue_size <= current_target_capacity, 1,0)
          , meet_future_40 = ifelse(queue_size <= future_target_capacity_40, 1,0)
  )
