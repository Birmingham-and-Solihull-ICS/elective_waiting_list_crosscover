#Generate artificial ENT scenario using real data control parameters
library(BSOLTheme)
library(tidyverse)
library(NHSRwaitinglist)
library(scales)
library(extrafont)
library(ggtext)
library(zoo)


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
    startdate = as.Date(c('01/07/2026', '01/01/2027'#, "02/01/2027"
    ), '%d/%m/%Y'),
    enddate = as.Date(c('31/12/2026', '01/01/2027'#, "31/03/2029"
    ), '%d/%m/%Y'),
    descr = c("T2", "T3"
              # , "T4"
    )
  )




############## Naive forecast - last point forward #####################################

control_periods <- 
  tibble::tribble(
          ~Start,         ~end, ~Period,    ~WL, ~Adds, ~Removes, ~WL_pressure, ~Adds_20, ~Removes_20, ~Adds_40, ~Removes_40,
    "01/10/2022", "31/12/2022",      1L, 16284L, 1629L,     430L,         3.79,    1629L,        430L,    1629L,        430L,
    "01/01/2023", "31/03/2023",      2L, 15566L,  394L,     392L,            1,     394L,        392L,     394L,        392L,
    "01/04/2023", "30/06/2023",      3L, 15630L,  376L,     423L,         0.89,     376L,        423L,     376L,        423L,
    "01/07/2023", "30/09/2023",      4L, 15074L,  360L,     441L,         0.82,     360L,        441L,     360L,        441L,
    "01/10/2023", "31/12/2023",      5L, 14045L,  387L,     402L,         0.96,     387L,        402L,     387L,        402L,
    "01/01/2024", "31/03/2024",      6L, 13354L,  401L,     462L,         0.87,     401L,        462L,     401L,        462L,
    "01/04/2024", "30/06/2024",      7L, 13447L,  395L,     404L,         0.98,     395L,        404L,     395L,        404L,
    "01/07/2024", "30/09/2024",      8L, 12025L,  396L,     494L,          0.8,     396L,        494L,     396L,        494L,
    "01/10/2024", "31/03/2025",      9L,     NA,  396L,     494L,          0.8,     396L,        494L,     396L,        494L,
    "01/04/2025", "31/03/2026",     10L,     NA,  400L,     494L,         0.81,     400L,        494L,     400L,        494L,
    "01/04/2026", "30/06/2026",     11L,     NA,  404L,     494L,         0.82,     404L,        494L,     404L,        494L,
    "01/07/2026", "30/09/2026",     12L,     NA,  404L,     494L,         0.82,     396L,        494L,     388L,        494L,
    "01/10/2026", "31/12/2026",     13L,     NA,  404L,     494L,         0.82,     388L,        494L,     372L,        494L,
    "01/01/2027", "31/03/2027",     14L,     NA,  404L,     494L,         0.82,     323L,        494L,     243L,        494L,
    "01/04/2027", "31/03/2028",     15L,     NA,  408L,     494L,         0.83,     327L,        494L,     245L,        494L,
    "01/04/2028", "31/03/2029",     16L,     NA,  412L,     494L,         0.83,     330L,        494L,     247L,        494L,
    "01/04/2029", "31/03/2030",     17L,     NA,  416L,     494L,         0.84,     333L,        494L,     250L,        494L
    )



# When system comes in December 27, T3
# When system comes full 18-month later, T4
# 

control_periods$Start <- as.Date(control_periods$Start, format = "%d/%m/%Y")
control_periods$end <- as.Date(control_periods$end, format = "%d/%m/%Y")


# version 1 with 5% referral growth
# set random number generation to defined start
set.seed(124)

ent_sim1 <-
  wl_simulator(control_periods$Start[1]
               , control_periods$end[1]
               , control_periods$Adds[1]
               , control_periods$Removes[1])

ent_queue1 <-
  wl_queue_size(ent_sim1)

ggplot(ent_queue1, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "ENT: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", control_periods$Removes[1], ", Demand=", control_periods$Adds[1]),
    y = "Queue Size",
    x = "Month"
  )

tail(ent_queue1)

ent_sim_20_1 <- ent_sim1
ent_sim_40_1 <-  ent_sim1


# Loop through and simulate each section
# altered to smooth out implementation period that doesn't affect the no-change scenario. Now 14 steps, not 19.
for(i in seq(2,17)){
  set.seed(125)  
  eval(
    call("<-"
         , as.name(paste0("ent_sim",as.character(i)))
         , wl_simulator(control_periods$Start[i]
                        , control_periods$end[i]
                        , control_periods$Adds[i]
                        , control_periods$Removes[i]
                        ,  waiting_list = get(paste0("ent_sim", as.character(i-1))))
    )
  )
  
}

# Loop through and simulate each section
for(i in seq(2,17)){
  set.seed(125)
  eval(
    call("<-"
         , as.name(paste0("ent_sim_20_",as.character(i)))
         , wl_simulator(control_periods$Start[i]
                        , control_periods$end[i]
                        , control_periods$Adds_20[i]
                        , control_periods$Removes_20[i]
                        ,  waiting_list = get(paste0("ent_sim_20_", as.character(i-1))))
    )
  )
  
}

# Loop through and simulate each section
for(i in seq(2,17)){
  set.seed(125)
  eval(
    call("<-"
         , as.name(paste0("ent_sim_40_",as.character(i)))
         , wl_simulator(control_periods$Start[i]
                        , control_periods$end[i]
                        , control_periods$Adds_40[i]
                        , control_periods$Removes_40[i]
                        ,  waiting_list = get(paste0("ent_sim_40_", as.character(i-1))))
    )
  )
  
}

# Phase 1:  pre-implementation, setting up the queue.  This may not be real, but should end at peak date

# ent_sim1 <-
#   wl_simulator(phase_1_start, phase_1_end, demand_phase1, capacity_phase1)

ent_queue16 <-
  wl_queue_size(ent_sim17) #Updated to 17 for extra line

ent_queue_20_16 <-
  wl_queue_size(ent_sim_20_17) #Updated to 17 for extra line

ent_queue_40_16 <-
  wl_queue_size(ent_sim_40_17) #Updated to 17 for extra line

ggplot(ent_queue16, aes(dates, queue_size)) +
  
  geom_line(col=colours[2], data=ent_queue_20_16) +
  geom_line(col=colours[3], data=ent_queue_40_16) +
  geom_line(col=colours[1]) +
  labs(
    title = bquote(bold("ENT:") ~ "First GP referral to first Outpatients waiting list:"),
    #subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )




# Target queue size for current and future queue
control_periods$target_queue_size <- calc_target_queue_size(control_periods$Adds, 6, factor = 1)
#control_periodsa$target_queue_size <- calc_target_queue_size(control_periodsa$Adds, 6, factor = 1)
control_periods$target_queue_size_20 <- calc_target_queue_size(control_periods$Adds_20, 6, factor = 1)
control_periods$target_queue_size_40 <- calc_target_queue_size(control_periods$Adds_40, 6, factor = 1)

control_periods$wl_load <- calc_queue_load(control_periods$Adds, control_periods$Removes)
#control_periodsa$wl_load <- calc_queue_load(control_periodsa$Adds, control_periodsa$Removes)
control_periods$wl_load_20 <- calc_queue_load(control_periods$Adds_20, control_periods$Removes_20)
control_periods$wl_load_40 <- calc_queue_load(control_periods$Adds_40, control_periods$Removes_40)

#target_queue_size <- calc_target_queue_size(tail(control_periods,1)$Adds, 6, factor = 1)
target_queue_size <- calc_target_queue_size(tail(control_periods,1)$Adds, 6, factor = 1)
target_queue_size_20 <- calc_target_queue_size(tail(control_periods,1)$Adds_20, 6, factor = 1)
target_queue_size_40 <- calc_target_queue_size(tail(control_periods,1)$Adds_40, 6, factor = 1)



# Phases 0
ent_queue16 <-
  ent_queue16 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_0 = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
  )

# Phases 0
ent_queue_20_16 <-
  ent_queue_20_16 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_20 = ifelse((queue_size <= target_queue_size_20) & (dates > as.Date('2023-01-01')), 1,0)
  )
# Phases 0
ent_queue_40_16 <-
  ent_queue_40_16 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_40 = ifelse((queue_size <= target_queue_size_40) & (dates > as.Date('2023-01-01')), 1,0)
  )


# Meeting target on it's own
ent_queue16 %>% 
  filter(meet_target == 1) %>% 
  slice_head()

# Meeting target in future without intervention
target_date <-
  ent_queue16 %>% 
  filter(meet_future_0 == 1) %>% 
  slice_head()

target_date

# Meeting target in future (20% reduction)
target_20_date <-
  ent_queue_20_16 %>% 
  filter(meet_future_20 == 1) %>% 
  slice_head()

target_20_date

# Meeting target in future (40% reduction)
target_40_date <-
  ent_queue_40_16 %>% 
  filter(meet_future_40 == 1) %>% 
  slice_head()

target_40_date

calc_target_capacity(control_periods$Adds[17], target_wait = 6, factor = 1) #Updated to 17 for extra line
calc_target_capacity(control_periods$Adds_20[17], target_wait = 6, factor = 1) #Updated to 17 for extra line
calc_target_capacity(control_periods$Adds_40[17], target_wait = 6, factor = 1) #Updated to 17 for extra line


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
#future_target_capacity_0 <- calc_target_capacity(control_periods$Adds[19], 6, factor = 1)
future_target_capacity_0 <- calc_target_capacity(control_periods$Adds[17], 6, factor = 1) #Updated to 17 for extra line
future_target_capacity_20 <- calc_target_capacity(control_periods$Adds_20[17], 6, factor = 1) #Updated to 17 for extra line
future_target_capacity_40 <- calc_target_capacity(control_periods$Adds_40[17], 6, factor = 1) #Updated to 17 for extra line

# capacity that release once implemented:

weekly_capacity_release_0 <- current_target_capacity - future_target_capacity_0
weekly_capacity_release_20 <- current_target_capacity - future_target_capacity_20
weekly_capacity_release_40 <- current_target_capacity - future_target_capacity_40

# what is queue at T3
t3_queue_size_0 <- ent_queue16 %>% filter(dates >= as.Date("01/01/2027", "%d/%m/%Y")) %>% head(1) %>% pull(queue_size)
t3_queue_size_20_0 <- ent_queue_20_16 %>% filter(dates >= as.Date("01/01/2027", "%d/%m/%Y")) %>% head(1) %>% pull(queue_size)
t3_queue_size_40_0 <- ent_queue_40_16 %>% filter(dates >= as.Date("01/01/2027", "%d/%m/%Y")) %>% head(1) %>% pull(queue_size)

# Difference from T3 to target
t3_queue_size_0 - target_queue_size
t3_queue_size_20_0 - target_queue_size_20
t3_queue_size_40_0 - target_queue_size_40

# How long after T3 to target
## Relief capacity at T3 - amount capacity over demand at T3
# 0
control_periods[14,]$Removes - control_periods[14,]$Adds
# How long till target in weeks
(t3_queue_size_0 - target_queue_size) / (control_periods[14,]$Removes - control_periods[14,]$Adds)

# 20
control_periods[14,]$Removes_20 - control_periods[14,]$Adds_20
# How long till target in weeks
(t3_queue_size_20_0 - target_queue_size_20) / (control_periods[14,]$Removes_20 - control_periods[14,]$Adds_20)

#40
control_periods[14,]$Removes_40 - control_periods[14,]$Adds_40
# How long till target in weeks
(t3_queue_size_40_0 - target_queue_size_40) / (control_periods[14,]$Removes_40 - control_periods[14,]$Adds_40)


difftime(target_date$dates, programme_dts$startdate[2], units = "weeks")
difftime(target_20_date$dates, programme_dts$startdate[2], units = "weeks")
difftime(target_40_date$dates, programme_dts$startdate[2], units = "weeks")


######
# Plot

with_popn_growth <-
  ggplot(ent_queue16, aes(dates, queue_size)) +
  
  geom_vline(xintercept = programme_dts$startdate[2], alpha=0.4
             , colour="red")+
  
  annotate("rect", xmin = programme_dts$startdate[1], xmax=programme_dts$enddate[1], ymin=0, ymax=Inf, alpha=0.5
           , fill="khaki1")+
  annotate("rect", xmin = programme_dts$startdate[3], xmax=programme_dts$enddate[3], ymin=0, ymax=Inf, alpha=0.2
           , fill="seagreen")+
  
  geom_line(col=colours[2], data=ent_queue_20_16) +
  geom_line(col=colours[3], data=ent_queue_40_16) +
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
  geom_text(x = as.Date("01-10-2026", format = "%d-%m-%Y")
            , label="T2"
            , y=8000
            , col="black"
            , size=4
            , family = "sans"
            #          , fontface = "bold"
  )+
  geom_text(x = as.Date("01-01-2027", format = "%d-%m-%Y")
            , label="T3"
            , y=8000
            , col="black"
            , size=4
            , family = "sans"
            #          , fontface = "bold"
  )+
  
  geom_text(x = as.Date("01-04-2028", format = "%d-%m-%Y")
            , label="T4"
            , y=8000
            , col="black"
            , size=4
            , family = "sans"
            #          , fontface = "bold"
  )+
  
  
  scale_y_continuous(labels=comma)+
  scale_x_date(date_breaks = "3 month"
               , date_labels = "%b-%y"
               , limits = c(
                 as.Date("2023-01-01")
                 , as.Date("2030-04-01")
                 
               )
               , expand = c(0,0))+
  labs(
    title = bquote(bold("Model 1: ") ~"ENT - First GP referral to first Outpatients waiting list (capacity maintained as at last data point):"),
    subtitle = "    Green = current demand projected forward, Orange = 20% demand reduced, Purple = 40% demand reduced.
    Dotted line represents target queue size",
    y = "Queue Size",
    x = "Month"
  )+
  theme(axis.text.x = element_text(angle=90)
        #,plot.subtitle = element_text(face = "plain")
        ,plot.margin = margin(2,4,2,2, "mm") 
  )


with_popn_growth



############## 2% clearance #######################################


control_periods_2  <-
  tibble::tribble(
          ~Start,         ~end, ~Period,    ~WL, ~Adds, ~Removes, ~WL_pressure, ~Adds_20, ~Removes_20, ~Adds_40, ~Removes_40,
    "01/10/2022", "31/12/2022",      1L, 16284L, 1629L,     430L,         3.79,    1629L,        430L,    1629L,        430L,
    "01/01/2023", "31/03/2023",      2L, 15566L,  394L,     392L,            1,     394L,        392L,     394L,        392L,
    "01/04/2023", "30/06/2023",      3L, 15630L,  376L,     423L,         0.89,     376L,        423L,     376L,        423L,
    "01/07/2023", "30/09/2023",      4L, 15074L,  360L,     441L,         0.82,     360L,        441L,     360L,        441L,
    "01/10/2023", "31/12/2023",      5L, 14045L,  387L,     402L,         0.96,     387L,        402L,     387L,        402L,
    "01/01/2024", "31/03/2024",      6L, 13354L,  401L,     462L,         0.87,     401L,        462L,     401L,        462L,
    "01/04/2024", "30/06/2024",      7L, 13447L,  395L,     404L,         0.98,     395L,        404L,     395L,        404L,
    "01/07/2024", "30/09/2024",      8L, 12025L,  396L,     494L,          0.8,     396L,        494L,     396L,        494L,
    "01/10/2024", "31/03/2025",      9L,     NA,  396L,     404L,         0.98,     396L,        404L,     396L,        404L,
    "01/04/2025", "31/03/2026",     10L,     NA,  400L,     404L,         0.99,     400L,        404L,     400L,        404L,
    "01/04/2026", "30/06/2026",     11L,     NA,  404L,     404L,            1,     404L,        404L,     404L,        404L,
    "01/07/2026", "30/09/2026",     12L,     NA,  404L,     404L,            1,     396L,        404L,     388L,        404L,
    "01/10/2026", "31/12/2026",     13L,     NA,  404L,     404L,            1,     388L,        404L,     372L,        404L,
    "01/01/2027", "31/03/2027",     14L,     NA,  404L,     404L,            1,     323L,        404L,     243L,        404L,
    "01/04/2027", "31/03/2028",     15L,     NA,  408L,     404L,         1.01,     327L,        404L,     245L,        404L,
    "01/04/2028", "31/03/2029",     16L,     NA,  412L,     404L,         1.02,     330L,        404L,     247L,        404L,
    "01/04/2029", "31/03/2030",     17L,     NA,  416L,     404L,         1.03,     333L,        404L,     250L,        404L
    )


# When system comes in December 27, T3
# When system comes full 18-month later, T4
# 

control_periods_2$Start <- as.Date(control_periods_2$Start, format = "%d/%m/%Y")
control_periods_2$end <- as.Date(control_periods_2$end, format = "%d/%m/%Y")



# version 1 with 5% referral growth
# set random number generation to defined start
set.seed(124)

ent_sim1_2 <-
  wl_simulator(control_periods_2$Start[1]
               , control_periods_2$end[1]
               , control_periods_2$Adds[1]
               , control_periods_2$Removes[1])

ent_queue1_2 <-
  wl_queue_size(ent_sim1_2)

ggplot(ent_queue1_2, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "ENT: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", control_periods_2$Removes[1], ", Demand=", control_periods_2$Adds[1]),
    y = "Queue Size",
    x = "Month"
  )

tail(ent_queue1_2)

ent_sim_20_1_2 <- ent_sim1_2
ent_sim_40_1_2 <-  ent_sim1_2


# Loop through and simulate each section
# Updated to 17 for additional year
for(i in seq(2,17)){
  set.seed(125)  
  eval(
    call("<-"
         , as.name(paste0("ent_sim",as.character(i), "_2"))
         , wl_simulator(control_periods_2$Start[i]
                        , control_periods_2$end[i]
                        , control_periods_2$Adds[i]
                        , control_periods_2$Removes[i]
                        ,  waiting_list = get(paste0("ent_sim", as.character(i-1), "_2")))
    )
  )
  
}

# Loop through and simulate each section
for(i in seq(2,17)){
  set.seed(125)  
  eval(
    call("<-"
         , as.name(paste0("ent_sim_20_",as.character(i), "_2"))
         , wl_simulator(control_periods_2$Start[i]
                        , control_periods_2$end[i]
                        , control_periods_2$Adds_20[i]
                        , control_periods_2$Removes_20[i]
                        ,  waiting_list = get(paste0("ent_sim_20_", as.character(i-1), "_2")))
    )
  )
}


# Loop through and simulate each section
for(i in seq(2,17)){
  set.seed(125)
  eval(
    call("<-"
         , as.name(paste0("ent_sim_40_",as.character(i), "_2"))
         , wl_simulator(control_periods_2$Start[i]
                        , control_periods_2$end[i]
                        , control_periods_2$Adds_40[i]
                        , control_periods_2$Removes_40[i]
                        ,  waiting_list = get(paste0("ent_sim_40_", as.character(i-1), "_2")))
    )
  )
  
}

# Phase 1:  pre-implementation, setting up the queue.  This may not be real, but should end at peak date

# ent_sim1 <-
#   wl_simulator(phase_1_start, phase_1_end, demand_phase1, capacity_phase1)

ent_queue16_2 <-
  wl_queue_size(ent_sim17_2) #  # Updated to 17 for additional year

ent_sim16_2$referral_after_t3 <- ifelse(ent_sim16_2$referral < programme_dts$startdate[2], 0, 1)

ent_2_t5_date <-
  ent_sim16_2 %>% 
  filter(referral_after_t3 == 0) %>% 
  summarise(max(removal)) %>% 
  pull()


ent_queue_20_16_2 <-
  wl_queue_size(ent_sim_20_17_2)  # Updated to 17 for additional year

ent_sim_20_16_2$referral_after_t3 <- ifelse(ent_sim_20_16_2$referral < programme_dts$startdate[2], 0, 1)

ent_2_20_t5_date <-
  ent_sim_20_16_2 %>% 
  filter(referral_after_t3 == 0) %>% 
  summarise(max(removal)) %>% 
  pull()


ent_queue_40_16_2 <-
  wl_queue_size(ent_sim_40_17_2)  # Updated to 17 for additional year


ent_sim_40_16_2$referral_after_t3 <- ifelse(ent_sim_40_16_2$referral < programme_dts$startdate[2], 0, 1)

ent_2_40_t5_date <-
  ent_sim_40_16_2 %>% 
  filter(referral_after_t3 == 0) %>% 
  summarise(max(removal)) %>% 
  pull()


ggplot(ent_queue16_2, aes(dates, queue_size)) +
  
  geom_line(col=colours[2], data=ent_queue_20_16_2) +
  geom_line(col=colours[3], data=ent_queue_40_16_2) +
  geom_line(col=colours[1]) +
  labs(
    title = "ENT: First GP referral to first Outpatients waiting list:",
    #subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )

calc_target_capacity(control_periods_2$Adds[17], target_wait = 6, factor = 1)  # Updated to 17 for additional year
#calc_target_capacity(control_periods_2a$Adds[14], target_wait = 6, factor = 1) 
calc_target_capacity(control_periods_2$Adds_20[17], target_wait = 6, factor = 1)  # Updated to 17 for additional year
calc_target_capacity(control_periods_2$Adds_40[17], target_wait = 6, factor = 1)  # Updated to 17 for additional year



# Target queue size for current and future queue
control_periods_2$target_queue_size <- calc_target_queue_size(control_periods_2$Adds, 6, factor = 1)
#control_periods_2a$target_queue_size <- calc_target_queue_size(control_periods_2a$Adds, 6, factor = 1)
control_periods_2$target_queue_size_20 <- calc_target_queue_size(control_periods_2$Adds_20, 6, factor = 1)
control_periods_2$target_queue_size_40 <- calc_target_queue_size(control_periods_2$Adds_40, 6, factor = 1)

control_periods_2$wl_load <- calc_queue_load(control_periods_2$Adds, control_periods_2$Removes)
#control_periods_2a$wl_load <- calc_queue_load(control_periods_2a$Adds, control_periods_2a$Removes)
control_periods_2$wl_load_20 <- calc_queue_load(control_periods_2$Adds_20, control_periods_2$Removes_20)
control_periods_2$wl_load_40 <- calc_queue_load(control_periods_2$Adds_40, control_periods_2$Removes_40)

target_queue_size_2 <- calc_target_queue_size(tail(control_periods_2,1)$Adds, 6, factor = 1)
#target_queue_size_2 <- calc_target_queue_size(tail(control_periods_2a,1)$Adds, 6, factor = 1)
target_queue_size_20_2 <- calc_target_queue_size(tail(control_periods_2,1)$Adds_20, 6, factor = 1)
target_queue_size_40_2 <- calc_target_queue_size(tail(control_periods_2,1)$Adds_40, 6, factor = 1)



# Phases 0
ent_queue16_2 <-
  ent_queue16_2 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_0 = ifelse((queue_size <= target_queue_size) & (dates > as.Date('2023-01-01')), 1,0)
  )

# Phases 0
ent_queue_20_16_2 <-
  ent_queue_20_16_2 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size_2) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_20 = ifelse((queue_size <= target_queue_size_20_2) & (dates > as.Date('2023-01-01')), 1,0)
  )
# Phases 0
ent_queue_40_16_2 <-
  ent_queue_40_16_2 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size_2) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_40 = ifelse((queue_size <= target_queue_size_40_2) & (dates > as.Date('2023-01-01')), 1,0)
  )


# Meeting target on it's own
ent_queue16_2 %>% 
  filter(meet_target == 1) %>% 
  slice_head()

# Meeting target in future without intervention
target_date_2 <-
  ent_queue16_2 %>% 
  filter(meet_future_0 == 1) %>% 
  slice_head()

target_date_2
# Meeting target in future (20% reduction)
target_20_date_2 <-
  ent_queue_20_16_2 %>% 
  filter(meet_future_20 == 1) %>% 
  slice_head()

target_20_date_2

# Meeting target in future (40% reduction)
target_40_date_2 <-
  ent_queue_40_16_2 %>% 
  filter(meet_future_40 == 1) %>% 
  slice_head()

target_40_date_2



# T4 date for 0% sceanrio
ent_t4_date_2 <-
  ent_queue16_2 %>% 
  filter(dates == ent_2_t5_date) %>% 
  slice_head()

# T4 date for 20% sceanrio
ent_t4_date_20_2 <-
  ent_queue_20_16_2 %>% 
  filter(dates == ent_2_20_t5_date) %>% 
  slice_head()


# T4 date for 40% sceanrio
ent_t4_date_40_2 <-
  ent_queue_40_16_2 %>% 
  filter(dates == ent_2_40_t5_date) %>% 
  slice_head()



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
future_target_capacity_0_2 <- calc_target_capacity(control_periods_2$Adds[17], 6, factor = 1)  # Updated to 17 for additional year
future_target_capacity_20_2 <- calc_target_capacity(control_periods_2$Adds_20[17], 6, factor = 1)  # Updated to 17 for additional year
future_target_capacity_40_2 <- calc_target_capacity(control_periods_2$Adds_40[17], 6, factor = 1)  # Updated to 17 for additional year

# capacity that release once implemented:

weekly_capacity_release_0_2 <- current_target_capacity_2 - future_target_capacity_0_2
weekly_capacity_release_20_2 <- current_target_capacity_2 - future_target_capacity_20_2
weekly_capacity_release_40_2 <- current_target_capacity_2 - future_target_capacity_40_2

# what is queue at T3
t3_queue_size_2 <- ent_queue16_2 %>% filter(dates >= as.Date("01/01/2027", "%d/%m/%Y")) %>% head(1) %>% pull(queue_size)
t3_queue_size_20_2 <- ent_queue_20_16_2 %>% filter(dates >= as.Date("01/01/2027", "%d/%m/%Y")) %>% head(1) %>% pull(queue_size)
t3_queue_size_40_2 <- ent_queue_40_16_2 %>% filter(dates >= as.Date("01/01/2027", "%d/%m/%Y")) %>% head(1) %>% pull(queue_size)

# Difference from T3 to target
t3_queue_size_2 - target_queue_size_2
t3_queue_size_20_2 - target_queue_size_20_2
t3_queue_size_40_2 - target_queue_size_40_2

# How long after T3 to target
## Relief capacity at T3 - amount capacity over demand at T3
# 0
control_periods_2[14,]$Removes - control_periods_2[14,]$Adds
# How long till target in weeks
(t3_queue_size_2 - target_queue_size_2) / (control_periods_2[14,]$Removes - control_periods_2[14,]$Adds)

# 20
control_periods_2[14,]$Removes_20 - control_periods_2[14,]$Adds_20
# How long till target in weeks
(t3_queue_size_20_2 - target_queue_size_20_2) / (control_periods_2[14,]$Removes_20 - control_periods_2[14,]$Adds_20)

#40
control_periods_2[14,]$Removes_40 - control_periods_2[14,]$Adds_40
# How long till target in weeks
(t3_queue_size_40_2 - target_queue_size_40_2) / (control_periods_2[14,]$Removes_40 - control_periods_2[14,]$Adds_40)


difftime(target_date_2$dates, programme_dts$startdate[2], units = "weeks")
difftime(target_20_date_2$dates, programme_dts$startdate[2], units = "weeks")
difftime(target_40_date_2$dates, programme_dts$startdate[2], units = "weeks")


######
# Plot
#colours <- RColorBrewer::brewer.pal(n = 3, name="Dark2")

with_popn_growth_2 <-
  ggplot(ent_queue16_2, aes(dates, queue_size)) +
  #geom_rect(xmin = programme_dts$dates[1], xmax=programme_dts$dates[2], ymin=0, ymax=Inf, alpha=0.05
  #          , fill="khaki1")+
  
  geom_vline(xintercept = programme_dts$startdate[2], alpha=0.4
             , colour="red")+
  
  annotate("rect", xmin = programme_dts$startdate[1], xmax=programme_dts$enddate[1], ymin=0, ymax=Inf, alpha=0.5
           , fill="khaki1")+
  annotate("rect", xmin = programme_dts$startdate[3], xmax=programme_dts$enddate[3], ymin=0, ymax=Inf, alpha=0.2
           , fill="seagreen")+
  
  geom_line(col=colours[2], data=ent_queue_20_16_2) +
  geom_line(col=colours[3], data=ent_queue_40_16_2) +
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
  geom_text(x = as.Date("01-10-2026", format = "%d-%m-%Y")
            , label="T2"
            , y=8000
            , col="black"
            , size=4
            , family = "sans"
            #          , fontface = "bold"
  )+
  geom_text(x = as.Date("01-01-2027", format = "%d-%m-%Y")
            , label="T3"
            , y=8000
            , col="black"
            , size=4
            , family = "sans"
            #          , fontface = "bold"
  )+
  
  geom_text(x = as.Date("01-04-2028", format = "%d-%m-%Y")
            , label="T4"
            , y=8000
            , col="black"
            , size=4
            , family = "sans"
            #          , fontface = "bold"
  )+
  
  scale_y_continuous(labels=comma)+
  scale_x_date(date_breaks = "3 month"
               , date_labels = "%b-%y"
               , limits = c(
                 as.Date("2023-01-01")
                 , as.Date("2030-04-01")
                 
               )
               , expand = c(0,0))+
  labs(
    title = bquote(bold("Model 2: ")  ~"ENT - First GP referral to first Outpatients waiting list (2% relief capacity maintained):"),
    subtitle = "    Green = current demand projected forward, Orange = 20% demand reduced, Purple = 40% demand reduced
    Dotted line represents target queue size",
    y = "Queue Size",
    x = "Month"
  )+
  theme(axis.text.x = element_text(angle=90))

with_popn_growth_2


############## 5% clearance #######################################


control_periods_5 <- 
tibble::tribble(
        ~Start,         ~end, ~Period,    ~WL, ~Adds, ~Removes, ~WL_pressure, ~Adds_20, ~Removes_20, ~Adds_40, ~Removes_40,
  "01/10/2022", "31/12/2022",      1L, 16284L, 1629L,     430L,         3.79,    1629L,        430L,    1629L,        430L,
  "01/01/2023", "31/03/2023",      2L, 15566L,  394L,     392L,            1,     394L,        392L,     394L,        392L,
  "01/04/2023", "30/06/2023",      3L, 15630L,  376L,     423L,         0.89,     376L,        423L,     376L,        423L,
  "01/07/2023", "30/09/2023",      4L, 15074L,  360L,     441L,         0.82,     360L,        441L,     360L,        441L,
  "01/10/2023", "31/12/2023",      5L, 14045L,  387L,     402L,         0.96,     387L,        402L,     387L,        402L,
  "01/01/2024", "31/03/2024",      6L, 13354L,  401L,     462L,         0.87,     401L,        462L,     401L,        462L,
  "01/04/2024", "30/06/2024",      7L, 13447L,  395L,     404L,         0.98,     395L,        404L,     395L,        404L,
  "01/07/2024", "30/09/2024",      8L, 12025L,  396L,     494L,          0.8,     396L,        494L,     396L,        494L,
  "01/10/2024", "31/03/2025",      9L,     NA,  396L,     416L,         0.95,     396L,        416L,     396L,        416L,
  "01/04/2025", "31/03/2026",     10L,     NA,  400L,     416L,         0.96,     400L,        416L,     400L,        416L,
  "01/04/2026", "30/06/2026",     11L,     NA,  404L,     416L,         0.97,     404L,        416L,     404L,        416L,
  "01/07/2026", "30/09/2026",     12L,     NA,  404L,     416L,         0.97,     396L,        416L,     388L,        416L,
  "01/10/2026", "31/12/2026",     13L,     NA,  404L,     416L,         0.97,     388L,        416L,     372L,        416L,
  "01/01/2027", "31/03/2027",     14L,     NA,  404L,     416L,         0.97,     323L,        416L,     243L,        416L,
  "01/04/2027", "31/03/2028",     15L,     NA,  408L,     416L,         0.98,     327L,        416L,     245L,        416L,
  "01/04/2028", "31/03/2029",     16L,     NA,  412L,     416L,         0.99,     330L,        416L,     247L,        416L,
  "01/04/2029", "31/03/2030",     17L,     NA,  416L,     416L,            1,     333L,        416L,     250L,        416L
  )



# When system comes in December 27, T3
# When system comes full 18-month later, T4
# 

control_periods_5$Start <- as.Date(control_periods_5$Start, format = "%d/%m/%Y")
control_periods_5$end <- as.Date(control_periods_5$end, format = "%d/%m/%Y")


# version 1 with 5% referral growth
# set random number generation to defined start
set.seed(124)

ent_sim1_5 <-
  wl_simulator(control_periods_5$Start[1]
               , control_periods_5$end[1]
               , control_periods_5$Adds[1]
               , control_periods_5$Removes[1])

ent_queue1_5 <-
  wl_queue_size(ent_sim1_5)

ggplot(ent_queue1_5, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "ENT: First GP referral to first Outpatients waiting list:",
    subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", control_periods_5$Removes[1], ", Demand=", control_periods_5$Adds[1]),
    y = "Queue Size",
    x = "Month"
  )

tail(ent_queue1_5)

ent_sim_20_1_5 <- ent_sim1_5
ent_sim_40_1_5 <-  ent_sim1_5


# Loop through and simulate each section
# Use control period 5a here to remove jagged monthly part, as not monthly ramp to the no-effect sceanrio
# Updated to 17 for additional year
for(i in seq(2,17)){
  set.seed(125)  
  eval(
    call("<-"
         , as.name(paste0("ent_sim",as.character(i), "_5"))
         , wl_simulator(control_periods_5$Start[i]
                        , control_periods_5$end[i]
                        , control_periods_5$Adds[i]
                        , control_periods_5$Removes[i]
                        ,  waiting_list = get(paste0("ent_sim", as.character(i-1), "_5")))
    )
  )
  
}

# Loop through and simulate each section
# Updated to 17 for additional year
for(i in seq(2,17)){
  set.seed(125)  
  eval(
    call("<-"
         , as.name(paste0("ent_sim_20_",as.character(i), "_5"))
         , wl_simulator(control_periods_5$Start[i]
                        , control_periods_5$end[i]
                        , control_periods_5$Adds_20[i]
                        , control_periods_5$Removes_20[i]
                        ,  waiting_list = get(paste0("ent_sim_20_", as.character(i-1), "_5")))
    )
  )
}


# Loop through and simulate each section
# Updated to 17 for additional year
for(i in seq(2,17)){
  set.seed(125)
  eval(
    call("<-"
         , as.name(paste0("ent_sim_40_",as.character(i), "_5"))
         , wl_simulator(control_periods_5$Start[i]
                        , control_periods_5$end[i]
                        , control_periods_5$Adds_40[i]
                        , control_periods_5$Removes_40[i]
                        ,  waiting_list = get(paste0("ent_sim_40_", as.character(i-1), "_5")))
    )
  )
  
}

# Phase 1:  pre-implementation, setting up the queue.  This may not be real, but should end at peak date

# ent_sim1 <-
#   wl_simulator(phase_1_start, phase_1_end, demand_phase1, capacity_phase1)

ent_queue16_5 <-
  wl_queue_size(ent_sim17_5) # Updated to 17 for additional year

ent_sim16_5$referral_after_t3 <- ifelse(ent_sim16_5$referral < programme_dts$startdate[2], 0, 1)

ent_5_t5_date <-
  ent_sim16_5 %>% 
  filter(referral_after_t3 == 0) %>% 
  summarise(max(removal)) %>% 
  pull()


ent_queue_20_16_5 <-
  wl_queue_size(ent_sim_20_17_5) # Updated to 17 for additional year

ent_sim_20_16_5$referral_after_t3 <- ifelse(ent_sim_20_16_5$referral < programme_dts$startdate[2], 0, 1)

ent_5_20_t5_date <-
  ent_sim_20_16_5 %>% 
  filter(referral_after_t3 == 0) %>% 
  summarise(max(removal)) %>% 
  pull()


ent_queue_40_16_5 <-
  wl_queue_size(ent_sim_40_17_5) # Updated to 17 for additional year


ent_sim_40_16_5$referral_after_t3 <- ifelse(ent_sim_40_16_5$referral < programme_dts$startdate[2], 0, 1)

ent_5_40_t5_date <-
  ent_sim_40_16_5 %>% 
  filter(referral_after_t3 == 0) %>% 
  summarise(max(removal)) %>% 
  pull()


ggplot(ent_queue16_5, aes(dates, queue_size)) +
  
  geom_line(col=colours[2], data=ent_queue_20_16_5) +
  geom_line(col=colours[3], data=ent_queue_40_16_5) +
  geom_line(col=colours[1]) +
  labs(
    title = "ENT: First GP referral to first Outpatients waiting list:",
    #subtitle = paste("Phase 1: baseline setting up waiting list \nCapacity = ", capacity_phase1, ", Demand=", demand_phase1),
    y = "Queue Size",
    x = "Month"
  )



# Target queue size for current and future queue
control_periods_5$target_queue_size_5 <- calc_target_queue_size(control_periods_5$Adds, 6, factor = 1)
#control_periods_5a$target_queue_size_5 <- calc_target_queue_size(control_periods_5a$Adds, 6, factor = 1)
control_periods_5$target_queue_size_20_5 <- calc_target_queue_size(control_periods_5$Adds_20, 6, factor = 1)
control_periods_5$target_queue_size_40_5 <- calc_target_queue_size(control_periods_5$Adds_40, 6, factor = 1)

control_periods_5$wl_load <- calc_queue_load(control_periods_5$Adds, control_periods_5$Removes)
#control_periods_5a$wl_load <- calc_queue_load(control_periods_5a$Adds, control_periods_5a$Removes)
control_periods_5$wl_load_20 <- calc_queue_load(control_periods_5$Adds_20, control_periods_5$Removes_20)
control_periods_5$wl_load_40 <- calc_queue_load(control_periods_5$Adds_40, control_periods_5$Removes_40)

target_queue_size_5 <- calc_target_queue_size(tail(control_periods_5,1)$Adds, 6, factor = 1) 
#target_queue_size_5 <- calc_target_queue_size(tail(control_periods_5a,1)$Adds, 6, factor = 1)
target_queue_size_20_5 <- calc_target_queue_size(tail(control_periods_5,1)$Adds_20, 6, factor = 1)
target_queue_size_40_5 <- calc_target_queue_size(tail(control_periods_5,1)$Adds_40, 6, factor = 1)


# Phases 0
ent_queue16_5 <-
  ent_queue16_5 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size_5) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_0 = ifelse((queue_size <= target_queue_size_5) & (dates > as.Date('2023-01-01')), 1,0)
  )

# Phases 0
ent_queue_20_16_5 <-
  ent_queue_20_16_5 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size_5) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_20 = ifelse((queue_size <= target_queue_size_20_5) & (dates > as.Date('2023-01-01')), 1,0)
  )
# Phases 0
ent_queue_40_16_5 <-
  ent_queue_40_16_5 %>% 
  mutate( meet_target = ifelse((queue_size <= target_queue_size_5) & (dates > as.Date('2023-01-01')), 1,0)
          , meet_future_40 = ifelse((queue_size <= target_queue_size_40_5) & (dates > as.Date('2023-01-01')), 1,0)
  )


# Meeting target on it's own
ent_queue16_5 %>% 
  filter(meet_target == 1) %>% 
  slice_head()

# Meeting target in future without intervention
target_date_5 <-
  ent_queue16_5 %>% 
  filter(meet_future_0 == 1) %>% 
  slice_head()

target_date_5

# Meeting target in future (20% reduction)
target_20_date_5 <-
  ent_queue_20_16_5 %>% 
  filter(meet_future_20 == 1) %>% 
  slice_head()

target_20_date_5

# Meeting target in future (40% reduction)
target_40_date_5 <-
  ent_queue_40_16_5 %>% 
  filter(meet_future_40 == 1) %>% 
  slice_head()

target_40_date_5


# T4 date for 40% sceanrio
ent_t4_date_5 <-
  ent_queue16_5 %>% 
  filter(dates == ent_5_t5_date) %>% 
  slice_head()

# T4 date for 40% sceanrio
ent_t4_date_20_5 <-
  ent_queue_20_16_5 %>% 
  filter(dates == ent_5_20_t5_date) %>% 
  slice_head()


# T4 date for 40% sceanrio
ent_t4_date_40_5 <-
  ent_queue_40_16_5 %>% 
  filter(dates == ent_5_40_t5_date) %>% 
  slice_head()



#######
# Calculate stats

calc_target_capacity(control_periods_5$Adds[17], target_wait = 6, factor = 1) # Updated to 17 for additional year
#calc_target_capacity(control_periods_5a$Adds[14], target_wait = 6, factor = 1)
calc_target_capacity(control_periods_5$Adds_20[17], target_wait = 6, factor = 1) # Updated to 17 for additional year
calc_target_capacity(control_periods_5$Adds_40[17], target_wait = 6, factor = 1) # Updated to 17 for additional year


# How long to meet 92% patients < 18 weeks?
# If we want the average wait to be within a target wait of 18 weeks
# For approx 2% chance of going over target
mean_wait6 <- calc_target_mean_wait(6, factor = 1)
mean_wait4 <- calc_target_mean_wait(6, factor = 1)
mean_wait3 <- calc_target_mean_wait(6, factor = 1)

# Target capacity after difference
current_target_capacity_5 <- calc_target_capacity(control_periods_5$Adds[8], 6, factor = 1)
future_target_capacity_0_5 <- calc_target_capacity(control_periods_5$Adds[17], 6, factor = 1) # Updated to 17 for additional year
#future_target_capacity_0_5 <- calc_target_capacity(control_periods_5a$Adds[14], 6, factor = 1)
future_target_capacity_20_5 <- calc_target_capacity(control_periods_5$Adds_20[17], 6, factor = 1) # Updated to 17 for additional year
future_target_capacity_40_5 <- calc_target_capacity(control_periods_5$Adds_40[17], 6, factor = 1) # Updated to 17 for additional year

# capacity that release once implemented:

weekly_capacity_release_0_5 <- current_target_capacity_5 - future_target_capacity_0_5
weekly_capacity_release_20_5 <- current_target_capacity_5 - future_target_capacity_20_5
weekly_capacity_release_40_5 <- current_target_capacity_5 - future_target_capacity_40_5



# what is queue at T3
t3_queue_size_5 <- ent_queue16_5 %>% filter(dates >= as.Date("01/01/2027", "%d/%m/%Y")) %>% head(1) %>% pull(queue_size)
t3_queue_size_20_5 <- ent_queue_20_16_5 %>% filter(dates >= as.Date("01/01/2027", "%d/%m/%Y")) %>% head(1) %>% pull(queue_size)
t3_queue_size_40_5 <- ent_queue_40_16_5 %>% filter(dates >= as.Date("01/01/2027", "%d/%m/%Y")) %>% head(1) %>% pull(queue_size)

# Difference from T3 to target
t3_queue_size_5 - target_queue_size_5
t3_queue_size_20_5 - target_queue_size_20_5
t3_queue_size_40_5 - target_queue_size_40_5

# How long after T3 to target
## Relief capacity at T3 - amount capacity over demand at T3
# 0
control_periods_5[14,]$Removes - control_periods_5[14,]$Adds
# How long till target in weeks
(t3_queue_size_5 - target_queue_size_5) / (control_periods_5[14,]$Removes - control_periods_5[14,]$Adds)

# 20
control_periods_5[14,]$Removes_20 - control_periods_5[14,]$Adds_20
# How long till target in weeks
(t3_queue_size_20_5 - target_queue_size_20_5) / (control_periods_5[14,]$Removes_20 - control_periods_5[14,]$Adds_20)

#40
control_periods_5[14,]$Removes_40 - control_periods_5[14,]$Adds_40
# How long till target in weeks
(t3_queue_size_40_5 - target_queue_size_40_5) / (control_periods_5[14,]$Removes_40 - control_periods_5[14,]$Adds_40)

difftime(target_date_5$dates, programme_dts$startdate[2], units = "weeks")
difftime(target_20_date_5$dates, programme_dts$startdate[2], units = "weeks")
difftime(target_40_date_5$dates, programme_dts$startdate[2], units = "weeks")


######
# Plot

with_popn_growth_5 <-
  ggplot(ent_queue16_5, aes(dates, queue_size)) +
  geom_vline(xintercept = programme_dts$startdate[2], alpha=1
             , colour="red")+
  # geom_vline(xintercept = target_40_date_5$dates, alpha=1
  #            , colour="dodgerblue2")+
  annotate("rect", xmin = programme_dts$startdate[1], xmax=programme_dts$enddate[1], ymin=0, ymax=Inf, alpha=0.5
           , fill="khaki1")+
  # annotate("rect", xmin = programme_dts$startdate[3], xmax=programme_dts$enddate[3], ymin=0, ymax=Inf, alpha=0.2
  #          , fill="seagreen")+
  
  # geom_line(col=colours[2], data=ent_queue_20_16_5) +
  geom_line(col=colours[2], data=ent_queue_40_16_5) +
  geom_line(col=colours[1]) +
  
  geom_hline(yintercept = target_queue_size_5, col=colours[1], linetype="dashed")+
  #  geom_hline(yintercept = target_queue_size_20_5, col = colours[2], linetype="dashed")+
  geom_hline(yintercept = target_queue_size_40_5, col = colours[2], linetype="dashed")+
  geom_hline(yintercept = target_queue_size_40_5, col = colours[2], linetype="dashed")+
  
  #geom_point(data=target_20_date, col = colours[2], linetype="dashed")+
  #geom_point(yintercept = target_20_date, col = colours[3], linetype="dashed")+
  geom_point(data=target_date_5, shape=4, col = colours[1], size = 5, stroke = 2)+
  #geom_point(data=ent_t4_date, shape=4, col = colours[1], size = 5, stroke = 2)+
  geom_point(data=ent_t4_date_40_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  #geom_point(data=target_20_date_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  geom_point(data=target_40_date_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  
  #geom_vline(data = programme_dts, aes(xintercept = dates), col="red", linetype="dashed")+
  geom_text(data = data.frame(dts = c(as.Date("2023-01-01"),as.Date("2023-01-01"))
                              , label = c("Target queue size", "Target queue size")
                              , y = c(target_queue_size_5, target_queue_size_40_5)
                              
  ), aes(x = dts, label=label, y= y)
  , col = c(colours[1], colours[2])
  , size=3.5
  , family = "sans"
  , fontface = "italic"
  , hjust = -0
  , vjust = -1
  )+
  
  
  geom_text(x = as.Date("01-10-2026", format = "%d-%m-%Y")
            , label="T2"
            , y=8000
            , col="black"
            , size=4
            , family = "sans"
            #          , fontface = "bold"
  )+
  geom_text(x = as.Date("01-01-2027", format = "%d-%m-%Y")
            , label="T3"
            , y=8000
            , col="black"
            , size=4
            , family = "sans"
            #          , fontface = "bold"
  )+
  
  geom_text(data = ent_t4_date_40_5
            , label="T4"
            ,# y=8000
            , col="black"
            , size=4
            , family = "sans"
            , hjust = -0.75
            , vjust = -1
            
            #          , fontface = "bold"
  )+
  
  geom_text(data = target_40_date_5
            , label="T5"
            #, y=8000
            , col="black"
            , size=4
            , family = "sans"
            , hjust = -0.75
            , vjust = -1
            
            #          , fontface = "bold"
  )+
  
  scale_y_continuous(labels=comma)+
  scale_x_date(date_breaks = "3 month"
               , date_labels = "%b-%y"
               , limits = c(
                 as.Date("2023-01-01")
                 , as.Date("2030-04-01")
                 
               )
               , expand = c(0,0)
               , 
  )+
  labs(
    title = bquote(bold("ENT: ") ~ "First GP referral to first Outpatients waiting list (5% relief capacity after Sept-24):"),
    subtitle = "    Green = current demand projected forward, Orange = 40% demand reduced",
    y = "Queue Size",
    x = "Month"
  )+
  theme(axis.text.x = element_text(angle=90),plot.margin = margin(2,4,2,2, "mm") )

with_popn_growth_5



# now fix above with simulation around target wait

# now fix above with simulation around target wait
#ent_filter_queue <- filter(ent_sim_16_5, referral < target_date_5$dates)
ent_filter_queue_40 <- filter(ent_sim_40_16_5, referral < target_40_date_5$dates)

# set.seed(125) 
# ent_40_operating_at_target <-
#   
#   wl_simulator(start_date = target_40_date_5$dates
#                , end_date = programme_dts$enddate[3]
#                , demand = control_periods_5$Adds_40[16]
#                , capacity = control_periods_5$Removes_40[16] - (current_target_capacity_5 + future_target_capacity_40_5)
#                , waiting_list = ent_filter_queue[1:2]
#   )
# 

#rpois(1,target_queue_size_40_5)



# #### Adjusted plot
ent_40_adjusted_queue <- ent_queue_40_16_5

ent_40_adjusted_queue_2 <-
  ent_queue_40_16_5 %>% 
  filter(meet_future_40 == 1)

ent_40_adjusted_queue_2$queue_size <-rpois(nrow(ent_40_adjusted_queue_2), target_queue_size_40_5)

ent_40_adjusted_queue<-
  ent_40_adjusted_queue %>% 
  left_join(ent_40_adjusted_queue_2, by = "dates", keep = TRUE) %>% 
  mutate(dates = dates.x, queue_size = coalesce(queue_size.y, queue_size.x), meet_target = meet_target.x,
         meet_future_40 = meet_future_40.x) %>% 
  select(dates, queue_size, meet_target, meet_future_40)



with_popn_growth_5_adj <-
  ggplot(ent_queue16_5, aes(dates, queue_size)) +
  geom_vline(xintercept = programme_dts$startdate[2], alpha=1
             , colour="red")+
  
  geom_point(data=ent_t4_date_40_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  #geom_point(data=target_20_date_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  geom_point(data=target_40_date_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  
  # geom_vline(xintercept = target_40_date_5$dates, alpha=1
  #            , colour="dodgerblue2")+
  annotate("rect", xmin = programme_dts$startdate[1], xmax=programme_dts$enddate[1], ymin=0, ymax=Inf, alpha=0.5
           , fill="khaki1")+
  # annotate("rect", xmin = programme_dts$startdate[3], xmax=programme_dts$enddate[3], ymin=0, ymax=Inf, alpha=0.2
  #          , fill="seagreen")+
  
  # geom_line(col=colours[2], data=ent_queue_20_16_5) +
  geom_line(col=colours[2], data=ent_40_adjusted_queue) +
  geom_line(col=colours[1]) +
  
  geom_hline(yintercept = target_queue_size_5, col=colours[1], linetype="dashed")+
  #  geom_hline(yintercept = target_queue_size_20_5, col = colours[2], linetype="dashed")+
  geom_hline(yintercept = target_queue_size_40_5, col = colours[2], linetype="dashed")+
  geom_hline(yintercept = target_queue_size_40_5, col = colours[2], linetype="dashed")+
  
  #geom_point(data=target_20_date, col = colours[2], linetype="dashed")+
  #geom_point(yintercept = target_20_date, col = colours[3], linetype="dashed")+
  geom_point(data=target_date_5, shape=4, col = colours[1], size = 5, stroke = 2)+
  #geom_point(data=ent_t4_date, shape=4, col = colours[2], size = 5, stroke = 2)+
  geom_point(data=ent_t4_date_40_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  #geom_point(data=target_20_date_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  geom_point(data=target_40_date_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  
  #geom_vline(data = programme_dts, aes(xintercept = dates), col="red", linetype="dashed")+
  geom_text(data = data.frame(dts = c(as.Date("2023-01-01"),as.Date("2023-01-01"))
                              , label = c("Target queue size", "Target queue size")
                              , y = c(target_queue_size_5, target_queue_size_40_5)
                              
  ), aes(x = dts, label=label, y= y)
  , col = c(colours[1], colours[2])
  , size=3.5
  , family = "sans"
  , fontface = "italic"
  , hjust = -0
  , vjust = -1
  )+
  
  
  geom_text(x = as.Date("01-10-2026", format = "%d-%m-%Y")
            , label="T2"
            , y=8000
            , col="black"
            , size=4
            , family = "sans"
            
  )+
  geom_text(x = as.Date("01-01-2027", format = "%d-%m-%Y")
            , label="T3"
            , y=8000
            , col="black"
            , size=4
            , family = "sans"
            
  )+
  
  geom_text(data = ent_t4_date_40_5
            , label="T4"
            ,# y=8000
            , col="black"
            , size=4
            , family = "sans"
            , hjust = -0.75
            , vjust = -1
            
            #          , fontface = "bold"
  )+
  
  geom_text(data = target_40_date_5
            , label="T5"
            #, y=8000
            , col="black"
            , size=4
            , family = "sans"
            , hjust = -0.75
            , vjust = -1
            
            #          , fontface = "bold"
  )+
  
  scale_y_continuous(labels=comma)+
  scale_x_date(date_breaks = "3 month"
               , date_labels = "%b-%y"
               , limits = c(
                 as.Date("2023-01-01")
                 , as.Date("2029-04-01")
                 
               )
               , expand = c(0,0)
               , 
  )+
  labs(
    title = bquote(bold("ENT: ") ~ "First GP referral to first Outpatients waiting list (5% relief capacity after Sept-24):"),
    subtitle = "    Green = current demand projected forward, Orange = 40% demand reduced",
    y = "Queue Size",
    x = "Month"
  )+
  theme(axis.text.x = element_text(angle=90),plot.margin = margin(2,4,2,2, "mm") )

with_popn_growth_5_adj

with_popn_growth
with_popn_growth_2
with_popn_growth_5




########### Save ################

saveRDS(ent_sim16, "./output/ent/ent_sim16.rds")
saveRDS(ent_sim_20_16, "./output/ent/ent_sim_20_16.rds")
saveRDS(ent_sim_40_16, "./output/ent/ent_sim_40_16.rds")
saveRDS(ent_sim16_2, "./output/ent/ent_sim16_2.rds")
saveRDS(ent_sim_20_16_2, "./output/ent/ent_sim_20_16_2.rds")
saveRDS(ent_sim_40_16_2, "./output/ent/ent_sim_40_16_2.rds")
saveRDS(ent_sim16_5, "./output/ent/ent_sim16_5.rds")
saveRDS(ent_sim_20_16_5, "./output/ent/ent_sim_20_16_5.rds")
saveRDS(ent_sim_40_16_5, "./output/ent/ent_sim_40_16_5.rds")
###############################