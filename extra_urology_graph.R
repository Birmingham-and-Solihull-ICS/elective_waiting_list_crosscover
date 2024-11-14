# now fix above with simulation around target wait
#uro_filter_queue <- filter(uro_sim_16_5, referral < target_date_5$dates)

#uro_filter_queue_20 <- filter(uro_sim_20_16_5, referral < target_20_date_5$dates)

# set.seed(125) 
# uro_40_operating_at_target <-
#   
#   wl_simulator(start_date = target_40_date_5$dates
#                , end_date = programme_dts$enddate[3]
#                , demand = control_periods_5$Adds_40[16]
#                , capacity = control_periods_5$Removes_40[16] - (current_target_capacity_5 + future_target_capacity_40_5)
#                , waiting_list = uro_filter_queue[1:2]
#   )
# 

#rpois(1,target_queue_size_40_5)



# #### Adjusted plot
uro_20_adjusted_queue_5 <- uro_queue_20_16_5

uro_20_adjusted_queue_2_5 <-
  uro_queue_20_16_5 %>% 
  filter(meet_future_20 == 1)

uro_20_adjusted_queue_2_5$queue_size <-rpois(nrow(uro_20_adjusted_queue_2_5), target_queue_size_20_5)

uro_20_adjusted_queue_5<-
  uro_20_adjusted_queue_5 %>% 
  left_join(uro_20_adjusted_queue_2_5, by = "dates", keep = TRUE) %>% 
  mutate(dates = dates.x, queue_size = coalesce(queue_size.y, queue_size.x), meet_target = meet_target.x,
         meet_future_20 = meet_future_20.x) %>% 
  select(dates, queue_size, meet_target, meet_future_20)




Extra_uro_graph <-
  
  ggplot(uro_queue16_5, aes(dates, queue_size)) +
  geom_vline(xintercept = programme_dts$startdate[2], alpha=1
             , colour="red")+
  
  geom_point(data=uro_t4_date_40, shape=4, col = colours[2], size = 5, stroke = 2)+
  #geom_point(data=target_20_date_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  geom_point(data=target_40_date_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  
  # geom_vline(xintercept = target_40_date_5$dates, alpha=1
  #            , colour="dodgerblue2")+
  annotate("rect", xmin = programme_dts$startdate[1], xmax=programme_dts$enddate[1], ymin=0, ymax=Inf, alpha=0.5
           , fill="khaki1")+
  # annotate("rect", xmin = programme_dts$startdate[3], xmax=programme_dts$enddate[3], ymin=0, ymax=Inf, alpha=0.2
  #          , fill="seagreen")+
  
  # geom_line(col=colours[2], data=uro_queue_20_16_5) +
  geom_line(col=colours[2], data=uro_40_adjusted_queue) +
  geom_line(col=colours[2], data=uro_20_adjusted_queue) +
  geom_ribbon(data= data.frame(dates= uro_40_adjusted_queue$dates
                               , ymin = uro_40_adjusted_queue$queue_size
                               , ymax = uro_20_adjusted_queue$queue_size)
              , aes(ymin=ymin, ymax=ymax, y=NULL), col=colours[2], fill=colours[2], alpha=0.4)+
  geom_line(col=colours[1]) +
  geom_line(data=uro_queue16_2, col=colours[1]) +
  geom_ribbon(data= data.frame(dates= uro_queue16_5$dates
                               , ymin = uro_queue16_5$queue_size
                               , ymax = uro_queue16_2$queue_size)
              , aes(ymin=ymin, ymax=ymax, y=NULL), col=colours[1], fill=colours[1], alpha=0.4)+
  
  geom_hline(yintercept = target_queue_size_5, col=colours[1], linetype="dashed")+
  geom_hline(yintercept = target_queue_size_20_5, col = colours[2], linetype="dashed")+
  #geom_hline(yintercept = target_queue_size_40_5, col = colours[2], linetype="dashed")+
  geom_hline(yintercept = target_queue_size_40_5, col = colours[2], linetype="dashed")+
  
  #geom_point(data=target_20_date, col = colours[2], linetype="dashed")+
  #geom_point(yintercept = target_20_date, col = colours[3], linetype="dashed")+
  geom_point(data=target_date_5, shape=4, col = colours[1], size = 5, stroke = 2)+
  #geom_point(data=uro_t4_date, shape=4, col = colours[2], size = 5, stroke = 2)+
  geom_point(data=uro_t4_date_40, shape=4, col = colours[2], size = 5, stroke = 2)+
  geom_point(data=target_20_date_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  geom_point(data=target_40_date_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  
  #geom_vline(data = programme_dts, aes(xintercept = dates), col="red", linetype="dashed")+
  geom_text(data = data.frame(dts = c(as.Date("2023-01-01"),as.Date("2023-01-01"),as.Date("2023-01-01"))
                              , label = c("Target queue size","Target queue size 20","Target queue size 40")
                              , y = c(target_queue_size_5, target_queue_size_20_5, target_queue_size_40_5)
                              
  ), aes(x = dts, label=label, y= y)
  , col = c(colours[1], colours[2], colours[2])
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
  
  geom_text(data = uro_t4_date_40
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
    title = bquote(bold("Urology: ") ~ "First GP referral to first Outpatients waiting list (5% relief capacity after Sept-24):"),
    subtitle = "    Green = current demand projected forward, Orange = 40% demand reduced",
    y = "Queue Size",
    x = "Month"
  )+
  theme(axis.text.x = element_text(angle=90),plot.margin = margin(2,4,2,2, "mm") )

Extra_uro_graph 




# #### Adjusted plot - 2% 20


#uro_filter_queue_20 <- filter(uro_sim_20_16_2, referral < target_20_date_2$dates)

uro_20_adjusted_queue_2 <- uro_queue_20_16_2

uro_20_adjusted_queue_2_2 <-
  uro_queue_20_16_2 %>% 
  filter(meet_future_20 == 1)

uro_20_adjusted_queue_2_2$queue_size <-rpois(nrow(uro_20_adjusted_queue_2_2), target_queue_size_20_2)

uro_20_adjusted_queue_2<-
  uro_20_adjusted_queue_2 %>% 
  left_join(uro_20_adjusted_queue_2_2, by = "dates", keep = TRUE) %>% 
  mutate(dates = dates.x, queue_size = coalesce(queue_size.y, queue_size.x), meet_target = meet_target.x,
         meet_future_20 = meet_future_20.x) %>% 
  select(dates, queue_size, meet_target, meet_future_20)





Extra_uro_graph_20241112 <-
  
  ggplot(uro_queue16, aes(dates, queue_size)) +
  geom_vline(xintercept = programme_dts$startdate[1], alpha=1
             , colour="black")+
  geom_vline(xintercept = programme_dts$startdate[2], alpha=1
             , colour="black")+
  geom_vline(xintercept = uro_t4_date_20_2$dates, alpha=1
             , colour="black")+
  geom_vline(xintercept = target_20_date_2$dates, alpha=1
             , colour="black")+
  
  #geom_point(data=uro_t4_date_20_2, shape=4, col = colours[2], size = 5, stroke = 2)+
  #geom_point(data=target_20_date_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  #geom_point(data=target_20_date_2, shape=4, col = colours[2], size = 5, stroke = 2)+
  
  # geom_vline(xintercept = target_40_date_5$dates, alpha=1
  #            , colour="dodgerblue2")+
  # annotate("rect", xmin = programme_dts$startdate[1], xmax=programme_dts$enddate[1], ymin=0, ymax=Inf, alpha=0.5
  #          , fill="khaki1")+
  # annotate("rect", xmin = programme_dts$startdate[3], xmax=programme_dts$enddate[3], ymin=0, ymax=Inf, alpha=0.2
  #          , fill="seagreen")+
  
  # geom_line(col=colours[2], data=uro_queue_20_16_5) +
  #geom_line(col=colours[2], data=uro_40_adjusted_queue) +
  geom_line(col=colours[2], data=uro_20_adjusted_queue_2) +
  # geom_ribbon(data= data.frame(dates= uro_queue16_2$dates
  #                              , ymin = uro_queue16_2$queue_size
  #                              , ymax = uro_20_adjusted_queue$queue_size)
  #             , aes(ymin=ymin, ymax=ymax, y=NULL), col=colours[2], fill=colours[2], alpha=0.4)+
  geom_line(col=colours[1], linetype="dotted") +
  geom_line(data=uro_queue16_2, col=colours[1]) +
  # geom_ribbon(data= data.frame(dates= uro_queue16_2$dates
  #                              , ymin = uro_queue16$queue_size
  #                              , ymax = uro_queue16_2$queue_size)
  #             , aes(ymin=ymin, ymax=ymax, y=NULL), col=colours[1], fill=colours[1], alpha=0.4)+
  
  geom_hline(yintercept = target_queue_size_2, col=colours[1], linetype="dashed")+
  geom_hline(yintercept = target_queue_size_20_2, col = colours[2], linetype="dashed")+
  #geom_hline(yintercept = target_queue_size_40_5, col = colours[2], linetype="dashed")+
  
  
  # geom_hline(yintercept = target_queue_size_40_5, col = colours[2], linetype="dashed")+
  
  #geom_point(data=target_20_date, col = colours[2], linetype="dashed")+
  #geom_point(yintercept = target_20_date, col = colours[3], linetype="dashed")+
  #geom_point(data=target_date_2, shape=4, col = colours[1], size = 5, stroke = 2)+
  #geom_point(data=uro_t4_date, shape=4, col = colours[2], size = 5, stroke = 2)+
  #geom_point(data=uro_t4_date_40, shape=4, col = colours[2], size = 5, stroke = 2)+
  # geom_point(data=target_20_date_2, shape=4, col = colours[2], size = 5, stroke = 2)+
  # geom_point(data=target_40_date_5, shape=4, col = colours[2], size = 5, stroke = 2)+
  
  #geom_vline(data = programme_dts, aes(xintercept = dates), col="red", linetype="dashed")+
  geom_text(data = data.frame(dts = c(as.Date("2023-01-01"),as.Date("2023-01-01"))
                              , label = c("Target queue size","Target queue size (20% reduced demand)")
                              , y = c(target_queue_size_2, target_queue_size_20_2)
                              
  ), aes(x = dts, label=label, y= y)
  , col = c(colours[1], colours[2])
  , size=3.5
  , family = "sans"
  , fontface = "italic"
  , hjust = -0
  , vjust = -0.6
  )+
  
  
  geom_text(x = programme_dts$startdate[1]
            , label = "T2"
            , y=6300
            , col="black"
            , size=4
            #, family = "sans"
            #, fontface = "bold"
            
  )+
  geom_text(x = programme_dts$startdate[2]
            , label = "T3"
            , y=6300
            , col="black"
            , size=4
            #, family = "sans"
            #, fontface = "bold"
            
  )+
  
  geom_text(data = uro_t4_date_20_2
            , label = "T4"
            #, y=8000
            ,, y=6300
            , col="black"
            , size=4
            #, family = "sans"
            , fontface = "bold"
  )+
  
  geom_text(data = target_20_date_2
            , label = "T5"
            , y=6300
            , col="black"
            , size=4
            #, family = "sans"
            , fontface = "bold"
  )+
  
  
  geom_text(data = data.frame(dates =  as.Date(c("01-01-2025", "01-01-2025"), format = "%d-%m-%Y")
                              , labels = c("Demand modelled at 98% current capacity"
                                           ,"Current capacity maintained indefinitely")
                              , y = c(5200, 4450)
  )
  , aes(label=labels, y=y)
  #, y=8000
  , col=colours[1]
  , size=3
  #, family = "sans"
  , hjust = 0.1
  , vjust = -1
  , angle = c(0,-27)
  
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
    title = bquote(bold("Urology: ") ~ "First GP referral to first Outpatients waiting list (2% relief capacity after Sept-24):"),
    subtitle = "    Green = current demand projected forward, Orange = 20% demand reduced from T3",
    y = "Queue Size",
    x = "Month"
  )+
  theme(axis.text.x = element_text(angle=90),plot.margin = margin(2,4,2,2, "mm") 
        , text = element_text(family="sans"))

Extra_uro_graph_20241112 
