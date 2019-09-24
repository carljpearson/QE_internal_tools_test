#SUM -----

head(df_full)

df_sum <-  df_full %>% 
  na.omit(time,satisfaction) %>% #remove na
  group_by(task) %>%
  filter(satisfaction >= 5) %>% #only sat above 5
  summarize(tspec = median(time)) %>% #median to use for timespec
  right_join(df_full) %>% #rejoin
  filter(task!="5") %>% #task 5 was all fails
  na.omit(time) %>%
  rename(sat=satisfaction,
         comp=completion,
         group=product) %>%
  mutate(comp=as.numeric(as.character(comp)))
zval <- 1.64 #z value at 90% confidence

#create satisfaction df -----
df_sum %>% 
  group_by(group,task) %>% #group analyses broadly by product/version group and then by each task
  summarise(mean=mean(sat),sd=sd(sat),n=n()) %>% #get means, std deviation, and total observations
  mutate(se=(sd / sqrt(n))) %>% #std error
  mutate(marg=se*zval) %>% #margin of error based on zval
  mutate(lowerci=mean-marg) %>% #lower ci
  mutate(lowerci = ifelse(lowerci <= 0, 0, lowerci)) %>% #keep lower ci above 0
  mutate(upperci=mean+marg) %>% #upper ci
  mutate(upperci = ifelse(upperci >= 7, 7, upperci)) %>% #keep upper ci below max
  mutate(point_est.z = pnorm((mean - 5.6)/sd)) %>% #z transform based on sd
  mutate(lowerci.z=pnorm((lowerci-5.6)/sd)) %>%  #z transform lower ci
  mutate(upperci.z=pnorm((upperci-5.6)/sd)) %>% #z transform upper ci
  mutate(point_est.nps=(point_est.z - .5) * 200)%>% #nps-ify
  mutate(lowerci.nps=(lowerci.z- .5 )* 200)%>% #nps-ify
  mutate(upperci.nps=(upperci.z- .5 )* 200)%>% #nps-ify
  mutate(Measure="Satisfaction") %>% #name measure as var
  mutate(spec=4) %>% #define spec var for raw plots
  rename(point.est=mean) -> df_sat


#creat time df----
df_sum %>%
  filter(comp==1) %>% #only completed tasks
  group_by(group,task,tspec) %>% #group analyses broadly by product/version group and then by each task, including task time spec
  summarise(mean=psych::geometric.mean(time),sd = sd(time),n=n()) %>% #get mean, sd and n
  mutate(se=(sd / sqrt(n))) %>% #calculate std error
  mutate(marg=se*zval) %>% #calculate margin of error
  mutate(lowerci=mean-marg) %>% #lower ci
  mutate(lowerci = ifelse(lowerci <= 0, 0, lowerci)) %>% #keep lower ci above 0
  mutate(upperci=mean+marg) %>% #upper ci
  mutate(point_est.z = 1-pnorm((mean - tspec)/sd)) %>% #reverse proportion of z
  mutate(upperci.z=1-pnorm((lowerci-tspec)/sd)) %>% #upperci comes from lowerci after inversion
  mutate(lowerci.z=1-pnorm((upperci-tspec)/sd)) %>% #opposite as ^
  mutate(point_est.nps=(point_est.z - .5) * 200)%>% #nps-ify
  mutate(lowerci.nps=(lowerci.z- .5 )* 200)%>% #nps-ify
  mutate(upperci.nps=(upperci.z- .5 )* 200)%>%# nps-ify
  rename(point.est=mean,spec=tspec) %>% #rename some variables to fit into bind_rows
  mutate(Measure="Time") -> df_time

#create completion df ----
df_sum %>%
  group_by(group,task) %>% #group analyses broadly by product/version group and then by each task
  summarise(pass=sum(comp),n=n()) %>% #get n successes and n trials
  mutate(prop = pass / n) %>% #exact proportion from succesess/trials
  mutate(laplace = (pass + 1) / (n + 2)) %>% #laplace point estimate
  mutate(p_adj = (n * prop + (zval * zval) / 2) / (n + (zval * zval))) %>% #adjust p for wald calculation
  mutate(n_adj = n + (zval * zval)) %>% #adjust n for wald calculation
  mutate(marg =  zval * sqrt(p_adj * (1 - p_adj) / n_adj)) %>% #wald margin value
  mutate(lowerci = p_adj - marg) %>% #lower wald ci
  mutate(lowerci = ifelse(lowerci <= 0, 0, lowerci)) %>% #keep lower ci above 0
  mutate(upperci = p_adj + marg) %>% #upper wald ci
  mutate(upperci = ifelse(upperci >= 1, 1, upperci)) %>% #keep upper ci below 1
  mutate(point_est.z = qnorm(laplace) ) %>% #z score transform based on .78 baseline and bernouli variance
  mutate(lowerci.z= qnorm(laplace)-qnorm(marg) ) %>% #z score transform for conf intervals
  mutate(upperci.z = qnorm(laplace)+qnorm(marg) ) %>% #z score transform for conf intervals
  rename(point.est=laplace) %>% 
  mutate(point_est.nps=(point.est - .5) * 200)%>% #nps-ify
  mutate(lowerci.nps=(lowerci- .5 )* 200)%>% #nps-ify
  mutate(upperci.nps=(upperci- .5 )* 200)%>%# nps-ify
  mutate(Measure="Completion") %>% #name measure as var
  mutate(spec=.78 #define spec var for raw plots
  ) -> df_comp

#combining all dfs
bind_rows(df_comp,df_sat,df_time) -> df_summarised #df from row bind



#all sub scores on nps version
df_summarised %>%
  group_by(task) %>%
  # mutate(vjust_value=ifelse(point_est.nps<6,-.4,1.4)) %>%
  #mutate(hjust_value=ifelse(Group=="3.5",1.5,-1)) %>%
  ggplot(aes(x=Measure, y=point_est.nps,fill=group)) + 
  geom_bar(aes(fill=group),position=position_dodge(), stat="identity") +
  coord_cartesian(ylim=c(-100,100)) +
  geom_errorbar(aes(ymin=lowerci.nps, ymax=upperci.nps),position=position_dodge(.9), stat="identity",color="gray",width=.2) +
  #geom_abline(intercept=.5,slope=0, color = "gray",linetype = 2, size=2) +
  #  geom_text(aes(label = round(point_est.nps,0),hjust = hjust_value,vjust=vjust_value,y=0), size = 3, position = "identity") +
  # scale_y_continuous(breaks=c(0:3)) +
  labs(x="", y="Standard score") +
  ggtitle(label = "Standardized Scores for All Tasks",
          subtitle = "Confidence Intervals at 90%") +
  ggthemes::theme_tufte(base_family="GillSans") + 
  theme(
    axis.text.x = element_text(angle = 45,hjust=1),
    #axis.text.y = element_text(size = 15),
    #axis.title.x = element_text(size = 15),
    #axis.title.y = element_text(size = 15),
    #title = element_text(size = 18),
  ) +
  facet_grid(.~task) 

#task level
df_summarised %>% 
  group_by(group,task) %>% #keep product/versions separate, and only aggregate at task level
  summarise(point_est.z=mean(point_est.z), #average of point estimate std values
            lowerci.z=mean(lowerci.z), #average of lower CI std values
            upperci.z=mean(upperci.z), #average of upper CI std values
            point_est.nps=mean(point_est.nps), #average of pointe estimate NPS-like values
            lowerci.nps=mean(lowerci.nps), #average of lower CI NPS-like values
            upperci.nps=mean(upperci.nps) #average of upper CI NPS-like values
  ) -> df_task

#Task level plot
df_task %>%
  mutate(vjust_value=ifelse(point_est.nps<6,-.4,1.4)) %>%
  ggplot(aes(x=task, y=point_est.nps, fill=group)) + 
  geom_bar(aes(fill=group),position=position_dodge(), stat="identity") +
  
  geom_errorbar(aes(ymin=lowerci.nps, ymax=upperci.nps),position=position_dodge(.9), stat="identity",color="gray",width=.2) +
  # geom_text(aes(label = round(point_est.nps,0),hjust=ifelse(Group=="3.11",-1.5,1.5),vjust=vjust_value,y=0), size = 5, position = "identity") +
  coord_cartesian(ylim=c(-100,100)) +
  labs(x="", y="SUM scores") +
  ggtitle(label = "SUM scores for each measure on all tasks",
          subtitle = "Confidence Intervals at 90%") +
  #scale_y_continuous(breaks=c(-3:3)) +
  #scale_fill_manual(values=colpal)+
  ggthemes::theme_tufte(base_family="GillSans") + 
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_blank()) #-> p.all_tasks_nps

ggsave(
  "plot_all_task_nps.png",
  device = "png",
  bg =  "transparent",
  width = 10,
  height = 5)


#Overall SUM score----

df_summarised %>%
  group_by(group) %>% #keep products separate
  summarise(point_est.z=mean(point_est.z), #average point estimates
            lowerci.z=mean(lowerci.z), #average lower CI
            upperci.z=mean(upperci.z) #average upper CI
  ) %>%
  data.frame() %>% 
  group_by(group) %>%
  summarise(mean=mean(point_est.z), 
            sd=sd(point_est.z),
            n=n(),
            lowerci.z=mean(lowerci.z),
            upperci.z=mean(upperci.z)) %>%
  mutate(se=(sd / sqrt(n))) %>%
  mutate(marg=se*zval) %>%
  mutate(lowerci.grand=mean-marg) %>%
  mutate(lowerci.grand = ifelse(lowerci.grand <= 0, 0, lowerci.grand)) %>%
  mutate(upperci.grand=mean+marg) %>%
  mutate(upperci.grand = ifelse(upperci.grand >= 5, 5, upperci.grand)) %>%
  mutate(group=as.factor(group)) %>%
  rename(point_est.z=mean)%>%
  mutate(point_est.nps=(point_est.z - .5) * 200)%>%
  mutate(lowerci.nps=(lowerci.z- .5 )* 200)%>%
  mutate(upperci.nps=(upperci.z- .5 )* 200
  )%>%
  mutate(Group=as.factor(recode(group,"1"="3.5","2"="3.11"))) %>%
  mutate(Group=factor(Group, levels = c("3.5","3.11")))-> df_sum_scores





#nps version final sum
df_sum_scores %>% 
  ggplot(aes(x=group, y=point_est.nps,fill=group,ymin=lowerci.nps, ymax=upperci.nps)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  coord_cartesian(ylim=c(-100,100)) +
  geom_errorbar(position=position_dodge(), stat="identity",color="gray",width=.2) +
  #geom_abline(intercept=.5,slope=0, color = "gray",linetype = 2, size=2) +
  geom_text(aes(label = round(point_est.nps,0),hjust=2,vjust=1.5,y=0), size = 10, position = "identity") +
  labs(x="", y="SUM scores") +
  ggtitle(label = "Final Product SUM Score",
          subtitle = "Confidence Intervals at 90%") +
  ggthemes::theme_tufte(base_family="GillSans") + 
  theme(
    axis.text.x = element_blank())
ggsave(
  "plot_final_nps.png",
  device = "png",
  bg =  "transparent",
  width = 7,
  height = 5)

#scale final scores

43  -> prac_scale
16  -> xray_scale

((prac_scale + 100) / 200) * 4 + 1
((xray_scale + 100) / 200) * 4 + 1

((100 + 100) / 200) * 4 + 1



#sum but keeping task 5

#SUM -----

head(df_full)

df_sum <-  df_full %>% 
  na.omit(time,satisfaction) %>% #remove na
  group_by(task) %>%
  filter(satisfaction >= 5) %>% #only sat above 5
  summarize(tspec = median(time)) %>% #median to use for timespec
  right_join(df_full) %>% #rejoin
 # filter(task!="5") %>% #task 5 was all fails
  na.omit(time) %>%
  rename(sat=satisfaction,
         comp=completion,
         group=product) %>%
  mutate(comp=as.numeric(as.character(comp)))
zval <- 1.64 #z value at 90% confidence

#create satisfaction df -----
df_sum %>% 
  group_by(group,task) %>% #group analyses broadly by product/version group and then by each task
  summarise(mean=mean(sat),sd=sd(sat),n=n()) %>% #get means, std deviation, and total observations
  mutate(se=(sd / sqrt(n))) %>% #std error
  mutate(marg=se*zval) %>% #margin of error based on zval
  mutate(lowerci=mean-marg) %>% #lower ci
  mutate(lowerci = ifelse(lowerci <= 0, 0, lowerci)) %>% #keep lower ci above 0
  mutate(upperci=mean+marg) %>% #upper ci
  mutate(upperci = ifelse(upperci >= 7, 7, upperci)) %>% #keep upper ci below max
  mutate(point_est.z = pnorm((mean - 5.6)/sd)) %>% #z transform based on sd
  mutate(lowerci.z=pnorm((lowerci-5.6)/sd)) %>%  #z transform lower ci
  mutate(upperci.z=pnorm((upperci-5.6)/sd)) %>% #z transform upper ci
  mutate(point_est.nps=(point_est.z - .5) * 200)%>% #nps-ify
  mutate(lowerci.nps=(lowerci.z- .5 )* 200)%>% #nps-ify
  mutate(upperci.nps=(upperci.z- .5 )* 200)%>% #nps-ify
  mutate(Measure="Satisfaction") %>% #name measure as var
  mutate(spec=4) %>% #define spec var for raw plots
  rename(point.est=mean) -> df_sat


#creat time df----
df_sum %>%
  filter(comp==1) %>% #only completed tasks
  group_by(group,task,tspec) %>% #group analyses broadly by product/version group and then by each task, including task time spec
  summarise(mean=psych::geometric.mean(time),sd = sd(time),n=n()) %>% #get mean, sd and n
  mutate(se=(sd / sqrt(n))) %>% #calculate std error
  mutate(marg=se*zval) %>% #calculate margin of error
  mutate(lowerci=mean-marg) %>% #lower ci
  mutate(lowerci = ifelse(lowerci <= 0, 0, lowerci)) %>% #keep lower ci above 0
  mutate(upperci=mean+marg) %>% #upper ci
  mutate(point_est.z = 1-pnorm((mean - tspec)/sd)) %>% #reverse proportion of z
  mutate(upperci.z=1-pnorm((lowerci-tspec)/sd)) %>% #upperci comes from lowerci after inversion
  mutate(lowerci.z=1-pnorm((upperci-tspec)/sd)) %>% #opposite as ^
  mutate(point_est.nps=(point_est.z - .5) * 200)%>% #nps-ify
  mutate(lowerci.nps=(lowerci.z- .5 )* 200)%>% #nps-ify
  mutate(upperci.nps=(upperci.z- .5 )* 200)%>%# nps-ify
  rename(point.est=mean,spec=tspec) %>% #rename some variables to fit into bind_rows
  mutate(Measure="Time") -> df_time

#create completion df ----
df_sum %>%
  group_by(group,task) %>% #group analyses broadly by product/version group and then by each task
  summarise(pass=sum(comp),n=n()) %>% #get n successes and n trials
  mutate(prop = pass / n) %>% #exact proportion from succesess/trials
  mutate(laplace = (pass + 1) / (n + 2)) %>% #laplace point estimate
  mutate(p_adj = (n * prop + (zval * zval) / 2) / (n + (zval * zval))) %>% #adjust p for wald calculation
  mutate(n_adj = n + (zval * zval)) %>% #adjust n for wald calculation
  mutate(marg =  zval * sqrt(p_adj * (1 - p_adj) / n_adj)) %>% #wald margin value
  mutate(lowerci = p_adj - marg) %>% #lower wald ci
  mutate(lowerci = ifelse(lowerci <= 0, 0, lowerci)) %>% #keep lower ci above 0
  mutate(upperci = p_adj + marg) %>% #upper wald ci
  mutate(upperci = ifelse(upperci >= 1, 1, upperci)) %>% #keep upper ci below 1
  mutate(point_est.z = qnorm(laplace) ) %>% #z score transform based on .78 baseline and bernouli variance
  mutate(lowerci.z= qnorm(laplace)-qnorm(marg) ) %>% #z score transform for conf intervals
  mutate(upperci.z = qnorm(laplace)+qnorm(marg) ) %>% #z score transform for conf intervals
  rename(point.est=laplace) %>% 
  mutate(point_est.nps=(point.est - .5) * 200)%>% #nps-ify
  mutate(lowerci.nps=(lowerci- .5 )* 200)%>% #nps-ify
  mutate(upperci.nps=(upperci- .5 )* 200)%>%# nps-ify
  mutate(Measure="Completion") %>% #name measure as var
  mutate(spec=.78 #define spec var for raw plots
  ) -> df_comp

#combining all dfs
bind_rows(df_comp,df_sat,df_time) -> df_summarised #df from row bind



#all sub scores on nps version
df_summarised %>%
  group_by(task) %>%
  # mutate(vjust_value=ifelse(point_est.nps<6,-.4,1.4)) %>%
  #mutate(hjust_value=ifelse(Group=="3.5",1.5,-1)) %>%
  ggplot(aes(x=Measure, y=point_est.nps,fill=group)) + 
  geom_bar(aes(fill=group),position=position_dodge(), stat="identity") +
  coord_cartesian(ylim=c(-100,100)) +
  geom_errorbar(aes(ymin=lowerci.nps, ymax=upperci.nps),position=position_dodge(.9), stat="identity",color="gray",width=.2) +
  #geom_abline(intercept=.5,slope=0, color = "gray",linetype = 2, size=2) +
  #  geom_text(aes(label = round(point_est.nps,0),hjust = hjust_value,vjust=vjust_value,y=0), size = 3, position = "identity") +
  # scale_y_continuous(breaks=c(0:3)) +
  labs(x="", y="Standard score") +
  ggtitle(label = "Standardized Scores for All Tasks",
          subtitle = "Confidence Intervals at 90%") +
  ggthemes::theme_tufte(base_family="GillSans") + 
  theme(
    axis.text.x = element_text(angle = 45,hjust=1),
    #axis.text.y = element_text(size = 15),
    #axis.title.x = element_text(size = 15),
    #axis.title.y = element_text(size = 15),
    #title = element_text(size = 18),
  ) +
  facet_grid(.~task) 

#task level
df_summarised %>% 
  group_by(group,task) %>% #keep product/versions separate, and only aggregate at task level
  summarise(point_est.z=mean(point_est.z), #average of point estimate std values
            lowerci.z=mean(lowerci.z), #average of lower CI std values
            upperci.z=mean(upperci.z), #average of upper CI std values
            point_est.nps=mean(point_est.nps), #average of pointe estimate NPS-like values
            lowerci.nps=mean(lowerci.nps), #average of lower CI NPS-like values
            upperci.nps=mean(upperci.nps) #average of upper CI NPS-like values
  ) -> df_task

#Task level plot
df_task %>%
  mutate(vjust_value=ifelse(point_est.nps<6,-.4,1.4)) %>%
  ggplot(aes(x=task, y=point_est.nps, fill=group)) + 
  geom_bar(aes(fill=group),position=position_dodge(), stat="identity") +
  
  geom_errorbar(aes(ymin=lowerci.nps, ymax=upperci.nps),position=position_dodge(.9), stat="identity",color="gray",width=.2) +
  # geom_text(aes(label = round(point_est.nps,0),hjust=ifelse(Group=="3.11",-1.5,1.5),vjust=vjust_value,y=0), size = 5, position = "identity") +
  coord_cartesian(ylim=c(-100,100)) +
  labs(x="", y="SUM scores") +
  ggtitle(label = "SUM scores for each measure on all tasks",
          subtitle = "Confidence Intervals at 90%") +
  #scale_y_continuous(breaks=c(-3:3)) +
  #scale_fill_manual(values=colpal)+
  ggthemes::theme_tufte(base_family="GillSans") + 
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_blank()) #-> p.all_tasks_nps

ggsave(
  "plot_all_task_nps.png",
  device = "png",
  bg =  "transparent",
  width = 10,
  height = 5)


#Overall SUM score----

df_summarised %>%
  group_by(group) %>% #keep products separate
  summarise(point_est.z=mean(point_est.z), #average point estimates
            lowerci.z=mean(lowerci.z), #average lower CI
            upperci.z=mean(upperci.z) #average upper CI
  ) %>%
  data.frame() %>% 
  group_by(group) %>%
  summarise(mean=mean(point_est.z), 
            sd=sd(point_est.z),
            n=n(),
            lowerci.z=mean(lowerci.z),
            upperci.z=mean(upperci.z)) %>%
  mutate(se=(sd / sqrt(n))) %>%
  mutate(marg=se*zval) %>%
  mutate(lowerci.grand=mean-marg) %>%
  mutate(lowerci.grand = ifelse(lowerci.grand <= 0, 0, lowerci.grand)) %>%
  mutate(upperci.grand=mean+marg) %>%
  mutate(upperci.grand = ifelse(upperci.grand >= 5, 5, upperci.grand)) %>%
  mutate(group=as.factor(group)) %>%
  rename(point_est.z=mean)%>%
  mutate(point_est.nps=(point_est.z - .5) * 200)%>%
  mutate(lowerci.nps=(lowerci.z- .5 )* 200)%>%
  mutate(upperci.nps=(upperci.z- .5 )* 200
  )%>%
  mutate(Group=as.factor(recode(group,"1"="3.5","2"="3.11"))) %>%
  mutate(Group=factor(Group, levels = c("3.5","3.11")))-> df_sum_scores





#nps version final sum
df_sum_scores %>% 
  ggplot(aes(x=group, y=point_est.nps,fill=group,ymin=lowerci.nps, ymax=upperci.nps)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  coord_cartesian(ylim=c(-100,100)) +
  geom_errorbar(position=position_dodge(), stat="identity",color="gray",width=.2) +
  #geom_abline(intercept=.5,slope=0, color = "gray",linetype = 2, size=2) +
  geom_text(aes(label = round(point_est.nps,0),hjust=2,vjust=1.5,y=0), size = 10, position = "identity") +
  labs(x="", y="SUM scores") +
  ggtitle(label = "Final Product SUM Score",
          subtitle = "Confidence Intervals at 90%") +
  ggthemes::theme_tufte(base_family="GillSans") + 
  theme(
    axis.text.x = element_blank())
ggsave(
  "plot_final_nps.png",
  device = "png",
  bg =  "transparent",
  width = 7,
  height = 5)

#scale final scores

13  -> prac_scale
-10  -> xray_scale

((prac_scale + 100) / 200) * 4 + 1
((xray_scale + 100) / 200) * 4 + 1

((100 + 100) / 200) * 4 + 1
