library(tidyverse)
library(googlesheets)

#data and cleaning ----
#get data for timing and completion

gs_ls() #list google sheets
#practitest
sheets <- gs_title("QE tool test - PractiTest (Responses)") #raw data from google sheet title
gs_ws_ls(sheets) #list worksheets
data1 <- gs_read(ss=sheets, #google sheet
                ws="Timings") #specific worksheet
df_pt <- data.frame(data1[1:13,]) #convert to dataframe
#Xray
sheets <- gs_title("QE tool test - Xray for JIRA (Responses)") #raw data from google sheet title
gs_ws_ls(sheets) #list worksheets
data2 <- gs_read(ss=sheets, #google sheet
                ws="Timings") #specific worksheet
df_xr <- data.frame(data2[1:10,]) #convert to dataframe

#combine and clean

df_pt$product <- "Practitest"
df_xr$product <- "Xray"

df <- bind_rows (df_pt,df_xr) #join dfs

dfc <-  df %>% #clean df creation
  select(name=What.is.your.name., T1:C6,product) %>% #select variables
  pivot_longer( #make it longer
                 T1:C6, 
                 names_to = "measure"
                 ) %>%
  separate(measure, into=c("measure","task"),sep = 1) %>% #split joint col into time,comp and task#
  pivot_wider( #make time and completion appear in same case
    names_from = measure,
    values_from = value
  ) %>%
  mutate(
    #name=as.numeric(as.factor(name)), #make names anonymous,
    time=as.numeric(as.character(`T`)), #make timing numeric and rename
    completion = as.factor(as.character(C)), #make completion a factor
  ) %>%
  select(-`T`,-C) #drop old names
  #na.omit()

  
#get data for sat and participant info

gs_ls() #list google sheets
#practitest
sheets <- gs_title("QE tool test - PractiTest (Responses)") #raw data from google sheet title
gs_ws_ls(sheets) #list worksheets
data1_b <- gs_read(ss=sheets, #google sheet
                 ws="Form Responses 1") #specific worksheet
df_pt_b <- data.frame(data1_b[1:13,]) #convert to dataframe
#Xray
sheets <- gs_title("QE tool test - Xray for JIRA (Responses)") #raw data from google sheet title
gs_ws_ls(sheets) #list worksheets
data2_b <- gs_read(ss=sheets, #google sheet
                 ws="Form Responses 1") #specific worksheet
df_xr_b <- data.frame(data2_b[1:10,]) #convert to dataframe

#combine and clean

df_pt_b$product <- "Practitest"
df_xr_b$product <- "Xray"

df_b <- bind_rows (df_pt_b,df_xr_b) #join dfs


dfc_b <- df_b %>%
  select( #select and rename useful vars
    name=What.is.your.name., 
        role=What.is.your.role.in.your.current.position.,
         role_length=How.long.have.you.been.in.that.role.,
        polarion_use= How.frequently.do.you.use.Polarion., #data uncleaned
         polarion_skill=How.would.you.consider.your.skill.level.with.Polarion.,
         contains("difficult"),
          product
         ) %>%
    pivot_longer( #make it longer for sat data
      contains("difficult"), 
      names_to = "task"
    ) %>%
    mutate(
      #clean up task values
      task=gsub("[^0-9.-]", "", task),
      task=gsub("\\.", "", task),
      task=case_when(
        task == "" ~ 1,
        task == 1 ~ 2,
        task == 2 ~ 3,
        task == 3 ~ 4,
        task == 4 ~ 5,
        task == 5 ~ 6,
      ),
      task=as.character(as.numeric(task)),
      #rename satisfaction score
      satisfaction=value,
    ) 


#combine time/comp with sat df

df_full <- right_join(dfc,dfc_b,by=c("task","name","product"))

#make better task name
df_full <- df_full %>%
  mutate(
    task_name = factor(case_when(
      task==1 ~ "Create test case",
      task==2 ~ "Add requirement link",
      task==3 ~ "Create child requirement",
      task==4 ~ "Attach screenshot",
      task==5 ~ "Assign user test case",
      task==6 ~ "Find test case"
      
    ),
    levels = c(
     "Create test case",
      "Add requirement link",
     "Create child requirement",
      "Attach screenshot",
     "Assign user test case",
      "Find test case"
            )
      
      )
  )

#umux-lite
df_pt_u <- df_pt_b %>%
  select(name=What.is.your.name.,
         umux_req = PractiTest.s.capabilities.meet.my.requirements.,
         umux_ease = PractiTest.is.easy.to.use.,
         product
         )

df_xr_u <- df_xr_b %>%
  select(name=What.is.your.name.,
         umux_req = Xray.for.JIRA.s.capabilities.meet.my.requirements.,
         umux_ease = Xray.for.JIRA.is.easy.to.use.,
         product
  )

df_umux <- rbind(df_xr_u,df_pt_u)

#visualizing ----

#time
df_full %>%
  filter(completion=="1") %>%
  select(name,product,time,task_name) %>%
  ggplot(aes(x=product,y=time)) +
  geom_point(aes(color=product),
             position=position_jitterdodge(jitter.width = .1,seed=5),
             pch=20,
             size=4 ) +
  stat_summary(fun.y = mean, color = "red", size = 2) +
 # theme_minimal() +
  scale_fill_manual(values="Set1") +
  facet_grid(.~task_name) +
  labs(title = "Times to completion across tasks and products",
       subtitle="Unsuccessful trials removed",
       y="Seconds",x="")+
  theme(strip.text.x = element_text(size = 6))

ggsave(
  "time.png",
  device = "png",
  bg =  "transparent",
  width = 7,
  height = 5)

#completion
zval = 1.64
df_full %>%
  na.omit(completion) %>%
  group_by(product,task_name) %>%
  summarise(n=n(),
            prop=mean(as.numeric(as.character(completion)))) %>%
  mutate(pass=n*prop) %>%
  mutate(laplace = (pass + 1) / (n + 2)) %>% #laplace point estimate
  mutate(p_adj = (n * prop + (zval * zval) / 2) / (n + (zval * zval))) %>% #adjust p for wald calculation
  mutate(n_adj = n + (zval * zval)) %>% #adjust n for wald calculation
  mutate(marg =  zval * sqrt(p_adj * (1 - p_adj) / n_adj)) %>% #wald margin value
  mutate(lowerci = p_adj - marg) %>% #lower wald ci
  mutate(lowerci = ifelse(lowerci <= 0, 0, lowerci)) %>% #keep lower ci above 0
  mutate(upperci = p_adj + marg) %>% #upper wald ci
  mutate(upperci = ifelse(upperci >= 1, 1, upperci)) %>%
  ggplot(aes(x=task_name, y=prop,fill=product)) +  #vars to be plotted
  geom_bar(aes(fill=product),position=position_dodge(), stat="identity") + #make a bar plot with color palette
  #geom_abline(intercept=.78,slope=0, color = "gray",linetype = 2, size=2) + #horizontal benchmark line
  #geom_point(aes(y=laplace),position = position_dodge(width=.9)) +
  coord_cartesian(ylim=c(0,1)) + #limit y axis between 0-1
  geom_errorbar(aes(ymin=lowerci, ymax=upperci),position=position_dodge(.9), stat="identity",color="black",width=.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Task", y="Completion rates") +
  ggtitle(label = "Completion rates across all tasks",
          subtitle = "Confidence at 90%, based on LaPlace point esimates") +
  ggthemes::theme_tufte(base_family="GillSans") + 
  theme(
    axis.text.x = element_text(size = 6),
    axis.title.x = element_blank())

ggsave(
  "completion.png",
  device = "png",
  bg =  "transparent",
  width = 7,
  height = 5)


#satisfaction

df_full %>%
  na.omit(satisfaction) %>%
  group_by(task_name,product) %>%
  summarise(sd=sd(satisfaction),
            n=n(),
            satisfaction=mean(satisfaction)) %>%
  mutate(
    se=(sd / sqrt(n)),
    marg=se*zval,
    y.min = satisfaction-marg,
    y.max = satisfaction+marg
    )  %>%
  ggplot(aes(x=product,y=satisfaction,fill=product)) +
  geom_bar(stat="identity",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=y.min,ymax=y.max,width=.3),position = position_dodge(width=.9)) +
  facet_grid(.~task_name) +
  labs(title="Satisfaction scores across all tasks",
       y="Average satisfaction",x="")+
  theme(strip.text.x = element_text(size = 5),
        axis.text.x = element_blank())

ggsave(
  "sat.png",
  device = "png",
  bg =  "transparent",
  width = 7,
  height = 5)

#umux

df_umux %>%
  pivot_longer(contains("umux"),
               names_to = "question") %>%
  ggplot(aes(value,fill=product)) +
  geom_density(alpha=.3) +
  facet_wrap(.~question,ncol=1) +
  ggthemes::theme_tufte(base_family = "sans") +
  labs(title="UMUX-lite score distributions",
       y="Score value density (amount)")

ggsave(
  "umux.png",
  device = "png",
  bg =  "transparent",
  width = 7,
  height = 5)











