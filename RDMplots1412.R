install.packages(c("tidyverse","ggplot2","plyr","dplyr","scales"))
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)

#Load RDM_dataset
RDM_dataset <- read.csv("RDM_dataset.csv");#  read.csv('https://datahub.hku.hk/ndownloader/files/38527043')


#Faculty and occupation

#Renaming columns to make it shorter for nicer plots
RDM_dataset$Occupation <-recode(RDM_dataset$Occupation, 'Principal Investigators or academic staff' = 'PI/Academic staff', 'Research Postgraduate' = 'RPg', "Research focused staff (Post-Doc, RA, Bioinformatician etc)" = "Research focused staff", "Research Support staff (Technician)" = "Research support staff")
#Sort to alphabetical order 
RDM_dataset$Faculty <- factor(RDM_dataset$Faculty, levels = c('Social Sciences', 'Science', 'Medicine', 'Law','Engineering','Education','Dentistry','CETL', 'Business', 'Arts', 'Architecture'))

#Figure 1a. number of respondents from each faculty
RDM_dataset%>%
  ggplot(aes(Faculty, fill=Occupation))+
  geom_bar(position = "stack", alpha = 1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "Number of respondents from each faculty",
        x = "Faculty", y = "Count")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
  coord_flip()+
  theme(legend.position='bottom')+
  theme(axis.title.x = element_text(size= 18))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(legend.text = element_text(size=16))+
  guides(fill=guide_legend(nrow = 2, byrow=TRUE))


#Figure 1b Distribution of data sizes

#renaming data sizes for nicer plotting
RDM_dataset$Data_size <-recode(RDM_dataset$Data_size, 'Gigabytes' = 'Gigabytes', 'Terabytes (1-500 TB)' = 'Terabytes (low)', "Terabytes (500-1000TB)" = "Terabytes (high)", "Petabytes" = "Petabytes","Larger than Petabytes" = "Larger than Petabytes")
RDM_dataset$Data_size<- factor(RDM_dataset$Data_size, levels = c("I don't know", 'Larger than Petabytes', 'Petabytes','Terabytes (high)', 'Terabytes (low)', 'Gigabytes'))
RDM_dataset%>%
  ggplot(aes(Data_size, fill=Occupation))+
  geom_bar(position = "stack", alpha = 1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "Distribution of different data sizes",
        x = "Data size", y = "Count")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
  coord_flip()+
  theme(legend.position='bottom')+
  theme(axis.title.x = element_text(size=18, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(legend.text = element_text(size=16))+
  guides(fill=guide_legend(nrow = 2, byrow=TRUE))

#Figure 1c Faculty and data size 
RDM_dataset%>%
  ggplot(aes(Faculty, fill=Data_size))+
  geom_bar(position = "stack", alpha = 1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "Distribution sizes as stratified by faculty",
        x = "Faculty", y = "Count")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
  coord_flip()+
  theme(legend.position='bottom')+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(legend.text = element_text(size=16))+
  scale_fill_discrete(breaks=c("Gigabytes","Terabytes (low)", "Terabytes (high)","Petabytes", "Larger than Petabytes", "I don't know"))+
  guides(fill=guide_legend(nrow = 2, byrow=TRUE, title='Data sizes'))

#Figure 1d DMP revision as stratified by occupation
#DMP plan
RDM_dataset$DMP_revision <-recode(RDM_dataset$DMP_revision, 'Continuously updated throughout the study' = 'Continuously updated', 'Primarily at the beginning of the study' = 'Beginning of the study', "Primarily at the beginning and at the end of the study" = "Primarily at the beginning and the end", "Primarily at the end of the study" = "Primarily at the end")
RDM_dataset%>%
  ggplot(aes(Occupation, fill=DMP_revision))+
  scale_y_continuous(labels = scales::percent)+
  geom_bar(position = "fill", alpha = 1)+
  geom_text(aes(label = after_stat(count)), stat = "count", position = "fill", vjust=.5, hjust =2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "DMP revision as stratified by occupation",
        x = "Occupation", y = "Percentage")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
  coord_flip()+
  theme(legend.position='bottom')+
  theme(axis.title.x = element_text(size=18, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(legend.text = element_text(size=16))+
  guides(fill=guide_legend(nrow = 2, byrow=TRUE, title='DMP revision'))

#Figure 1e archive plan as stratified by data sizes
RDM_dataset%>%
  ggplot(aes(Data_size, fill=Archive_data))+
  geom_bar(position = "fill", alpha = 1)+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = ..count..), stat = "count", position = "fill", vjust=.5, hjust =2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "Archive plan as stratified by data sizes",
        x = "Data size", y = "Percentage")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
  coord_flip()+
  theme(legend.position='bottom')+
  theme(axis.title.x = element_text(size=18, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(legend.text = element_text(size=16))+
  scale_fill_discrete(breaks=c("Yes", "No"))+
  guides(fill=guide_legend(title='Archive plan'))

#Figure 1f deletion plan as stratified by data sizes
RDM_dataset%>%
  ggplot(aes(Data_size, fill=Deletion_plan))+
  geom_bar(position = "fill", alpha = 1)+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = after_stat(count)), stat = "count", position = "fill", vjust=.5, hjust =2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "Deletion plan as stratified by data sizes",
        x = "Data size", y = "Percentage")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
  coord_flip()+
  theme(legend.position='bottom')+
  theme(axis.title.x = element_text(size=18, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(legend.text = element_text(size=16))+
  scale_fill_discrete(breaks=c("Yes", "No"))+
  guides(fill=guide_legend(title='Deletion plan'))

#Figure 2 Data ownership

#creation of data frame after data restructuring and counting on excel 
df <- data.frame (value = c("91 (42%)", "28 (13%)", "33 (15%)", "39 (18%)", "27 (12%)"), 
                  group  = c("University owns", "I do not know", "Head of department owns", "I own", "Others"))

#using the above data frame to visualise Data ownership 
ggplot(df, aes(x = "", y = value, fill = group)) +
  geom_col() +
  coord_polar(theta = "y")+
  theme_void()+
  geom_text(aes(label = value),
            position = position_stack(vjust = 0.5), size =3.5)+
  labs (title = "Data ownership", hjust = .5)+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
  theme(axis.text.y = element_text(size=16))+
  theme(legend.text = element_text(size=16))+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title='Data ownership',nrow = 2, byrow=TRUE))

#Figure 3a who should pay for research data management costs
#Renaming for better plots
RDM_dataset$Pay_RDM_costs<-recode(RDM_dataset$Pay_RDM_costs, 'Co-financed by PI and University' = 'Co-financed by PI and University', 'Solely by principal investigator (PI)' = 'Solely PI', "Solely University" = "Solely University")


RDM_dataset%>%
  ggplot(aes(Occupation, fill=Pay_RDM_costs))+
  geom_bar(position = "stack", alpha = 1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "Who should pay for research data management costs?",
        x = "", y = "Count")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
  geom_text(aes(label = ..count..), stat = "count", position = "stack", hjust =1)+
  coord_flip()+
  theme(legend.position='bottom')+
  theme(axis.title.x = element_text(size=18, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(legend.text = element_text(size=18))+
  guides(fill=guide_legend(title='RDM financing',nrow = 2, byrow=TRUE))

#Figure 3b
RDM_dataset%>%
  filter(RDM_dataset$Data_size != "I don't know")%>%
  ggplot(aes(x=Proportion_supported_by_PI, y = Data_size))+
  geom_jitter(position = position_jitter(seed= 1, width = 0.2))+
  geom_boxplot(alpha = 0, fill = 'lightblue', outlier.shape = NA)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "What proportion of the costs of research data management should be supported by a PI?",
        x = "Proportion supported by PI", y = "")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
  theme(axis.title.x = element_text(size = 18))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(legend.text = element_text(size=16))

#Figure 4 Number of storage types utilised by respondents
#Creation of a new data frame after restructuring and counting were done on excel

df2 <- data.frame (value = c(39,57,51,38,17,12,1,3), 
                  group  = c("1","2","3","4","5","6","7","8"))

#calculating and inserting percentages into the dataframe
df2<- df2%>%
  dplyr::mutate(perc = scales::percent(value / sum(value), accuracy = .1, trim = FALSE))

#reordering for better plots
df2$group <- factor(df2$group, levels = c('8', '7', '6', '5','4','3','2','1'))

ggplot(df2, aes(x=group, y=value)) + 
  geom_col(fill = "#F8766D")+geom_text(aes(label = perc), hjust = -.05) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "Distribution of the number of storage types utilised",
        x = "No. of storage types", y = "count")+
  coord_flip()+
  theme(legend.position='bottom')+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))+
  theme(axis.title.x = element_text(size=18, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(legend.text = element_text(size=18))






  
