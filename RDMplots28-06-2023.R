install.packages(c("tidyverse","ggplot2","plyr","dplyr","scales", "stringr", "viridis", "gridExtra"))
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)
library(stringr)
library(viridis)
library(gridExtra)

#Load RDM_dataset
RDM_dataset <- read.csv('https://datahub.hku.hk/ndownloader/files/38527043')

#Faculty and occupation

#Renaming columns to make it shorter for nicer plots
RDM_dataset$Occupation <-recode(RDM_dataset$Occupation, 'Principal Investigators or academic staff' = 'PI/Academic Staff', 'Research Postgraduate' = 'RPg', "Research focused staff (Post-Doc, RA, Bioinformatician etc)" = "Research Focused Staff", "Research Support staff (Technician)" = "Research Support Staff")
#Sort to alphabetical order 
RDM_dataset$Faculty <- factor(RDM_dataset$Faculty, levels = c('Social Sciences', 'Science', 'Medicine', 'Law','Engineering','Education','Dentistry','CETL', 'Business', 'Arts', 'Architecture'))

#Figure 1a. number of respondents from each faculty
a1 <- RDM_dataset%>%
  ggplot(aes(Faculty, fill=Occupation))+
  geom_bar(position = "stack", alpha = 1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "(a)",
        x = "Faculty", y = "Number of Respondents")+
  theme(plot.title = element_text(hjust = 0, size = 8, margin = margin(0,0,-2.5,0)), plot.title.position = "plot")+
  coord_flip()+
  theme(legend.position='bottom', legend.justification='left', legend.margin=margin(t = -0.3, l = -2.4, unit='cm'))+
  theme(axis.title.x = element_text(size= 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size=8))+
  theme(axis.text.y = element_text(size=8))+
  theme(legend.title = element_text(size=8))+
  theme(legend.text = element_text(size=8))+
  guides(fill=guide_legend(title.position = "top", title.hjust = 0.5, nrow = 2, byrow=TRUE, keywidth = 0.5, keyheight = 0.5))+
  scale_fill_viridis(discrete = TRUE, option = "turbo", end = 0.8)

#Figure 1b Distribution of different data sizes

#renaming data sizes for nicer plotting
RDM_dataset$Data_size <-recode(RDM_dataset$Data_size, 'Gigabytes' = 'Gigabytes', 'Terabytes (1-500 TB)' = 'Terabytes (Low)', "Terabytes (500-1000TB)" = "Terabytes (High)", "Petabytes" = "Petabytes","Larger than Petabytes" = "> Petabytes","I don't know" = "I Don't Know")
RDM_dataset$Data_size<- factor(RDM_dataset$Data_size, levels = c("I Don't Know", '> Petabytes', 'Petabytes','Terabytes (High)', 'Terabytes (Low)', 'Gigabytes'))
b1 <- RDM_dataset%>%
  ggplot(aes(Data_size, fill=Occupation))+
  geom_bar(position = "stack", alpha = 1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "(b)",
        x = "Data Size", y = "Number of Respondents")+
  theme(plot.title = element_text(hjust = 0, size = 8, margin = margin(0,0,-2.5,0)), plot.title.position = "plot")+
  coord_flip()+
  theme(legend.position='bottom', legend.justification='left', legend.margin=margin(t = -0.3, l = -2.5, unit='cm'))+
  theme(axis.title.x = element_text(size=8, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size=8))+
  theme(axis.text.y = element_text(size=8))+
  theme(legend.title = element_text(size=8))+
  theme(legend.text = element_text(size=8))+
  guides(fill=guide_legend(title.position = "top", title.hjust = 0.5, nrow = 2, byrow=TRUE, keywidth = 0.5, keyheight = 0.5))+
  scale_fill_viridis(discrete = TRUE, option = "turbo", end = 0.8)

#Figure 1c Distribution sizes as stratified by faculty 
c1 <- RDM_dataset%>%
  ggplot(aes(Faculty, fill=Data_size))+
  geom_bar(position = "stack", alpha = 1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "(c)",
        x = "Faculty", y = "Number of Respondents")+
  theme(plot.title = element_text(hjust = 0, size = 8, margin = margin(0,0,-2.5,0)), plot.title.position = "plot")+
  coord_flip()+
  theme(legend.position='bottom', legend.justification='left', legend.margin=margin(t = -0.3, l = -2.4, unit='cm'))+
  theme(axis.title.x = element_text(size=8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size=8))+
  theme(axis.text.y = element_text(size=8))+
  theme(legend.title = element_text(size=8))+
  theme(legend.text = element_text(size=8))+
  scale_fill_viridis(discrete = TRUE, option = "turbo", breaks=c("Gigabytes","Terabytes (Low)", "Terabytes (High)","Petabytes", "> Petabytes", "I Don't Know"))+
  guides(fill=guide_legend(title.position = "top", title.hjust = 0.5, nrow = 2, byrow=TRUE, keywidth = 0.5, keyheight = 0.5, title='Data Sizes'))

#Figure 1d DMP revision as stratified by occupation
#DMP plan
RDM_dataset$DMP_revision <-recode(RDM_dataset$DMP_revision, 'Continuously updated throughout the study' = 'Continuously Updated', 'Primarily at the beginning of the study' = 'Beginning of the Study', "Primarily at the beginning and at the end of the study" = "Primarily at the Beginning and the End", "Primarily at the end of the study" = "Primarily at the End")
d1 <- RDM_dataset%>%
  ggplot(aes(str_wrap(Occupation, width = 20), fill=str_wrap(DMP_revision, width = 30)))+
  scale_y_continuous(labels = scales::percent)+
  geom_bar(position = "fill", alpha = 1)+
  geom_text(aes(label = after_stat(count)), stat = "count", position = "fill", vjust=.5, hjust =1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "(d)",
        x = "Occupation", y = "Percentage")+
  theme(plot.title = element_text(hjust = 0, size = 8, margin = margin(0,0,-2.5,0)), plot.title.position = "plot")+
  coord_flip()+
  theme(legend.position='bottom', legend.justification='left', legend.margin=margin(t = -0.3, l = -3, unit='cm'))+
  theme(axis.title.x = element_text(size=8, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size=8))+
  theme(axis.text.y = element_text(size=8))+
  theme(legend.title = element_text(size=8))+
  theme(legend.text = element_text(size=8))+
  guides(fill=guide_legend(title.position = "top", title.hjust = 0.5, nrow = 2, byrow=TRUE, keywidth = 0.5, keyheight = 0.5, title='DMP Revision'))+
  scale_fill_viridis(discrete = TRUE, option = "turbo", begin = 0.1, end = 0.8)

#Figure 1e Archive plan as stratified by data sizes
e1 <- RDM_dataset%>%
  ggplot(aes(Data_size, fill=Archive_data))+
  geom_bar(position = "fill", alpha = 1)+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = after_stat(count)), stat = "count", position = "fill", vjust=.5, hjust =1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "(e)",
        x = "Data Size", y = "Percentage")+
  theme(plot.title = element_text(hjust = 0, size = 8, margin = margin(0,0,-2.5,0)), plot.title.position = "plot")+
  coord_flip()+
  theme(legend.position='bottom', legend.justification='left', legend.margin=margin(t = -0.3, l = -0.9, unit='cm'))+
  theme(axis.title.x = element_text(size=8, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size=8))+
  theme(axis.text.y = element_text(size=8))+
  theme(legend.title = element_text(size=8))+
  theme(legend.text = element_text(size=8))+
  guides(fill=guide_legend(title.position = "left", keywidth = 0.5, keyheight = 0.5, title='Archive Plan'))+
  scale_fill_viridis(discrete = TRUE, option = "turbo", begin = 0.2, end = 0.7, breaks=c("Yes", "No"))

#Figure 1f Deletion plan as stratified by data sizes
f1 <- RDM_dataset%>%
  ggplot(aes(Data_size, fill=Deletion_plan))+
  geom_bar(position = "fill", alpha = 1)+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = after_stat(count)), stat = "count", position = "fill", vjust=.5, hjust =1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "(f)",
        x = "Data Size", y = "Percentage")+
  theme(plot.title = element_text(hjust = 0, size = 8, margin = margin(0,0,-2.5,0)), plot.title.position = "plot")+
  coord_flip()+
  theme(legend.position='bottom', legend.justification='left', legend.margin=margin(t = -0.3, l = -0.9, unit='cm'))+
  theme(axis.title.x = element_text(size=8, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.text.x = element_text(size=8))+
  theme(axis.text.y = element_text(size=8))+
  theme(legend.title = element_text(size=8))+
  theme(legend.text = element_text(size=8))+
  guides(fill=guide_legend(title.position = "left", keywidth = 0.5, keyheight = 0.5, title='Deletion Plan'))+
  scale_fill_viridis(discrete = TRUE, option = "turbo", begin = 0.2, end = 0.7, breaks=c("Yes", "No"))

# Combine a1 to f1 into Figure 1
Figure1 <- grid.arrange(a1, b1, c1, d1, e1, f1, nrow = 3, ncol = 2)
ggsave(filename = file.path("Figures", "Figure1.png"), plot = Figure1, device = "png", width = 6000, height = 6000, units = "px", dpi = 1000)

#Figure 2 Data ownership

#creation of data frame after data restructuring and counting on excel 
df <- data.frame (value = str_wrap(c("91 (42%)", "28 (13%)", "33 (15%)", "39 (18%)", "27 (12%)"), width = 2), 
                  group  = c("University Owns", "I Do Not Know", "Head of Department Owns", "I Own", "Others"))

#using the above data frame to visualise Data ownership 
ggplot(df, aes(x = "", y = value, fill = group)) +
  geom_col() +
  coord_polar(theta = "y")+
  theme_void()+
  geom_text(aes(label = value),
            position = position_stack(vjust = 0.5), size =4)+
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  theme(axis.text.y = element_text(size=10))+
  theme(legend.title = element_text(size=10))+
  theme(legend.text = element_text(size=10))+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title.position = "top", title.hjust = 0.5, title='Data Ownership', nrow = 3, byrow=TRUE, keywidth = 1, keyheight = 1))+
  scale_fill_viridis(discrete = TRUE, option = "turbo", begin = 0.3, end = 0.85)
ggsave(filename = file.path("Figures", "Figure2.png"), device = "png", width = 3500, height = 4200, units = "px", dpi = 1000, bg = "white")

#Figure 3a Who should pay for research data management costs?
#Renaming for better plots
RDM_dataset$Pay_RDM_costs<-recode(RDM_dataset$Pay_RDM_costs, 'Co-financed by PI and University' = 'Co-financed by PI and University', 'Solely by principal investigator (PI)' = 'Solely PI', "Solely University" = "Solely University")

a3 <- RDM_dataset%>%
  ggplot(aes(str_wrap(Occupation, width = 15), fill=Pay_RDM_costs))+
  geom_bar(position = "stack", alpha = 1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "(a)",
        x = "", y = "Number of Respondents")+
  theme(plot.title = element_text(hjust = 0, size = 12, margin = margin(0,0,-2.5,0)), plot.title.position = "plot")+
  geom_text(aes(label = after_stat(count)), stat = "count", position = "stack", hjust =1)+
  coord_flip()+
  theme(legend.position='bottom', legend.justification='center', legend.margin=margin(l = -2.8, unit='cm'), legend.spacing.x = unit(0.25, 'cm'))+
  theme(axis.title.x = element_text(size=10, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.x = element_text(size=10))+
  theme(axis.text.y = element_text(size=10))+
  theme(legend.title = element_text(size=10))+
  theme(legend.text = element_text(size=10))+
  guides(fill=guide_legend(title='RDM Financing Preference', title.position = "top", title.hjust = 0.5, keywidth = 1, keyheight = 1))+
  scale_fill_viridis(discrete = TRUE, option = "turbo", begin = 0.2, end = 0.7)

#Figure 3b What proportion of the costs of research data management should be supported by a PI?
b3 <- RDM_dataset%>%
  filter(RDM_dataset$Data_size != "I Don't Know")%>%
  ggplot(aes(x=Proportion_supported_by_PI, y = Data_size))+
  geom_jitter(position = position_jitter(seed= 1, width = 0.2))+
  geom_boxplot(alpha = 0, fill = 'lightblue', outlier.shape = NA)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (title = "(b)",
        x = "Proportion of RDM Costs That Should be Supported by PI / %", y = "")+
  theme(plot.title = element_text(hjust = 0, size = 12, margin = margin(0,0,-2.5,0)), plot.title.position = "plot")+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.text.x = element_text(size=10))+
  theme(axis.text.y = element_text(size=10))+
  theme(legend.text = element_text(size=10))

#Combine a3 and b3 into Figure 3
Figure3 <- grid.arrange(a3, b3, nrow = 2)
ggsave(filename = file.path("Figures", "Figure3.png"), plot = Figure3, device = "png", width = 6000, height = 5400, units = "px", dpi = 1000)

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
  geom_col(fill = "#F8766D")+geom_text(aes(label = perc), hjust = -0.05) +
  scale_y_continuous(limits=c(0,60))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs (x = "No. of Storage Types", y = "Number of Respondents")+
  coord_flip()+
  theme(legend.position='bottom')+
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  theme(axis.title.x = element_text(size=10, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.x = element_text(size=10))+
  theme(axis.text.y = element_text(size=10))+
  theme(legend.text = element_text(size=10))
ggsave(filename = file.path("Figures", "Figure4.png"), device = "png", width = 6000, height = 2700, units = "px", dpi = 1000)