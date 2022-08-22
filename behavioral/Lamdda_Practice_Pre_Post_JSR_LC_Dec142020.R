#load functions
#setwd("/Users/rudoler/Desktop/Data and Analysis/Analyses") #Windows
#setwd("/Users/jeremyrudoler/Desktop/oak_mount/projects/lchen32/2018_MathFUN_mindset/data/behavior/Subject_Selection") #connect to oak from jeremy's laptop
source("R_functions.R")  #version 1.9.0 used

#read in data for all visits, set equal to fullData
#setwd("/Users/rudoler/Desktop/Data and Analysis/Raw_Data") #Windows
fullData<-read.csv("Behavioral_Data_Full.csv", sep=",")
colnames(fullData)[5]<-"Age"

###################################################################################
#####Data filtering for Pre and Post data only#####################################
#remove the WJ calculated grades and version
fullData<-fullData[,-which(colnames(fullData) %in% c("wj.iii_calculated.grade","wj.iii_version"))]
##remove columns with raw data or percentile data
data<-fullData[,-c(grep("_raw",colnames(fullData)),grep("_percentile",colnames(fullData)))]
##remove data from visit 3 and visit 4 to get the pre and post data (ppdata)
ppdata<-data[data$Visit<3,]
#Filter the data and only keep the variables needed for the data analysis (selected = sl)
sldata<-ppdata[,grepl("PID|Visit|Group|Gender|Age|wasi|wj.iii|ATL|Add",colnames(ppdata))]
sldata<-sldata[,-grep("vocab|block|similarities|matrix|word|ATL_Math|Read|Intelligence",colnames(sldata))]
##rename the columns for abbreviation 
colnames(sldata)[6:11]<-c("VIQ","PIQ","FSIQ","WJ_Cal","WJ_MFlu","WJ_AReas")
#Calculate scanner task efficiency
sldata$Add_Efficiency <- sldata$Add_Accuracy / (sldata$Add_RT_corr/1000)
##reorder the factor levels of Group
sldata$Group<-factor(sldata$Group,levels=c("Tutoring","NCC"))
##save the data
write.csv(sldata,"selected_Pre_Post_data_MathFUN.csv",row.names=F)
##make the wide form of the data using PID as the matching variable
sldata_w<-read.csv("selected_Pre_Post_data_MathFUN.csv")
sldata_w<-reshape(sldata,idvar=c("PID","Group","Gender"),timevar="Visit",direction="wide",sep="_")
##exclude IQ for time 2 (not measured)
sldata_w<-sldata_w[,-grep("IQ_2",colnames(sldata_w))]
colnames(sldata_w)[grep("IQ_1",colnames(sldata_w))]<-c("VIQ","PIQ","FSIQ")
###factorize the PID
sldata_w$PID<-as.factor(sldata_w$PID)
write.csv(sldata_w, "selected_Pre_Post_data_MathFUN_wideformat.csv",row.names=F)
###some demographic info
nrow(sldata_w)
table(sldata_w[,c("Group","Gender")])
chisq.test(table(sldata_w[,c("Group","Gender")]))

###################################################################################
###exclude subjects have missing data patterns#####################################
##exclude subjects have missing ATL at both pre and post
#sldata_w<-sldata_w[(is.na(sldata_w$ATL_Total_1)+is.na(sldata_w$ATL_Total_2))<2,]
###chages made on May 13th; exclude those who have missing data for either pre or post
sldata_w<-read.csv("selected_Pre_Post_data_MathFUN_wideformat.csv")
##Missing pattern
sum(is.na(sldata_w$ATL_Total_1))
sum(is.na(sldata_w$ATL_Total_2))
sum(is.na(sldata_w$ATL_Total_1) & is.na(sldata_w$ATL_Total_2))
sldata_w<-sldata_w[(!is.na(sldata_w$ATL_Total_1) & !is.na(sldata_w$ATL_Total_2)),]
nrow(sldata_w)
table(sldata_w[,c("Group","Gender")])
chisq.test(table(sldata_w[,c("Group","Gender")]))
summary(sldata_w$Age_1)
sd(sldata_w$Age_1)
##check the missing pattern for each subject  ##just for checking, did not do exclusion based on this (June 18th, 2020)
misscount<-cbind(sldata_w$PID,rowSums(is.na(sldata_w))/ncol(sldata_w))
histogram(misscount[,2])
##listwise exclude subjects with more than 40% of the data
#sldata_w<-sldata_w[misscount[,2]<0.4,]

###NEW May 13th: exclude the NCC-MD (too small)
#sldata_w<-sldata_w[!(sldata_w$Group=="NCC" & sldata_w$WJ_MFlu_1<90),]
#summary(sldata_w$Group)
#summary(sldata_w)
###################################################################################
#calculate Post-Pre differences
diff<-sldata_w[,which(colnames(sldata_w)=="WJ_Cal_2"):which(colnames(sldata_w)=="Add_Efficiency_2")]-
      sldata_w[,which(colnames(sldata_w)=="WJ_Cal_1"):which(colnames(sldata_w)=="Add_Efficiency_1")]
colnames(diff)<-sub("_2","_2v1",colnames(diff))
sldata_w<-as.data.frame(cbind(sldata_w,diff))
write.csv(sldata_w,"selected_Pre_Post_data_MathFUN_wideformat_withDiff.csv",row.names=F)
###################################################################################
###End of the data filtering and manipulation######################################
###################################################################################

###################################################################################
##Three group approach:Tut_TD,Tut_MD,NCC_TD
######################
sldata_w<-read.csv("selected_Pre_Post_data_MathFUN_wideformat_withDiff.csv")
##group comparison of time 1 measures and Delta scores (Tut vs. NCC)
mk_ttable(sldata_w,"Group",c("NCC","Tutoring"),which(colnames(sldata_w)=="Age_1"),"MathFUN_Pre_Post",3,3)

###make Math Group in Tutoring
##define MD on MathFlu
sldata_w$MathGroup<- as.factor(ifelse(sldata_w$Group == "NCC", "NCC", ifelse(sldata_w$WJ_MFlu_1 < 90, "MD", "TD")))
##reorder levels of MathGroup
sldata_w$MathGroup<-factor(sldata_w$MathGroup,levels=c("MD","TD","NCC"))
summary(sldata_w$MathGroup)
####
table(sldata_w[,c("MathGroup","Gender")])
mytable<-table(sldata_w[,c("MathGroup","Gender")])
chisq.test(mytable)
table(sldata_w[sldata_w$MathGroup!="NCC",c("MathGroup","Gender")])
chisq.test(table(sldata_w[sldata_w$MathGroup!="NCC",c("MathGroup","Gender")])[-3,])
##move the column to the front
sldata_w<-sldata_w[,c(1:2,which(colnames(sldata_w)=="MathGroup"),3:(ncol(sldata_w)-1))]
##reorder the factor level of MathGroup to make the NCC come at last
sldata_w$MathGroup<-factor(sldata_w$MathGroup,levels=c("MD","TD","NCC"))
###Tut vs. NCC
mk_ttable(sldata_w,"Group",c("Tutoring","NCC"),
          which(colnames(sldata_w)=="Age_1"),"MathFUN_Pre_Post",3,3,equalvar = T)
####Tut_MD vs. Tut_TD
mk_ttable(sldata_w,"MathGroup",c("MD","TD"),
          which(colnames(sldata_w)=="Age_1"),"MathFUN_Pre_Post",3,3,equalvar = T)
####Tut_MD vs. NCC
mk_ttable(sldata_w,"MathGroup",c("MD","NCC"),
          which(colnames(sldata_w)=="Age_1"),"MathFUN_Pre_Post",3,3,equalvar = T)
####Tut_TD vs. NCC
mk_ttable(sldata_w,"MathGroup",c("TD","NCC"),
          which(colnames(sldata_w)=="Age_1"),"MathFUN_Pre_Post",3,3,equalvar = T)

#######Pre Post change of ATL
####Two-group
data_atl<-sldata_w[,c(which(colnames(sldata_w) %in% c("PID","Group")),grep("ATL_Total_",colnames(sldata_w)))]
data_atl_long<-melt(data_atl[,-5],id.vars = c("PID","Group"))
data_atl_long$PID<-as.factor(data_atl_long$PID)
sink("ATL_pre_post_2group_ANOVA.txt")
cat("Group comp planned t-tests\n")
t.test(value~Group,data = data_atl_long[data_atl_long$variable=="ATL_Total_1",],var.equal =T)
t.test(value~Group,data = data_atl_long[data_atl_long$variable=="ATL_Total_2",],var.equal =T)
require(effsize)
cohen.d(value~Group,data = data_atl_long[data_atl_long$variable=="ATL_Total_1",],var.equal =T)
cohen.d(value~Group,data = data_atl_long[data_atl_long$variable=="ATL_Total_2",],var.equal =T)
cat("Omnibus ANOVA\n")
mod<-aov(value~Group*variable+Error(PID/variable),data=data_atl_long)
summary(mod)
t.test(value~variable,data = data_atl_long[data_atl_long$Group=="Tutoring",],paired=T)
t.test(value~variable,data = data_atl_long[data_atl_long$Group=="NCC",],paired=T)

cohen.d(value~variable | Subject(PID),data=data_atl_long[data_atl_long$Group=="Tutoring",], paired =T)
cohen.d(value~variable | Subject(PID),data=data_atl_long[data_atl_long$Group=="NCC",], paired =T)
sink()
##figure in Tutoring
data_atl_tut<-summarySE(data_atl_long[data_atl_long$Group=="Tutoring",],groupvars = c("variable"),
                        measurevar = "value")
atl_line_tut<-ggplot(data_atl_tut,aes(x=variable,y=value,group=1,color="firebrick1")) +
  geom_line(data=data_atl_long[data_atl_long$Group=="Tutoring",],aes(x=variable,y=value,group=PID),
            size = 0.5,color="gray") +
  #geom_point(data=data_atl_long[data_atl_long$Group=="Tutoring",],aes(x=variable,y=value,group=PID),
  #          size = 1,color="gray") +
  geom_line(size=2) + 
  #geom_point(size = 2,color="firebrick1") +
  geom_errorbar(aes(ymin=value-se,ymax=value+se),width=0.2,size=1) +
  scale_x_discrete(name="",labels=c("Pre","Post")) +
  scale_y_continuous(name="Growth Mindset",limits = c(2,5)) +
  theme_bw()

atl_line_tut<-atl_line_tut +
  theme(text = element_text(size = 20, colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")
print(atl_line_tut) 

png(file="Mindset_ATL_Pre_Post_Tutoring_line.png",width = 1500,height = 1500,res = 300)
print(atl_line_tut)
dev.off()
##figure in NCC
data_atl_ncc<-summarySE(data_atl_long[data_atl_long$Group=="NCC",],groupvars = c("variable"),
                        measurevar = "value")
atl_line_ncc<-ggplot(data_atl_ncc,aes(x=variable,y=value,group=1,color="deepskyblue1")) +
  geom_line(data=data_atl_long[data_atl_long$Group=="NCC",],aes(x=variable,y=value,group=PID),
            size = 0.5,color="gray") +
  #geom_point(data=data_atl_long[data_atl_long$Group=="NCC",],aes(x=variable,y=value,group=PID),
  #           size = 1,color="gray") +
  geom_line(size=2,color="deepskyblue1") + 
  #geom_point(size = 2,color="deepskyblue1") +
  geom_errorbar(aes(ymin=value-se,ymax=value+se),width=0.2,size=1,color="deepskyblue1") +
  scale_x_discrete(name="",labels=c("Pre","Post")) +
  scale_y_continuous(name="Growth Mindset",limits = c(2,5)) +
  theme_bw()

atl_line_ncc<-atl_line_ncc +
  theme(text = element_text(size = 20, colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")
print(atl_line_ncc) 

png(file="Mindset_ATL_Pre_Post_NCC_line.png",width = 1500,height = 1500,res = 300)
print(atl_line_ncc)
dev.off()

###Three-group
##bargraph for the change of scores in ATL
data_atl<-sldata_w[,c(which(colnames(sldata_w) %in% c("PID","MathGroup")),grep("ATL_Total_",colnames(sldata_w)))]
# mk_grpbargraph(data_atl,2,3:4,c("MD","TD","NCC"),0,"number",1) ##by time
# atl_bar<-mk_grpbargraph(data_atl,2,3:4,c("Pre","Post"),1,"star",2.5) #by group
# ####fine-tuning the graph
# atl_bar<-atl_bar + 
#   theme (text = element_text(size = 20, colour = "black"),panel.grid.major = element_blank(),
#                           panel.grid.minor = element_blank(),legend.position = c(0.85,0.1),legend.title = element_blank(),
#                           legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
#   scale_fill_manual(labels=c("Pre","Post"),values = c("lightgoldenrod1","limegreen"))
# 
# print(atl_bar)
####use line graphs for keeping the color scheme consistent
data_atl_long<-melt(data_atl[,-5],id.vars = c("PID","MathGroup"))
data_atl_summ<-summarySE(data_atl_long,measurevar = c("value"),groupvars = c("MathGroup","variable"))
atl_line<-ggplot(data = data_atl_summ, aes(x = variable, y = value, group = MathGroup, color=MathGroup)) +
  geom_line(data = data_atl_long[data_atl_long$MathGroup=="MD",],aes(x=variable,y=value,group=PID),size=0.3,
            color="firebrick1",alpha =0.8) +
  geom_line(data = data_atl_long[data_atl_long$MathGroup=="TD",],aes(x=variable,y=value,group=PID),size=0.3,
            color="lightpink1",alpha =0.8) +
  geom_line(data = data_atl_long[data_atl_long$MathGroup=="NCC",],aes(x=variable,y=value,group=PID),size=0.3,
            color="deepskyblue1",alpha =0.4) +
  geom_line(data = data_atl_summ, aes(x = variable, y = value, group = MathGroup, color=MathGroup),
            size = 2) + 
  #geom_point(data = data_atl_long,aes(x=variable,y=value,fill=MathGroup),shape=21,size=1.5,color="white",stroke=0.4,
  #           position = position_jitter(width = 0.2))+
  geom_errorbar(data = data_atl_summ,aes(ymin=value-se,ymax=value+se),width=0.2,size=1) +
  scale_x_discrete(name="",labels=c("Pre","Post")) +
  scale_y_continuous(name="Growth Mindset",limits = c(2,5)) +
  scale_color_manual(values = c("firebrick1","lightpink1","deepskyblue1")) +
  scale_fill_manual(values = c("firebrick1","lightpink1","deepskyblue1")) +
  theme_bw()
##fine-tuning the graph
atl_line<-atl_line + 
  theme (text = element_text(size = 20, colour = "black"),panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),legend.position = c(0.85,0.15),legend.title = element_blank(),
         legend.background = element_blank()) 
print(atl_line)

png(file="Mindset_ATL_Pre_Post_3Group_line.png",width = 1500,height = 1500,res = 300)
print(atl_line)
dev.off()
###MD vs. NCC
atl_line_mdncc<-ggplot(data = data_atl_summ[data_atl_summ$MathGroup!="TD",], aes(x = variable, y = value, group = MathGroup, color=MathGroup)) +
  geom_line(data = data_atl_long[data_atl_long$MathGroup=="MD",],aes(x=variable,y=value,group=PID),size=0.3,
            color="firebrick1",alpha =0.6) +
  geom_line(data = data_atl_long[data_atl_long$MathGroup=="NCC",],aes(x=variable,y=value,group=PID),size=0.3,
            color="deepskyblue1",alpha =0.4) +
  geom_line(data = data_atl_summ[data_atl_summ$MathGroup!="TD",], aes(x = variable, y = value, group = MathGroup, color=MathGroup),
            size = 2) + 
  #geom_point(data = data_atl_long,aes(x=variable,y=value,fill=MathGroup),shape=21,size=1.5,color="white",stroke=0.4,
  #           position = position_jitter(width = 0.2))+
  geom_errorbar(data = data_atl_summ[data_atl_summ$MathGroup!="TD",],aes(ymin=value-se,ymax=value+se),width=0.2,size=1) +
  scale_x_discrete(name="",labels=c("Pre","Post")) +
  scale_y_continuous(name="Growth Mindset",limits = c(2,5)) +
  scale_color_manual(values = c("firebrick1","deepskyblue1")) +
  scale_fill_manual(values = c("firebrick1","deepskyblue1")) +
  theme_bw()
##fine-tuning the graph
atl_line_mdncc<-atl_line_mdncc + 
  theme (text = element_text(size = 20, colour = "black"),panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),legend.position = c(0.85,0.15),legend.title = element_blank(),
         legend.background = element_blank()) 
print(atl_line_mdncc)

png(file="Mindset_ATL_Pre_Post_MLDvsNCC_line.png",width = 1500,height = 1500,res = 300)
print(atl_line_mdncc)
dev.off()

####TD vs. NCC
atl_line_tdncc<-ggplot(data = data_atl_summ[data_atl_summ$MathGroup!="MD",], aes(x = variable, y = value, group = MathGroup, color=MathGroup)) +
  geom_line(data = data_atl_long[data_atl_long$MathGroup=="TD",],aes(x=variable,y=value,group=PID),size=0.3,
            color="lightpink1",alpha =0.8) +
  geom_line(data = data_atl_long[data_atl_long$MathGroup=="NCC",],aes(x=variable,y=value,group=PID),size=0.3,
            color="deepskyblue1",alpha =0.4) +
  geom_line(data = data_atl_summ[data_atl_summ$MathGroup!="MD",], aes(x = variable, y = value, group = MathGroup, color=MathGroup),
            size = 2) + 
  #geom_point(data = data_atl_long,aes(x=variable,y=value,fill=MathGroup),shape=21,size=1.5,color="white",stroke=0.4,
  #           position = position_jitter(width = 0.2))+
  geom_errorbar(data = data_atl_summ[data_atl_summ$MathGroup!="MD",],aes(ymin=value-se,ymax=value+se),width=0.2,size=1) +
  scale_x_discrete(name="",labels=c("Pre","Post")) +
  scale_y_continuous(name="Growth Mindset",limits = c(2,5)) +
  scale_color_manual(values = c("lightpink1","deepskyblue1")) +
  scale_fill_manual(values = c("lightpink1","deepskyblue1")) +
  theme_bw()
##fine-tuning the graph
atl_line_tdncc<-atl_line_tdncc + 
  theme (text = element_text(size = 20, colour = "black"),panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),legend.position = c(0.85,0.15),legend.title = element_blank(),
         legend.background = element_blank()) 
print(atl_line_tdncc)

png(file="Mindset_ATL_Pre_Post_TDvsNCC_line.png",width = 1500,height = 1500,res = 300)
print(atl_line_tdncc)
dev.off()

####TD vs. MD
atl_line_tdmd<-ggplot(data = data_atl_summ[data_atl_summ$MathGroup!="NCC",], aes(x = variable, y = value, group = MathGroup, color=MathGroup)) +
  geom_line(data = data_atl_long[data_atl_long$MathGroup=="MD",],aes(x=variable,y=value,group=PID),size=0.3,
            color="firebrick1",alpha =0.8) +
  geom_line(data = data_atl_long[data_atl_long$MathGroup=="TD",],aes(x=variable,y=value,group=PID),size=0.3,
            color="lightpink1",alpha =0.5) +
  geom_line(data = data_atl_summ[data_atl_summ$MathGroup!="NCC",], aes(x = variable, y = value, group = MathGroup, color=MathGroup),
            size = 2) + 
  #geom_point(data = data_atl_long,aes(x=variable,y=value,fill=MathGroup),shape=21,size=1.5,color="white",stroke=0.4,
  #           position = position_jitter(width = 0.2))+
  geom_errorbar(data = data_atl_summ[data_atl_summ$MathGroup!="NCC",],aes(ymin=value-se,ymax=value+se),width=0.2,size=1) +
  scale_x_discrete(name="",labels=c("Pre","Post")) +
  scale_y_continuous(name="Growth Mindset",limits = c(2,5)) +
  scale_color_manual(values = c("firebrick1","lightpink1")) +
  scale_fill_manual(values = c("firebrick1","lightpink1")) +
  theme_bw()
##fine-tuning the graph
atl_line_tdmd<-atl_line_tdmd + 
  theme (text = element_text(size = 20, colour = "black"),panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),legend.position = c(0.85,0.15),legend.title = element_blank(),
         legend.background = element_blank()) 
print(atl_line_tdmd)

png(file="Mindset_ATL_Pre_Post_MDvsTD_line.png",width = 1500,height = 1500,res = 300)
print(atl_line_tdmd)
dev.off()

###Omnibus ANOVA test
data_atl_long<-melt(data_atl[,-5],id.vars = c("PID","MathGroup"))
data_atl_long$PID<-as.factor(data_atl_long$PID)
mod<-aov(value~MathGroup*variable+Error(PID/variable),data=data_atl_long)
summary(mod)
t1<-t.test(value~variable,data=data_atl_long[data_atl_long$MathGroup=="MD",],paired =T, var.equal=T)
print(t1)
t2<-t.test(value~variable,data=data_atl_long[data_atl_long$MathGroup=="TD",],paired =T, var.equal=T)
print(t2)
t3<-t.test(value~variable,data=data_atl_long[data_atl_long$MathGroup=="NCC",],paired =T, var.equal=T)
print(t3)
##Simple interaction
mod1<-aov(value~MathGroup*variable+Error(PID/variable),data=data_atl_long[data_atl_long$MathGroup!="TD",])
summary(mod1)
mod2<-aov(value~MathGroup*variable+Error(PID/variable),data=data_atl_long[data_atl_long$MathGroup!="MD",])
summary(mod2)
mod3<-aov(value~MathGroup*variable+Error(PID/variable),data=data_atl_long[data_atl_long$MathGroup!="NCC",])
summary(mod3)
sink(file="ATL_Pre_Post_Change_3Group_ANOVA.txt")
print(summary(mod))
print(t1)
print(t2)
print(t3)
cohen.d(value~variable | Subject(PID),data=data_atl_long[data_atl_long$MathGroup=="MD",], paired =T)
cohen.d(value~variable | Subject(PID),data=data_atl_long[data_atl_long$MathGroup=="TD",], paired =T)
cohen.d(value~variable | Subject(PID),data=data_atl_long[data_atl_long$MathGroup=="NCC",], paired =T)
print("MLD vs. NCC")
print(summary(mod1))
print("TD vs. NCC")
print(summary(mod2))
print("MLD vs. TD")
print(summary(mod3))
sink()

#######Pre Post change of Math Performance
# ##bargraph for the change of scores in Add_Acc
data_acc<-na.omit(sldata_w[,c("PID","MathGroup","Add_Accuracy_1","Add_Accuracy_2"),])
# mk_grpbargraph(data_acc,2,3:4,c("MD","TD","NCC"),0,"number",1) #by time
# acc_bar<-mk_grpbargraph(data_acc,2,3:4,c("Pre","Post"),1,"star",2) #by group
# ##fine-tune the graph
# acc_bar<-acc_bar + 
#   theme (text = element_text(size = 20, colour = "black"),panel.grid.major = element_blank(),
#          panel.grid.minor = element_blank(),legend.position = c(0.85,0.1),legend.title = element_blank(),
#          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
#   scale_fill_manual(labels=c("Pre","Post"),values = c("lightgoldenrod1","limegreen"))
# print(acc_bar)

####use line graphs for keeping the color scheme consistent
data_acc_long<-melt(data_acc,id.vars = c("PID","MathGroup"))
data_acc_summ<-summarySE(data_acc_long,measurevar = c("value"),groupvars = c("MathGroup","variable"))
acc_line<-ggplot(data = data_acc_summ, aes(x = variable, y = value, group = MathGroup, color=MathGroup)) +
  geom_line(data = data_acc_long[data_acc_long$MathGroup=="MD",],aes(x=variable,y=value,group=PID),size=0.3,
            color="firebrick1",alpha =0.8) +
  geom_line(data = data_acc_long[data_acc_long$MathGroup=="TD",],aes(x=variable,y=value,group=PID),size=0.3,
            color="lightpink1",alpha =0.8) +
  geom_line(data = data_acc_long[data_acc_long$MathGroup=="NCC",],aes(x=variable,y=value,group=PID),size=0.3,
            color="deepskyblue1",alpha =0.4) +
  geom_line(size = 2) + 
  geom_errorbar(aes(ymin=value-se,ymax=value+se),width=0.2,size=1) +
  scale_x_discrete(name="",labels=c("Pre","Post")) +
  scale_y_continuous(name="Accuracy (Add)",limits = c(0.4,1.0)) +
  scale_color_manual(values = c("firebrick1","lightpink1","deepskyblue1")) +
  scale_fill_manual(values = c("firebrick1","lightpink1","deepskyblue1")) +
  theme_bw()
##fine-tuning the graph
acc_line<-acc_line + 
  theme (text = element_text(size = 20, colour = "black"),panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),legend.position = "none") 
print(acc_line)

png(file="Mindset_AddAcc_Pre_Post_3Group_line.png",width = 1500,height = 1500,res = 300)
print(acc_line)
dev.off()
##ANOVA test
data_acc_long<-melt(data_acc,id.vars = c("PID","MathGroup"))
data_acc_long$PID<-as.factor(data_acc_long$PID)
mod<-aov(value~MathGroup*variable+Error(PID/variable),data=data_acc_long)
summary(mod)
t1<-t.test(value~variable,data=data_acc_long[data_acc_long$MathGroup=="MD",],paired =T, var.equal=T)
print(t1)
t2<-t.test(value~variable,data=data_acc_long[data_acc_long$MathGroup=="TD",],paired =T, var.equal=T)
print(t2)
t3<-t.test(value~variable,data=data_acc_long[data_acc_long$MathGroup=="NCC",],paired =T, var.equal=T)
print(t3)
##Simple interaction
mod1<-aov(value~MathGroup*variable+Error(PID/variable),data=data_acc_long[data_acc_long$MathGroup!="TD",])
summary(mod1)
mod2<-aov(value~MathGroup*variable+Error(PID/variable),data=data_acc_long[data_acc_long$MathGroup!="MD",])
summary(mod2)
mod3<-aov(value~MathGroup*variable+Error(PID/variable),data=data_acc_long[data_acc_long$MathGroup!="NCC",])
summary(mod3)
sink(file="AddAcc_Pre_Post_3Group_ANOVA.txt")
print(summary(mod))
print(t1)
print(t2)
print(t3)
cohen.d(value~variable | Subject(PID),data=data_acc_long[data_acc_long$MathGroup=="MD",], paired =T)
cohen.d(value~variable | Subject(PID),data=data_acc_long[data_acc_long$MathGroup=="TD",], paired =T)
cohen.d(value~variable | Subject(PID),data=data_acc_long[data_acc_long$MathGroup=="NCC",], paired =T)
print("MLD vs. NCC")
print(summary(mod1))
print("TD vs. NCC")
print(summary(mod2))
print("MLD vs. TD")
print(summary(mod3))
sink()

# ##bargraph for the change of scores in Add_Eff
# data_eff<-na.omit(sldata_w[,c("PID","MathGroup","Add_Efficiency_1","Add_Efficiency_2"),])
# mk_grpbargraph(data_eff,2,3:4,c("MD","TD","NCC"),0,"number",1) #by time
# mk_grpbargraph(data_eff,2,3:4,c("Pre","Post"),1,"number",2) #by group
# 
# ##bargraph for the change of scores in WJ_Cal
# data_cal<-na.omit(sldata_w[,which(colnames(sldata_w) %in% c("PID","MathGroup","WJ_Cal_1","WJ_Cal_2"))])
# mk_grpbargraph(data_cal,2,3:4,c("MD","TD","NCC"),0,"number",1) ##by time
# mk_grpbargraph(data_cal,2,3:4,c("Pre","Post"),1,"number",1) #by group

##bargraph for the change of scores in WJ_MFlu
data_MFlu<-na.omit(sldata_w[,which(colnames(sldata_w) %in% c("PID","MathGroup","WJ_MFlu_1","WJ_MFlu_2"))])
# mk_grpbargraph(data_MFlu,2,3:4,c("MD","TD","NCC"),0,"number",1) ##by time
# mflu_bar<-mk_grpbargraph(data_MFlu,2,3:4,c("Pre","Post"),1,"star",2.5) #by group
# ##Fine-tune the graph
# mflu_bar<-mflu_bar + 
#   theme (text = element_text(size = 20, colour = "black"),panel.grid.major = element_blank(),
#          panel.grid.minor = element_blank(),legend.position = c(0.85,0.1),legend.title = element_blank(),
#          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
#   scale_fill_manual(labels=c("Pre","Post"),values = c("lightgoldenrod1","limegreen")) +
#   coord_cartesian(ylim=c(60,120))
# print(mflu_bar)

####use line graphs for keeping the color scheme consistent
data_mflu_long<-melt(data_MFlu,id.vars = c("PID","MathGroup"))
data_mflu_summ<-summarySE(data_mflu_long,measurevar = c("value"),groupvars = c("MathGroup","variable"))
mflu_line<-ggplot(data = data_mflu_summ, aes(x = variable, y = value, group = MathGroup, color=MathGroup)) +
  geom_line(data = data_mflu_long[data_mflu_long$MathGroup=="MD",],aes(x=variable,y=value,group=PID),size=0.3,
            color="firebrick1",alpha =0.8) +
  geom_line(data = data_mflu_long[data_mflu_long$MathGroup=="TD",],aes(x=variable,y=value,group=PID),size=0.3,
            color="lightpink1",alpha =0.8) +
  geom_line(data = data_mflu_long[data_mflu_long$MathGroup=="NCC",],aes(x=variable,y=value,group=PID),size=0.3,
            color="deepskyblue1",alpha =0.4) +
  geom_line(size = 2) + 
  geom_errorbar(aes(ymin=value-se,ymax=value+se),width=0.2,size=1) +
  scale_x_discrete(name="",labels=c("Pre","Post")) +
  scale_y_continuous(name="Math Fluency",limits = c(75,135)) +
  scale_color_manual(values = c("firebrick1","lightpink1","deepskyblue1")) +
  scale_fill_manual(values = c("firebrick1","lightpink1","deepskyblue1")) +
  theme_bw()
##fine-tuning the graph
mflu_line<-mflu_line + 
  theme (text = element_text(size = 20, colour = "black"),panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),legend.position = "none") 
print(mflu_line)

png(file="Mindset_MathFluency_Pre_Post_3Group_line.png",width = 1500,height = 1500,res = 300)
print(mflu_line)
dev.off()
##ANOVA test
data_MFlu_long<-melt(na.omit(data_MFlu),id.vars = c("PID","MathGroup"))
data_MFlu_long$PID<-as.factor(data_MFlu_long$PID)
mod<-aov(value~MathGroup*variable+Error(PID/variable),data=data_MFlu_long)
summary(mod)
t1<-t.test(value~variable,data=data_MFlu_long[data_MFlu_long$MathGroup=="MD",],paired =T, var.equal=T)
print(t1)
t2<-t.test(value~variable,data=data_MFlu_long[data_MFlu_long$MathGroup=="TD",],paired =T, var.equal=T)
print(t2)
t3<-t.test(value~variable,data=data_MFlu_long[data_MFlu_long$MathGroup=="NCC",],paired =T, var.equal=T)
print(t3)
##Simple interaction
mod1<-aov(value~MathGroup*variable+Error(PID/variable),data=data_MFlu_long[data_MFlu_long$MathGroup!="TD",])
summary(mod1)
mod2<-aov(value~MathGroup*variable+Error(PID/variable),data=data_MFlu_long[data_MFlu_long$MathGroup!="MD",])
summary(mod2)
mod3<-aov(value~MathGroup*variable+Error(PID/variable),data=data_MFlu_long[data_MFlu_long$MathGroup!="NCC",])
summary(mod3)
sink(file="MathFluency_Pre_Post_Change_3Group_ANOVA.txt")
print(summary(mod))
print(t1)
print(t2)
print(t3)
cohen.d(value~variable | Subject(PID),data=data_mflu_long[data_mflu_long$MathGroup=="MD",], paired =T)
cohen.d(value~variable | Subject(PID),data=data_mflu_long[data_mflu_long$MathGroup=="TD",], paired =T)
cohen.d(value~variable | Subject(PID),data=data_mflu_long[data_mflu_long$MathGroup=="NCC",], paired =T)
print("MLD vs. NCC")
print(summary(mod1))
print("TD vs. NCC")
print(summary(mod2))
print("MLD vs. TD")
print(summary(mod3))
sink()

################################################################################################################################
#Whole Group Analyses: Continuous approach
#####################
##examine the correlation between differences and starting points
##correlation matrix for all the subjects, different groups

mk_rtable(sldata_w[sldata_w$Group=="Tutoring",],which(colnames(sldata_w)=="Age_1"),3,"MathFUN_Pre_Post_Tutoring_correltaion.csv")
mk_rtable(sldata_w[sldata_w$Group=="NCC",],which(colnames(sldata_w)=="Age_1"),3,"MathFUN_Pre_Post_NCC_correltaion.csv")
mk_rtable(sldata_w[sldata_w$MathGroup=="TD",],which(colnames(sldata_w)=="Age_1"),3,"MathFUN_Pre_Post_TD_correltaion.csv")
mk_rtable(sldata_w[sldata_w$MathGroup=="MD",],which(colnames(sldata_w)=="Age_1"),3,"MathFUN_Pre_Post_MD_correltaion.csv")

###identify outlier in ATL_Total_2v1 (univariate)
sd_cri<-3
x<-sldata_w$ATL_Total_2v1
upp.cutoff<-mean(x)+sd_cri*sd(x)
low.cutoff<-mean(x)-sd_cri*sd(x)
pid<-sldata_w$PID[c(which(sldata_w$ATL_Total_2v1>upp.cutoff),which(sldata_w$ATL_Total_2v1<low.cutoff))]
val<-sldata_w$ATL_Total_2v1[c(which(sldata_w$ATL_Total_2v1>upp.cutoff),which(sldata_w$ATL_Total_2v1<low.cutoff))]

# ##identify outlier in ATL_Total_2v1 and WJ_Cal_1 (bivariate)
# x<-sldata_w[sldata_w$MathGroup=="TD",c("WJ_Cal_1","ATL_Total_2v1")]
# y<-cbind.data.frame(sldata_w$PID[sldata_w$MathGroup=="TD"],mahalanobis(x,colMeans(x),cov(x)))
# colnames(y)<-c("PID","M_dist")
# y$outlier<-ifelse(y$M_dist>3*mean(y$M_dist),"Yes","No")
# print(y$PID[y$outlier=="Yes"])

##T1 math achievement with ATL change
# ##WJ_Cal
# cal_scatter<-mk_scatter(sldata_w,"WJ_Cal_1","ATL_Total_2v1","MathGroup",seshade = F)
# ##fine-tune the graph
# cal_scatter<-cal_scatter + 
#   theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
#          legend.position = c(0.15,0.15),
#          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
#   scale_color_manual(values = c("firebrick1","lightpink1","deepskyblue1"))+
#   scale_fill_manual(values = c("firebrick1","lightpink1","deepskyblue1"))
# print(cal_scatter)
# png(file="Mindset_Calculation_Change_WJ_Cal.png",width = 1500,height = 1500,res = 300)
# print(cal_scatter)
# dev.off()
# #mk_scatter(sldata_w[-which(sldata_w$PID %in% pid),],"WJ_Cal_1","ATL_Total_2v1","MathGroup",seshade = F)
# #mk_scatter(sldata_w[-which(sldata_w$PID %in% y$PID[y$outlier=="Yes"]),],"WJ_Cal_1","ATL_Total_2v1","MathGroup",seshade = F)
# ##WJ_MFlu
# mk_scatter(sldata_w,"WJ_MFlu_1","ATL_Total_2v1","MathGroup",seshade = F)
# ##Add_Acc
# mk_scatter(na.omit(sldata_w[,c("Add_Accuracy_1","ATL_Total_2v1","MathGroup")]),"Add_Accuracy_1","ATL_Total_2v1","MathGroup",seshade = F)
##T1 ATL with ATL change
##Two-group solution
#reorder levels
sldata_w$Group<-factor(sldata_w$Group,levels=c("Tutoring","NCC"))
atl_scatter<-mk_scatter(sldata_w,"ATL_Total_1","ATL_Total_2v1","Group",seshade = F)
#Fine-tune the graph
atl_scatter<-atl_scatter + 
  scale_color_manual(values = c("indianred1","deepskyblue1")) +
  scale_fill_manual(values = c("indianred1","deepskyblue1")) +
  theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
         legend.position = c(0.20,0.15),legend.background = element_blank())
print(atl_scatter)
png(file="Mindset_ATL_Change_ATL_Pre_2Group.png",width = 1500,height = 1500,res = 300)
print(atl_scatter)
dev.off()
##Three-group solution
atl_scatter<-mk_scatter(sldata_w,"ATL_Total_1","ATL_Total_2v1","MathGroup",seshade = F)
#Fine-tune the graph
atl_scatter<-atl_scatter + 
  theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
         legend.position = c(0.20,0.15),legend.background = element_blank()) +
  scale_color_manual(values = c("firebrick1","lightpink1","deepskyblue1")) +
  scale_fill_manual(values = c("firebrick1","lightpink1","deepskyblue1"))
print(atl_scatter)
png(file="Mindset_ATL_Change_ATL_Pre_3Group.png",width = 1500,height = 1500,res = 300)
print(atl_scatter)
dev.off()

######################some exploration
### T1 ATL with math achievement change
# ##WJ_Cal
# cal2_scatter<-mk_scatter(na.omit(sldata_w[sldata_w$WJ_Cal_2v1<50,c("Group","ATL_Total_1","WJ_Cal_2v1")]),
#                          "ATL_Total_1","WJ_Cal_2v1","Group",seshade = F)
# ##WJ_MFlu
# Mflu2_scatter<-mk_scatter(na.omit(sldata_w[,c("Group","ATL_Total_1","WJ_MFlu_2v1")]),
#                          "ATL_Total_1","WJ_MFlu_2v1","Group",seshade = F)
# ##T1 with T2
# mk_scatter(na.omit(sldata_w[,c("Group","ATL_Total_1","WJ_Cal_2")]),
#            "ATL_Total_1","WJ_Cal_2","Group",seshade = F)
# mk_scatter(na.omit(sldata_w[,c("Group","ATL_Total_1","WJ_MFlu_2")]),
#            "ATL_Total_1","WJ_MFlu_2","Group",seshade = F)
# mk_scatter(na.omit(sldata_w[,c("Group","WJ_Cal_1","ATL_Total_2")]),
#            "WJ_Cal_1","ATL_Total_2","Group",seshade = F)
# mk_scatter(na.omit(sldata_w[,c("Group","WJ_MFlu_1","ATL_Total_2")]),
#            "WJ_MFlu_1","ATL_Total_2","Group",seshade = F)

# ###Residual plots
# ##WJ_Cal
# data_reg_cal<-sldata_w[sldata_w$WJ_Cal_2v1<50,c("Group","WJ_Cal_1","WJ_Cal_2","ATL_Total_1")]
# data_reg_cal<-na.omit(data_reg_cal)
# cal_reg_mod<-lm(WJ_Cal_2~WJ_Cal_1,data = data_reg_cal)
# summary(cal_reg_mod)
# data_reg_cal$WJ_Cal_2_res<-cal_reg_mod$residuals
# mk_scatter(data_reg_cal,"ATL_Total_1","WJ_Cal_2_res","Group",seshade = F)
# ###MFlu
# data_reg_mflu<-sldata_w[,c("MathGroup","WJ_MFlu_1","WJ_MFlu_2","ATL_Total_1")]
# data_reg_mflu<-na.omit(data_reg_mflu)
# mflu_reg_mod<-lm(WJ_MFlu_2~WJ_MFlu_1,data = data_reg_mflu)
# summary(mflu_reg_mod)
# data_reg_mflu$WJ_MFlu_2_res<-mflu_reg_mod$residuals
# mk_scatter(data_reg_mflu,"ATL_Total_1","WJ_MFlu_2_res","MathGroup",seshade = F)
# mk_scatter(sldata_w,"WJ_Cal_1","WJ_Cal_2","Group",seshade = F)


##correlation
subgrp<-levels(sldata_w$MathGroup)
ivs<-c("WJ_Cal_1","ATL_Total_1")
cor.method<-c("pearson","spearman")
for (i in cor.method) {  #for each method
  for (j in subgrp) { ##for each subgroup
    for (k in ivs) {  #for each iv
      x<-sldata_w[sldata_w$MathGroup==j,k]
      y<-sldata_w[sldata_w$MathGroup==j,"ATL_Total_2v1"]
      mod<-cor.test(x,y,method = i,exact = F)
      mod$parameter<-ifelse(length(mod$parameter)==0,"NA",mod$parameter)
      if (j== subgrp[1] & k ==ivs[1] & i ==cor.method[1]) {
        output<-cbind.data.frame(i,j,k,round(mod$estimate,4),mod$statistic,mod$parameter,round(mod$p.value,4))
      } else {
        output<-rbind.data.frame(output,cbind.data.frame(i,j,k,round(mod$estimate,4),mod$statistic,mod$parameter,round(mod$p.value,4)))
      }
    }
  }
}
colnames(output)<-c("Method","Subgroup","IV","r/rho","stats(t/S)","df(only for r)","p-value")
print(output)
##test the differences between correlations
r.test(n=20,r12=-0.51,r34=0.0996,n2=22)  ##MD vs. NCC WJ_Cal_1
r.test(n=32,r12=-0.4439,r34=0.0996,n2=22)  ##TD vs. NCC WJ_Cal_1
r.test(n=20,r12=-0.51,r34=-0.4439,n2=32)  ##MLD vs. TD WJ_Cal_1
r.test(n=20,r12=-0.7546,r34=-0.3471,n2=22)  ##MD vs. NCC ATL_Total_1
r.test(n=32,r12=-0.6959,r34=-0.3471,n2=22)  ##TD vs. NCC ATL_Total_1
r.test(n=20,r12=-0.7546,r34=-0.6959,n2=32)  ##MLD vs. TD ATL_Total_1

####Tutoring and NCC
cor.test(sldata_w[sldata_w$Group=="Tutoring",]$ATL_Total_2v1,
         sldata_w[sldata_w$Group=="Tutoring",]$ATL_Total_1)
require(compute.es)
res(-0.75178,n=52)
cor.test(sldata_w[sldata_w$Group=="NCC",]$ATL_Total_2v1,
         sldata_w[sldata_w$Group=="NCC",]$ATL_Total_1)
res(-0.34526,n=27)
r.test(n=52,r12=-0.7518,r34=-0.3453,n2=27)  ##Tut vs. NCC ATL_Total_1
#######################################################################
###SEM model part
#################
require(lavaan)
##########Tutoring group
##WJ_MathFlu
data_tut_MFlu<-sldata_w[sldata_w$Group=="Tutoring",c("PID","Age_1","FSIQ","WJ_MFlu_1","WJ_MFlu_2","ATL_Total_1","ATL_Total_2")]
##only in the MD
#data_tut_MFlu<-sldata_w[sldata_w$MathGroup=="MD",c("PID","Age_1","FSIQ","WJ_MFlu_1","WJ_MFlu_2","ATL_Total_1","ATL_Total_2")]
##normalized the data
data_tut_MFlus<-data_tut_MFlu
data_tut_MFlus[,-1]<-scale(data_tut_MFlu[,-1],center = T, scale = T)
mflu.model<-'
            #define the regression model
            ##ATL part
            ATL_Total_2 ~ ATL_Total_1 + WJ_MFlu_1

            ##Math part
            WJ_MFlu_2 ~ ATL_Total_1 + WJ_MFlu_1
            
            ##T1 part
            #WJ_Cal_1 ~ Age_1 + FSIQ
            #ATL_Total_1 ~ Age_1 + FSIQ

            ##residual correlation
            ATL_Total_1 ~~ WJ_MFlu_1
            ATL_Total_2 ~~ WJ_MFlu_2
            #Age_1 ~~ FSIQ
'

mflu.tut.fit<-sem(mflu.model,data=data_tut_MFlus,fixed.x=F,missing="ML")
#mflu.tut.fit<-sem(mflu.model,data=data_tut_MFlus,fixed.x=F)
summary(mflu.tut.fit,fit.measures=T,standardized=T)

##########NCC group
##WJ_MathFlu
data_ncc_MFlu<-sldata_w[sldata_w$Group=="NCC",c("PID","Age_1","FSIQ","WJ_MFlu_1","WJ_MFlu_2","ATL_Total_1","ATL_Total_2")]
##normalized the data
data_ncc_MFlus<-data_ncc_MFlu
data_ncc_MFlus[,-1]<-scale(data_ncc_MFlu[,-1],center = T, scale = T)

mflu.ncc.fit<-sem(mflu.model,data=data_ncc_MFlus,fixed.x=F,missing="ML")
summary(mflu.ncc.fit,fit.measures=T,standardized=T)

##multi-group analysis
data_tutncc_MFlu<-sldata_w[,c("PID","Group","MathGroup","Age_1","FSIQ","WJ_MFlu_1","WJ_MFlu_2","ATL_Total_1","ATL_Total_2")]
##Tut vs. NCC
multi.mflu.fit<-sem(mflu.model,data=data_tutncc_MFlu,fixed.x=F,missing="ML", group = "Group")
summary(multi.mflu.fit)
##constrained model
mflu.model.cons<-'
            #define the regression model
            ##ATL part
            ATL_Total_2 ~ ATL_Total_1 + c("a1","a1")*WJ_MFlu_1

            ##Math part
            #WJ_MFlu_2 ~ c("b1","b1")*ATL_Total_1 + WJ_MFlu_1
            WJ_MFlu_2 ~ ATL_Total_1 + WJ_MFlu_1
            
            ##T1 part
            #WJ_Cal_1 ~ Age_1 + FSIQ
            #ATL_Total_1 ~ Age_1 + FSIQ

            ##residual correlation
            ATL_Total_1 ~~ WJ_MFlu_1
            ATL_Total_2 ~~ WJ_MFlu_2
            #Age_1 ~~ FSIQ
'
multi.mflu.fit.cons<-sem(mflu.model.cons,data=data_tutncc_MFlu,fixed.x=F,missing="ML", group = "Group")
summary(multi.mflu.fit.cons)
anova(multi.mflu.fit,multi.mflu.fit.cons)



##WJ_Cal
# data_tut_cal<-sldata_w[sldata_w$Group=="Tutoring",c("PID","Age_1","FSIQ","WJ_Cal_1","WJ_Cal_2","ATL_Total_1","ATL_Total_2")]
# ##Take out one outlier
# #data_tut_cal<-sldata_w[(sldata_w$Group=="Tutoring" & sldata_w$WJ_Cal_2v1<50),c("PID","Age_1","FSIQ","WJ_Cal_1","WJ_Cal_2","ATL_Total_1","ATL_Total_2")]
# ##normalized the data
# data_tut_cals<-data_tut_cal
# data_tut_cals[,-1]<-scale(data_tut_cal[,-1],center = T, scale = T)
# cal.model<-'
#             #define the regression model
#             ##ATL part
#             ATL_Total_2 ~ ATL_Total_1 + WJ_Cal_1
# 
#             ##Math part
#             WJ_Cal_2 ~ ATL_Total_1 + WJ_Cal_1
#             
#             ##T1 part
#             #WJ_Cal_1 ~ Age_1 + FSIQ
#             #ATL_Total_1 ~ Age_1 + FSIQ
# 
#             ##residual correlation
#             ATL_Total_1 ~~ WJ_Cal_1
#             ATL_Total_2 ~~ WJ_Cal_2
#             #Age_1 ~~ FSIQ
# '
# 
# cal.tut.fit<-sem(cal.model,data=data_tut_cals,fixed.x=F,missing="ML")
# #cal.tut.fit<-sem(cal.model,data=data_tut_cals,fixed.x=F)  #listwise deletion for missing data
# summary(cal.tut.fit,fit.measures=T,standardized=T)
##Add_Acc
# data_tut_acc<-sldata_w[sldata_w$Group=="Tutoring",c("PID","Age_1","FSIQ","Add_Accuracy_1","Add_Accuracy_2","ATL_Total_1","ATL_Total_2")]
# ##normalized the data
# data_tut_accs<-data_tut_acc
# data_tut_accs[,-1]<-scale(data_tut_acc[,-1],center = T, scale = T)
# acc.model<-'
#             #define the regression model
#             ##ATL part
#             ATL_Total_2 ~ ATL_Total_1 + Add_Accuracy_1
# 
#             ##Math part
#             Add_Accuracy_2 ~ ATL_Total_1 + Add_Accuracy_1
# 
#             ##T1 part
#             Add_Accuracy_1 ~ Age_1 + FSIQ
#             ATL_Total_1 ~ Age_1 + FSIQ
# 
#             ##residual correlation
#             ATL_Total_1 ~~ Add_Accuracy_1
#             ATL_Total_2 ~~ Add_Accuracy_2
#             Age_1 ~~ FSIQ
# '
# 
# acc.tut.fit<-sem(acc.model,data=data_tut_accs,fixed.x=F,missing="ML")
# acc.tut.fit<-sem(acc.model,data=data_tut_accs,fixed.x=F)
# summary(acc.tut.fit,fit.measures=T,standardized=T)

# ##WJ_Cal
# data_ncc_cal<-sldata_w[sldata_w$Group=="NCC",c("PID","Age_1","FSIQ","WJ_Cal_1","WJ_Cal_2","ATL_Total_1","ATL_Total_2")]
# ##normalized the data
# data_ncc_cals<-data_ncc_cal
# data_ncc_cals[,-1]<-scale(data_ncc_cal[,-1],center = T, scale = T)
# 
# cal.ncc.fit<-sem(cal.model,data=data_ncc_cals,fixed.x=F,missing="ML")
# summary(cal.ncc.fit,fit.measures=T,standardized=T)

# ##Add_Acc
# data_ncc_acc<-sldata_w[sldata_w$Group=="NCC",c("PID","Age_1","FSIQ","Add_Accuracy_1","Add_Accuracy_2","ATL_Total_1","ATL_Total_2")]
# ##normalized the data
# data_ncc_accs<-data_ncc_acc
# data_ncc_accs[,-1]<-scale(data_ncc_acc[,-1],center = T, scale = T)
# 
# acc.ncc.fit<-sem(acc.model,data=data_ncc_accs,fixed.x=F,missing="ML")
# summary(acc.ncc.fit,fit.measures=T,standardized=T)

