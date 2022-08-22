##load functions
source("R_functions.R")  #version 1.9.1 used

##read in the data
mydata<-read.csv("gPPI_ROI_link_con1_ATL_positive.csv")
colnames(mydata)
mydata<-mydata[,-which(colnames(mydata)=="Session")]
mydata_w<-reshape(mydata,timevar = "Visit",idvar = "PID",sep = "_",direction = "wide")
###save file
write.csv(mydata_w, file = "Mindset_gPPI_roi_prepost.csv",row.names = F)

###read NP data
npdata<-read.csv("selected_Pre_Post_data_MathFUN_wideformat_withDiff.csv")
roidata<-read.csv("Mindset_gPPI_roi_prepost.csv")
##merge
alldata<-merge.data.frame(npdata,roidata,by="PID",all=T)
write.csv(alldata, file= "Mindset_NP_roi_data_merged.csv")

##exclude the NCC-MD
#alldata<-alldata[!is.na(alldata$Group),]
##select those with scan data
alldata<-alldata[!is.na(alldata$lACC_1.lACC_2_1),]
write.csv(alldata, file= "Mindset_NP_roi_data_merged_scansample.csv")

###select data columns
sldata_scan<-alldata[,c("PID","Gender","Group","Age_1","FSIQ",
                  colnames(alldata)[grepl("_2v1|WJ_MFlu_1|ACC_1|ACC_2|ACC_3|RvClaustrum|RvClaustrum|RHippo|NAc",
                                         colnames(alldata))])]
colnames(sldata_scan)
##calculate the differences in ROIs
#lACC_1
# sldata_scan$lACC_1.lACC_2_2v1<-sldata_scan$lACC_1.lACC_2_2-sldata_scan$lACC_1.lACC_2_1
# sldata_scan$lACC_1.rACC_1_2v1<-sldata_scan$lACC_1.rACC_1_2-sldata_scan$lACC_1.rACC_1_1
# sldata_scan$lACC_1.rACC_2_2v1<-sldata_scan$lACC_1.rACC_2_2-sldata_scan$lACC_1.rACC_2_1
sldata_scan$lACC_1.rACC_3_2v1<-sldata_scan$lACC_1.rACC_3_2-sldata_scan$lACC_1.rACC_3_1
#sldata_scan$lACC_1.RvClaustrum_2v1<-sldata_scan$lACC_1.RvClaustrum_2-sldata_scan$lACC_1.RvClaustrum_1
sldata_scan$lACC_1.RdClaustrum_2v1<-sldata_scan$lACC_1.RdClaustrum_2-sldata_scan$lACC_1.RdClaustrum_1
sldata_scan$lACC_1.RHippo_2v1<-sldata_scan$lACC_1.RHippo_2-sldata_scan$lACC_1.RHippo_1
sldata_scan$lACC_1.combinedNAc_2v1<-sldata_scan$lACC_1.combinedNAc_2-sldata_scan$lACC_1.combinedNAc_1

# #lACC_2
# sldata_scan$lACC_2.rACC_1_2v1<-sldata_scan$lACC_2.rACC_1_2-sldata_scan$lACC_2.rACC_1_1
# sldata_scan$lACC_2.rACC_2_2v1<-sldata_scan$lACC_2.rACC_2_2-sldata_scan$lACC_2.rACC_2_1
# sldata_scan$lACC_2.rACC_3_2v1<-sldata_scan$lACC_2.rACC_3_2-sldata_scan$lACC_2.rACC_3_1
# sldata_scan$lACC_2.RvClaustrum_2v1<-sldata_scan$lACC_2.RvClaustrum_2-sldata_scan$lACC_2.RvClaustrum_1
# sldata_scan$lACC_2.RdClaustrum_2v1<-sldata_scan$lACC_2.RdClaustrum_2-sldata_scan$lACC_2.RdClaustrum_1
# sldata_scan$lACC_2.RHippo_2v1<-sldata_scan$lACC_2.RHippo_2-sldata_scan$lACC_2.RHippo_1
# sldata_scan$lACC_2.combinedNAc_2v1<-sldata_scan$lACC_2.combinedNAc_2-sldata_scan$lACC_2.combinedNAc_1
# 
# #rACC_1
# sldata_scan$rACC_1.rACC_2_2v1<-sldata_scan$rACC_1.rACC_2_2-sldata_scan$rACC_1.rACC_2_1
# sldata_scan$rACC_1.rACC_3_2v1<-sldata_scan$rACC_1.rACC_3_2-sldata_scan$rACC_1.rACC_3_1
# sldata_scan$rACC_1.RvClaustrum_2v1<-sldata_scan$rACC_1.RvClaustrum_2-sldata_scan$rACC_1.RvClaustrum_1
# sldata_scan$rACC_1.RdClaustrum_2v1<-sldata_scan$rACC_1.RdClaustrum_2-sldata_scan$rACC_1.RdClaustrum_1
# sldata_scan$rACC_1.RHippo_2v1<-sldata_scan$rACC_1.RHippo_2-sldata_scan$rACC_1.RHippo_1
# sldata_scan$rACC_1.combinedNAc_2v1<-sldata_scan$rACC_1.combinedNAc_2-sldata_scan$rACC_1.combinedNAc_1
# 
# #rACC_2
# sldata_scan$rACC_2.rACC_3_2v1<-sldata_scan$rACC_2.rACC_3_2-sldata_scan$rACC_2.rACC_3_1
# sldata_scan$rACC_2.RvClaustrum_2v1<-sldata_scan$rACC_2.RvClaustrum_2-sldata_scan$rACC_2.RvClaustrum_1
# sldata_scan$rACC_2.RdClaustrum_2v1<-sldata_scan$rACC_2.RdClaustrum_2-sldata_scan$rACC_2.RdClaustrum_1
# sldata_scan$rACC_2.RHippo_2v1<-sldata_scan$rACC_2.RHippo_2-sldata_scan$rACC_2.RHippo_1
# sldata_scan$rACC_2.combinedNAc_2v1<-sldata_scan$rACC_2.combinedNAc_2-sldata_scan$rACC_2.combinedNAc_1

#rACC_3
#sldata_scan$rACC_3.RvClaustrum_2v1<-sldata_scan$rACC_3.RvClaustrum_2-sldata_scan$rACC_3.RvClaustrum_1
sldata_scan$rACC_3.RdClaustrum_2v1<-sldata_scan$rACC_3.RdClaustrum_2-sldata_scan$rACC_3.RdClaustrum_1
sldata_scan$rACC_3.RHippo_2v1<-sldata_scan$rACC_3.RHippo_2-sldata_scan$rACC_3.RHippo_1
sldata_scan$rACC_3.combinedNAc_2v1<-sldata_scan$rACC_3.combinedNAc_2-sldata_scan$rACC_3.combinedNAc_1

#sldata_scan$RvClaustrum.RdClaustrum_2v1<-sldata_scan$RvClaustrum.RdClaustrum_2-sldata_scan$RvClaustrum.RdClaustrum_1
#sldata_scan$RvClaustrum.RHippo_2v1<-sldata_scan$RvClaustrum.RHippo_2-sldata_scan$RvClaustrum.RHippo_1
#sldata_scan$RvClaustrum.combinedNAc_2v1<-sldata_scan$RvClaustrum.combinedNAc_2-sldata_scan$RvClaustrum.combinedNAc_1
sldata_scan$RdClaustrum.RHippo_2v1<-sldata_scan$RdClaustrum.RHippo_2-sldata_scan$RdClaustrum.RHippo_1
sldata_scan$RdClaustrum.combinedNAc_2v1<-sldata_scan$RdClaustrum.combinedNAc_2-sldata_scan$RdClaustrum.combinedNAc_1
sldata_scan$RHippo.combinedNAc_2v1<-sldata_scan$RHippo.combinedNAc_2-sldata_scan$RHippo.combinedNAc_1

#reorder levels
sldata_scan$Group<-factor(sldata_scan$Group,levels = c("Tutoring","NCC"))

####correlation table
mk_rtable(sldata_scan[sldata_scan$Group=="Tutoring",grep("Age|FSIQ|_2v1",colnames(sldata_scan))],
          which(colnames(sldata_scan)=="Age_1"),2,"MathFUN_Pre_Post_Tutoring_gPPI_roi_correlaion.csv")
mk_rtable(sldata_scan[sldata_scan$Group=="NCC",grep("Age|FSIQ|_2v1",colnames(sldata_scan))],
          which(colnames(sldata_scan)=="Age_1"),2,"MathFUN_Pre_Post_NCC_gPPI_roi_correlaion.csv")
###make Math Group in Tutoring
##define MD on MathFlu
sldata_scan$MathGroup<- as.factor(ifelse(sldata_scan$Group == "NCC", "NCC", ifelse(sldata_scan$WJ_MFlu_1 < 90, "MD", "TD")))
summary(sldata_scan$MathGroup)
##move the column to the front
sldata_scan<-sldata_scan[,c(1:2,which(colnames(sldata_scan)=="MathGroup"),3:(ncol(sldata_scan)-1))]
##reorder the factor level of MathGroup to make the NCC come at last
sldata_scan$MathGroup<-factor(sldata_scan$MathGroup,levels=c("MD","TD","NCC"))

##make scatter
# mk_scatter(sldata_scan,"ATL_Total_2v1","ACC_1.ACC_2_2v1","Group",seshade = F)
# mk_scatter(sldata_scan,"ATL_Total_2v1","ACC_1.RdClaustrum_2v1","Group",seshade = F)
# mk_scatter(sldata_scan,"ATL_Total_2v1","ACC_1.RHippo_2v1","Group",seshade = F)
# mk_scatter(sldata_scan,"ATL_Total_2v1","ACC_2.RdClaustrum_2v1","Group",seshade = F)
# mk_scatter(sldata_scan,"ATL_Total_2v1","ACC_2.RvClaustrum_2v1","Group",seshade = F)
# mk_scatter(sldata_scan,"ATL_Total_2v1","ACC_2.RHippo_2v1","Group",seshade = F)
# mk_scatter(sldata_scan,"ATL_Total_2v1","RdClaustrum.RHippo_2v1","Group",seshade = F)

###Tutoring group analysis
###multivariate prediction
##demographic
mod1<-lm(ATL_Total_2v1~Gender+Age_1,
         data=sldata_scan[sldata_scan$Group=="Tutoring",])
print(summary(mod1))
##task connectivity
mod2<-lm(ATL_Total_2v1~lACC_1.rACC_3_2v1+lACC_1.RdClaustrum_2v1+lACC_1.RHippo_2v1+rACC_3.RdClaustrum_2v1+
           rACC_3.RHippo_2v1+RdClaustrum.RHippo_2v1,data=sldata_scan[sldata_scan$Group=="Tutoring",])
print(summary(mod2))
##additional variance from connectivity over demographic and cog
mod3<-lm(ATL_Total_2v1~Gender+Age_1+lACC_1.rACC_3_2v1+lACC_1.RdClaustrum_2v1+lACC_1.RHippo_2v1+rACC_3.RdClaustrum_2v1+
           rACC_3.RHippo_2v1+RdClaustrum.RHippo_2v1,data=sldata_scan[sldata_scan$Group=="Tutoring",])
print(summary(mod3))
anova(mod1,mod3)
##Mflu
##demographic
mod4<-lm(WJ_MFlu_2v1~Gender+Age_1,
         data=sldata_scan[sldata_scan$Group=="Tutoring",])
print(summary(mod4))
#task connectivity predicting Mflu
mod5<-lm(WJ_MFlu_2v1~lACC_1.rACC_3_2v1+lACC_1.RdClaustrum_2v1+lACC_1.RHippo_2v1+rACC_3.RdClaustrum_2v1+
           rACC_3.RHippo_2v1+RdClaustrum.RHippo_2v1,data=sldata_scan[sldata_scan$Group=="Tutoring",])
print(summary(mod5))
mod6<-lm(WJ_MFlu_2v1~Gender+Age_1+lACC_1.rACC_3_2v1+lACC_1.RdClaustrum_2v1+lACC_1.RHippo_2v1+rACC_3.RdClaustrum_2v1+
           rACC_3.RHippo_2v1+RdClaustrum.RHippo_2v1,data=sldata_scan[sldata_scan$Group=="Tutoring",])
print(summary(mod5))
anova(mod4,mod6)

####multivariate regression scatter plot
temp.data<-sldata_scan[sldata_scan$Group=="Tutoring",grep("PID|MathGroup|_2v1",colnames(sldata_scan))]

# temp.data$atl.res<-mod1$residuals
# temp.data$mflu.res<-mod4$residuals
# ###residual atl on connectivity
# mod7<-lm(atl.res~lACC_1.rACC_3_2v1+lACC_1.RdClaustrum_2v1+lACC_1.RHippo_2v1+rACC_3.RdClaustrum_2v1+
#            rACC_3.RHippo_2v1+RdClaustrum.RHippo_2v1,data = temp.data)
# summary(mod7)
temp.data$atl.pred<-mod2$fitted.values
temp.data$atl.res<-mod2$residuals
# ###residual mflu on connectivity
# mod8<-lm(mflu.res~lACC_1.rACC_3_2v1+lACC_1.RdClaustrum_2v1+lACC_1.RHippo_2v1+rACC_3.RdClaustrum_2v1+
#            rACC_3.RHippo_2v1+RdClaustrum.RHippo_2v1,data = temp.data)
# summary(mod8)
temp.data$mflu.pred<-mod5$fitted.values
temp.data$mflu.res<-mod5$residuals
multi.scatter_plot.atl<-mk_scatter_oneGroup(temp.data,"atl.pred","ATL_Total_2v1")
multi.scatter_plot.atl<-multi.scatter_plot.atl +
  scale_y_continuous(breaks=seq(-1,2,0.5)) +
  theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
         legend.position = c(0.85,0.15),
         legend.background = element_blank())
print(multi.scatter_plot.atl)
png(filename = "Tutoring_ATL_total_2v1_multivariate_scatter.png",width=1500,height=1500,res = 300)
print(multi.scatter_plot.atl)
dev.off()
##two groups in tutoring
temp.data$MathGroup<-factor(temp.data$MathGroup,levels=c("MD","TD"))
multi.scatter_plot.atl<-mk_scatter_oneGroup(temp.data,"atl.pred","ATL_Total_2v1","MathGroup")
multi.scatter_plot.atl<-multi.scatter_plot.atl +
  scale_y_continuous(breaks=seq(-1,2,0.5)) +
  scale_color_manual(values = c("firebrick1","lightpink1")) +
  scale_fill_manual(values = c("firebrick1","lightpink1")) +
  theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
         legend.position = c(0.85,0.15),
         legend.background = element_blank())
print(multi.scatter_plot.atl)
png(filename = "Tutoring_ATL_total_2v1_multivariate_scatter_2Group_1line.png",width=1500,height=1500,res = 300)
print(multi.scatter_plot.atl)
dev.off()
#two groups two lines
multi.scatter_plot.atl<-mk_scatter(temp.data,"atl.pred","ATL_Total_2v1","MathGroup")
multi.scatter_plot.atl<-multi.scatter_plot.atl +
  scale_y_continuous(breaks=seq(-1,2,0.5)) +
  scale_color_manual(values = c("firebrick1","lightpink1")) +
  scale_fill_manual(values = c("firebrick1","lightpink1")) +
  theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
         legend.position = c(0.85,0.15),
         legend.background = element_blank())
print(multi.scatter_plot.atl)
png(filename = "Tutoring_ATL_total_2v1_multivariate_scatter_2Group_2line.png",width=1500,height=1500,res = 300)
print(multi.scatter_plot.atl)
dev.off()
###test the interaction
summary(lm(ATL_Total_2v1~MathGroup*atl.pred,data = temp.data))


######NAc links predict ATL
##task connectivity
mod<-lm(ATL_Total_2v1~lACC_1.combinedNAc_2v1+rACC_3.combinedNAc_2v1+RHippo.combinedNAc_2v1+RdClaustrum.combinedNAc_2v1,data=sldata_scan[sldata_scan$Group=="Tutoring",])
print(summary(mod))
temp.data$atl.pred.nac<-mod$fitted.values
multi.scatter_plot.atl.nac<-mk_scatter_oneGroup(temp.data,"atl.pred.nac","ATL_Total_2v1")
print(multi.scatter_plot.atl.nac)
png(filename = "Tutoring_ATL_total_2v1_multivariate_scatter_NAc.png",width=1500,height=1500,res = 300)
print(multi.scatter_plot.atl.nac)
dev.off()

# multi.scatter_plot.mflu<-mk_scatter_oneGroup(temp.data,"mflu.pred","WJ_MFlu_2v1")
# multi.scatter_plot.mflu<-multi.scatter_plot.mflu +
#   scale_y_continuous(breaks=seq(-20,20,10)) +
#   theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
#          legend.position = c(0.85,0.15),
#          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
# print(multi.scatter_plot.mflu)
# png(filename = "Tutoring_MFlu_total_2v1_multivariate_scatter.png",width=1500,height=1500,res = 300)
# print(multi.scatter_plot.atl)
# dev.off()
sink(file = "Tutoring_multivariate_regression.txt")
print(summary(mod2))
print(summary(mod5))
sink()


##scatter plots in tutoring group (individual link)
#lACC_1 - rACC_3
scatter_plot<-mk_scatter_oneGroup(sldata_scan[sldata_scan$Group=="Tutoring",],"lACC_1.rACC_3_2v1","ATL_Total_2v1")
scatter_plot<-scatter_plot +
       theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
       legend.position = c(0.85,0.15),
       legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
print(scatter_plot)
png(filename = "Tutoring_lACC_1.rACC_3_2v1_ATL_total_2v1_scatter.png",width=1500,height=1500,res = 300)
print(scatter_plot)
dev.off()
#two groups 2 lines
mk_scatter(temp.data,"lACC_1.rACC_3_2v1","ATL_Total_2v1","MathGroup")
####regression
print(summary(lm(ATL_Total_2v1~lACC_1.rACC_3_2v1+Gender+Age_1+WJ_MFlu_2v1,
                 data=sldata_scan[sldata_scan$Group=="Tutoring",])))
###test the interaction
summary(lm(ATL_Total_2v1~MathGroup*lACC_1.rACC_3_2v1,data = sldata_scan[sldata_scan$Group=="Tutoring",]))

#rACC_3 - RdClaustrum
scatter_plot<-mk_scatter_oneGroup(sldata_scan[sldata_scan$Group=="Tutoring",],"rACC_3.RdClaustrum_2v1","ATL_Total_2v1")
scatter_plot<-scatter_plot +
  theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
         legend.position = c(0.85,0.15),
         legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
print(scatter_plot)
png(filename = "Tutoring_rACC_3.RdClaustrum_2v1_ATL_total_2v1_scatter.png",width=1500,height=1500,res = 300)
print(scatter_plot)
dev.off()
#two groups 2 lines
mk_scatter(temp.data,"rACC_3.RdClaustrum_2v1","ATL_Total_2v1","MathGroup")
# ##no outlier
# scatter_plot<-mk_scatter_oneGroup((sldata_scan[(sldata_scan$Group=="Tutoring" & sldata_scan$rACC_3.RdClaustrum_2v1<0.5),]),
#                                   "rACC_3.RdClaustrum_2v1","ATL_Total_2v1")
# scatter_plot<-scatter_plot +
#   theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
#          legend.position = c(0.85,0.15),
#          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
# print(scatter_plot)
# png(filename = "Tutoring_rACC_3.RdClaustrum_2v1_ATL_total_2v1_scatter_noOutlier.png",width=1500,height=1500,res = 300)
# print(scatter_plot)
# dev.off()

####regression
print(summary(lm(ATL_Total_2v1~rACC_3.RdClaustrum_2v1+Gender+Age_1+WJ_MFlu_2v1,
                 data=sldata_scan[sldata_scan$Group=="Tutoring",])))
###test the interaction
summary(lm(ATL_Total_2v1~MathGroup*rACC_3.RdClaustrum_2v1,data = sldata_scan[sldata_scan$Group=="Tutoring",]))


##adjsted p values for all links in the tutoring group
p.adjust(c(0.0259,0.0648,0.9441,0.0047,0.3386,0.693),method = "fdr")
####regression exploration
# print(summary(lm(rACC_3.RdClaustrum_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#                  data=sldata_scan[sldata_scan$Group=="Tutoring",])))
# 
# print(summary(lm(rACC_3.RdClaustrum_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#                  data=sldata_scan[(sldata_scan$Group=="Tutoring" & sldata_scan$rACC_3.RdClaustrum_2v1<0.5),])))
# 
# mod1<-lm(ATL_Total_2v1~Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,data=sldata_scan[sldata_scan$Group=="Tutoring",])
# summary(mod1)
# mod2<-lm(ATL_Total_2v1~Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1+lACC_1.rACC_3_2v1+rACC_3.RdClaustrum_2v1,
#          data=sldata_scan[sldata_scan$Group=="Tutoring",])
# summary(mod2)
# anova(mod1,mod2)


#####NCC group analysis
###multivariate prediction
##demographic
mod1<-lm(ATL_Total_2v1~Gender+Age_1,
         data=sldata_scan[sldata_scan$Group=="NCC",])
print(summary(mod1))
##task connectivity
mod2<-lm(ATL_Total_2v1~lACC_1.rACC_3_2v1+lACC_1.RdClaustrum_2v1+lACC_1.RHippo_2v1+rACC_3.RdClaustrum_2v1+
           rACC_3.RHippo_2v1+RdClaustrum.RHippo_2v1,data=sldata_scan[sldata_scan$Group=="NCC",])
print(summary(mod2))
##additional variance from connectivity over demographic and cog
mod3<-lm(ATL_Total_2v1~Gender+Age_1+lACC_1.rACC_3_2v1+lACC_1.RdClaustrum_2v1+lACC_1.RHippo_2v1+rACC_3.RdClaustrum_2v1+
           rACC_3.RHippo_2v1+RdClaustrum.RHippo_2v1,data=sldata_scan[sldata_scan$Group=="NCC",])
print(summary(mod3))
anova(mod1,mod3)
##Mflu
##demographic
mod4<-lm(WJ_MFlu_2v1~Gender+Age_1,
         data=sldata_scan[sldata_scan$Group=="NCC",])
print(summary(mod4))
#task connectivity predicting Mflu
mod5<-lm(WJ_MFlu_2v1~lACC_1.rACC_3_2v1+lACC_1.RdClaustrum_2v1+lACC_1.RHippo_2v1+rACC_3.RdClaustrum_2v1+
           rACC_3.RHippo_2v1+RdClaustrum.RHippo_2v1,data=sldata_scan[sldata_scan$Group=="NCC",])
print(summary(mod5))
mod6<-lm(WJ_MFlu_2v1~Gender+Age_1+lACC_1.rACC_3_2v1+lACC_1.RdClaustrum_2v1+lACC_1.RHippo_2v1+rACC_3.RdClaustrum_2v1+
           rACC_3.RHippo_2v1+RdClaustrum.RHippo_2v1,data=sldata_scan[sldata_scan$Group=="NCC",])
print(summary(mod5))
anova(mod4,mod6)

####multivariate regression scatter plot
temp.data<-sldata_scan[sldata_scan$Group=="NCC",grep("PID|_2v1",colnames(sldata_scan))]
# temp.data$atl.res<-mod1$residuals
# temp.data$mflu.res<-mod4$residuals
###residual atl on connectivity
# mod7<-lm(atl.res~lACC_1.rACC_3_2v1+lACC_1.RdClaustrum_2v1+lACC_1.RHippo_2v1+rACC_3.RdClaustrum_2v1+
#            rACC_3.RHippo_2v1+RdClaustrum.RHippo_2v1,data = temp.data)
# summary(mod7)
temp.data$atl.pred<-mod2$fitted.values
###residual mflu on connectivity
# mod8<-lm(mflu.res~lACC_1.rACC_3_2v1+lACC_1.RdClaustrum_2v1+lACC_1.RHippo_2v1+rACC_3.RdClaustrum_2v1+
#            rACC_3.RHippo_2v1+RdClaustrum.RHippo_2v1,data = temp.data)
# summary(mod8)
temp.data$mflu.pred<-mod5$fitted.values
multi.scatter_plot.atl<-mk_scatter_oneGroup(temp.data,"atl.pred","ATL_Total_2v1",colors=c("deepskyblue1"))
multi.scatter_plot.atl<-multi.scatter_plot.atl +
  scale_y_continuous(breaks=seq(-1,2,0.5)) +
  theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
         legend.position = c(0.85,0.15),
         legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
print(multi.scatter_plot.atl)
#png(filename = "NCC_ATL_total_2v1_multivariate_scatter.png",width=1500,height=1500,res = 300)
#print(multi.scatter_plot.atl)
#dev.off()
multi.scatter_plot.mflu<-mk_scatter_oneGroup(temp.data,"mflu.pred","WJ_MFlu_2v1",colors=c("deepskyblue1"))
multi.scatter_plot.mflu<-multi.scatter_plot.mflu +
  scale_y_continuous(breaks=seq(-20,20,10)) +
  theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
         legend.position = c(0.85,0.15),
         legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
print(multi.scatter_plot.mflu)
#png(filename = "NCC_MFlu_total_2v1_multivariate_scatter.png",width=1500,height=1500,res = 300)
#print(multi.scatter_plot.mflu)
#dev.off()

sink(file = "NCC_multivariate_regression.txt")
print(summary(mod2))
print(summary(mod5))
sink()

##scatter plots in NCC group (individual link)
scatter_plot<-mk_scatter_oneGroup(sldata_scan[sldata_scan$Group=="NCC",],"lACC_1.rACC_3_2v1","ATL_Total_2v1",
                                  colors=c("deepskyblue1"))
scatter_plot<-scatter_plot +
  theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
         legend.position = c(0.85,0.15),
         legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))

print(scatter_plot)
png(filename = "NCC_lACC_1.rACC_3_2v1_ATL_total_2v1_scatter.png",width=1500,height=1500,res = 300)
print(scatter_plot)
dev.off()
####regression
print(summary(lm(ATL_Total_2v1~lACC_1.rACC_3_2v1+Gender+Age_1+WJ_MFlu_2v1,
                 data=sldata_scan[sldata_scan$Group=="NCC",])))

scatter_plot<-mk_scatter_oneGroup(sldata_scan[sldata_scan$Group=="NCC",],"rACC_3.RdClaustrum_2v1","ATL_Total_2v1",
                                  colors=c("deepskyblue1"))
scatter_plot<-scatter_plot +
  theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
         legend.position = c(0.85,0.15),
         legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
print(scatter_plot)
png(filename = "NCC_rACC_3.RdClaustrum_2v1_ATL_total_2v1_scatter.png",width=1500,height=1500,res = 300)
print(scatter_plot)
dev.off()
####regression
print(summary(lm(ATL_Total_2v1~rACC_3.RdClaustrum_2v1+Gender+Age_1+WJ_MFlu_2v1,
                 data=sldata_scan[sldata_scan$Group=="NCC",])))



