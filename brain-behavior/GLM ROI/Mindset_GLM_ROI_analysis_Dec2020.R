##load functions
source("R_functions.R")  #version 1.9.1 used

##read in the data
mydata<-read.csv("roi_beta_average_final.csv")
colnames(mydata)
##subselect data
sldata<-mydata[,c("Subject","Visit","Session",colnames(mydata)[grepl("problem_correctxpmod.1",
                                                    colnames(mydata))])]
##colname change
colnames(sldata)<-c("PID","Visit","Session","lACC_1_ss1","lACC_1_ss2","lACC_2_ss1","lACC_2_ss2",
                     "rACC_1_ss1","rACC_1_ss2","rACC_2_ss1","rACC_2_ss2","rACC_3_ss1","rACC_3_ss2",
                     "RvClaustrum_ss1","RvClaustrum_ss2","RdClaustrum_ss1","RdClaustrum_ss2",
                     "RHippo_ss1","RHippo_ss2","LNAc_ss1","LNAc_ss2","RNAc_ss1","RNAc_ss2",
                     "CombinedNAc_ss1","CombinedNAc_ss2")
##average across sessions
sldata$lACC_1<-rowMeans(sldata[,c("lACC_1_ss1","lACC_1_ss2")],na.rm = T)
#sldata$lACC_2<-rowMeans(sldata[,c("lACC_2_ss1","lACC_2_ss2")],na.rm = T)
#sldata$rACC_1<-rowMeans(sldata[,c("rACC_1_ss1","rACC_1_ss2")],na.rm = T)
#sldata$rACC_2<-rowMeans(sldata[,c("rACC_2_ss1","rACC_2_ss2")],na.rm = T)
sldata$rACC_3<-rowMeans(sldata[,c("rACC_3_ss1","rACC_3_ss2")],na.rm = T)
#sldata$RvClaustrum<-rowMeans(sldata[,c("RvClaustrum_ss1","RvClaustrum_ss2")],na.rm = T)
sldata$RdClaustrum<-rowMeans(sldata[,c("RdClaustrum_ss1","RdClaustrum_ss2")],na.rm = T)
sldata$RHippo<-rowMeans(sldata[,c("RHippo_ss1","RHippo_ss2")],na.rm = T)
#sldata$LNAc<-rowMeans(sldata[,c("LNAc_ss1","LNAc_ss2")],na.rm = T)
#sldata$RNAc<-rowMeans(sldata[,c("RNAc_ss1","RNAc_ss2")],na.rm = T)
sldata$CombinedNAc<-rowMeans(sldata[,c("CombinedNAc_ss1","CombinedNAc_ss2")],na.rm = T)

####select only averaged data and then make the format wide
#sldata1<-sldata[,c("PID","Visit","lACC_1","lACC_2","rACC_1","rACC_2","rACC_3",
#                   "RvClaustrum","RdClaustrum","RHippo","LNAc","RNAc","CombinedNAc")]
sldata1<-sldata[,c("PID","Visit","lACC_1","rACC_3","RdClaustrum","RHippo","CombinedNAc")]
##get rid of visit is Visit column
require(stringr)
sldata1$Visit<-as.numeric(str_split_fixed(sldata1$Visit,"visit",2)[,2])
sldata1_w<-reshape(sldata1,timevar = "Visit",idvar = "PID",sep = "_",direction = "wide")
###save file
write.csv(sldata1_w, file = "Mindset_GLM_roi_prepost_beta.csv",row.names = F)

###read NP data
npdata<-read.csv("selected_Pre_Post_data_MathFUN_wideformat_withDiff.csv")
roidata<-read.csv("Mindset_GLM_roi_prepost_beta.csv")
##merge
alldata<-merge.data.frame(npdata,roidata,by="PID",all=T)
write.csv(alldata, file= "Mindset_NP_roi_data_merged.csv")

##exclude the NCC-MD
##alldata<-alldata[!is.na(alldata$Group),]
##select those with scan data
table(alldata[,c("Group","Gender")])
alldata<-alldata[!is.na(alldata$lACC_1_1),]
table(alldata[,c("Group","Gender")])
chisq.test(table(alldata[,c("Group","Gender")]))
write.csv(alldata, file= "Mindset_NP_roi_data_merged_scansample.csv")
###behavioral and demo info the the scan sample
beh_data_scansample<-alldata[,grep("PID|Group|Age|FSIQ|ATL|MFlu|Add_Accuracy",colnames(alldata))]
mk_ttable(beh_data_scansample,which(colnames(beh_data_scansample)=="Group"),
          c("Tutoring","NCC"),which(colnames(beh_data_scansample)=="Age_1"),"Mindset_scansample",3,2)
###select data columns
#sldata_scan<-alldata[,c("PID","Gender","Group","Age_1","FSIQ",
#                  colnames(alldata)[grepl("WJ_Cal|WJ_MFlu|ATL_Total|Add_Accuracy|ACC_1|ACC_2|ACC_3|RvClaustrum|RdClaustrum|RHippo|NAc",
#                                         colnames(alldata))])]
sldata_scan<-alldata[,c("PID","Gender","Group","Age_1","FSIQ",
                        colnames(alldata)[grepl("WJ_Cal|WJ_MFlu|ATL_Total|Add_Accuracy|ACC_1|ACC_3|RdClaustrum|RHippo|NAc",
                                                colnames(alldata))])]
##calculate the differences in ROIs
sldata_scan$lACC_1_2v1<-sldata_scan$lACC_1_2-sldata_scan$lACC_1_1
# sldata_scan$lACC_2_2v1<-sldata_scan$lACC_2_2-sldata_scan$lACC_2_1
# sldata_scan$rACC_1_2v1<-sldata_scan$rACC_1_2-sldata_scan$rACC_1_1
# sldata_scan$rACC_2_2v1<-sldata_scan$rACC_2_2-sldata_scan$rACC_2_1
sldata_scan$rACC_3_2v1<-sldata_scan$rACC_3_2-sldata_scan$rACC_3_1
#sldata_scan$RvClaustrum_2v1<-sldata_scan$RvClaustrum_2-sldata_scan$RvClaustrum_1
sldata_scan$RdClaustrum_2v1<-sldata_scan$RdClaustrum_2-sldata_scan$RdClaustrum_1
sldata_scan$RHippo_2v1<-sldata_scan$RHippo_2-sldata_scan$RHippo_1
#sldata_scan$LNAc_2v1<-sldata_scan$LNAc_2-sldata_scan$LNAc_1
#sldata_scan$RNAc_2v1<-sldata_scan$RNAc_2-sldata_scan$RNAc_1
sldata_scan$CombNAc_2v1<-sldata_scan$CombinedNAc_2-sldata_scan$CombinedNAc_1
#reorder levels
sldata_scan$Group<-factor(sldata_scan$Group,levels = c("Tutoring","NCC"))

####correlation table
mk_rtable(sldata_scan[sldata_scan$Group=="Tutoring",],which(colnames(sldata_scan)=="Age_1"),2,"MathFUN_Pre_Post_Tutoring_roi_correlaion.csv")
mk_rtable(sldata_scan[sldata_scan$Group=="NCC",],which(colnames(sldata_scan)=="Age_1"),2,"MathFUN_Pre_Post_NCC_roi_correlaion.csv")

###make Math Group in Tutoring
##define MD on MathFlu
sldata_scan$MathGroup<- as.factor(ifelse(sldata_scan$Group == "NCC", "NCC", ifelse(sldata_scan$WJ_MFlu_1 < 90, "MD", "TD")))
summary(sldata_scan$MathGroup)
##move the column to the front
sldata_scan<-sldata_scan[,c(1:2,which(colnames(sldata_scan)=="MathGroup"),3:(ncol(sldata_scan)-1))]
##reorder the factor level of MathGroup to make the NCC come at last
sldata_scan$MathGroup<-factor(sldata_scan$MathGroup,levels=c("MD","TD","NCC"))

#make scatter plot for each ROI in each group
#roi.list<-c("lACC_1","lACC_2","rACC_1","rACC_2","rACC_3",
#            "RvClaustrum","RdClaustrum","RHippo","LNAc","RNAc","CombNAc")
roi.list<-c("lACC_1","rACC_3","RdClaustrum","RHippo","CombNAc")
for (j in c("Tutoring","NCC")) {
  for (i in 1:length(roi.list)) {
    
    if (j == "Tutoring") {
      scatter_plot<-mk_scatter_oneGroup(sldata_scan[sldata_scan$Group==j,],"ATL_Total_2v1",
                                        paste(roi.list[i],"2v1",sep = "_"))
    } else {
      scatter_plot<-mk_scatter_oneGroup(sldata_scan[sldata_scan$Group==j,],"ATL_Total_2v1",
                                        paste(roi.list[i],"2v1",sep = "_"),color = c("deepskyblue1"))
    }
    scatter_plot<-scatter_plot + 
      scale_fill_manual(values = color[2]) +
      theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
             legend.position = c(0.85,0.15),
             legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) 
    
    print(scatter_plot)
    png(filename = paste0(j,"_",roi.list[i],"_2v1_ATL_total_2v1_scatter.png"),width=1500,height=1500,res = 300)
    print(scatter_plot)
    dev.off()
  }
}
##2 groups in tutoring
sldata_scan_tut<-sldata_scan[sldata_scan$Group=="Tutoring",]
sldata_scan_tut$MathGroup<-factor(sldata_scan_tut$MathGroup, levels=c("MD","TD"))
##test group interaction
sink("Tutoring_GLM_MLDvsTD_diff_testing.txt")
for (i in 1:length(roi.list)) {
    cat("ROI: ",roi.list[i],"\n")
    print(summary(lm(formula(paste0(roi.list[i],"_2v1~ATL_Total_2v1*MathGroup")),data = sldata_scan_tut)))
}
sink()
##2 groups one regression line
for (i in 1:length(roi.list)) {
    scatter_plot<-mk_scatter_oneGroup(sldata_scan_tut,"ATL_Total_2v1",paste(roi.list[i],"2v1",sep = "_"),"MathGroup")
    scatter_plot<-scatter_plot + 
      scale_color_manual(values = c("firebrick1","lightpink1")) +
      scale_fill_manual(values = c("firebrick1","lightpink1")) +
      theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
             legend.position = c(0.85,0.15),
             legend.background = element_blank()) 
    print(scatter_plot)
    png(filename = paste0("Tutoring",roi.list[i],"_2v1_ATL_total_2v1_scatter_2group_oneline.png"),width=1500,height=1500,res = 300)
    print(scatter_plot)
    dev.off()
}

#2 groups 2 regresssion line
for (i in 1:length(roi.list)) {
  scatter_plot<-mk_scatter(sldata_scan_tut,"ATL_Total_2v1",paste(roi.list[i],"2v1",sep = "_"),"MathGroup")
  scatter_plot<-scatter_plot + 
    scale_color_manual(values = c("firebrick1","lightpink1")) +
    scale_fill_manual(values = c("firebrick1","lightpink1")) +
    theme (text = element_text(size = 20, colour = "black"), legend.title = element_blank(),
           legend.position = c(0.85,0.15),
           legend.background = element_blank()) 
  
  print(scatter_plot)
  png(filename = paste0("Tutoring",roi.list[i],"_2v1_ATL_total_2v1_scatter_2group_2line.png"),width=1500,height=1500,res = 300)
  print(scatter_plot)
  dev.off()
}

###regression analysis in Tutoring group
sink(file="Tutoring_ROI_regression_analysis.txt")
print(summary(lm(lACC_1_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
           data=sldata_scan[sldata_scan$Group=="Tutoring",])))
#print(summary(lm(lACC_2_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#           data=sldata_scan[sldata_scan$Group=="Tutoring",])))
#print(summary(lm(rACC_1_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#                 data=sldata_scan[sldata_scan$Group=="Tutoring",])))
#print(summary(lm(rACC_2_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#                 data=sldata_scan[sldata_scan$Group=="Tutoring",])))
print(summary(lm(rACC_3_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
                 data=sldata_scan[sldata_scan$Group=="Tutoring",])))
#print(summary(lm(RvClaustrum_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#           data=sldata_scan[sldata_scan$Group=="Tutoring",])))
print(summary(lm(RdClaustrum_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
           data=sldata_scan[sldata_scan$Group=="Tutoring",])))
print(summary(lm(RHippo_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
           data=sldata_scan[sldata_scan$Group=="Tutoring",])))
print(summary(lm(CombNAc_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
           data=sldata_scan[sldata_scan$Group=="Tutoring",])))
sink()

###regression analysis in NCC group
sink(file="NCC_ROI_regression_analysis.txt")
print(summary(lm(lACC_1_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
           data=sldata_scan[sldata_scan$Group=="NCC",])))
#print(summary(lm(lACC_2_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#           data=sldata_scan[sldata_scan$Group=="NCC",])))
#print(summary(lm(rACC_1_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#                 data=sldata_scan[sldata_scan$Group=="NCC",])))
#print(summary(lm(rACC_2_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#                 data=sldata_scan[sldata_scan$Group=="NCC",])))
print(summary(lm(rACC_3_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
                 data=sldata_scan[sldata_scan$Group=="NCC",])))
#print(summary(lm(RvClaustrum_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#           data=sldata_scan[sldata_scan$Group=="NCC",])))
print(summary(lm(RdClaustrum_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
           data=sldata_scan[sldata_scan$Group=="NCC",])))
print(summary(lm(RHippo_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
           data=sldata_scan[sldata_scan$Group=="NCC",])))
print(summary(lm(CombNAc_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
           data=sldata_scan[sldata_scan$Group=="NCC",])))
sink()

# summary(lm(ACC_1_2v1~ATL_Total_2v1+Gender+Age_1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$Group=="Tutoring",]))
# summary(lm(ACC_2_2v1~ATL_Total_2v1+Gender+Age_1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$Group=="Tutoring",]))
# summary(lm(RClaustrum_2v1~ATL_Total_2v1+Gender+Age_1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$Group=="Tutoring",]))
# summary(lm(RHippo_2v1~ATL_Total_2v1+Gender+Age_1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$Group=="Tutoring",]))
# summary(lm(CombNAc_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$Group=="Tutoring",]))

# summary(lm(ACC_1_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$MathGroup=="MD",]))
# summary(lm(ACC_2_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$MathGroup=="MD",]))
# summary(lm(RClaustrum_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$MathGroup=="MD",]))
# summary(lm(RHippo_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$MathGroup=="MD",]))
# summary(lm(CombNAc_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$MathGroup=="MD",]))
# 
# summary(lm(ACC_1_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$MathGroup=="TD",]))
# summary(lm(ACC_2_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$MathGroup=="TD",]))
# summary(lm(RClaustrum_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$MathGroup=="TD",]))
# summary(lm(RHippo_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$MathGroup=="TD",]))
# summary(lm(CombNAc_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_Cal_2v1+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$MathGroup=="TD",]))

# summary(lm(ACC_1_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$Group=="NCC",]))
# summary(lm(ACC_2_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$Group=="NCC",]))
# summary(lm(RClaustrum_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$Group=="NCC",]))
# summary(lm(RHippo_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$Group=="NCC",]))
# summary(lm(CombNAc_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+WJ_MFlu_2v1+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$Group=="NCC",]))
# summary(lm(CombNAc_2v1~ATL_Total_2v1+Gender+Age_1+FSIQ+Add_Accuracy_2v1,
#            data=sldata_scan[sldata_scan$Group=="NCC",]))

####################Additional exploratory part
##make scatter (2 groups)
#mk_scatter(sldata_scan,"ATL_Total_2v1","ACC_1_2v1","Group",seshade = F)
# mk_scatter(sldata_scan,"ATL_Total_2v1","ACC_2_2v1","Group",seshade = F)
# mk_scatter(sldata_scan,"ATL_Total_2v1","RClaustrum_2v1","Group",seshade = F)
# mk_scatter(sldata_scan,"ATL_Total_2v1","RHippo_2v1","Group",seshade = F)
# mk_scatter(sldata_scan,"ATL_Total_2v1","CombNAc_2v1","Group",seshade = F)
# mk_scatter(sldata_scan[!is.na(sldata_scan$Add_Accuracy_2v1),],"Add_Accuracy_2v1","CombNAc_2v1","Group",seshade = F)
# 
# ##make scatter (3 groups)
# mk_scatter(sldata_scan,"ATL_Total_2v1","ACC_1_2v1","MathGroup",seshade = F)
# mk_scatter(sldata_scan,"ATL_Total_2v1","ACC_2_2v1","MathGroup",seshade = F)
# mk_scatter(sldata_scan,"ATL_Total_2v1","RClaustrum_2v1","MathGroup",seshade = F)
# mk_scatter(sldata_scan,"ATL_Total_2v1","RHippo_2v1","MathGroup",seshade = F)
# mk_scatter(sldata_scan,"ATL_Total_2v1","CombNAc_2v1","MathGroup",seshade = F)
# 
# summary(lm(ACC_1_2v1~ATL_Total_2v1+Add_Accuracy_2v1,data=sldata_scan))
# 
# table(sldata_scan[,c("Group")])
# table(sldata_scan[,c("Group","Gender")])
# table(sldata_scan[,c("MathGroup","Gender")])
# ####
# mytable<-table(sldata_scan[,c("Group","Gender")])
# chisq.test(mytable)
# mytable<-table(sldata_scan[,c("MathGroup","Gender")])
# chisq.test(mytable)
# 
# ###Plot by region
# ####Tutoring
# tut_acc_nac<-sldata_scan[sldata_scan$Group=="Tutoring",c("PID","ATL_Total_2v1","ACC_1_2v1","CombNAc_2v1")]
# tut_acc_nac_long<-melt(tut_acc_nac,id.vars = c("PID","ATL_Total_2v1"))
# mk_scatter(tut_acc_nac_long,"ATL_Total_2v1","value","variable",seshade = F)
# ###r difference test
# cor.test(tut_acc_nac$ATL_Total_2v1,tut_acc_nac$ACC_1_2v1,method="pearson")
# cor.test(tut_acc_nac$ATL_Total_2v1,tut_acc_nac$CombNAc_2v1,method="pearson")
# cor.test(tut_acc_nac$ACC_1_2v1,tut_acc_nac$CombNAc_2v1,method="pearson")
# r.test(n=38,r12 = 0.4662,r13 = -0.025496,r23=0.2321,twotailed = T)
# 
# tut_hippo_nac<-sldata_scan[sldata_scan$Group=="Tutoring",c("PID","ATL_Total_2v1","RHippo_2v1","CombNAc_2v1")]
# tut_hippo_nac_long<-melt(tut_hippo_nac,id.vars = c("PID","ATL_Total_2v1"))
# mk_scatter(tut_hippo_nac_long,"ATL_Total_2v1","value","variable",seshade = F)
# ###r difference test
# cor.test(tut_hippo_nac$ATL_Total_2v1,tut_hippo_nac$RHippo_2v1,method="pearson")
# cor.test(tut_hippo_nac$ATL_Total_2v1,tut_hippo_nac$CombNAc_2v1,method="pearson")
# cor.test(tut_hippo_nac$RHippo_2v1,tut_hippo_nac$CombNAc_2v1,method="pearson")
# r.test(n=38,r12 = 0.43609,r13 = -0.025496,r23=0.2196,twotailed = T)
# 
# ####NCC
# ncc_acc_nac<-sldata_scan[sldata_scan$Group=="NCC",c("PID","ATL_Total_2v1","ACC_1_2v1","CombNAc_2v1")]
# ncc_acc_nac_long<-melt(ncc_acc_nac,id.vars = c("PID","ATL_Total_2v1"))
# mk_scatter(ncc_acc_nac_long,"ATL_Total_2v1","value","variable",seshade = F)
# 
# ncc_hippo_nac<-sldata_scan[sldata_scan$Group=="NCC",c("PID","ATL_Total_2v1","RHippo_2v1","CombNAc_2v1")]
# ncc_hippo_nac_long<-melt(ncc_hippo_nac,id.vars = c("PID","ATL_Total_2v1"))
# mk_scatter(ncc_hippo_nac_long,"ATL_Total_2v1","value","variable",seshade = F)


