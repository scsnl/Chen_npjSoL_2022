##Version 1.9.0#####

#require(Hmisc) #for rcorr
#require(reshape2) #for melt
#require(ggplot2)
#require(psych) #for descriptive stats
#require(Rmisc) #for summarySE
#require(car) #for scatter plot matrix
#require(GPArotation) #for factor analysis

##load required packages
packages <- c("Hmisc", "reshape2", "ggplot2", "psych", "Rmisc", "car",
                "GPArotation","stringr","corrplot")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

invisible(lapply(packages,require,c=T)) #invisible suppressed the output
###Setting up some parameters

statsRD<-2 ##decimal place for means, std, etc
pvalRD<-3 ###decimal place for p value

###########

##function to make histogram matrix for continuous variables   ##GRAPH
hist_mat<-function (data,stp,filehead) {  #data: dataset # stp: starting column number of continuous variable; filehead: the file name head for the histogram
	stp<-stp   #the starting number of continuous variables
	range<-stp:ncol(data)
	ncl<-round(sqrt(ncol(data)-stp-1))   #number of columns you want in the graph
	data_long<-melt(data,id.var=c(colnames(data)[1:(stp-1)])) #other non-continuous varaibles are conisdered as id
	print(summary(data_long))
	hist_p<-ggplot(data_long,aes(x=value)) +
        geom_histogram(colour="black",fill="white",bins=round(nrow(data)/3)) +  #colour and fill: outline & fill for bars; bins define the total of bars
		facet_wrap(~variable,ncol=ncl,scales="free")
	pdf(paste0(filehead,"_histogram.pdf"), height=10,width=10)
	print(hist_p)
	dev.off()
}


###group different t-table  ##Inferential test
##group t-tests
mk_ttable<-function (data,pgrpv,grplabels,stp,filehead,statsRD,pvalRD,equalvar=T) { 
  #pgrpv: column number for the group variable to compare 
  #equalvar: 1=equal variance; 0 = unequal variance
  #grplabels: a character vector with two elements to specify the two subgroups to compare
  ###drop unused levels of the group variable
  data<-data[(data[,pgrpv]==grplabels[1] |data[,pgrpv]==grplabels[2]),]
  ##drop the unselected levels
  data[,pgrpv]<-factor(data[,pgrpv])
  stp<-stp   #the starting number of continuous variables
	range<-stp:ncol(data)
	if(equalvar) {
		x<-lapply(data[,range], function(x) t.test(x~data[,pgrpv],var.equal=T,conf.level=.95))
	} else {
		x<-lapply(data[,range], function(x) t.test(x~data[,pgrpv],var.equal=F,conf.level=.95))
	}
	sd_grp1<-unlist(lapply(data[data[,pgrpv]==levels(data[,pgrpv])[1],range],sd,na.rm=T))
	mean_grp1<-unlist(lapply(data[data[,pgrpv]==levels(data[,pgrpv])[1],range],mean,na.rm=T))
	sd_grp2<-unlist(lapply(data[data[,pgrpv]==levels(data[,pgrpv])[2],range],sd,na.rm=T))
	mean_grp2<-unlist(lapply(data[data[,pgrpv]==levels(data[,pgrpv])[2],range],mean,na.rm=T))
	t.table<-matrix(,length(x),7) #create an empty matrix to save the t-tests results for all variables, there are 7 values, t, df, p, ci.lower, ci.upper, group1mean,group2mean
	##put mean and sd
	##if mean and sd are smaller than 0.005, they will be rounded to 0.00, so change to scienfic display if so
	t.table[,1]<-paste0(ifelse(abs(mean_grp1)>0.005,round(mean_grp1,digits=statsRD),
	                           formatC(mean_grp1,format = "e",digits=statsRD))," (",
	                    ifelse(abs(sd_grp1)>0.005,round(sd_grp1,digits=statsRD),formatC(sd_grp1,format = "e",digits=statsRD)),")")
	t.table[,2]<-paste0(ifelse(abs(mean_grp2)>0.005,round(mean_grp2,digits=statsRD),
	                           formatC(mean_grp2,format = "e",digits=statsRD))," (",
	                    ifelse(abs(sd_grp2)>0.005,round(sd_grp2,digits=statsRD),formatC(sd_grp2,format = "e",digits=statsRD)),")")
	for (i in 1:nrow(t.table)) {
		t.table[i,3]<-round(x[[i]]$statistic,digits=statsRD)
		t.table[i,4]<-x[[i]]$parameter
		t.table[i,5]<-ifelse(round(x[[i]]$p.value,digits=pvalRD)<0.001,"<.001",round(x[[i]]$p.value,digits=pvalRD))
		t.table[i,6]<-round(x[[i]]$conf.int[1],digits=statsRD)
		t.table[i,7]<-round(x[[i]]$conf.int[2],digits=statsRD)
	}
	
	colnames(t.table)<-c(paste0("Mean (SD) in ",levels(data[,pgrpv])[1]),
	                     paste0("Mean (SD) in ",levels(data[,pgrpv])[2]),
	                     "t-value","df","p-value","lower.ci","upper.ci")
	rownames(t.table)<-colnames(data[,range])
	
	##save t-tests results
	grpname<-levels(data[,pgrpv])
	write.csv(t.table,file=paste0(filehead,"_",grpname[1],"_vs_",grpname[2],"comp_t-test_summary_table.csv"),quote=F,row.names=T)
}

####make descriptive data summary table
mk_destable<-function (data,pgrpv,stp,filehead,statsRD) {  #pgrpv: column number for group variable; if equals to 0, no stats separate by group will be done
    stp<-stp   #the starting number of continuous variables
	drange<-stp:ncol(data)
	###descriptive data
	if (pgrpv == 0) {
	  sd_grp<-unlist(lapply(data[,drange],sd,na.rm=T))
	  sd_grp<-ifelse(abs(sd_grp)>0.005,round(sd_grp,statsRD),formatC(sd_grp,format = "e",digits = statsRD))
	  mean_grp<-unlist(lapply(data[,drange],mean,na.rm=T))
	  mean_grp<-ifelse(abs(mean_grp)>0.005,round(mean_grp,statsRD),formatC(mean_grp,format = "e",digits = statsRD))
	  min_grp<-unlist(lapply(data[,drange],min,na.rm=T))
	  max_grp<-unlist(lapply(data[,drange],max,na.rm=T))
	  range_grp<-paste(ifelse(abs(min_grp)>0.005,round(min_grp,statsRD),formatC(min_grp,format = "e",digits = statsRD)),
	                   ifelse(abs(max_grp)>0.005,round(max_grp,statsRD),formatC(max_grp,format = "e",digits = statsRD)),sep = "-")
	  n_grp<-unlist(lapply(data[,drange], function(x) sum(!is.na(x))))
	  summ_grp<-cbind(colnames(data[,drange]),n_grp,paste0(mean_grp," (",sd_grp,")"),range_grp)
	  colnames(summ_grp)<-c("variable","n","mean (SD)","range")
		write.csv(summ_grp,file=paste0(filehead,"_whole_sample_descriptive.csv"),row.names = F)
	} else {
		
		for (i in 1:length(levels(data[,pgrpv]))) {
		  sub_data<-data[data[,pgrpv]==levels(data[,pgrpv])[i],]
		  sd_grp<-unlist(lapply(data[,drange],sd,na.rm=T))
		  sd_grp<-ifelse(abs(sd_grp)>0.005,round(sd_grp,statsRD),formatC(sd_grp,format = "e",digits = statsRD))
		  mean_grp<-unlist(lapply(data[,drange],mean,na.rm=T))
		  mean_grp<-ifelse(abs(mean_grp)>0.005,round(mean_grp,statsRD),formatC(mean_grp,format = "e",digits = statsRD))
		  min_grp<-unlist(lapply(data[,drange],min,na.rm=T))
		  max_grp<-unlist(lapply(data[,drange],max,na.rm=T))
		  range_grp<-paste(ifelse(abs(min_grp)>0.005,round(min_grp,statsRD),formatC(min_grp,format = "e",digits = statsRD)),
		                   ifelse(abs(max_grp)>0.005,round(max_grp,statsRD),formatC(max_grp,format = "e",digits = statsRD)),sep = "-")
		  n_grp<-unlist(lapply(sub_data[,drange], function(x) sum(!is.na(x))))
		  summ_grp<-cbind(colnames(data[,drange]),n_grp,paste0(mean_grp," (",sd_grp,")"),range_grp)
		  colnames(summ_grp)<-c("variable","n","mean (SD)","range")
			write.csv(summ_grp,file=paste0(filehead,"_",levels(data[,pgrpv])[i],"_group_descriptive.csv"),row.names = F)
		}	
	}
}


###Correlation matrix
mk_rtable<-function (data,stp,statsRD,filehead) {
    data_temp<-data[,(stp:ncol(data))]
	r.mat<-corr.test(data_temp)
	r.table<-r.mat$r
	r.table<-round(r.table,digits=statsRD)
	p<-r.mat$p
	p[is.na(p)]<-1
	r.table[p<0.05]<-paste0(r.table[p<0.05],"*")
	r.table[p<0.01]<-paste0(r.table[p<0.01],"*")
	r.table[p<0.001]<-paste0(r.table[p<0.001],"*")
	r.table[(p<.10 & p>.05)]<-paste0(r.table[(p<.10 & p>.05)],"#")
	#r.table<-matrix(r.table,ncol(data_temp),ncol(data_temp),byrow=T)
	r.table[upper.tri(r.table)]<-""
	diag(r.table)<-"-"
	colnames(r.table)<-colnames(data_temp)
	rownames(r.table)<-colnames(data_temp)
	write.csv(r.table, file=paste0(filehead,"_correlation_matrix.csv"),quote=F,row.names=T)
}

######GRAPH

color<-c("firebrick1","deepskyblue1","green2","purple2","yellow1","chocolate1","salmon")

###make bar graph for group comparison on multiple repeated measures
#measrange: col numbers of measures; 
#grpasx: wehther to plot group as x-axis, 1= Yes, 0 = No; 
#grplabels are used to mark the group or variable names for the legend
#style: "value" or "star"

mk_grpbargraph<-function (data,pgrpv,measrange,grplabels,grpasx,style,annotate_dist) {  
  pgrpv<-pgrpv   #the starting number of continuous variables
	range<-measrange
	data_part<-data[,c(pgrpv,range)]
	data_long<-melt(data_part,id.var=c(colnames(data)[pgrpv])) #other non-continuous varaibles are conisdered as id
	colnames(data_long)[1]<-"Group" ##standardize the name 
	#print(summary(data_long))
	data_long_sum<-summarySE(data_long,measurevar="value",groupvar=c("variable","Group"),na.rm=T)
	if (grpasx == 1) {    #controling whether the bar cluster is organized by group or variable/measures
		xval <-"Group" #in order to pass the group variable name in ggplot
		fval <-"variable"
		ngrp<-length(range)
		bgrp<-colnames(data)[range]
		ngrpclust<-length(levels(data_long_sum$Group)) ##find the unique levels of groups
		##y position of the hline is 2*SE about the error bar as default
		if( missing(annotate_dist)) {
		  ycor<-2*ddply(data_long_sum,.(Group),summarise, Value=max(se))[,2] +
		    ddply(data_long_sum,.(Group),summarise, Value=max(value))[,2]
		} else {
		  ycor<-annotate_dist*ddply(data_long_sum,.(Group),summarise, Value=max(se))[,2] +
		    ddply(data_long_sum,.(Group),summarise, Value=max(value))[,2]
		}
	} else {
		fval <-"Group"
		xval <-"variable"
		ngrp <-length(levels(data_long_sum$Group))
		bgrp<-levels(data_long_sum$Group)
		xn<-"variable"
		ngrpclust<-length(levels(data_long_sum$variable))##when group is not the x-axis, find the total levels of other variable
		##y position of the hline is 2*SE about the error bar as default
		if( missing(annotate_dist)) {
		  ycor<-2*ddply(data_long_sum,.(variable),summarise, Value=max(se))[,2] +
		    ddply(data_long_sum,.(variable),summarise, Value=max(value))[,2]
		} else {
		  ycor<-annotate_dist*ddply(data_long_sum,.(variable),summarise, Value=max(se))[,2] +
		    ddply(data_long_sum,.(variable),summarise, Value=max(value))[,2]
		}
		
	}
    bar_graph<-ggplot(data_long_sum,aes(x=get(xval),y=value,fill=factor(get(fval)))) +
               geom_bar(position=position_dodge(), stat="identity",color="black", width=0.8) +               
               geom_errorbar(aes(ymin=value-se,ymax=value+se),width=0.4,position=position_dodge(.8)) +
               coord_cartesian(ylim=c(floor((min(data_long_sum$value)-2*max(data_long_sum$sd))*10)/10,
                                      ceiling((max(data_long_sum$value)+max(data_long_sum$sd)/2)*10)/10)) +
               scale_fill_manual(name="",values=color[1:ngrp],breaks=bgrp,labels=grplabels) +   #set the color scheme which needs to match the total number of variables
               #scale_x_discrete(breaks=bgrp,labels=xlab) +
               #labs(y="value",x=xn) +
               theme_bw()
    ##adding annotation
    if (grpasx == 1) {
      for (i in 1:ngrpclust) {
        ##instead of using t.test, using F-test to be more flexible #however, when using grp as x-axis, we should use repeated ANOVA!
        dat_temp<-data_long[data_long$Group==levels(data_long$Group)[i],]
        ##add PID (maybe need to reformat this in the future)
        dat_temp$PID<-as.factor(rep(1:(length(dat_temp$variable)/length(levels(dat_temp$variable))),length(levels(dat_temp$variable))))
        x<-summary(aov(value~variable+Error(PID/variable),data=dat_temp))
        if (style %in% c("value","values","number","numbers")) {
          pval<-ifelse(x[[2]][[1]]$`Pr(>F)`[1]<0.001,"p<0.001",paste0("p=",round(x[[2]][[1]]$`Pr(>F)`[1],3)))
        } else {
          pval<-ifelse(x[[2]][[1]]$`Pr(>F)`[1]<0.001,"***",ifelse(x[[2]][[1]]$`Pr(>F)`[1]<0.01,"**",
                                                             ifelse(x[[2]][[1]]$`Pr(>F)`[1]<0.05,"*",ifelse(x[[2]][[1]]$`Pr(>F)`[1]<0.1,
                                                                                                       paste0("p=",round(x[[2]][[1]]$`Pr(>F)`[1],3)),"n.s."))))
        }
        bar_graph<-bar_graph + 
          annotate("segment",x=i-0.25,xend=i+0.25,y=ycor[i],yend=ycor[i])+
          annotate("text",x=i,y=1.02*ycor[i],size=5,label=pval)
      }
    } else {
      for (i in 1:ngrpclust) {
        x<-summary(aov(value~Group,data_long[data_long$variable==levels(data_long$variable)[i],]))
        if (style %in% c("value","values","number","numbers")) {
          pval<-ifelse(x[[1]]$`Pr(>F)`[1]<0.001,"p<0.001",paste0("p=",round(x[[1]]$`Pr(>F)`[1],3)))
        } else {
          pval<-ifelse(x[[1]]$`Pr(>F)`[1]<0.001,"***",ifelse(x[[1]]$`Pr(>F)`[1]<0.01,"**",
                                                             ifelse(x[[1]]$`Pr(>F)`[1]<0.05,"*",ifelse(x[[1]]$`Pr(>F)`[1]<0.1,
                                                                           paste0("p=",round(x[[1]]$`Pr(>F)`[1],3)),"n.s."))))
        }
        bar_graph<-bar_graph + 
          annotate("segment",x=i-0.25,xend=i+0.25,y=ycor[i],yend=ycor[i])+
          annotate("text",x=i,y=1.05*ycor[i],size=5,label=pval)
      }
    }
    
	return(bar_graph)
  #pdf(paste0(filehead,"_",paste(bgrp,collapse="_"),"_group_comp_bargraph.pdf"),height=3*ngrp,width=4*ngrp)
	#plot(bar_graph)
	#dev.off()
}

##make scatter plots for different groups
#colnames for x, y, and grouping variables in the data;seshade: 1 plot SE shades
mk_scatter<-function (data,xname,yname,gname,seshade=F) { 
	xp<-which(colnames(data)==xname)
	yp<-which(colnames(data)==yname)
	gp<-which(colnames(data)==gname)
	##get the iv range to scale the x-axis
	xrange<-range(data[,c(xname)])[2]-range(data[,c(xname)])[1]
	yrange<-range(data[,c(yname)])[2]-range(data[,c(yname)])[1]
	#plotting
	scatter_plot<-ggplot(data,aes(get(colnames(data)[xp]),get(colnames(data)[yp]),color=get(colnames(data)[gp]))) +
	              geom_point(shape=21,size=4,aes(fill=get(colnames(data)[gp])),color="white",stroke=1) +
				  #scale_colour_hue(l=70) +   #use a slightly darker palette than normal
				        geom_smooth(method=lm,se=seshade) +
				        scale_fill_manual(name=colnames(data)[gp],values=color[1:length(levels(data[,gp]))]) +
	              scale_color_manual(name=colnames(data)[gp],values=color[1:length(levels(data[,gp]))]) +
                labs(y=colnames(data)[yp],x=colnames(data)[xp]) +
	              scale_x_continuous(limits=c(floor(min(data[,c(xname)])*10)/10,ceiling((max(data[,c(xname)])+0.15*xrange)*10)/10)) +
	              scale_y_continuous(limits=c(floor(min(data[,c(yname)])*10)/10,ceiling((max(data[,c(yname)])+0.15*yrange)*10)/10)) +
                theme_bw()
	scatter_plot<-scatter_plot + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
	
	for (i in 1:length(levels(data[,c(gname)]))) {
	  x<-rcorr(as.matrix(na.omit(data[data[,c(gname)]==levels(data[,c(gname)])[i],c(xname,yname)])))
	  rpval<-paste0(ifelse(abs(x$r[1,2])<0.001,"|r|< 0.001",paste0("r = ",round(x$r[1,2],3)))," ",
	                ifelse(x$P[1,2]<0.001,"***",ifelse(x$P[1,2]<0.01,"**",ifelse(x$P[1,2]<0.05,"*",ifelse(x$P[1,2]<0.1,"#","n.s.")))))
	  ##adding annotations
	  scatter_plot<-scatter_plot + 
	    annotate("text",x=max(data[,c(xname)]),y=max(data[,c(yname)])+(0.15-0.06*(i-1))*yrange,size=5,label=rpval,color=color[i])
	}
	
	
	return(scatter_plot)
	#pdf(paste0(filehead,"_",xname,"_with_",yname,"_by_",gname,"_scatterplot.pdf"),width=10,height=10)
	#plot(scatter_plot)
	#dev.off()
}

##specific function for one group
mk_scatter_oneGroup<-function (data,xname,yname,gname="one",seshade=F) { #default gname is "one"
  xp<-which(colnames(data)==xname)
  yp<-which(colnames(data)==yname)
  ##get the iv range to scale the x-axis
  xrange<-range(data[,c(xname)])[2]-range(data[,c(xname)])[1]
  yrange<-range(data[,c(yname)])[2]-range(data[,c(yname)])[1]
  
  if((gname!="one" & gname!="One")) {
    gp<-which(colnames(data)==gname)
    #plotting
    scatter_plot<-ggplot(data,aes(get(colnames(data)[xp]),get(colnames(data)[yp]))) +
      geom_point(shape=21,size=4,aes(fill=get(colnames(data)[gp])),color="white",stroke=1) +
      #scale_colour_hue(l=70) +   #use a slightly darker palette than normal
      #geom_smooth(method=lm) +
      #geom_smooth(method=lm,formula = colnames(data)[yp]~colnames(data)[xp], color="black", size=5, se=seshade) +
      scale_fill_manual(name=colnames(data)[gp],values=color[1:length(levels(data[,gp]))]) +
      labs(y=colnames(data)[yp],x=colnames(data)[xp]) +
      scale_x_continuous(limits=c(floor(min(data[,c(xname)])*10)/10,ceiling((max(data[,c(xname)])+0.15*xrange)*10)/10)) +
      scale_y_continuous(limits=c(floor(min(data[,c(yname)])*10)/10,ceiling((max(data[,c(yname)])+0.15*yrange)*10)/10)) +
      theme_bw()
  } else {
    #plotting
    scatter_plot<-ggplot(data,aes(get(colnames(data)[xp]),get(colnames(data)[yp]))) +
      geom_point(shape=21,size=4,fill=color[1],color="white",stroke=1) +
      #scale_colour_hue(l=70) +   #use a slightly darker palette than normal
      #geom_smooth(method=lm) +
      #geom_smooth(method=lm,formula = colnames(data)[yp]~colnames(data)[xp], color="black", size=5, se=seshade) +
      labs(y=colnames(data)[yp],x=colnames(data)[xp]) +
      scale_x_continuous(limits=c(floor(min(data[,c(xname)])*10)/10,ceiling((max(data[,c(xname)])+0.15*xrange)*10)/10)) +
      scale_y_continuous(limits=c(floor(min(data[,c(yname)])*10)/10,ceiling((max(data[,c(yname)])+0.15*yrange)*10)/10)) +
      theme_bw()
  }
  
  scatter_plot<-scatter_plot + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
  ##get r and p
  x<-rcorr(as.matrix(na.omit(data[,c(xname,yname)])))
  rpval<-paste0(ifelse(abs(x$r[1,2])<0.001,"|r|< 0.001",paste0("r = ",round(x$r[1,2],3)))," ",
                ifelse(x$P[1,2]<0.001,"***",ifelse(x$P[1,2]<0.01,"**",ifelse(x$P[1,2]<0.05,"*",ifelse(x$P[1,2]<0.1,"#","n.s.")))))
  ##get regression line
  xreg<-lm(data[,c(yname)]~data[,c(xname)])
  
  ##adding annotations
  
  scatter_plot<-scatter_plot + 
    annotate("text",x=max(data[,c(xname)]),y=max(data[,c(yname)]+0.15*yrange),size=3,label=rpval,color="black") +
    annotate("segment",x=min(data[,c(xname)]),xend=max(data[,c(xname)]),y=xreg$coefficients[1]+xreg$coefficients[2]*min(data[,c(xname)]),
             yend=xreg$coefficients[1]+xreg$coefficients[2]*max(data[,c(xname)]),color="black",size=2)
  
  
  return(scatter_plot)
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
						  