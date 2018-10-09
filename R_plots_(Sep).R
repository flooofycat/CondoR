R Code for making Bar graphs and Scatter plots
#Barchart Condor Autopilot

#Install ggplot2 package
#install.packages("ggplot2")
#install.packages("dplyr")
library(ggplot2)
library(scales)
library(dplyr)


# Read CSV file from folder
setwd("C:\\Users\\I\\Desktop\\Blackhall\\Westpac\\csv_dedup")
Ex.data<-read.csv(file="Outgroup_Actor_Sep.csv",header=TRUE,check.names = FALSE)

Ex.data$team_name<-as.character(Ex.data$team_name)
Ex.data$team_name<-replace(Ex.data$team_name,Ex.data$team_name=="CTPCTeam","CPDO")
Ex.data$team_name<-replace(Ex.data$team_name,Ex.data$team_name=="FinanceTransformation","Finance")
Ex.data$team_name<-replace(Ex.data$team_name,Ex.data$team_name=="BrandLeads","BrandLeads")
Ex.data$team_name<-replace(Ex.data$team_name,Ex.data$team_name=="InnovationTeam","Innovation")
Ex.data$team_name<-as.factor(Ex.data$team_name)


#Filter down to 4 Teams
#this.ctpc<-subset(Ex.data,team_name=="CTPCTeam")
#this.finance<-subset(Ex.data,team_name=="FinanceTransformation")
#this.brand<-subset(Ex.data,team_name=="BrandLeads")
#this.inno<-subset(Ex.data, team_name=="InnovationTeam")

#Internal Dataset
In.data<-read.csv(file="CPDO_Actor_Sep.csv",header=TRUE,check.names = FALSE)
In.data2<-read.csv(file="Finance_Actor_Sep.csv",header=TRUE,check.names = FALSE)
In.data3<-read.csv(file="Brand_Actor_Sep.csv",header=TRUE,check.names = FALSE)
In.data4<-read.csv(file="Inno_Actor_Sep.csv",header=TRUE,check.names = FALSE)

#Column Names of Desired Attributes
this.attribute_out<-colnames(Ex.data[c(5,7,16,17,27,31,34,37,39,43)])
#team vector
this.team<-c("CPDO","Finance","BrandLeads","Innovation")


# Ex.data[,this.arrtibut[2]]
#subset(Ex.data,team_name=="CTPCTeam")[,this.attribute_out[1]]
for (j in 1:length(this.team)){
for (i in 1:length(this.attribute_out))
{ 
  #Puts ingroup in a vector
  Attribute_in=c(mean(In.data[,this.attribute_out[i]],na=TRUE),
                 mean(In.data2[,this.attribute_out[i]],na=TRUE),
                 mean(In.data3[,this.attribute_out[i]],na=TRUE),
                 mean(In.data4[,this.attribute_out[i]],na=TRUE))
  #Filter Ex.data to just the 4 teams
  WBM_Out<-subset(Ex.data,team_name!="")
  
  Team_out=mean(subset(WBM_Out,team_name==this.team[j])[,this.attribute_out[i]],na=TRUE)
  Team_in=Attribute_in[j]
  
  #Selects the other 3 teams
  WBM_In=mean(Attribute_in[Attribute_in!=Attribute_in[j]],na=TRUE)
  #Selects the other 3 teams
  WBM_Out<-mean(subset(WBM_Out,team_name!=this.team[j])[,this.attribute_out[i]],na=TRUE)
  
    df2<-data.frame(I_E=rep(c("External","Internal"), each=2),this.metric=c(Team_out,WBM_Out,Team_in,WBM_In), 
                    Team_name=c(this.team[j],"Westpac \nBenchmark"))
    
  #cpdo_out=this.ctpc[,this.attribute_out[i]]
  #cpdo_in=In.data[,this.attribute_out[i]]
  #finance_out=this.finance[,this.attribute_out[i]]
  #finance_in=In.data2[,this.attribute_out[i]]
  #brand_out=this.brand[,this.attribute_out[i]]
  #brand_in=In.data3[,this.attribute_out[i]]
  #inno_out=this.inno[,this.attribute_out[i]]
  #inno_in=In.data4[,this.attribute_out[i]]
  #WBM_Out=mean(c(finance_out,brand_out,inno_out),na=TRUE)
  #WBM_In=mean(c(finance_in,brand_in,inno_in),na=TRUE)
  
  if (this.attribute_out[i]!="Balanced Contributor") {
    
    if (max(Team_in,Team_out,WBM_Out,WBM_In)>100) {
      L=c(floor(min(Team_in,Team_out,WBM_Out,WBM_In))/2,150)
    }else if (max(Team_in,Team_out,WBM_Out,WBM_In)>2){
      L=c(floor(min(Team_in,Team_out,WBM_Out,WBM_In))/2,
          ceiling(max(Team_in,Team_out,WBM_Out,WBM_In))*1.2)
    } else if (max(Team_in,Team_out,WBM_Out,WBM_In)<0.5) {
      L=c(floor(min(Team_in,Team_out,WBM_Out,WBM_In)),
          ceiling(max(Team_in,Team_out,WBM_Out,WBM_In))/2)
    } else {
      L=c(floor(min(Team_in,Team_out,WBM_Out,WBM_In)),
          ceiling(max(Team_in,Team_out,WBM_Out,WBM_In)))
    }
      
      
      
    this.bar<-ggplot(df2[order(df2$I_E,decreasing=FALSE),],
                     aes(x=Team_name,y=this.metric,fill=factor(I_E,levels=rev(levels(I_E))))) + 
      geom_bar(stat="identity",position=position_dodge(),width=0.5)+
      theme_minimal()+guides(fill=guide_legend(reverse=TRUE))+
      scale_y_continuous(limits=L,oob = rescale_none,expand=c(0,0))
    
    this.bar+
      coord_flip()+scale_fill_manual(values=c('#0070c0','#002060'))+
      theme_classic()+
      theme(panel.grid.major = element_line(colour="grey",size=0.5))+
      theme(panel.grid.major.y=element_blank())+
      theme(legend.title=element_blank())+
      theme(axis.title.x = element_blank())+
      theme(axis.title.y=element_blank())+
      theme(axis.text=element_text(colour="black"))+
      ggtitle(this.attribute_out[i])+
      theme(plot.title=element_text(hjust=0.5,vjust=0.5,size=11,face="bold"))
    
      ggsave(filename=paste(this.team[j],this.attribute_out[i],".png"),
                      scale=1,width=100,height=60,units="mm",dpi="print")
    
    
    
  } else{
    
    #Plotting barchart
    x_pos<-ifelse(df2$Team_name==this.team[j],1,2) # Specifies the distance from the x-axis
    y_pos<-ifelse(df2$Team_name==this.team[j],.3,.3) # Specifies the distance from the y-axis
    
    this.bar<-ggplot(df2[order(df2$I_E,decreasing=FALSE),],aes(x=Team_name,y=this.metric,
                                                              fill=factor(I_E,levels=rev(levels(I_E))))) + 
      geom_bar(stat="identity",position=position_dodge(),width=0.5)+
      guides(fill=guide_legend(reverse=TRUE))+
      scale_y_continuous(limits=c(-1,1),breaks=pretty(c(-1,1),n=10),oob = rescale_none)+
      geom_text(aes(y=y_pos,x=x_pos,label =Team_name),color= "black",size=3.5)
    
    this.bar+coord_flip()+scale_fill_manual(values=c('#0070c0','#002060'))+theme_classic()+
      theme(legend.title=element_blank())+theme(axis.title.x = element_blank())+
      theme(axis.title.y=element_blank())+theme(axis.ticks.y=element_blank())+
      theme(axis.line.y=element_blank())+theme(axis.text.y=element_blank())+
      theme(panel.grid.major=element_line(colour="grey",size=0.5))+
      theme(panel.grid.major.y = element_blank())+
      geom_hline(yintercept =0, colour = "black" )+
      theme(axis.text=element_text(colour="black"))+
      ggtitle("Balance Contributor") +
      theme(plot.title=element_text(hjust=0.5,vjust=0.5,size=11,face="bold"))+
      theme(plot.margin=unit(c(1,1,1,1),"mm"))
    
    ggsave(filename=paste(this.team[j],this.attribute_out[i],".png"),
           scale=1,width=100,height=60,units="mm",dpi="print")
#Condor Autopilot Scatter Plots

#Install ggplot2 package
#install.packages("ggplot2")
#install.packages("dplyr")
library(ggplot2)
library(scales)
library(dplyr)
library(ggrepel)


# Read CSV file from folder
setwd("C:\\Users\\I\\Desktop\\Blackhall\\Westpac\\csv_dedup")
Ex.data<-read.csv(file="Outgroup_Actor_Sep.csv",header=TRUE,check.names = FALSE)

Ex.data$team_name<-as.character(Ex.data$team_name)
Ex.data$team_name<-replace(Ex.data$team_name,Ex.data$team_name=="CTPCTeam","CPDO")
Ex.data$team_name<-replace(Ex.data$team_name,Ex.data$team_name=="FinanceTransformation","Finance")
Ex.data$team_name<-replace(Ex.data$team_name,Ex.data$team_name=="BrandLeads","BrandLeads")
Ex.data$team_name<-replace(Ex.data$team_name,Ex.data$team_name=="InnovationTeam","Innovation")
Ex.data$team_name<-as.factor(Ex.data$team_name)

In.data<-read.csv(file="4 Teams.csv",header=TRUE,check.names = FALSE)

In.data$team_name<-as.character(In.data$team_name)
In.data$team_name<-replace(In.data$team_name,In.data$team_name=="CTPCTeam","CPDO")
In.data$team_name<-replace(In.data$team_name,In.data$team_name=="FinanceTransformation","Finance")
In.data$team_name<-replace(In.data$team_name,In.data$team_name=="BrandLeads","BrandLeads")
In.data$team_name<-replace(In.data$team_name,In.data$team_name=="InnovationTeam","Innovation")
In.data$team_name<-as.factor(In.data$team_name)

#Column Names of Desired Attributes
this.attribute_out<-colnames(Ex.data[c(5,7,16,17,27,31,34,37,39,43)])
#team vector
this.team<-c("CPDO","Finance","BrandLeads","Innovation")

for (j in 1:length(this.team)){
  for (i in 1:length(this.attribute_out)) {
    this.rank<-as.factor(subset(In.data,team_name==this.team[j])[,"Rank"])
    
    this.scatter<-ggplot(subset(In.data,team_name==this.team[j]),aes(x=subset(In.data,team_name==this.team[j])[,"Messages total"],
      y=subset(In.data,team_name==this.team[j])[,this.attribute_out[i]],label=Name,colour=this.rank))+
      geom_point()+
      geom_text_repel(aes(label=Name),hjust=0,vjust=0,size=2)+
      theme_classic()+
      labs(title=this.attribute_out[i],x="Messages Total",y=this.attribute_out[i])+
      theme(plot.title=element_text(hjust=0.5,vjust=0.5,size=11,face="bold"))+
      theme(axis.text=element_text(colour="black"))+
      theme(axis.text=element_text(size=8))+
      theme(axis.title=element_text(size=10))+
      scale_color_manual(values=c('#e41a1c','#377eb8', '#4daf4a','#ffaec9','#ff9700'))+
      theme(legend.position = "none")
    this.scatter
    ggsave(filename=paste("Scatter", this.team[j],this.attribute_out[i],".png"),
                          scale=1,width=80,height=80,units="mm",dpi="print")
    
  }
}


    
    
  }
}
}
  




























# 26th September 2018
# SPAM FILTER AND DEDUPLICATION

import os
import csv
import re

# Work directory 
os.chdir('C:\\Users\\vincent\\Desktop\\Westpac\\Python_experiments')

# Name of csvs for filtering
spam_list = 'spam_list_v2.csv'
keep_list = 'keep_list_domain.csv'

# List of email logs
infile = 'email_logs_july_aug_cc.csv'

# Name of file after spam removal
outfile_spam_rem = 'temp.csv'

# Name of file after duplicate removal
outfile_spam_dup_rem = 'email_logs_july_august_cc_clean_2018_09_26.csv'

# Delimiter_in
# delim_in = '|'

## Loads into memory the list of spam emails
with open(spam_list, 'r', encoding='utf-8') as f:
	spam_list = set([l.strip().lower() for l in f.readlines()])

## Loads into memory the list email domains to keep
with open(keep_list, 'r', encoding='utf-8') as g:
	keep_list = set([l.strip().lower() for l in g.readlines()])


## Filters out the spam in the email logs
with open(infile, 'r', errors='ignore') as csv_file:
	reader = csv.DictReader(csv_file, delimiter = '|')
	# row_count = sum(1 for line in csv_file)
	# print(row_count)
	with open(outfile_spam_rem, 'w', newline = '') as new_file:
		fieldnames = ['date_time', 'sender_address', 'recipient_address', 'message_subject', 'recipient_status']
		csv_writer = csv.DictWriter(new_file, fieldnames = fieldnames, delimiter = '|')
		csv_writer.writeheader()
		for i, line in enumerate(reader):
			# print(re.search("@[\w.]+", line['recipient_address'])[0])
			if re.search("@[\w.]+", line['sender_address']) is None: 
				continue
			if re.search("@[\w.]+", line['recipient_address']) is None: 
				continue
			if (line['recipient_address'] in spam_list) or (line['sender_address'] in spam_list):
				continue
			if re.search("^DL", line['sender_address']) is True:
				continue
			if re.search("^DL", line['recipient_address']) is True:
				continue
			# if set([  re.search("@[\w.]+", line['recipient_address'])[0], re.search("@[\w.]+", line['sender_address'])[0] ]) <= set(['@westpac.com.au', '@stgeorge.com.au', '@banksa.com.au', '@wbg.com.au', '@bankofmelbourne.com.au', '@btfinancialgroup.com.au', '@rams.com.au' ]):
			# 	csv_writer.writerow(line)
			if set([  re.search("@[\w.]+", line['recipient_address'])[0], re.search("@[\w.]+", line['sender_address'])[0] ]) <= set(keep_list):
				csv_writer.writerow(line)
			if i % 10000 == 0:
				# percent = i/row_count
				print(i)
				# print(row_count)
				i+=1
				
# Deduplicates the emails
with open(outfile_spam_rem,'r') as in_file, open(outfile_spam_dup_rem,'w') as out_file:
    seen = set() # set for fast O(1) amortized lookup
    for line in in_file:
        if line in seen: continue # skip duplicate

        seen.add(line)
        out_file.write(line)

# Removes the temporary file spam_removed 
os.remove(outfile_spam_rem)

# re.search("@[\w.]+", "comboy@gmail.com.au")
# "comboy@gmail.com.au".split("@")[1].split(".")[0]





Time Series R

#Time Series#

#install.packages("tidyr")
#installed.packages("dplyr")
#install.packages("chron")
#install.packages("lubridate")

library(chron)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)



setwd("C:\\Users\\I\\Desktop\\Blackhall\\Westpac\\Condor_Second_Stage")
this.BC<-read.csv(file="BC_TS.csv",header=TRUE,check.names = FALSE)
this.rank<-read.csv(file="rank_label.csv",header=TRUE,check.names=FALSE)
colnames(this.rank)[1]<-"Uuid"

this.BC<-merge(this.rank,this.BC,"Uuid")
#n <- 10
#qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
#col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

for (i in 2:3) {
  tempdata=subset(this.BC,Rank==i)
  tempdata[is.na(tempdata)]<-0
  tempdata<-t(tempdata)
  tempdata<-tempdata[-c(1,2),]
  colnames(tempdata)<-tempdata[1,]
  tempdata<-tempdata[-1,]
  tempdata<- cbind(rownames(tempdata),data.frame(tempdata,row.names=NULL))
  colnames(tempdata)[1]="date"
  tempdata$date<-substring(tempdata$date,7)
  tempdata$date<-substr(tempdata$date,1,10)
  tempdata$date<-dmy(tempdata$date)
  
  for (j in 2:length(colnames(tempdata))) {
    tempdata[,j]<-as.numeric(as.character(tempdata[,j]))
    
  }
  
  
  bc <- tempdata %>%
    select(as.character((colnames(tempdata)))) %>%
    gather(key = "variable", value = "value", -date)
  
  print(ggplot(bc, aes(x = date, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    scale_colour_brewer("Colors in Set1", palette="Dark2") +
    theme_minimal()+
    theme(legend.title = element_blank()))
}
