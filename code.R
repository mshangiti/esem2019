#########################
#########################
# This file can be used to recreate all the figures shown in the ESEM 2019 paper titled: 
# (Why is Developing Machine Learning Applications Challenging? A Study on Stack Overflow Posts)
#########################
#########################


#########################
#Creating Figure 1
#########################
#loading libs
library(ggplot2)
library(reshape)
#reading data
resultset = rbind(read.csv2("./quantitative_sample/ml_quan_sample_1.csv", stringsAsFactors=FALSE), 
                  read.csv2("./quantitative_sample/ml_quan_sample_2.csv", stringsAsFactors=FALSE),
                  read.csv2("./quantitative_sample/ml_quan_sample_3.csv", stringsAsFactors=FALSE),
                  read.csv2("./quantitative_sample/ml_quan_sample_4.csv", stringsAsFactors=FALSE))
#creating a year column
resultset[,"year"] = as.numeric(format(as.POSIXlt(resultset[,"CreationDate"]),"%Y"))
#creating a placeholder for results
temp = data.frame("Year" = as.factor(names(table(resultset$year))), "TotalPosts" = as.numeric(table(resultset$year)), "TotalUsers" = rep(0,11))
#How many users each user?
temp[1,"TotalUsers"] = length(unique(resultset[resultset$year==2008,"UserIdCombined"]))
temp[2,"TotalUsers"] = length(unique(resultset[resultset$year==2009,"UserIdCombined"]))
temp[3,"TotalUsers"] = length(unique(resultset[resultset$year==2010,"UserIdCombined"]))
temp[4,"TotalUsers"] = length(unique(resultset[resultset$year==2011,"UserIdCombined"]))
temp[5,"TotalUsers"] = length(unique(resultset[resultset$year==2012,"UserIdCombined"]))
temp[6,"TotalUsers"] = length(unique(resultset[resultset$year==2013,"UserIdCombined"]))
temp[7,"TotalUsers"] = length(unique(resultset[resultset$year==2014,"UserIdCombined"]))
temp[8,"TotalUsers"] = length(unique(resultset[resultset$year==2015,"UserIdCombined"]))
temp[9,"TotalUsers"] = length(unique(resultset[resultset$year==2016,"UserIdCombined"]))
temp[10,"TotalUsers"] = length(unique(resultset[resultset$year==2017,"UserIdCombined"]))
temp[11,"TotalUsers"] = length(unique(resultset[resultset$year==2018,"UserIdCombined"]))
#putting everything in the right shape for plotting
temp2 = melt(temp[which(temp$Year!=2018),],year=c("year"))
myplot = ggplot(data=temp2, aes(x=Year, y = value ))+
  geom_bar(stat="identity", color="black", position = "dodge", aes(fill=variable)) + scale_fill_brewer(palette="OrRd")+
  labs(title="", x="",y="") +
  theme_bw() + theme(legend.title=element_blank())+ theme(legend.position="top") +
  theme(legend.spacing.x = unit(0.5,"cm"), legend.text = element_text(size=30, face = "bold", color = "black") ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=30), axis.text.x = element_text(size=30, face = "bold", color = "black"), axis.text.y = element_text(size=30, face = "bold", color = "black"))
myplot







#########################
#Creating Figure 2
#########################
#loadings libs
library(ggplot2)
#reading files
mltags = read.csv2("./custom/ml_tags_stat.csv")
finalset = read.csv("./custom/others_tag_stat.csv")
#creating a placeholder for results
isML= as.factor(c(rep("Quantitative Sample \n(ML tags)",dim(mltags)[1]),rep("Baseline Sample \n(5K tags)",dim(finalset)[1])))
input = c(mltags$noAcceptedAnswerCount/mltags$totalCount)
input = c(input, finalset$noAcceptedAnswerCount/finalset$totalCount)
mydata =  data.frame(input=input
                     , group = isML)
#plotting 1
myplot= ggplot(mydata, aes(x = input*100, y=..scaled..,fill = group)) + 
  geom_density(alpha = 0.5)+
  ggtitle("")+
  labs(x="% of questions with no accepted answer", y ="Density") + theme_bw() +
  theme(legend.title=element_blank(), panel.grid.major = element_blank())+ theme(legend.position="top") + scale_fill_brewer(palette="OrRd")+
  theme(text = element_text(size=20))+
  theme(text = element_text(size=40,face = "bold"),axis.text.x = element_text(size=40, face = "bold", color = "black"), axis.text.y = element_text(size=40, face = "bold", color = "black")) +
  theme(legend.spacing.x = unit(0.5,"cm"), legend.text = element_text(size=40, face = "bold", color = "black"))
myplot

#creating a placeholder for results
input = c(mltags$noResponseCount/mltags$totalCount)
input = c(input, finalset$noResponseCount/finalset$totalCount)
isML= as.factor(c(rep("Quantitative Sample \n(ML tags)",dim(mltags)[1]),rep("Baseline Sample \n(5K tags)",dim(finalset)[1])))
mydata =  data.frame(input=input
                     , group = isML)
#plotting 2
myplot= ggplot(mydata, aes(x = input*100, y=..scaled..,fill = group)) + 
  geom_density(alpha = 0.5)+
  ggtitle("")+
  labs(x="% of questions with no responses", y ="Density") + theme_bw() + scale_fill_brewer(palette="OrRd")+
  theme(legend.title=element_blank(), panel.grid.major = element_blank())+ theme(legend.position="top") +
  #theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=40,face = "bold"),axis.text.x = element_text(size=40, face = "bold", color = "black"), axis.text.y = element_text(size=40, face = "bold", color = "black")) +
  theme(legend.spacing.x = unit(0.5,"cm"), legend.text = element_text(size=40, face = "bold", color = "black"))
myplot





#########################
#Creating Figure 3
#########################
#loadings libs
library(ggplot2)
#reading files
webdataset = read.csv("./quantitative_sample/web_sample_answers.csv", stringsAsFactors = F)
mldataset = read.csv("./quantitative_sample/ml_quan_sample_answers.csv", stringsAsFactors = F)
#placeholder for results
temp = data.frame(domain=c(rep("Baseline Sample \n (Web Development)",dim(webdataset)[1]),rep("Quantitative Sample \n (Machine Learning)",dim(mldataset)[1])), 
                  ResponseTime = c(as.numeric(difftime(webdataset$A.CreationDate,webdataset$Q.CreationDate, units="mins")), as.numeric(difftime(mldataset$A.CreationDate,mldataset$Q.CreationDate, units="mins"))))
#plotting
myplot= ggplot(temp, aes(x=domain, y=ResponseTime, fill=domain)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=median, geom="point", shape=20, size=8) + scale_fill_brewer(palette="OrRd")+
  coord_cartesian(ylim=boxplot.stats(temp$ResponseTime)$stats[c(1, 5)])+
  theme_bw()   + 
  theme( text = element_text(size=35, face="bold"), axis.text.x = element_text(size=30, face = "bold", color = "black"), axis.text.y = element_text(size=30, face = "bold", color = "black"))+
  theme(legend.title = element_blank(),legend.position = "none") +
  xlab("") + ylab("Response Time (Minutes)") 
myplot




#########################
#Creating Figure 4
#########################
#loadings libs
library(ggplot2)
#reading files
mldomain = read.csv("./custom/ml_users_ExpertiseRank.csv")
webdevdomain= read.csv("./custom/web_users_ExpertiseRank.csv")
#taking the log of the expertise score
mldomain$logpr = log(mldomain$pr_score)
webdevdomain$logpr = log(webdevdomain$pr_score)
#placeholder for results
temp = data.frame(Domain=c(rep("Machine Learning",dim(mldomain)[1]), rep("Web Development",dim(webdevdomain)[1])), pr=c(mldomain$pr_score,webdevdomain$pr_score), prlog=c(mldomain$logpr, webdevdomain$logpr))
#plotting
myplot = ggplot(temp, aes(x=prlog,y=..scaled.., fill=Domain)) + 
  labs(title="", y="Density",x="Log(ExpertiseRank)") +
  theme_bw() + theme(legend.title=element_blank()) +  theme(legend.position="top") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=25, face="bold", color="black"), axis.text.x = element_text(size=25, face = "bold", color = "black"), axis.text.y = element_text(size=25, face = "bold", color = "black"))+
  #geom_density() + scale_fill_manual(values = c(rgb(0.498,0.875,0.882),rgb(0.988,0.729,0.714))) +
  geom_density() + scale_fill_brewer(palette="OrRd")+
  theme(legend.spacing.x = unit(0.5,"cm"), legend.text =  element_text(size=30, face = "bold", color = "black"))
myplot





#########################
#Creating Figure 5
#########################
#loadings libs
library(ggplot2)
library(irr)
#reading files
#Kappa ~= 0.92
mlusers = read.csv("./qualitative_sample/ml_qual_user_expertise.csv")
webdevusers= read.csv("./qualitative_sample/web_sample_user_expertise.csv")

#placeholder for results
temp = data.frame(UserExpertise = c(as.character(mlusers$label), as.character(webdevusers$label)), Domain=c(rep("Machine Learning", 50),rep("Web Development", 50)))
temp2 = table(temp)/50
temp3 = data.frame(UserExpertise=c(temp2[1],temp2[2],temp2[3],temp2[4],temp2[5],temp2[6]),
                   Domain = c(rep("Machine Learning", 3), rep("Web Development", 3)),
                   Type = c("Expert", "Intermediate", "Novice",
                            "Expert", "Intermediate", "Novice"))

#plotting
myplot = ggplot(temp3, aes(x=Type , y=UserExpertise*100, fill=Domain)) + 
  geom_bar(stat = "identity", position = "dodge", color="black") +
  #scale_fill_manual(values = c(rgb(0.498,0.875,0.882),rgb(0.988,0.729,0.714)))  + 
  scale_fill_brewer(palette="OrRd")+
  labs(title="", x="",y="% of users") + 
  theme_bw() + theme(legend.title=element_blank())+ theme(legend.position="top") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=30,face = "bold"),axis.text.x = element_text(size=30, face = "bold", color = "black"), axis.text.y = element_text(size=30, face = "bold", color = "black")) + 
  theme(legend.spacing.x = unit(0.5,"cm"), legend.text = element_text(size=30, face = "bold", color = "black"))
myplot

#calculating agreement
kappa2(mlusers[,c("label","label2")])
kappa2(webdevusers[,c("label","label2")])
(as.numeric(kappa2(mlusers[,c("label","label2")])[5]) + as.numeric(kappa2(webdevusers[,c("label","label2")])[5]))/2



#########################
#Creating Figure 6
#########################
#loadings libs
library(ggplot2)
library(irr)

#reading files
ml_qual = read.csv("./qualitative_sample/ml_qual_sample.csv", stringsAsFactors = F)

#measuring agreement
#kappa ~= 0.8
kappa2(ml_qual[which(ml_qual$label_question_phase_2nd!="deleted"),c("label_question_phase","label_question_phase_2nd")])

#placeholder for results
temp = data.frame(Phase=c("PD", "DP", "MF","MT", "ME", "MD", "others"),naa = c(length(which(ml_qual$label_question_phase_2nd=="PD" & ml_qual$QuestionStatus=="no-accepted-answer")),
                                                                               length(which(ml_qual$label_question_phase_2nd=="DP" & ml_qual$QuestionStatus=="no-accepted-answer")),
                                                                               length(which(ml_qual$label_question_phase_2nd=="MF" & ml_qual$QuestionStatus=="no-accepted-answer")),
                                                                               length(which(ml_qual$label_question_phase_2nd=="MT" & ml_qual$QuestionStatus=="no-accepted-answer")),
                                                                               length(which(ml_qual$label_question_phase_2nd=="ME" & ml_qual$QuestionStatus=="no-accepted-answer")),
                                                                               length(which(ml_qual$label_question_phase_2nd=="MD" & ml_qual$QuestionStatus=="no-accepted-answer")),
                                                                               length(which(ml_qual$label_question_phase_2nd=="others" & ml_qual$QuestionStatus=="no-accepted-answer"))))
temp$Phase = factor(temp$Phase, levels=c("PD", "DP", "MF","MT", "ME", "MD", "others"))

#plotting
myplot = ggplot(temp, aes(x=Phase, y = (naa/length(which(ml_qual$QuestionStatus=="no-accepted-answer")))*100)) + 
  #geom_bar(color="black", fill=rgb(0.498,0.875,0.882), stat="identity") +
  geom_bar(color="black", fill=rgb(0.996,0.847,0.737), stat="identity") +
  labs(title="", x="",y="% no accepted answer") +
  theme_bw() + theme(legend.title=element_blank())+ theme(legend.position="top") +
  theme(panel.grid.major = element_blank(), text = element_text(size=30, face = "bold", color = "black"), axis.text.x =element_text(size=35, face = "bold", color = "black"), axis.text.y = element_text(size=40, face = "bold", color = "black"))
myplot


#########################
#Creating Figure 7
#########################


#########################
#Creating Figure 8
#########################