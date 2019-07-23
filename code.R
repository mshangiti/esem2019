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
webdataset = read.csv("./quantitative_sample/web_sample.csv", stringsAsFactors = F)
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
#Creating Figure 7 and Figure 8
#########################
#The topics were genereated in summary using the following approach:
#1. We used gensim to preprocess the ML quantitative_sample (title and body) while keeping numbers and removing HTML into a bag of word representation (including bigrams). 
#2. We then applied LDA using gensim lib as well on the corpus.
# parameters to know: number of topics (k) = 30, min_df=150, max_df=0.8 

##reading files
ldamat=read.csv("./custom/ml_lda_gensim_mat.txt",header=F)
ldaterms=read.csv("./custom/ml_lda_gensim_term.txt",header=F)
resultset = rbind(read.csv2("./quantitative_sample/ml_quan_sample_1.csv", stringsAsFactors=FALSE), 
                  read.csv2("./quantitative_sample/ml_quan_sample_2.csv", stringsAsFactors=FALSE),
                  read.csv2("./quantitative_sample/ml_quan_sample_3.csv", stringsAsFactors=FALSE),
                  read.csv2("./quantitative_sample/ml_quan_sample_4.csv", stringsAsFactors=FALSE))
#creating a year column
resultset[,"year"] = as.numeric(format(as.POSIXlt(resultset[,"CreationDate"]),"%Y"))


##preprocessing required for plotting

#finding dominant topic
resultset$dominatingTopic = "NA"
for(i in 1:dim(ldamat)[1]){
  print(i)
  #maxtopic = c(maxtopic,as.numeric(sort(lda[i,],decreasing = T)[1]))
  #maxtopic2 = c(maxtopic2,as.numeric(sort(lda[i,],decreasing = T)[2]))
  resultset[i,"dominatingTopic"] = names(sort(ldamat[i,],decreasing = T)[1])
  resultset[i,"dominatingProb"] = as.numeric(sort(ldamat[i,],decreasing = T)[1])
}


## Cleaning up topics

# 1. The following five noise topics (captures commonly used language in the corpus, i.e., ML related stop words)
#Solution: each question that falls under one of those topics will be assigned the 2nd most dominating topic
topicId = c("V8","V11","V19","V10","V30")
for(i in 1:length(topicId)){
  print(paste("Working on topic", i))
  #generating list of question posts that fall under current topic
  questionlist = which(resultset$dominatingTopic==topicId[i])
  #looping over question posts
  for (j in 1:length(questionlist)){
    #currrentquestion
    myid = questionlist[j]
    #finding the new dominating topic, starting with the 2nd topic with the highest prob.
    #if it happens that 2nd topic is in our list of topicId, then more to the 3rd, etc.
    newtopicid=2
    while(names(sort(ldamat[myid,],decreasing = T)[newtopicid]) %in% topicId){
      newtopicid = newtopicid + 1
    }
    resultset[myid,"dominatingTopic"] = names(sort(ldamat[myid,],decreasing = T)[newtopicid])
    resultset[myid,"dominatingProb"] = as.numeric(sort(ldamat[myid,],decreasing = T)[newtopicid])
  }
}

# 1. The following topic will be merged with another (they both represent data preparation)
resultset[which(resultset$dominatingTopic=="V15"),"dominatingTopic"] = "V14"


#creating placeholder for topics
#create an ordered list from existing topics and then create a placeholder that represents the topics
require('gtools')
topiclist = mixedsort(unique(resultset$dominatingTopic))
k = length(topiclist)
topics = data.frame(topicId=as.factor(topiclist),topicName=(rep(0,k)),topicWords=(rep(0,k)),topicTags=(rep(0,k)), postCount=(rep(0,k)),viewsCount=(rep(0,k)))
#filling placeholder with data
#assigning topic names, words, and tags
topics[which(topics$topicId=="V1"),c("topicName", "topicWords", "topicTags")] = c("Neural_Networks", "weight,layer,neural_network","neural-network,multi-layer,backpropagation")#excellent
topics[which(topics$topicId=="V2"),c("topicName", "topicWords", "topicTags")] = c("Applied_NN", "input,output,tensor,batch","tensorflow,neural-network,rnn")#fair
topics[which(topics$topicId=="V3"),c("topicName", "topicWords", "topicTags")] = c("Decision_Tree", "tree,leaf,node","parsing,IR,decision-tree,classification")#fair
topics[which(topics$topicId=="V4"),c("topicName", "topicWords", "topicTags")] = c("Visualization", "line,plot,point","tensorboard,ggplot2,boxplot,matlab")#fair
topics[which(topics$topicId=="V5"),c("topicName", "topicWords", "topicTags")] = c("Optimization", "gradient,cost,theta","gradient-descent,cost-based-optimizer,neural-network")
topics[which(topics$topicId=="V6"),c("topicName", "topicWords", "topicTags")] = c("TF_Installation", "tensorflow,error,install","tensorflow,pip,installation")
topics[which(topics$topicId=="V7"),c("topicName", "topicWords", "topicTags")] = c("Environment_Setup", "import,recent_call,return_self","scikit-learn,anaconda")
#topic 8 is out
topics[which(topics$topicId=="V9"),c("topicName", "topicWords", "topicTags")] = c("TF_graph", "tf,graph,graph_def","tensorflow,python") #low dominance
#topic 10 is out
#topic 11 is out
topics[which(topics$topicId=="V12"),c("topicName", "topicWords", "topicTags")] = c("Convergence_Determination", "train_accuraci,loss,learn_rate","softmax,covergence,nn")
topics[which(topics$topicId=="V13"),c("topicName", "topicWords", "topicTags")] = c("Deployment", "user,product,api","google-prediction,azure,api,web-services,firebase")
topics[which(topics$topicId=="V14"),c("topicName", "topicWords", "topicTags")] = c("Data_Prep", "nan,label,format","data-management,aggregation,input")
#topic 15 is out
topics[which(topics$topicId=="V16"),c("topicName", "topicWords", "topicTags")] = c("TF_debugging", "tf,sess_run,tf_variabl","tensorflow,iterable,tensor")
topics[which(topics$topicId=="V17"),c("topicName", "topicWords", "topicTags")] = c("TF_prog_err", "py_line,packag_tensorflow,run","tensorflow,django,ubuntu,low-memory,seq-to-seq")
topics[which(topics$topicId=="V18"),c("topicName", "topicWords", "topicTags")] = c("Model_Saving", "model,load,save","tensorflow-serving,google-cloud-functions")
# topic 19 is out
topics[which(topics$topicId=="V20"),c("topicName", "topicWords", "topicTags")] = c("Evaluation_Classification", "class,accuraci,confus_matrix","classification,precision-recall,analytics,predictive")
topics[which(topics$topicId=="V21"),c("topicName", "topicWords", "topicTags")] = c("Object_Detection", "object_detect,face,bound_box","image-segmentation,object-detection,opencv,image-processing,motion-detection,tracking,feature-extraction")
topics[which(topics$topicId=="V22"),c("topicName", "topicWords", "topicTags")] = c("Feature_Preprocessing", "featur,transform,split","pandas,feature-extraction,feature-selection")
topics[which(topics$topicId=="V23"),c("topicName", "topicWords", "topicTags")] = c("Model_Training", "train,sampl,fit","cross-validation,validation,gradient-descent")
topics[which(topics$topicId=="V24"),c("topicName", "topicWords", "topicTags")] = c("keras", "kera,model,input_shape","keras,conv-neural-network,deep-learning,neural-networks")
topics[which(topics$topicId=="V25"),c("topicName", "topicWords", "topicTags")] = c("Clustering", "cluster,distanc,,centroid","hierarchical-clustering,cluster-analysis,kmeans")
topics[which(topics$topicId=="V26"),c("topicName", "topicWords", "topicTags")] = c("TF_hardware", "tensorflow,gpu,cpu","multithreading,gpu,gensorflow-gpu")
topics[which(topics$topicId=="V27"),c("topicName", "topicWords", "topicTags")] = c("Evaluation_Regression", "regress,error,auc","lasso,linear-regression,glmnet,interpretation")
topics[which(topics$topicId=="V28"),c("topicName", "topicWords", "topicTags")] = c("NLP", "text,tweet,,senti","nlp,classification,stanford-nlp,opennlp")
topics[which(topics$topicId=="V29"),c("topicName", "topicWords", "topicTags")] = c("Preprocessing_images", "img,mask,convert","pickle,cnn,feature-extration,unit,image-processing")
#topic 30 is out

#assigning overall topic label 
topics[which(topics$topicId=="V4" | topics$topicId=="V6" | topics$topicId=="V7" | topics$topicId=="V9"| 
               topics$topicId=="V13"|topics$topicId=="V14"|topics$topicId=="V15"|topics$topicId=="V16"| topics$topicId=="V2" |
               topics$topicId=="V17" | topics$topicId=="V18" | topics$topicId=="V22" | topics$topicId=="V26" | topics$topicId=="V29" | topics$topicId=="V24"),"topicClass"]  = "ML Library"
topics[which(topics$topicId=="V5" | topics$topicId=="V12" | topics$topicId=="V20" | topics$topicId=="V21" | topics$topicId=="V23"| topics$topicId=="V27"|topics$topicId=="V28"),"topicClass"]  = "ML Concept"
topics[which(topics$topicId=="V1" | topics$topicId=="V3" | topics$topicId=="V25"),"topicClass"]  = "ML Algorithm"

#generating topic stat
for(i in 1:length(topiclist)){
  topics[which(topics$topicId==topiclist[i]),"postCount"] = length(which(resultset$dominatingTopic==topiclist[i]))
  topics[which(topics$topicId==topiclist[i]),"viewsCount"] = sum(resultset[which(resultset$dominatingTopic==topiclist[i]),c("ViewCount")])/sum(resultset[,c("ViewCount")])
  topics[which(topics$topicId==topiclist[i]),"unanswered_topic"] = length(resultset[which(resultset$dominatingTopic==topiclist[i] & resultset$AcceptedAnswerId=="None"),c("AcceptedAnswerId")]) /length(resultset[which(resultset$dominatingTopic==topiclist[i]),c("AcceptedAnswerId")])
}

#creating a columns for plotting purposes
topics[,"unanswered_topic_factor"] = "Low % Unanswered Questions (< Median)"
topics[which(topics$unanswered_topic>median(topics$unanswered_topic)),"unanswered_topic_factor"] = "High % Unanswered Questions (> Median)"
topics = topics[order(topics$unanswered_topic),]
topics[,"plotorder"] = 1:k
topics[which(topics$topicName=="NLP"),"plotorder"] = 1
topics[which(topics$topicName=="TF_debugging"),"plotorder"] = 2
topics[which(topics$topicName=="Evaluation_Classification"),"plotorder"] = 3
topics[which(topics$topicName=="Convergence_Determination"),"plotorder"] = 4
topics[which(topics$topicName=="Visualization"),"plotorder"] = 5
topics[which(topics$topicName=="Decision_Tree"),"plotorder"] = 6
topics[which(topics$topicName=="Optimization"),"plotorder"] = 8
topics[which(topics$topicName=="keras"),"plotorder"] = 7
topics[which(topics$topicName=="Clustering"),"plotorder"] = 10
topics[which(topics$topicName=="Neural_Networks"),"plotorder"] = 11
topics[which(topics$topicName=="Data_Prep"),"plotorder"] = 12
topics[which(topics$topicName=="Evaluation_Regression"),"plotorder"] = 13
topics[which(topics$topicName=="Feature_Preprocessing"),"plotorder"] = 14
topics[which(topics$topicName=="Environment_Setup"),"plotorder"] = 19
topics[which(topics$topicName=="Data_Prep2"),"plotorder"] = 20
topics[which(topics$topicName=="Preprocessing_images"),"plotorder"] = 21
topics[which(topics$topicName=="Object_Detection"),"plotorder"] = 22
topics[which(topics$topicName=="Applied_NN"),"plotorder"] = 23
topics[which(topics$topicName=="Model_Training"),"plotorder"] = 24
topics[which(topics$topicName=="TF_hardware"),"plotorder"] = 25
topics[which(topics$topicName=="TF_Installation"),"plotorder"] = 26
topics[which(topics$topicName=="TF_graph"),"plotorder"] = 30
topics[which(topics$topicName=="TF_prog_err"),"plotorder"] = 29
topics[which(topics$topicName=="Model_Saving"),"plotorder"] = 28
topics[which(topics$topicName=="Deployment"),"plotorder"] = 27



## PLOTTING Figure 7
myplot = ggplot(data=topics, aes(x= reorder(topicWords,plotorder)   , y = postCount, fill=topicClass))+
  geom_bar(stat="identity",colour = "black")+scale_fill_brewer(palette="Oranges")+
  geom_label(aes(label =topicName,fontface=2),position = position_stack(vjust = 1),size =5.5,fill = 'white', colour = 'black')+
  labs(title="", x="Three-word Topics",y="#Questions") +
  theme_bw() +theme(panel.grid.major = element_blank())+ 
  theme(legend.title=element_blank())+ theme(legend.position="top") + theme(legend.key.width=unit(1.5,"cm")) +
  theme(legend.spacing.x = unit(0.5,"cm"), legend.text = element_text(size=20, face = "bold", color = "black") ) +
  theme(text = element_text(size=30), axis.text.x = element_text(size=20,face = "bold"), axis.text.y = element_text(size=35))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 
myplot


## PLOTTING Figure 8
library(ggplot2)
library(ggExtra)
p=ggplot(topics, aes(x=unanswered_topic, y=viewsCount, color=unanswered_topic_factor, label=topicName)) +
  geom_point() +    geom_label(aes(label =topicName,fontface=2),position = position_stack(vjust = 1),size =8,fill = 'white', colour = 'black') + xlim(0.52,0.68) + labs(title="", x="Difficulty (% no accepted answer)",y="Popularity (% views)") +
  geom_hline(aes(yintercept = 0.050)) + geom_vline(aes(xintercept = 0.60)) + 
  theme(panel.background = element_blank())+
  theme(legend.position="none") +
  theme(text = element_text(size=30,face = "bold"), axis.text.x = element_text(size=30,face = "bold"), axis.text.y = element_text(size=30,face = "bold"))
p







#########################
#Others - Coherence and log_prex of the topics
#########################
#the following coherence and log_prex were calculated as follows
#1. We used gensim to preprocess the ML quantitative_sample (title and body) while keeping numbers and removing HTML into a bag of word representation (including bigrams). 
#2. We then applied LDA using gensim lib as well on the corpus.
# -- used gensim CoherenceModel() to get coherence
# -- used gensim log_perplexity to get preplx 

#results
coherence = c(0.55557681617464572,
              0.55840985073853777,
              0.56874894113621732,
              0.55972013170397117,
              0.53851956745620599,
              0.4965928502938467,
              0.48059205000443794)
k = c(10, 20, 30, 40, 50, 80, 100)
log_prex = c(-6.7311955042887579,
             -6.6776722214222417,
             -6.6569918329032696,
             -6.6870875786131343,
             -6.6975109524108394,
             -6.771210775346705,
             -6.8733234425788874)

#plotting coherence
temp = data.frame(K=k, Coherence = coherence)
qplot(as.factor(temp$K), temp$Coherence)
myplot = ggplot(data=temp, aes(x=K, y = Coherence))+
  #geom_bar(fill = "#4589c6", color="black")+
  geom_line(col=rgb(0.1,0.7,0.1,0.8), lwd=2 ) + geom_point( pch=20 , cex=8) +
  geom_point(data=temp[3,], aes(x=K, y=Coherence), colour="#008ECC", size=10) +
  labs(title="", x="Number of topics (K)",y="Cv Topic Coherence") +
  theme_bw() + theme(legend.title=element_blank())+ theme(legend.position="top") +
  theme_bw() +theme(panel.grid.major = element_blank())+
  theme(text = element_text(size=30), axis.text.x = element_text(size=35,face = "bold"), axis.text.y = element_text(size=35))
myplot


#plotting perp
temp = data.frame(K=k, Log_Perplexity = log_prex)
qplot(as.factor(temp$K), temp$Log_Perplexity)
myplot = ggplot(data=temp, aes(x=K, y = Log_Perplexity))+
  #geom_bar(fill = "#4589c6", color="black")+
  geom_line(col=rgb(0.1,0.7,0.1,0.8), lwd=2 ) + geom_point( pch=20 , cex=8) +
  geom_point(data=temp[3,], aes(x=K, y=Log_Perplexity), colour="#008ECC", size=10) +
  labs(title="", x="Number of topics (K)",y="Log_Perplexity") +
  theme_bw() + theme(legend.title=element_blank())+ theme(legend.position="top") +
  theme_bw() +theme(panel.grid.major = element_blank())+
  theme(text = element_text(size=30), axis.text.x = element_text(size=35,face = "bold"), axis.text.y = element_text(size=35))
myplot