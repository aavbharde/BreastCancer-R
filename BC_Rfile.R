breast_cancer <- read.csv("C:/Users/aakan/Desktop/Stevens/Semester3/EM622/Assignments/Breast_Cancer/bc1.csv",header = FALSE)
head(breast_cancer)
# Renaming columns
colnames(breast_cancer)=c("class","age","menopause","tumor.size","inv-nodes","node-caps","deg.malig","breast","breast.quad","irradiat")
str(breast_cancer)
View(breast_cancer)
summary(breast_cancer)

# Replacing question marks with NA

breast_cancer[breast_cancer == '?'] <- "NA"
# For the column node-caps
# Below is the data frame which has NA value, which I decided to replace with 'no'
breast_cancer[breast_cancer$age == "40-49" & breast_cancer$menopause == "premeno" & breast_cancer$'tumor.size'=="25-29" & breast_cancer$`inv-nodes`=="0-2",]

# Using below formula makes change in the whole breast_cancer
# df[is.na(df)] <- "no" 

breast_cancer[breast_cancer$age == "40-49" & breast_cancer$menopause == "premeno" & breast_cancer$'tumor.size'=="25-29" & breast_cancer$`inv-nodes`=="0-2",][is.na(breast_cancer[breast_cancer$age == "40-49" & breast_cancer$menopause == "premeno" & breast_cancer$'tumor.size'=="25-29" & breast_cancer$`inv-nodes`=="0-2",])] <- "no"

# For the above filter similar entries have node-caps as 'no', so I predict ? to be 'no' 
#______________________________________________________________________________________________________
# for the column breast-quad
breast_cancer[is.na(breast_cancer[breast_cancer$`breast-quad`,])]

breast_cancer[breast_cancer$class == "recurrence-events" & breast_cancer$age == "50-59" & breast_cancer$menopause == "ge40"& breast_cancer$`tumor.size`=="30-34",]
# no any way to find the replacement for NA, so I keep it as NA
#___________________________________________________________________________
library(ggplot2)
bc <- ggplot(data=breast_cancer,aes(x=menopause,y=deg.malig))
bc + geom_violin(aes(fill=age)) + stat_summary(fun.y = median,geom="point",fill="black",size=1,shape=8) +
xlab("Type of Menopause") + ylab("Degree of Malignancy") +
ggtitle("Degree of Malignancy for Breast Cancer with Age and Menopause Type") +
labs(caption = "Data Source: http://archive.ics.uci.edu/ml/datasets/Breast+Cancer") +
guides(fill=guide_legend(title='Age Range')) +
# theme is used for changing the appearance of titles
theme(axis.title.x = element_text(size=14),
axis.title.y = element_text(size=14),
axis.text.x = element_text(size=12, color= 'black'),
axis.text.y = element_text(size=12,color= 'black'),
plot.title = element_text(size=15, family="Courier",hjust = 0.5,face = "bold"),
legend.title = element_text(size = 14),plot.caption = element_text(size = 10,hjust = -0.2)) 


d1 <- breast_cancer[breast_cancer$age == "50-59",]
d1
d<-na.omit(d1)
qlu <- d[d$breast.quad == "left_up",]
qlu
qru <- d[d$breast.quad == "right_up",]
qll <-d[d$breast.quad == "left_low",]
qrl <- d[d$breast.quad == "right_low",]


a<- ggplot(data=qlu,aes(x=breast.quad,y=deg.malig)) + geom_violin(aes(fill=menopause))+ xlab("")+
ylab("Degree of Malignancy") + theme(axis.text.x = element_text(debug = "",size = 15,color = "black"),axis.title.y = element_text(size = 15)) +
scale_x_discrete(labels="Breast Quad - Left Up") 

b<-ggplot(data=qru,aes(x=breast.quad,y=deg.malig)) + geom_violin(aes(fill=menopause)) +
xlab("") + ylab("Degree of Malignancy") + theme(axis.text.x = element_text(size = 15,color = "black"),axis.title.y = element_text(size = 15)) +
  scale_x_discrete(labels="Breast Quad - Right Up") 

c<-ggplot(data=qll,aes(x=breast.quad,y=deg.malig)) + geom_violin(aes(fill=menopause)) +
xlab("") + ylab("Degree of Malignancy") + theme(axis.text.x = element_text(size = 15,color = "black"),axis.title.y = element_text(size = 15)) +
  scale_x_discrete(labels="Breast Quad - Left Down")

e<-ggplot(data=qrl,aes(x=breast.quad,y=deg.malig)) + geom_violin(aes(fill=menopause)) +
xlab("") + ylab("Degree of Malignancy") + theme(axis.text.x = element_text(size = 15,color = "black"),axis.title.y = element_text(size = 15)) +
scale_x_discrete(labels="Breast Quad - Right Down") 


library(ggpubr)
figure <- ggarrange(a, b, c, e, ncol=2, nrow=2, common.legend = TRUE, legend="top")
annotate_figure(figure,top=text_grob("Malignancy of Breast Cancer for the age 50-59",size=20,face="bold"),bottom=text_grob("Data Source: http://archive.ics.uci.edu/ml/datasets/Breast+Cancer",hjust = 0.5,vjust = 0))
