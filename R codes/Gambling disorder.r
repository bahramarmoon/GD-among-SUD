###Prevalence gambling Disorder###
library(readxl)
Gambling_Disorder <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx")
View(Gambling_Disorder)

###Total###
library(readxl)
library(meta)
m1<- metaprop(Event.Gambling.Disorder,n,Author,data =Gambling_Disorder ,method = "Inverse",method.ci = gs("method.ci.prop"),level = gs("level"),sm = "PRAW",fixed =FALSE)
forest(m1,xlim=c(0,1),print.I2=TRUE,print.tau2 = FALSE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",header=TRUE,sortvar =year)
baujat(m1)

metareg(m1, Country.Cat)
metareg(m1,Diagnostic.criteria)
metareg(m1,type.of.drugs)
metareg(m1,Questinner.for.GD)
metareg(m1,Treatment)
metareg(m1,Sample.Size)
metareg(m1,Age.Cat)
metareg(m1, Age.Cat+ type.of.drugs+ Sample.Size)

###Year subgroup Prevalence of Gambling disorder ### 
library(readxl)
Gambling_Disorder <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx")

library(meta)
m10<- metaprop(Event.Gambling.Disorder,n,Author,data =Gambling_Disorder,method = "Inverse",method.ci = gs("method.ci.prop"),level = gs("level"),sm = "PRAW",fixed =FALSE,subgroup =Year.Cat1)
forest(m10,xlim=c(0,1),print.I2=TRUE,print.tau2 = FALSE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",header=TRUE,sortvar =year)

###Country subgroup Gambling_Disorder ### 
library(readxl)
Gambling_Disorder <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx")

library(meta)
m11<- metaprop(Event.Gambling.Disorder,n,Author,data =Gambling_Disorder,method = "Inverse",method.ci = gs("method.ci.prop"),level = gs("level"),sm = "PRAW",fixed =FALSE,subgroup =Country.Cat)
forest(m11,xlim=c(0,1),print.I2=TRUE,print.tau2 = FALSE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",header=TRUE,sortvar =year)


###Diagnostic criteria subgroup Gambling_Disorder ### 
library(readxl)
Gambling_Disorder <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx")

library(meta)
m12<- metaprop(Event.Gambling.Disorder,n,Author,data =Gambling_Disorder,method = "Inverse",method.ci = gs("method.ci.prop"),level = gs("level"),sm = "PRAW",fixed =FALSE,subgroup =Diagnostic.criteria)
forest(m12,xlim=c(0,1),print.I2=TRUE,print.tau2 = FALSE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",header=TRUE,sortvar =year)

###Type of Drugs subgroup Gambling_Disorder ### 
library(readxl)
Gambling_Disorder <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx")

library(meta)
m13<- metaprop(Event.Gambling.Disorder,n,Author,data =Gambling_Disorder,method = "Inverse",method.ci = gs("method.ci.prop"),level = gs("level"),sm = "PRAW",fixed =FALSE,subgroup =type.of.drugs)
forest(m13,xlim=c(0,1),print.I2=TRUE,print.tau2 = FALSE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",header=TRUE,sortvar =year)


###Questioner for GD subgroup Gambling_Disorder ### 
library(readxl)
Gambling_Disorder <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx")

library(meta)
m14<- metaprop(Event.Gambling.Disorder,n,Author,data =Gambling_Disorder,method = "Inverse",method.ci = gs("method.ci.prop"),level = gs("level"),sm = "PRAW",fixed =FALSE,subgroup =Questinner.for.GD)
forest(m14,xlim=c(0,1),print.I2=TRUE,print.tau2 = FALSE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",header=TRUE,sortvar =year)


###Treatment subgroup Gambling_Disorder ### 
library(readxl)
Gambling_Disorder <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx")

library(meta)
m15<- metaprop(Event.Gambling.Disorder,n,Author,data =Gambling_Disorder,method = "Inverse",method.ci = gs("method.ci.prop"),level = gs("level"),sm = "PRAW",fixed =FALSE,subgroup =Treatment )
forest(m15,xlim=c(0,1),print.I2=TRUE,print.tau2 = FALSE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",header=TRUE,sortvar =year)


###Sample size subgroup Gambling_Disorder ### 
library(readxl)
Gambling_Disorder <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx")

library(meta)
m16<- metaprop(Event.Gambling.Disorder,n,Author,data =Gambling_Disorder,method = "Inverse",method.ci = gs("method.ci.prop"),level = gs("level"),sm = "PRAW",fixed =FALSE,subgroup =Sample.Size )
forest(m16,xlim=c(0,1),print.I2=TRUE,print.tau2 = FALSE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",header=TRUE,sortvar =year)


###Age Category subgroup Gambling_Disorder ### 
library(readxl)
Gambling_Disorder <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx")

library(meta)
m16<- metaprop(Event.Gambling.Disorder,n,Author,data =Gambling_Disorder,method = "Inverse",method.ci = gs("method.ci.prop"),level = gs("level"),sm = "PRAW",fixed =FALSE,subgroup =Age.Cat)
forest(m16,xlim=c(0,1),print.I2=TRUE,print.tau2 = FALSE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",header=TRUE,sortvar =year)







###########Odds Ratios#####################
##Article 1 Graph1##
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 1)

m1<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Graph=="1" ,subgroup =Group)
forest(m1,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)


###ORS individually###
##Being male##
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 1)

m10<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Group=="Being male")
forest(m10,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)
baujat(m10)

metabias(m10)
metabias(m10, method.bias = "Begg")
metabias(m10, method.bias = "Begg", correct = TRUE)

##Being older##
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 1)

m11<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Group=="Being older")
forest(m11,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)
baujat(m11)


###Depressive disorders###
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 1)

m12<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Group=="Depressive disorders")
forest(m12,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)
baujat(m12)


###Mood disorders###
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 1)

m13<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Group=="Mood disorders")
forest(m13,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)
baujat(m13)




###########Odds Ratios#####################
##Article 1 Graph2##
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 1)

m2<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Graph=="2" ,subgroup =Group)
forest(m2,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)

###ORS individually###
###Tobacco use disorders##
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 1)

m20<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Group=="Tobacco use disorders")
forest(m20,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)

###Alcohol use disorders###
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 1)

m21<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Group=="Alcohol use disorders")
forest(m21,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)


###Cannabis use disorders###
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 1)

m21<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Group=="Cannabis use disorders")
forest(m21,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)




###########Odds Ratios#####################
##Article 2 Graph1##
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 2)

m3<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Graph=="1" ,subgroup =Group)
forest(m3,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)


###ORS individually###
###University educational level##
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 2)

m30<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Group=="University educational level")
forest(m30,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)


###Ethnicity (African-American)##
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 2)

m31<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Group=="Ethnicity (African-American)")
forest(m31,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)

###Problem gaming##
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 2)

m32<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Group=="Problem gaming")
forest(m32,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)


###Anxiety disorder##
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 2)

m33<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Group=="Anxiety disorder")
forest(m33,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)


###Personality disorders##
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 2)

m34<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Group=="Personality disorders")
forest(m34,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)



##Article 2 Graph2###
library(readxl)
library(meta)
Data.Gambling <- read_excel("D:/Analyze/Bahram Armon/Pathological gambling/Gambling_SRD_Final.xlsx", sheet = "OR")
Gambling <- subset (Data.Gambling, Data.Gambling$Article %in% 2)

m3<-metagen(Gambling$logOR, Gambling$SelogOR,Gambling$Author, null.effect = 1,fixed=FALSE,backtransf=TRUE,data=Gambling,sm="OR",subset =Graph=="2" ,subgroup =Group)
forest(m3,xlim=c(0.1,20),print.I2=TRUE,col.random="red",comb.random=TRUE,col.inside="black",col.square="sky blue",
       col.diamond="red",print.tau2 = FALSE,print.I2.ci = FALSE,print.Te =FALSE,sortvar=year)
