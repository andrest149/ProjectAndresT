# ProjectAndresT
This is a repository with a project proposed by Getting and Cleaning Data Course 
#Searching the main path where data are saved
setwd("C:/Users/Tinoco/Google Drive/2. Research (Current)/(2015) Especialization in Data Science/3. Getting and Cleaning Data/Programming Project/UCI HAR Dataset")
frs<-read.table("features.txt")
AcLabel<-read.table("activity_labels.txt")

# Extracting Test Data

path1=paste0(getwd(),"/test")
setwd(path1)
Stest<-read.table("subject_test.txt")
X1<-read.table("X_test.txt")
Y1=read.table("Y_test.txt")
p=paste0(getwd(),"/Inertial Signals")

# Changing Path
setwd(p)
#######################################
AccBxTe<-read.table("body_acc_x_test.txt")
AccByTe<-read.table("body_acc_y_test.txt")
AccBzTe<-read.table("body_acc_z_test.txt")

AccTxTe<-read.table("total_acc_x_test.txt")
AccTyTe<-read.table("total_acc_y_test.txt")
AccTzTe<-read.table("total_acc_z_test.txt")

BgxTe<-read.table("body_gyro_x_test.txt")
BgyTe<-read.table("body_gyro_y_test.txt")
BgzTe<-read.table("body_gyro_z_test.txt")

###########################


###########################
setwd("C:/Users/Tinoco/Google Drive/2. Research (Current)/(2015) Especialization in Data Science/3. Getting and Cleaning Data/Programming Project/UCI HAR Dataset")

# Extracting Train Data

path2=paste0(getwd(),"/train")
setwd(path2)
Strain<-read.table("subject_train.txt")
X2<-read.table("X_train.txt")
Y2=read.table("Y_train.txt")
p=paste0(getwd(),"/Inertial Signals")

## Changing Path
setwd(p)
###################

#######################################
AccBxTr<-read.table("body_acc_x_train.txt")
AccByTr<-read.table("body_acc_y_train.txt")
AccBzTr<-read.table("body_acc_z_train.txt")

AccTxTr<-read.table("total_acc_x_train.txt")
AccTyTr<-read.table("total_acc_y_train.txt")
AccTzTr<-read.table("total_acc_z_train.txt")

BgxTr<-read.table("body_gyro_x_train.txt")
BgyTr<-read.table("body_gyro_y_train.txt")
BgzTr<-read.table("body_gyro_z_train.txt")

##################################################
Ht=as.character(frs[,2])
colnames(X1)<-Ht
colnames(X2)<-Ht
colnames(Y1)<-"Activity"
colnames(Y2)<-"Activity"
colnames(Stest)<-"S_test"
colnames(Strain)<-"S_train"
Xte<-colnames(AccBxTe, do.NULL = FALSE, prefix = "MsTest")
Xtr<-colnames(AccBxTr, do.NULL = FALSE, prefix = "MsTrain")

######################## Names of Columns for Each Measurement
colnames(AccBxTe)<-Xte
colnames(AccByTe)<-Xte
colnames(AccBzTe)<-Xte
colnames(AccTxTe)<-Xte
colnames(AccTyTe)<-Xte
colnames(AccTzTe)<-Xte
colnames(BgxTe)<-Xte
colnames(BgyTe)<-Xte
colnames(BgzTe)<-Xte
##############################

colnames(AccBxTr)<-Xtr
colnames(AccByTr)<-Xtr
colnames(AccBzTr)<-Xtr
colnames(AccTxTr)<-Xtr
colnames(AccTyTr)<-Xtr
colnames(AccTzTr)<-Xtr
colnames(BgxTr)<-Xtr
colnames(BgyTr)<-Xtr
colnames(BgzTr)<-Xtr
 

###################################################
# Creating data frames
#################################################
# test data
T1=data.frame(Y1,X1)
DimT1=length(T1[,1])
# train data
T2=data.frame(Y2,X2)
DimT2=length(T2[,1])
## Completing the Dataset
T1[(DimT1+1):DimT2,]<-NA
##Merge data T1 and T2
#T1[T1[,1]==1,1]<-"WALKING"

# Activity 1
T3<-merge(T1,T2,by=intersect(names(T1),names(T2)),all=TRUE)

## Means and Standar deviation 

Mean2<-apply(T3[1:DimT2,2:562],2,mean)
Mean1<-apply(T3[(DimT2+1):(DimT2+DimT1),2:562],2,sd)
Sd2<-apply(T3[1:DimT2,2:562],2,mean)
Sd1<-apply(T3[(DimT2+1):(DimT2+DimT1),2:562],2,sd)

#Activity 4
Ac2 <- matrix(0, ncol=6, nrow=561 )
Ac2<-data.frame(Ac2)
Ac1 <- matrix(0, ncol=6, nrow=561 )
Ac1<-data.frame(Ac1)
Ac22 <- matrix(0, ncol=max(Strain), nrow=561 )
Ac22<-data.frame(Ac22)
Ac11 <- matrix(0, ncol=max(Stest), nrow=561 )
Ac11<-data.frame(Ac11)
pos2=unique(Strain)
pos1=unique(Stest)
##
SAc2 <- matrix(0, ncol=6, nrow=561 )
SAc2<-data.frame(SAc2)
SAc1 <- matrix(0, ncol=6, nrow=561 )
SAc1<-data.frame(SAc1)
SAc22 <- matrix(0, ncol=max(Strain), nrow=561 )
SAc22<-data.frame(SAc22)
SAc11 <- matrix(0, ncol=max(Stest), nrow=561 )
SAc11<-data.frame(SAc11)


# Calculating mean and standard deviation of Activities and Subjects

for(i in 1:561){
  
Ac2[i,1:5]<-tapply(T3[1:DimT2,(i+1)],T3[1:DimT2,1],mean)
Ac1[i,1:6]<-tapply(T3[(DimT2+1):(DimT2+DimT1),(i+1)],T3[(DimT2+1):(DimT2+DimT1),1],mean)
Ac22[i,pos2[,1]]<-tapply(T3[1:DimT2,(i+1)],Strain,mean)
Ac11[i,pos1[,1]]<-tapply(T3[(DimT2+1):(DimT2+DimT1),(i+1)],Stest,mean)

SAc2[i,1:5]<-tapply(T3[1:DimT2,(i+1)],T3[1:DimT2,1],sd)
SAc1[i,1:6]<-tapply(T3[(DimT2+1):(DimT2+DimT1),(i+1)],T3[(DimT2+1):(DimT2+DimT1),1],sd)
SAc22[i,pos2[,1]]<-tapply(T3[1:DimT2,(i+1)],Strain,sd)
SAc11[i,pos1[,1]]<-tapply(T3[(DimT2+1):(DimT2+DimT1),(i+1)],Stest,sd)
}

####
tAc2=t(Ac2)
tAc1=t(Ac1)
tAc22=t(Ac22)
tAc11=t(Ac11)
##############################
StAc2=t(SAc2)
StAc1=t(SAc1)
StAc22=t(SAc22)
StAc11=t(SAc11)
##############################

colnames(tAc2)<-frs[,2]
rownames(tAc2)<-colnames(matrix(0,nrow=1,ncol=6), do.NULL = FALSE, prefix="Mean_Activity_Train")
colnames(tAc1)<-frs[,2]
rownames(tAc1)<-colnames(matrix(0,nrow=1,ncol=6), do.NULL = FALSE, prefix="Mean_Activity_Test")
colnames(tAc22)<-frs[,2]
rownames(tAc22)<- colnames(matrix(0,nrow=1,ncol=30), do.NULL = FALSE, prefix="Mean_Subject_Train")
colnames(tAc11)<-frs[,2]
rownames(tAc11)<-colnames(matrix(0,nrow=1,ncol=24), do.NULL = FALSE, prefix = "Mean_Subject_Test")

colnames(StAc2)<-frs[,2]
rownames(StAc2)<-colnames(matrix(0,nrow=1,ncol=6), do.NULL = FALSE, prefix="Sd_Activity_Train")
colnames(StAc1)<-frs[,2]
rownames(StAc1)<-colnames(matrix(0,nrow=1,ncol=6), do.NULL = FALSE, prefix="Sd_Activity_Test")
colnames(StAc22)<-frs[,2]
rownames(StAc22)<- colnames(matrix(0,nrow=1,ncol=30), do.NULL = FALSE, prefix="Sd_Subject_Train")
colnames(StAc11)<-frs[,2]
rownames(StAc11)<-colnames(matrix(0,nrow=1,ncol=24), do.NULL = FALSE, prefix = "Sd_Subject_Test")

####################################

T4<-data.frame(t(tAc1),t(tAc2),t(tAc11),t(tAc22),t(StAc1),t(StAc2),t(StAc11),t(StAc22))
rownames(T4)<-frs[,2]
##Saving Data in text File
setwd("C:/Users/Tinoco/Google Drive/2. Research (Current)/(2015) Especialization in Data Science/3. Getting and Cleaning Data/Programming Project")
write.table(T4,"DataMeanSd.txt")
