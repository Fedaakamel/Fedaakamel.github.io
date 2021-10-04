library(clusterSim)
require(zoo)
require(dtw)
setwd("C:/Users/fedaaelderdesawe/google drive/")
library(data.table)
datafolder="C:/Users/fedaaelderdesawe/Dropbox/thesis/datamining/dataset_b/labeled/"

files=list.files(datafolder)
files
listforX=list()

thissubject= read.table(paste0(datafolder,files[1]),header=FALSE)
name = strsplit(files[1], "_")
thissubject$name <- files[1]
thissubject$name1 <- name[[1]][1]
thissubject$name2 <- name[[1]][2]
thissubject$name3 <- name[[1]][3]
thissubject$sub <- name[[1]][4]

for(i in 1:length(files)){
  thissubject= read.table(paste0(datafolder,files[i]),header=FALSE)
  name = strsplit(files[i], "_")
  thissubject$name <- files[i]
  thissubject$name1 <- name[[1]][1]
  thissubject$name2 <- name[[1]][2]
  thissubject$name3 <- name[[1]][3]
  thissubject$sub <- name[[1]][4]
  
  listforX[[i]]=thissubject
  
  # datab=rbind(thissubject[[i]],thissubject)
  
}
alldata=do.call(rbind,listforX)

fin=alldata[,c(-5,-6,-7)]
#mouse 

subject_mc=fin[fin[,2]=="MC",]
subject_G=fin[fin[,2]=="G",]
subject_m=fin[fin[,2]=="M",]

##################
##################
# Mouse
session_0m=subject_m[subject_m[,5]=="0",]
session_1m=subject_m[subject_m[,5]=="1",]
session_2m=subject_m[subject_m[,5]=="2",]

########################################################################################
##############first session 
#scaling 
#x coordinates
setDT(session_0m)
scalex_0=session_0m[, .( V3 =data.Normalization(V3,type="n4")), by = .(sub)]
#########
setDT(session_1m)
scalex_1=session_1m[, .( V3 =data.Normalization(V3,type="n4")), by = .(sub)]
###########
setDT(session_2m)
scalex_2=session_2m[, .( V3 =data.Normalization(V3,type="n4")), by = .(sub)]
###########################################################################################
## y coordinates  scaling 
setDT(session_0m)
scaley_0=session_0m[, .( V4 =data.Normalization(V4,type="n4")), by = .(sub)]
#########
setDT(session_1m)
scaley_1=session_1m[, .( V4 =data.Normalization(V4,type="n4")), by = .(sub)]
###########
setDT(session_2m)
scaley_2=session_2m[, .( V4 =data.Normalization(V4,type="n4")), by = .(sub)]
####################################################################################
setDT(session_0m)
scalet_0=session_0m[, .( V1 =data.Normalization(V1,type="n4")), by = .(sub)]
#########
setDT(session_1m)
scalet_1=session_1m[, .( V1 =data.Normalization(V1,type="n4")), by = .(sub)]
###########
setDT(session_2m)
scalet_2=session_2m[, .( V1 =data.Normalization(V1,type="n4")), by = .(sub)]
######################################################################################
#####################################################################
########################

#standard deviation
#############################################3
####for x coordinates 
setDT(session_0m)
standarddevmx0=session_0m[, .( V3 =sd(V3)), by = .(sub)]
setDT(session_1m)
standarddevmx1=session_1m[, .( V3 =sd(V3)), by = .(sub)]
setDT(session_2m)
standarddevmx2=session_2m[, .( V3 =sd(V3)), by = .(sub)]
#############################################################################################
#################33
#### for Y coordinates 
setDT(session_0m)
standarddevmy0=session_0m[, .( V4 =sd(V4)), by = .(sub)]
setDT(session_1m)
standarddevmy1=session_1m[, .( V4 =sd(V4)), by = .(sub)]
setDT(session_2m)
standarddevmy2=session_2m[, .( V4 =sd(V4)), by = .(sub)]
#############################################################################################
#################
#### for Time 
setDT(session_0m)
standarddevmt0=session_0m[, .( V1 =sd(V1)), by = .(sub)]
setDT(session_1m)
standarddevmt1=session_1m[, .( V1 =sd(V1)), by = .(sub)]
setDT(session_2m)
standarddevmt2=session_2m[, .( V1 =sd(V3)), by = .(sub)]
#################################################################################################
########################################################33
################################################################
##### difference 
#####
###### difference time
setDT(session_0m)
mm0=session_0m[, .( V1 =diff(V1)), by = .(sub)]

mm0$V1[mm0$V1== 0] <- mean(mm0$V1)
#####
setDT(session_1m)
mm1=session_1m[, .( V1 =diff(V1)), by = .(sub)]
mm1$V1[mm1$V1== 0] <- mean(mm1$V1)
############
setDT(session_2m)
mm2=session_2m[, .( V1 =diff(V1)), by = .(sub)]
mm2$V1[mm2$V1== 0] <- mean(mm2$V1)
#########################################################
##########diffx&y mouse
setDT(session_0m)
diffxm0=session_0m[, .( V3 =diff(V3)), by = .(sub)]
setDT(session_1m)
diffxm1=session_1m[, .( V3 =diff(V3)), by = .(sub)]
setDT(session_2m)
diffxm2=session_2m[, .( V3 =diff(V3)), by = .(sub)]
######################################################
#####Y coordinates 
setDT(session_0m)
diffym0=session_0m[, .( V4 =diff(V4)), by = .(sub)]
setDT(session_1m)
diffym1=session_1m[, .( V4 =diff(V4)), by = .(sub)]
setDT(session_2m)
diffym2=session_2m[, .( V4 =diff(V4)), by = .(sub)]
####################################################################################################################
#####################################################333
############################
#maximum 
setDT(session_0m)
maxtm0=session_0m[, .( V1 =max(V1)), by = .(sub)]
#####
setDT(session_1m)
maxtm1=session_1m[, .( V1 =max(V1)), by = .(sub)]
############
setDT(session_2m)
maxtm2=session_2m[, .( V1 =max(V1)), by = .(sub)]
#########################################################
##########diffx&y mouse
setDT(session_0m)
maxxm0=session_0m[, .( V3 =max(V3)), by = .(sub)]
setDT(session_1m)
maxxm1=session_1m[, .( V3 =max(V3)), by = .(sub)]
setDT(session_2m)
maxxm2=session_2m[, .( V3 =max(V3)), by = .(sub)]
######################################################
#####Y coordinates 
setDT(session_0m)
maxym0=session_0m[, .( V4 =max(V4)), by = .(sub)]
setDT(session_1m)
maxym1=session_1m[, .( V4 =max(V4)), by = .(sub)]
setDT(session_2m)
maxym2=session_2m[, .( V4 =max(V4)), by = .(sub)]
############################################################################################
################################################################################
########################################
#########MIN
####MINIMUM TIME
setDT(session_0m)
mintm0=session_0m[, .( V1 =min(V1)), by = .(sub)]
#####
setDT(session_1m)
mintm1=session_1m[, .( V1 =min(V1)), by = .(sub)]
############
setDT(session_2m)
mintm2=session_2m[, .( V1 =min(V1)), by = .(sub)]
#########################################################
##########
######MINIMUM X COORDINATES 
setDT(session_0m)
minxm0=session_0m[, .( V3 =min(V3)), by = .(sub)]
setDT(session_1m)
minxm1=session_1m[, .( V3 =min(V3)), by = .(sub)]
setDT(session_2m)
minxm2=session_2m[, .( V3 =min(V3)), by = .(sub)]
######################################################
#####
########################################################
##########
###### Direction angle

anglem0=diffym0$V4/diffxm0$V3
anglem1=diffym1$V4/diffxm1$V3
angelm2=diffym2$V4/diffxm2$V3
######################################################
#####
#Horizontal Velocity

vh0=diffxm0$V3/mm0$V1
vh1=diffxm1$V3/mm1$V1
vh2=diffxm2$V3/mm2$V1
#######################################################3
#####
#Vertical Velocity

vv0=diffym0$V4/mm0$V1
vv1=diffym1$V4/mm1$V1
vv2=diffym2$V4/mm2$V1
#############################################################
#######Slope angle of the tangent
m0=atan(session_0m$V4/session_0m$V3)
m1=atan(session_1m$V4/session_1m$V3)
m2=atan(session_2m$V4/session_2m$V3)
######################################################
#MIN Y coordinates 
setDT(session_0m)
minym0=session_0m[, .( V4 =min(V4)), by = .(sub)]
setDT(session_1m)
minym1=session_1m[, .( V4 =min(V4)), by = .(sub)]
setDT(session_2m)
minym2=session_2m[, .( V4 =min(V4)), by = .(sub)]
#############################################################################################
###############################################################################33 
############################################################################33
###########
#AVEREGE FUNCTION 
#AVERAGE TIME
setDT(session_0m)
meantm0=session_0m[, .( V1 =mean(V1)), by = .(sub)]
#####
setDT(session_1m)
meantm1=session_1m[, .( V1 =mean(V1)), by = .(sub)]
############
setDT(session_2m)
meantm2=session_2m[, .( V1 =mean(V1)), by = .(sub)]
#########################################################
##########AVERAGE X COORDINATES 
setDT(session_0m)
meanxm0=session_0m[, .( V3 =mean(V3)), by = .(sub)]
setDT(session_1m)
meanxm1=session_1m[, .( V3 =mean(V3)), by = .(sub)]
setDT(session_2m)
meanxm2=session_2m[, .( V3 =mean(V3)), by = .(sub)]
######################################################
#####
## AVERAGE Y coordinates 
setDT(session_0m)
meanym0=session_0m[, .( V4 =mean(V4)), by = .(sub)]
setDT(session_1m)
meanym1=session_1m[, .( V4 =mean(V4)), by = .(sub)]
setDT(session_2m)
meanym2=session_2m[, .( V4 =mean(V4)), by = .(sub)]

#############################################################################################
###############################################################################33 
############################################################################33
###########
#SUMFUNCTION 
#SUm TIME
setDT(session_0m)
sumtm0=session_0m[, .( V1 =sum(V1)), by = .(sub)]
#####
setDT(session_1m)
sumtm1=session_1m[, .( V1 =sum(V1)), by = .(sub)]
############
setDT(session_2m)
sumtm2=session_2m[, .( V1 =sum(V1)), by = .(sub)]
#########################################################
##########SUm X COORDINATES 
setDT(session_0m)
sumxm0=session_0m[, .( V3 =sum(V3)), by = .(sub)]
setDT(session_1m)
sumxm1=session_1m[, .( V3 =sum(V3)), by = .(sub)]
setDT(session_2m)
sumxm2=session_2m[, .( V3 =sum(V3)), by = .(sub)]
######################################################
#####
## SUM Y coordinates 
setDT(session_0m)
sumym0=session_0m[, .( V4 =sum(V4)), by = .(sub)]
setDT(session_1m)
sumym1=session_1m[, .( V4 =sum(V4)), by = .(sub)]
setDT(session_2m)
sumym2=session_2m[, .( V4 =sum(V4)), by = .(sub)]
##############################################################################################
#####################################################################################333
#################################################################################
#Length of the curve

lengthofcurvesm0=sqrt((diffxm0$V3)^2+((diffym0$V4)^2))
lengthofcurvesm0n=cbind(diffxm0[,],lengthofcurvesm0)
########
lengthofcurvesm1=sqrt((diffxm1$V3)^2+((diffym1$V4)^2))
lengthofcurvesm1n=cbind(diffxm1[,],lengthofcurvesm1)
########
lengthofcurvesm2=sqrt((diffxm2$V3)^2+((diffym2$V4)^2))
lengthofcurvesm2n=cbind(diffxm2[,],lengthofcurvesm2)

##############################################################33
######################3
#Curvature
c0=diff(m0)/diff(lengthofcurvesm0)
c1=diff(m1)/diff(lengthofcurvesm1)
c2=diff(m2)/diff(lengthofcurvesm2)
c0[c0==-Inf]=median(c0################################################################################################3
##############################333
###############speed 
speedm0=lengthofcurvesm0/mm0$V1
speedm0n=cbind(mm0[,],speedm0)
###
speedm1=lengthofcurvesm1/mm1$V1
speedm1n=cbind(mm1[,],speedm1)
###
speedm2=lengthofcurvesm2/mm2$V1
speedm2n=cbind(mm0[,],speedm2)
####################################################################
#########################3
########Acceleration 
Accelerationm0=speedm0/mm0$V1
Accelerationm0n=cbind(mm0[,],Accelerationm0)
######
Accelerationm1=speedm1/mm1$V1
Accelerationm1n=cbind(mm1[,],Accelerationm1)
############
Accelerationm2=speedm2/mm2$V1
Accelerationm2n=cbind(mm2[,],Accelerationm2)
#######################################################################
####Aggregate the speed
speedmean0=aggregate(speedm0 ~ sub , FUN =mean, data=speedm0n)
speedmean1=aggregate(speedm1 ~ sub , FUN =mean, data=speedm1n)
speedmean2=aggregate(speedm2 ~ sub , FUN =mean, data=speedm2n)
#aggregate length
lengthmean0=aggregate(lengthofcurvesm0~ sub , FUN =mean, data=lengthofcurvesm0n)
lengthmean1=aggregate(lengthofcurvesm1 ~ sub , FUN =mean, data=lengthofcurvesm1n)
lengthmean2=aggregate(lengthofcurvesm2 ~ sub , FUN =mean, data=lengthofcurvesm2n)
#aggregate acceleration
accmean0=aggregate(Accelerationm0~ sub , FUN =mean, data=Accelerationm0n)
accmean1=aggregate(Accelerationm1 ~ sub , FUN =mean, data=Accelerationm1n)
accmean2=aggregate(Accelerationm2 ~ sub , FUN =mean, data=Accelerationm2n)
################################################################################3
####Aggregate the Horizontal Velocity
hv0n=cbind(vh0,mm0)
hv1n=cbind(vh1,mm1)
hv2n=cbind(vh2,mm2)
################################
HVmean0=aggregate( vh0~ sub , FUN =mean, data=hv0n)
HVmean1=aggregate(vh1 ~ sub , FUN =mean, data=hv1n)
HVmean2=aggregate(vh2 ~ sub , FUN =mean, data=hv2n)
#######################################################3
#####
#Vertical Velocity
vv0n=cbind(vv0,mm0)
vv1n=cbind(vv1,mm1)
vv2n=cbind(vv2,mm2)
################################
vvmean0=aggregate( vv0~ sub , FUN =mean, data=vv0n)
vVmean1=aggregate(vv1 ~ sub , FUN =mean, data=vv1n)
vVmean2=aggregate(vv2 ~ sub , FUN =mean, data=vv2n)
#######################################################
#######Slope angle of the tangent
m0=atan(session_0m$V4/session_0m$V3)
m1=atan(session_1m$V4/session_1m$V3)
m2=atan(session_2m$V4/session_2m$V3)

m0n=cbind(m0,session_0m)
m1n=cbind(m1,session_1m)
m2n=cbind(m2,session_2m)
########################################
mmean0=aggregate(m0 ~  sub, FUN =mean, data=m0n)
mmean1=aggregate(m1 ~ sub  , FUN =mean, data=m1n)
mmean2=aggregate(m2 ~ sub  , FUN =mean, data=m2n)
#####################################################333
c0n=cbind(c0,lengthofcurvesm0n)
c1n=cbind(c1,lengthofcurvesm1n)
c2n=cbind(c2,lengthofcurvesm2n)
####################################################
cmin0=aggregate(c0 ~  sub, FUN =min, data=c0n)
cmin1=aggregate(c1 ~ sub  , FUN =min, data=c1n)
cmin2=aggregate(c2 ~ sub  , FUN =min, data=c2n)


#############################################################################################
###############################################################################3 
############################################################################
#############################################################################################
#############################################################################################
#######################################################################################33
### Aggregation
########Aggregation the differnece 
#mean of diffx
meandiffxm0=aggregate(V3 ~ sub , FUN =mean, data=diffxm0)
meandiffxm1=aggregate(V3 ~ sub , FUN =mean, data=diffxm1)
meandiffxm2=aggregate(V3 ~ sub , FUN =mean, data=diffxm2)
##############################333
###mean of difft
meandifftm0=aggregate(V1 ~ sub , FUN =mean, data=mm0)
meandifftm1=aggregate(V1 ~ sub , FUN =mean, data=mm1)
meandifftm2=aggregate(V1 ~ sub , FUN =mean, data=mm2)
####mean of diffy
meandiffym0=aggregate(V4 ~ sub , FUN =mean, data=diffym0)
meandiffym1=aggregate(V4 ~ sub , FUN =mean, data=diffym1)
meandiffym2=aggregate(V4 ~ sub , FUN =mean, data=diffym2)
#########################################################################333
########################33333
#Aggregation the scaled values
#
#0- aggregate the scales of X coordinates

scalexm0=aggregate(V3 ~ sub , FUN =mean, data=scalex_0)
scalexm1=aggregate(V3 ~ sub , FUN =mean, data=scalex_1)
scalexm2=aggregate(V3 ~ sub , FUN =mean, data=scalex_2)

##########
#1- aggregate the scales of Y coordinates

scaleym0=aggregate(V4 ~ sub , FUN =mean, data=scaley_0)
scaleym1=aggregate(V4 ~ sub , FUN =mean, data=scaley_1)
scaleym2=aggregate(V4 ~ sub , FUN =mean, data=scaley_2)
#####################################################333
############
#2- aggregate the scales of Times 

scaletm0=aggregate(V1 ~ sub , FUN =mean, data=scalet_0)
scaletm1=aggregate(V1 ~ sub , FUN =mean, data=scalet_1)
scaletm2=aggregate(V1 ~ sub , FUN =mean, data=scalet_2)
###############################


mousefeaturesb0=cbind( scalexm0,scaleym0, scaletm0,standarddevmx0,standarddevmy0,standarddevmt0, meandifftm0, meandiffxm0, meandiffym0,maxtm0,maxxm0,maxym0,mintm0,minxm0,minym0,meantm0,meanxm0,meanym0,sumtm0,
                       sumxm0,sumym0,speedmean0,lengthmean0,accmean0 ,HVmean0,vvmean0,mmean0, cmin0            )

mousefeaturesb0=mousefeaturesb0[,-c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55)]
colnames(mousefeaturesb0)[2] <- "scalex_m"
colnames(mousefeaturesb0)[3] <- "scaley_m"
colnames(mousefeaturesb0)[4] <- "scalet_m"
colnames(mousefeaturesb0)[5] <- "SD_X_m"
colnames(mousefeaturesb0)[6] <- "SD_y_m"
colnames(mousefeaturesb0)[7] <- "SD_t_m"
colnames(mousefeaturesb0)[8] <- "Mean_diff_T_M"
colnames(mousefeaturesb0)[9] <- "Mean_diff_x_M"
colnames(mousefeaturesb0)[10] <- "Mean_diff_y_M"
colnames(mousefeaturesb0)[11] <- "max_t_m"
colnames(mousefeaturesb0)[12] <- "max_x_m"
colnames(mousefeaturesb0)[13] <- "max_y_m"
colnames(mousefeaturesb0)[14] <- "min_t_m"
colnames(mousefeaturesb0)[15] <- "min_x_m"
colnames(mousefeaturesb0)[16] <- "min_y_m"
colnames(mousefeaturesb0)[17] <- "mean_t_m"
colnames(mousefeaturesb0)[18] <- "mean_x_m"
colnames(mousefeaturesb0)[19] <- "mean_y_m"
colnames(mousefeaturesb0)[20] <- "Sum_t_m"
colnames(mousefeaturesb0)[21] <- "Sum_x_m"
colnames(mousefeaturesb0)[22] <- "Sum_y_m"
colnames(mousefeaturesb0)[23] <- "speed_m"
colnames(mousefeaturesb0)[24] <- "length_m"
colnames(mousefeaturesb0)[25] <- "acceleration_m"
colnames(mousefeaturesb0)[26] <- "Horizontal_speed_m"
colnames(mousefeaturesb0)[27] <- "Vertical_speed_m"
colnames(mousefeaturesb0)[28] <- "tangent_angle_m"
colnames(mousefeaturesb0)[29] <- "Curvature_m"


write.csv(mousefeaturesb0,file = paste("mousefeaturesb0", i, ".csv") ,sep = " ")

#################################################################################3
##################################3333
#########session 1

mousefeaturesb1=cbind( scalexm1,scaleym1, scaletm1,standarddevmx1,standarddevmy1,standarddevmt1, meandifftm1, meandiffxm1, meandiffym1,maxtm1,maxxm1,maxym1,mintm1,minxm1,minym1,meantm1,meanxm1,meanym1,sumtm1,
                       sumxm1,sumym1,speedmean1,lengthmean1,accmean1 ,HVmean1,vVmean1,mmean1, cmin1                )


mousefeaturesb1=mousefeaturesb1[,-c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55)]
colnames(mousefeaturesb1)[2] <- "scalex_m"
colnames(mousefeaturesb1)[3] <- "scaley_m"
colnames(mousefeaturesb1)[4] <- "scalet_m"
colnames(mousefeaturesb1)[5] <- "SD_X_m"
colnames(mousefeaturesb1)[6] <- "SD_y_m"
colnames(mousefeaturesb1)[7] <- "SD_t_m"
colnames(mousefeaturesb1)[8] <- "Mean_diff_T_M"
colnames(mousefeaturesb1)[9] <- "Mean_diff_x_M"
colnames(mousefeaturesb1)[10] <- "Mean_diff_y_M"
colnames(mousefeaturesb1)[11] <- "max_t_m"
colnames(mousefeaturesb1)[12] <- "max_x_m"
colnames(mousefeaturesb1)[13] <- "max_y_m"
colnames(mousefeaturesb1)[14] <- "min_t_m"
colnames(mousefeaturesb1)[15] <- "min_x_m"
colnames(mousefeaturesb1)[16] <- "min_y_m"
colnames(mousefeaturesb1)[17] <- "mean_t_m"
colnames(mousefeaturesb1)[18] <- "mean_x_m"
colnames(mousefeaturesb1)[19] <- "mean_y_m"
colnames(mousefeaturesb1)[20] <- "Sum_t_m"
colnames(mousefeaturesb1)[21] <- "Sum_x_m"
colnames(mousefeaturesb1)[22] <- "Sum_y_m"
colnames(mousefeaturesb1)[23] <- "speed_m"
colnames(mousefeaturesb1)[24] <- "length_m"
colnames(mousefeaturesb1)[25] <- "acceleration_m"
colnames(mousefeaturesb1)[26] <- "Horizontal_speed_m"
colnames(mousefeaturesb1)[27] <- "Vertical_speed_m"
colnames(mousefeaturesb1)[28] <- "tangent_angle_m"
colnames(mousefeaturesb1)[29] <- "Curvature_m"




write.csv(mousefeaturesb1,file = paste("mousefeaturesb1", i, ".csv") ,sep = " ")

#session 2
#############################################################################################
####################################################################################
mousefeaturesb2=cbind( scalexm2,scaleym2, scaletm2,standarddevmx2,standarddevmy2,standarddevmt2, meandifftm2, meandiffxm2, meandiffym2,maxtm2,maxxm2,maxym2,mintm2,minxm2,minym2,meantm2,meanxm2,meanym2,sumtm2,
                       sumxm2,sumym2,speedmean2,lengthmean2,accmean2 ,HVmean2,vVmean2,mmean2, cmin2                 )



mousefeaturesb2=mousefeaturesb2[,-c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55)]
colnames(mousefeaturesb2)[2] <- "scalex_m"
colnames(mousefeaturesb2)[3] <- "scaley_m"
colnames(mousefeaturesb2)[4] <- "scalet_m"
colnames(mousefeaturesb2)[5] <- "SD_X_m"
colnames(mousefeaturesb2)[6] <- "SD_y_m"
colnames(mousefeaturesb2)[7] <- "SD_t_m"
colnames(mousefeaturesb2)[8] <- "Mean_diff_T_M"
colnames(mousefeaturesb2)[9] <- "Mean_diff_x_M"
colnames(mousefeaturesb2)[10] <- "Mean_diff_y_M"
colnames(mousefeaturesb2)[11] <- "max_t_m"
colnames(mousefeaturesb2)[12] <- "max_x_m"
colnames(mousefeaturesb2)[13] <- "max_y_m"
colnames(mousefeaturesb2)[14] <- "min_t_m"
colnames(mousefeaturesb2)[15] <- "min_x_m"
colnames(mousefeaturesb2)[16] <- "min_y_m"
colnames(mousefeaturesb2)[17] <- "mean_t_m"
colnames(mousefeaturesb2)[18] <- "mean_x_m"
colnames(mousefeaturesb2)[19] <- "mean_y_m"
colnames(mousefeaturesb2)[20] <- "Sum_t_m"
colnames(mousefeaturesb2)[21] <- "Sum_x_m"
colnames(mousefeaturesb2)[22] <- "Sum_y_m"
colnames(mousefeaturesb2)[23] <- "speed_m"
colnames(mousefeaturesb2)[24] <- "length_m"
colnames(mousefeaturesb2)[25] <- "acceleration_m"
colnames(mousefeaturesb2)[26] <- "Horizontal_speed_m"
colnames(mousefeaturesb2)[27] <- "Vertical_speed_m"
colnames(mousefeaturesb2)[28] <- "tangent_angle_m"
colnames(mousefeaturesb2)[29] <- "Curvature_m"



write.csv(mousefeaturesb2,file = paste("mousefeaturesb2", i, ".csv") ,sep = " ")

mouse_sessions=rbind(mousefeaturesb0,mousefeaturesb1,mousefeaturesb2)

write.csv(mouse_sessions,file = paste("allmousesessionsb", i, ".csv") ,sep = " ")

##############################################################33333
####################################################################33
####################################
###########
#^#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#EYE
#setDT(subject_G)
session_0g=subject_G[subject_G[,5]=="0",]
session_1g=subject_G[subject_G[,5]=="1",]
session_2g=subject_G[subject_G[,5]=="2",]

######################################################
#x coordinates
setDT(session_0g)
scalexg_0=session_0g[, .( V3 =data.Normalization(V3,type="n4")), by = .(sub)]
#########
setDT(session_1g)
scalexg_1=session_1g[, .( V3 =data.Normalization(V3,type="n4")), by = .(sub)]
###########
setDT(session_2g)
scalexg_2=session_2g[, .( V3 =data.Normalization(V3,type="n4")), by = .(sub)]
###########################################################################################
## y coordinates  scaling 
setDT(session_0g)
scaleyg_0=session_0g[, .( V4 =data.Normalization(V4,type="n4")), by = .(sub)]
#########
setDT(session_1g)
scaleyg_1=session_1g[, .( V4 =data.Normalization(V4,type="n4")), by = .(sub)]
###########
setDT(session_2g)
scaleyg_2=session_2g[, .( V4 =data.Normalization(V4,type="n4")), by = .(sub)]
####################################################################################
setDT(session_0g)
scaletg_0=session_0m[, .( V1 =data.Normalization(V1,type="n4")), by = .(sub)]
#########
setDT(session_1g)
scaletg_1=session_1g[, .( V1 =data.Normalization(V1,type="n4")), by = .(sub)]
###########
setDT(session_2g)
scaletg_2=session_2g[, .( V1 =data.Normalization(V1,type="n4")), by = .(sub)]


#standard deviation
#############################################3
####for x coordinates 
setDT(session_0g)
standarddevgx0=session_0g[, .( V3 =sd(V3)), by = .(sub)]
setDT(session_1g)
standarddevgx1=session_1g[, .( V3 =sd(V3)), by = .(sub)]
setDT(session_2g)
standarddevgx2=session_2g[, .( V3 =sd(V3)), by = .(sub)]
#############################################################################################
#################33
#### for Y coordinates 
setDT(session_0g)
standarddevgy0=session_0g[, .( V4 =sd(V4)), by = .(sub)]
setDT(session_1g)
standarddevgy1=session_1g[, .( V4 =sd(V4)), by = .(sub)]
setDT(session_2g)
standarddevgy2=session_2g[, .( V4 =sd(V4)), by = .(sub)]
#############################################################################################
#################
#### for Time 
setDT(session_0g)
standarddevgt0=session_0g[, .( V1 =sd(V1)), by = .(sub)]
setDT(session_1g)
standarddevgt1=session_1g[, .( V1 =sd(V1)), by = .(sub)]
setDT(session_2g)
standarddevgt2=session_2g[, .( V1 =sd(V3)), by = .(sub)]
#################################################################################################
########################################################33
################################################################
##### difference 
#####
###### difference X 
setDT(session_0g)
gg0=session_0g[, .( V1 =diff(V1)), by = .(sub)]
#####
setDT(session_1g)
gg1=session_1g[, .( V1 =diff(V1)), by = .(sub)]
############
setDT(session_2g)
gg2=session_2g[, .( V1 =diff(V1)), by = .(sub)]
#########################################################
##########diffx&y mouse
setDT(session_0g)
diffxg0=session_0g[, .( V3 =diff(V3)), by = .(sub)]
setDT(session_1g)
diffxg1=session_1g[, .( V3 =diff(V3)), by = .(sub)]
setDT(session_2g)
diffxg2=session_2g[, .( V3 =diff(V3)), by = .(sub)]
######################################################
#####Y coordinates 
setDT(session_0g)
diffyg0=session_0g[, .( V4 =diff(V4)), by = .(sub)]
setDT(session_1g)
diffyg1=session_1g[, .( V4 =diff(V4)), by = .(sub)]
setDT(session_2g)
diffyg2=session_2g[, .( V4 =diff(V4)), by = .(sub)]
####################################################################################################################
#####################################################333
############################
#maximum 
setDT(session_0g)
maxtg0=session_0g[, .( V1 =max(V1)), by = .(sub)]
#####
setDT(session_1g)
maxtg1=session_1g[, .( V1 =max(V1)), by = .(sub)]
############
setDT(session_2g)
maxtg2=session_2g[, .( V1 =max(V1)), by = .(sub)]
#########################################################
##########diffx&y mouse
setDT(session_0g)
maxxg0=session_0g[, .( V3 =max(V3)), by = .(sub)]
setDT(session_1g)
maxxg1=session_1g[, .( V3 =max(V3)), by = .(sub)]
setDT(session_2g)
maxxg2=session_2g[, .( V3 =max(V3)), by = .(sub)]
######################################################
#####Y coordinates 
setDT(session_0g)
maxyg0=session_0g[, .( V4 =max(V4)), by = .(sub)]
setDT(session_1g)
maxyg1=session_1g[, .( V4 =max(V4)), by = .(sub)]
setDT(session_2g)
maxyg2=session_2g[, .( V4 =max(V4)), by = .(sub)]
############################################################################################
################################################################################
########################################
#########MIN
####MINIMUM TIME
setDT(session_0g)
mintg0=session_0g[, .( V1 =min(V1)), by = .(sub)]
#####
setDT(session_1g)
mintg1=session_1g[, .( V1 =min(V1)), by = .(sub)]
############
setDT(session_2g)
mintg2=session_2g[, .( V1 =min(V1)), by = .(sub)]
#########################################################
##########
######MINIMUM X COORDINATES 
setDT(session_0g)
minxg0=session_0g[, .( V3 =min(V3)), by = .(sub)]
setDT(session_1g)
minxg1=session_1g[, .( V3 =min(V3)), by = .(sub)]
setDT(session_2g)
minxg2=session_2g[, .( V3 =min(V3)), by = .(sub)]
######################################################
#####
#MIN Y coordinates 
setDT(session_0g)
minyg0=session_0g[, .( V4 =min(V4)), by = .(sub)]
setDT(session_1g)
minyg1=session_1g[, .( V4 =min(V4)), by = .(sub)]
setDT(session_2g)
minyg2=session_2g[, .( V4 =min(V4)), by = .(sub)]
#############################################################################################
###############################################################################33 
############################################################################33
###########
#AVEREGE FUNCTION 
#AVERAGE TIME
setDT(session_0g)
meantg0=session_0g[, .( V1 =mean(V1)), by = .(sub)]
#####
setDT(session_1g)
meantg1=session_1g[, .( V1 =mean(V1)), by = .(sub)]
############
setDT(session_2g)
meantg2=session_2g[, .( V1 =mean(V1)), by = .(sub)]
#########################################################
##########AVERAGE X COORDINATES 
setDT(session_0g)
meanxg0=session_0g[, .( V3 =mean(V3)), by = .(sub)]
setDT(session_1g)
meanxg1=session_1g[, .( V3 =mean(V3)), by = .(sub)]
setDT(session_2g)
meanxg2=session_2g[, .( V3 =mean(V3)), by = .(sub)]
######################################################
#####
## AVERAGE Y coordinates 
setDT(session_0g)
meanyg0=session_0g[, .( V4 =mean(V4)), by = .(sub)]
setDT(session_1g)
meanyg1=session_1g[, .( V4 =mean(V4)), by = .(sub)]
setDT(session_2g)
meanyg2=session_2g[, .( V4 =mean(V4)), by = .(sub)]

#############################################################################################
###############################################################################33 
############################################################################33
###########
#SUMFUNCTION 
#SUm TIME
setDT(session_0g)
sumtg0=session_0g[, .( V1 =sum(V1)), by = .(sub)]
#####
setDT(session_1g)
sumtg1=session_1g[, .( V1 =sum(V1)), by = .(sub)]
############
setDT(session_2g)
sumtg2=session_2g[, .( V1 =sum(V1)), by = .(sub)]
#########################################################
##########SUm X COORDINATES 
setDT(session_0g)
sumxg0=session_0g[, .( V3 =sum(V3)), by = .(sub)]
setDT(session_1g)
sumxg1=session_1g[, .( V3 =sum(V3)), by = .(sub)]
setDT(session_2g)
sumxg2=session_2g[, .( V3 =sum(V3)), by = .(sub)]
######################################################
#####
## SUM Y coordinates 
setDT(session_0g)
sumyg0=session_0g[, .( V4 =sum(V4)), by = .(sub)]
setDT(session_1g)
sumyg1=session_1g[, .( V4 =sum(V4)), by = .(sub)]
setDT(session_2g)
sumyg2=session_2g[, .( V4 =sum(V4)), by = .(sub)]
##############################################################
######Length of curve 
#Length of the curve

lengthofcurvesg0=sqrt((diffxg0$V3)^2+((diffyg0$V4)^2))
lengthofcurvesg0n=cbind(diffxg0[,],lengthofcurvesg0)
########
lengthofcurvesg1=sqrt((diffxg1$V3)^2+((diffym1$V4)^2))
lengthofcurvesg1n=cbind(diffxg1[,],lengthofcurvesg1)
########
lengthofcurvesg2=sqrt((diffxg2$V3)^2+((diffyg2$V4)^2))
lengthofcurvesg2n=cbind(diffxg2[,],lengthofcurvesg2)

################################################################################################3
##############################333
###############speed 
speedg0=lengthofcurvesg0/gg0$V1
speedg0n=cbind(gg0[,],speedg0)
###
speedg1=lengthofcurvesg1/gg1$V1
speedg1n=cbind(gg1[,],speedg1)
###
speedg2=lengthofcurvesg2/gg2$V1
speedg2n=cbind(gg0[,],speedg2)
####################################################################
#########################3
########Acceleration 
Accelerationg0=speedg0/gg0$V1
Accelerationg0n=cbind(gg0[,],Accelerationg0)
######
Accelerationg1=speedg1/gg1$V1
Accelerationg1n=cbind(gg1[,],Accelerationg1)
############
Accelerationg2=speedg2/gg2$V1
Accelerationg2n=cbind(gg2[,],Accelerationg2)
###################################################
##### Direction angle

angleg0=diffyg0$V4/diffxg0$V3
angleg1=diffyg1$V4/diffxg1$V3
angelg2=diffyg2$V4/diffxg2$V3
######################################################
#####
#Horizontal Velocity

vhg0=diffxg0$V3/gg0$V1
vhg1=diffxg1$V3/gg1$V1
vhg2=diffxg2$V3/gg2$V1
#######################################################3
#####
#Vertical Velocity

vvg0=diffyg0$V4/gg0$V1
vvg1=diffyg1$V4/gg1$V1
vvg2=diffyg2$V4/gg2$V1
#############################################################
#######Slope angle of the tangent
g0=atan(session_0g$V4/session_0g$V3)
g1=atan(session_1g$V4/session_1g$V3)
g2=atan(session_2g$V4/session_2g$V3)

####################################################################33
######################3
#Curvature
cg0=diff(g0)/diff(lengthofcurvesg0)
cg1=diff(g1)/diff(lengthofcurvesg1)
cg2=diff(g2)/diff(lengthofcurvesg2)


#######################################################################
#####################################################################################33
###############################################################################################
###EYE

### Aggregation
########Aggregation the differnece 
#mean of diffx
meandiffxg0=aggregate(V3 ~ sub , FUN =mean, data=diffxg0)
meandiffxg1=aggregate(V3 ~ sub , FUN =mean, data=diffxg1)
meandiffxg2=aggregate(V3 ~ sub , FUN =mean, data=diffxg2)
##############################333
###mean of difft
meandifftg0=aggregate(V1 ~ sub , FUN =mean, data=gg0)
meandifftg1=aggregate(V1 ~ sub , FUN =mean, data=gg1)
meandifftg2=aggregate(V1 ~ sub , FUN =mean, data=gg2)
####mean of diffy
meandiffyg0=aggregate(V4 ~ sub , FUN =mean, data=diffyg0)
meandiffyg1=aggregate(V4 ~ sub , FUN =mean, data=diffyg1)
meandiffyg2=aggregate(V4 ~ sub , FUN =mean, data=diffyg2)

#########################################################################333
########################33333
#Aggregation the scaled values
#
#0- aggregate the scales of X coordinates

scalexg0=aggregate(V3 ~ sub , FUN =mean, data=scalexg_0)
scalexg1=aggregate(V3 ~ sub , FUN =mean, data=scalexg_1)
scalexg2=aggregate(V3 ~ sub , FUN =mean, data=scalexg_2)

##########
#1- aggregate the scales of Y coordinates

scaleyg0=aggregate(V4 ~ sub , FUN =mean, data=scaleyg_0)
scaleyg1=aggregate(V4 ~ sub , FUN =mean, data=scaleyg_1)
scaleyg2=aggregate(V4 ~ sub , FUN =mean, data=scaleyg_2)
#####################################################333
############
#2- aggregate the scales of Times 

scaletg0=aggregate(V1 ~ sub , FUN =mean, data=scaletg_0)
scaletg1=aggregate(V1 ~ sub , FUN =mean, data=scaletg_1)
scaletg2=aggregate(V1 ~ sub , FUN =mean, data=scaletg_2)
#############################################################
###################################3
####Aggregate the speed
speedmean0g=aggregate(speedg0 ~ sub , FUN =mean, data=speedg0n)
speedmean1g=aggregate(speedg1 ~ sub , FUN =mean, data=speedg1n)
speedmean2g=aggregate(speedg2 ~ sub , FUN =mean, data=speedg2n)
#aggregate length
lengthmean0g=aggregate(lengthofcurvesg0~ sub , FUN =mean, data=lengthofcurvesg0n)
lengthmean1g=aggregate(lengthofcurvesg1 ~ sub , FUN =mean, data=lengthofcurvesg1n)
lengthmean2g=aggregate(lengthofcurvesg2 ~ sub , FUN =mean, data=lengthofcurvesg2n)
#aggregate acceleration
accmean0g=aggregate(Accelerationg0~ sub , FUN =mean, data=Accelerationg0n)
accmean1g=aggregate(Accelerationg1 ~ sub , FUN =mean, data=Accelerationg1n)
accmean2g=aggregate(Accelerationg2 ~ sub , FUN =mean, data=Accelerationg2n)
###################################################################################33
####################################################3
####Aggregate the Horizontal Velocity
hv0ng=cbind(vhg0,gg0)
hv1ng=cbind(vhg1,gg1)
hv2ng=cbind(vhg2,gg2)
################################
HVmean0g=aggregate( vhg0~ sub , FUN =mean, data=hv0ng)
HVmean1g=aggregate(vhg1 ~ sub , FUN =mean, data=hv1ng)
HVmean2g=aggregate(vhg2 ~ sub , FUN =mean, data=hv2ng)
#######################################################3
#####
#Vertical Velocity
vv0ng=cbind(vvg0,gg0)
vv1ng=cbind(vvg1,gg1)
vv2ng=cbind(vvg2,gg2)
################################
vvmean0g=aggregate( vvg0~ sub , FUN =mean, data=vv0ng)
vVmean1g=aggregate(vvg1 ~ sub , FUN =mean, data=vv1ng)
vVmean2g=aggregate(vvg2 ~ sub , FUN =mean, data=vv2ng)
#######################################################
#######Slope angle of the tangent
g0=atan(session_0g$V4/session_0g$V3)
g1=atan(session_1g$V4/session_1g$V3)
g2=atan(session_2g$V4/session_2g$V3)

g0n=cbind(g0,session_0g)
g1n=cbind(g1,session_1g)
g2n=cbind(g2,session_2g)
########################################
gmmean0=aggregate(g0 ~  sub, FUN =mean, data=g0n)
gmmean1=aggregate(g1 ~ sub  , FUN =mean, data=g1n)
gmmean2=aggregate(g2 ~ sub  , FUN =mean, data=g2n)
#####################################################333
cg0n=cbind(cg0,lengthofcurvesg0n)
cg1n=cbind(cg1,lengthofcurvesg1n)
cg2n=cbind(cg2,lengthofcurvesg2n)
####################################################
cgmin0=aggregate(cg0 ~  sub, FUN =min, data=cg0n)
cgmin1=aggregate(cg1 ~ sub  , FUN =min, data=cg1n)
cgmin2=aggregate(cg2 ~ sub  , FUN =min, data=cg2n)




############################################################




eyefeaturesb0=cbind( scalexg0,scaleyg0, scaletg0,standarddevgx0,standarddevgy0,standarddevgt0, meandifftg0, meandiffxg0, meandiffyg0,maxtg0,maxxg0,maxyg0,mintg0,minxg0,minyg0,meantg0,meanxg0,meanyg0,sumtg0,
                     sumxg0,sumyg0,speedmean0g,lengthmean0g,accmean0g , HVmean0g,vvmean0g, gmmean0,  cgmin0          )

eyefeaturesb0=eyefeaturesb0[,-c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55)]
colnames(eyefeaturesb0)[2] <- "scalex_g"
colnames(eyefeaturesb0)[3] <- "scaley_g"
colnames(eyefeaturesb0)[4] <- "scalet_g"
colnames(eyefeaturesb0)[5] <- "SD_X_g"
colnames(eyefeaturesb0)[6] <- "SD_y_g"
colnames(eyefeaturesb0)[7] <- "SD_t_g"
colnames(eyefeaturesb0)[8] <- "Mean_diff_T_g"
colnames(eyefeaturesb0)[9] <- "Mean_diff_x_g"
colnames(eyefeaturesb0)[10] <- "Mean_diff_y_g"
colnames(eyefeaturesb0)[11] <- "max_t_g"
colnames(eyefeaturesb0)[12] <- "max_x_g"
colnames(eyefeaturesb0)[13] <- "max_y_g"
colnames(eyefeaturesb0)[14] <- "min_t_g"
colnames(eyefeaturesb0)[15] <- "min_x_g"
colnames(eyefeaturesb0)[16] <- "min_y_g"
colnames(eyefeaturesb0)[17] <- "mean_t_g"
colnames(eyefeaturesb0)[18] <- "mean_x_g"
colnames(eyefeaturesb0)[19] <- "mean_y_g"
colnames(eyefeaturesb0)[20] <- "Sum_t_g"
colnames(eyefeaturesb0)[21] <- "Sum_x_g"
colnames(eyefeaturesb0)[22] <- "Sum_y_g"
colnames(eyefeaturesb0)[23] <- "speed_g"
colnames(eyefeaturesb0)[24] <- "length_g"
colnames(eyefeaturesb0)[25] <- "acceleration_g"
colnames(eyefeaturesb0)[26] <- "Horizontal_speed_g"
colnames(eyefeaturesb0)[27] <- "Vertical_speed_g"
colnames(eyefeaturesb0)[28] <- "tangent_angle_g"
colnames(eyefeaturesb0)[29] <- "Curvature_g"


write.csv(eyefeaturesb0,file = paste("eyefeaturesb0", i, ".csv") ,sep = " ")



###################################################3
##################################
#####Binding


#################################################################################3
##################################3333
#########session 1



eyefeaturesb1=cbind( scalexg1,scaleyg1, scaletg1,standarddevgx1,standarddevgy1,standarddevgt1, meandifftg1, meandiffxg1, meandiffyg1,maxtg1,maxxg1,maxyg1,mintg1,minxg1,minyg1,meantg1,meanxg1,meanyg1,sumtg1,
                     sumxg1,sumyg1, speedmean1g,lengthmean1g,accmean1g ,HVmean1g, vVmean1g, gmmean1,  cgmin1               )

eyefeaturesb1=eyefeaturesb1[,-c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55)]
colnames(eyefeaturesb1)[2] <- "scalex_g"
colnames(eyefeaturesb1)[3] <- "scaley_g"
colnames(eyefeaturesb1)[4] <- "scalet_g"
colnames(eyefeaturesb1)[5] <- "SD_X_g"
colnames(eyefeaturesb1)[6] <- "SD_y_g"
colnames(eyefeaturesb1)[7] <- "SD_t_g"
colnames(eyefeaturesb1)[8] <- "Mean_diff_T_g"
colnames(eyefeaturesb1)[9] <- "Mean_diff_x_g"
colnames(eyefeaturesb1)[10] <- "Mean_diff_y_g"
colnames(eyefeaturesb1)[11] <- "max_t_g"
colnames(eyefeaturesb1)[12] <- "max_x_g"
colnames(eyefeaturesb1)[13] <- "max_y_g"
colnames(eyefeaturesb1)[15] <- "min_x_g"
colnames(eyefeaturesb1)[14] <- "min_t_g"
colnames(eyefeaturesb1)[16] <- "min_y_g"
colnames(eyefeaturesb1)[17] <- "mean_t_g"
colnames(eyefeaturesb1)[18] <- "mean_x_g"
colnames(eyefeaturesb1)[19] <- "mean_y_g"
colnames(eyefeaturesb1)[20] <- "Sum_t_g"
colnames(eyefeaturesb1)[21] <- "Sum_x_g"
colnames(eyefeaturesb1)[22] <- "Sum_y_g"
colnames(eyefeaturesb1)[23] <- "speed_g"
colnames(eyefeaturesb1)[24] <- "length_g"
colnames(eyefeaturesb1)[25] <- "acceleration_g"
colnames(eyefeaturesb1)[26] <- "Horizontal_speed_g"
colnames(eyefeaturesb1)[27] <- "Vertical_speed_g"
colnames(eyefeaturesb1)[28] <- "tangent_angle_g"
colnames(eyefeaturesb1)[29] <- "Curvature_g"


write.csv(eyefeaturesb1,file = paste("eyefeaturesb1", i, ".csv") ,sep = " ")



#################################################################################3
##################################3333
#########session 2



eyefeaturesb2=cbind( scalexg2,scaleyg2, scaletg2,standarddevgx2,standarddevgy2,standarddevgt2, meandifftg2, meandiffxg2, meandiffyg2,maxtg2,maxxg2,maxyg2,mintg2,minxg2,minyg2,meantg2,meanxg2,meanyg2,sumtg2,
                     sumxg2,sumyg2, speedmean2g,lengthmean2g,accmean2g  ,HVmean2g, vVmean2g, gmmean2,  cgmin2                     )

eyefeaturesb2=eyefeaturesb2[,-c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55)]
colnames(eyefeaturesb2)[2] <- "scalex_g"
colnames(eyefeaturesb2)[3] <- "scaley_g"
colnames(eyefeaturesb2)[4] <- "scalet_g"
colnames(eyefeaturesb2)[5] <- "SD_X_g"
colnames(eyefeaturesb2)[6] <- "SD_y_g"
colnames(eyefeaturesb2)[7] <- "SD_t_g"
colnames(eyefeaturesb2)[8] <- "Mean_diff_T_g"
colnames(eyefeaturesb2)[9] <- "Mean_diff_x_g"
colnames(eyefeaturesb2)[10] <- "Mean_diff_y_g"
colnames(eyefeaturesb2)[11] <- "max_t_g"
colnames(eyefeaturesb2)[12] <- "max_x_g"
colnames(eyefeaturesb2)[13] <- "max_y_g"
colnames(eyefeaturesb2)[14] <- "min_t_g"
colnames(eyefeaturesb2)[15] <- "min_x_g"
colnames(eyefeaturesb2)[16] <- "min_y_g"
colnames(eyefeaturesb2)[17] <- "mean_t_g"
colnames(eyefeaturesb2)[18] <- "mean_x_g"
colnames(eyefeaturesb2)[19] <- "mean_y_g"
colnames(eyefeaturesb2)[20] <- "Sum_t_g"
colnames(eyefeaturesb2)[21] <- "Sum_x_g"
colnames(eyefeaturesb2)[22] <- "Sum_y_g"
colnames(eyefeaturesb2)[23] <- "speed_g"
colnames(eyefeaturesb2)[24] <- "length_g"
colnames(eyefeaturesb2)[25] <- "acceleration_g"
colnames(eyefeaturesb2)[26] <- "Horizontal_speed_g"
colnames(eyefeaturesb2)[27] <- "Vertical_speed_g"
colnames(eyefeaturesb2)[28] <- "tangent_angle_g"
colnames(eyefeaturesb2)[29] <- "Curvature_g"


write.csv(eyefeaturesb2,file = paste("eyefeaturesb2", i, ".csv") ,sep = " ")

eye_sessions=rbind(eyefeaturesb0,eyefeaturesb1,eyefeaturesb2)


write.csv(eye_sessions,file = paste("alleye_sessionsb", i, ".csv") ,sep = " ")

#############################################################################################




#####################################################################################
##############################################################
#######################################
####MOUSE CLICK
subject_mc=fin[fin[,2]=="MC",]
#setDT(subject_mc)
session_0mc=subject_mc[subject_mc[,5]=="0",]
session_1mc=subject_mc[subject_mc[,5]=="1",]
session_2mc=subject_mc[subject_mc[,5]=="2",]

#scaling 
#x coordinates
setDT(session_0mc)
scalexmc_0=session_0mc[, .( V3 =data.Normalization(V3,type="n4")), by = .(sub)]
#########
setDT(session_1mc)
scalexmc_1=session_1mc[, .( V3 =data.Normalization(V3,type="n4")), by = .(sub)]
###########
setDT(session_2mc)
scalexmc_2=session_2mc[, .( V3 =data.Normalization(V3,type="n4")), by = .(sub)]
###########################################################################################
## y coordinates  scaling 
setDT(session_0mc)
scaleymc_0=session_0mc[, .( V4 =data.Normalization(V4,type="n4")), by = .(sub)]
#########
setDT(session_1mc)
scaleymc_1=session_1mc[, .( V4 =data.Normalization(V4,type="n4")), by = .(sub)]
###########
setDT(session_2mc)
scaleymc_2=session_2mc[, .( V4 =data.Normalization(V4,type="n4")), by = .(sub)]
####################################################################################
setDT(session_0mc)
scaletmc_0=session_0mc[, .( V1 =data.Normalization(V1,type="n4")), by = .(sub)]
#########
setDT(session_1mc)
scaletmc_1=session_1mc[, .( V1 =data.Normalization(V1,type="n4")), by = .(sub)]
###########
setDT(session_2m)
scaletmc_2=session_2mc[, .( V1 =data.Normalization(V1,type="n4")), by = .(sub)]
######################################################################################
#####################################################################
########################

#standard deviation
#############################################3
####for x coordinates 
setDT(session_0mc)
standarddevmcx0=session_0mc[, .( V3 =sd(V3)), by = .(sub)]
setDT(session_1mc)
standarddevmcx1=session_1mc[, .( V3 =sd(V3)), by = .(sub)]
setDT(session_2mc)
standarddevmcx2=session_2mc[, .( V3 =sd(V3)), by = .(sub)]
#############################################################################################
#################33
#### for Y coordinates 
setDT(session_0mc)
standarddevmcy0=session_0mc[, .( V4 =sd(V4)), by = .(sub)]
setDT(session_1mc)
standarddevmcy1=session_1mc[, .( V4 =sd(V4)), by = .(sub)]
setDT(session_2mc)
standarddevmcy2=session_2mc[, .( V4 =sd(V4)), by = .(sub)]
#############################################################################################
#################
#### for Time 
setDT(session_0mc)
standarddevmct0=session_0mc[, .( V1 =sd(V1)), by = .(sub)]
setDT(session_1mc)
standarddevmct1=session_1mc[, .( V1 =sd(V1)), by = .(sub)]
setDT(session_2mc)
standarddevmct2=session_2mc[, .( V1 =sd(V3)), by = .(sub)]
#################################################################################################
########################################################33
################################################################
##### difference 
#####
###### difference X 
setDT(session_0mc)
mc0=session_0mc[, .( V1 =diff(V1)), by = .(sub)]
#####
setDT(session_1mc)
mc1=session_1mc[, .( V1 =diff(V1)), by = .(sub)]
############
setDT(session_2m)
mc2=session_2m[, .( V1 =diff(V1)), by = .(sub)]
#########################################################
##########diffx&y mouse
setDT(session_0mc)
diffxmc0=session_0mc[, .( V3 =diff(V3)), by = .(sub)]
setDT(session_1mc)
diffxmc1=session_1mc[, .( V3 =diff(V3)), by = .(sub)]
setDT(session_2mc)
diffxmc2=session_2mc[, .( V3 =diff(V3)), by = .(sub)]
######################################################
#####Y coordinates 
setDT(session_0mc)
diffymc0=session_0mc[, .( V4 =diff(V4)), by = .(sub)]
setDT(session_1mc)
diffymc1=session_1mc[, .( V4 =diff(V4)), by = .(sub)]
setDT(session_2mc)
diffymc2=session_2mc[, .( V4 =diff(V4)), by = .(sub)]
####################################################################################################################
#####################################################333
############################
#maximum 
setDT(session_0mc)
maxtmc0=session_0mc[, .( V1 =max(V1)), by = .(sub)]
#####
setDT(session_1mc)
maxtmc1=session_1mc[, .( V1 =max(V1)), by = .(sub)]
############
setDT(session_2mc)
maxtmc2=session_2mc[, .( V1 =max(V1)), by = .(sub)]
#########################################################
##########diffx&y mouse
setDT(session_0mc)
maxxmc0=session_0mc[, .( V3 =max(V3)), by = .(sub)]
setDT(session_1mc)
maxxmc1=session_1mc[, .( V3 =max(V3)), by = .(sub)]
setDT(session_2mc)
maxxmc2=session_2mc[, .( V3 =max(V3)), by = .(sub)]
######################################################
#####Y coordinates 
setDT(session_0mc)
maxymc0=session_0mc[, .( V4 =max(V4)), by = .(sub)]
setDT(session_1mc)
maxymc1=session_1mc[, .( V4 =max(V4)), by = .(sub)]
setDT(session_2mc)
maxymc2=session_2mc[, .( V4 =max(V4)), by = .(sub)]
############################################################################################
################################################################################
########################################
#########MIN
####MINIMUM TIME
setDT(session_0mc)
mintmc0=session_0mc[, .( V1 =min(V1)), by = .(sub)]
#####
setDT(session_1mc)
mintmc1=session_1mc[, .( V1 =min(V1)), by = .(sub)]
############
setDT(session_2mc)
mintmc2=session_2mc[, .( V1 =min(V1)), by = .(sub)]
#########################################################
##########
######MINIMUM X COORDINATES 
setDT(session_0mc)
minxmc0=session_0mc[, .( V3 =min(V3)), by = .(sub)]
setDT(session_1mc)
minxmc1=session_1mc[, .( V3 =min(V3)), by = .(sub)]
setDT(session_2mc)
minxmc2=session_2mc[, .( V3 =min(V3)), by = .(sub)]
######################################################
#####
#MIN Y coordinates 
setDT(session_0mc)
minymc0=session_0mc[, .( V4 =min(V4)), by = .(sub)]
setDT(session_1mc)
minymc1=session_1mc[, .( V4 =min(V4)), by = .(sub)]
setDT(session_2mc)
minymc2=session_2mc[, .( V4 =min(V4)), by = .(sub)]
#############################################################################################
###############################################################################33 
############################################################################33
###########
#AVEREGE FUNCTION 
#AVERAGE TIME
setDT(session_0mc)
meantmc0=session_0mc[, .( V1 =mean(V1)), by = .(sub)]
#####
setDT(session_1m)
meantmc1=session_1mc[, .( V1 =mean(V1)), by = .(sub)]
############
setDT(session_2mc)
meantmc2=session_2mc[, .( V1 =mean(V1)), by = .(sub)]
#########################################################
##########AVERAGE X COORDINATES 
setDT(session_0mc)
meanxmc0=session_0mc[, .( V3 =mean(V3)), by = .(sub)]
setDT(session_1mc)
meanxmc1=session_1mc[, .( V3 =mean(V3)), by = .(sub)]
setDT(session_2mc)
meanxmc2=session_2mc[, .( V3 =mean(V3)), by = .(sub)]
######################################################
#####
## AVERAGE Y coordinates 
setDT(session_0mc)
meanymc0=session_0mc[, .( V4 =mean(V4)), by = .(sub)]
setDT(session_1mc)
meanymc1=session_1mc[, .( V4 =mean(V4)), by = .(sub)]
setDT(session_2mc)
meanymc2=session_2mc[, .( V4 =mean(V4)), by = .(sub)]

#############################################################################################
###############################################################################33 
############################################################################33
###########
#SUMFUNCTION 
#SUm TIME
setDT(session_0mc)
sumtmc0=session_0mc[, .( V1 =sum(V1)), by = .(sub)]
#####
setDT(session_1mc)
sumtmc1=session_1mc[, .( V1 =sum(V1)), by = .(sub)]
############
setDT(session_2mc)
sumtmc2=session_2mc[, .( V1 =sum(V1)), by = .(sub)]
#########################################################
##########SUm X COORDINATES 
setDT(session_0mc)
sumxmc0=session_0mc[, .( V3 =sum(V3)), by = .(sub)]
setDT(session_1mc)
sumxmc1=session_1mc[, .( V3 =sum(V3)), by = .(sub)]
setDT(session_2mc)
sumxmc2=session_2mc[, .( V3 =sum(V3)), by = .(sub)]
######################################################
#####
## SUM Y coordinates 
setDT(session_0mc)
sumymc0=session_0mc[, .( V4 =sum(V4)), by = .(sub)]
setDT(session_1mc)
sumymc1=session_1mc[, .( V4 =sum(V4)), by = .(sub)]
setDT(session_2mc)
sumymc2=session_2mc[, .( V4 =sum(V4)), by = .(sub)]


########################################################################################################

#############################################################################################
#######################################################################################33
### Aggregation
########Aggregation the differnece 
#mean of diffx
meandiffxmc0=aggregate(V3 ~ sub , FUN =mean, data=diffxmc0)
meandiffxmc1=aggregate(V3 ~ sub , FUN =mean, data=diffxmc1)
meandiffxmc2=aggregate(V3 ~ sub , FUN =mean, data=diffxmc2)
##############################333
###mean of difft
meandifftmc0=aggregate(V1 ~ sub , FUN =mean, data=mc0)
meandifftmc1=aggregate(V1 ~ sub , FUN =mean, data=mc1)
meandifftmc2=aggregate(V1 ~ sub , FUN =mean, data=mc2)
####mean of diffy
meandiffymc0=aggregate(V4 ~ sub , FUN =mean, data=diffymc0)
meandiffymc1=aggregate(V4 ~ sub , FUN =mean, data=diffymc1)
meandiffymc2=aggregate(V4 ~ sub , FUN =mean, data=diffymc2)
#########################################################################333
########################33333
#Aggregation the scaled values
#
#0- aggregate the scales of X coordinates

scalexmc0=aggregate(V3 ~ sub , FUN =mean, data=scalexmc_0)
scalexmc1=aggregate(V3 ~ sub , FUN =mean, data=scalexmc_1)
scalexmc2=aggregate(V3 ~ sub , FUN =mean, data=scalexmc_2)

##########
#1- aggregate the scales of Y coordinates

scaleymc0=aggregate(V4 ~ sub , FUN =mean, data=scaleymc_0)
scaleymc1=aggregate(V4 ~ sub , FUN =mean, data=scaleymc_1)
scaleymc2=aggregate(V4 ~ sub , FUN =mean, data=scaleymc_2)
#####################################################333
############
#2- aggregate the scales of Times 

scaletmc0=aggregate(V1 ~ sub , FUN =mean, data=scaletmc_0)
scaletmc1=aggregate(V1 ~ sub , FUN =mean, data=scaletmc_1)
scaletmc2=aggregate(V1 ~ sub , FUN =mean, data=scaletmc_2)

###############################################
######################################################3
#Length of the curve

lengthofcurvesmc0=sqrt((diffxmc0$V3)^2+((diffymc0$V4)^2))
lengthofcurvesmc0n=cbind(diffxmc0[,],lengthofcurvesmc0)
########
lengthofcurvesmc1=sqrt((diffxmc1$V3)^2+((diffymc1$V4)^2))
lengthofcurvesmc1n=cbind(diffxmc1[,],lengthofcurvesmc1)
########
lengthofcurvesmc2=sqrt((diffxmc2$V3)^2+((diffymc2$V4)^2))
lengthofcurvesmc2n=cbind(diffxmc2[,],lengthofcurvesmc2)

################################################################################################3
##############################
###############speed 
speedmc0=lengthofcurvesmc0/mc0$V1
speedmc0n=cbind(mc0[,],speedmc0)
###
speedmc1=lengthofcurvesmc1/mc1$V1
speedmc1n=cbind(mc1[,],speedmc1)
###
speedmc2=lengthofcurvesmc2/mc2$V1
speedmc2n=cbind(mc2[,],speedmc2)
####################################################################
#########################3
########Acceleration 
Accelerationmc0=speedmc0/mc0$V1
Accelerationmc0n=cbind(mc0[,],Accelerationmc0)
######
Accelerationmc1=speedmc1/mc1$V1
Accelerationmc1n=cbind(mc1[,],Accelerationmc1)
############
Accelerationmc2=speedm2/mc2$V1
Accelerationmc2n=cbind(mc2[,],Accelerationmc2)
##############################################################3
#################################33
###################################################
##### Direction angle

anglemc0=diffymc0$V4/diffxmc0$V3
anglemc1=diffymc1$V4/diffxmc1$V3
angelmc2=diffymc2$V4/diffxmc2$V3
######################################################
#####
#Horizontal Velocity

vhmc0=diffxmc0$V3/mc0$V1
vhmc1=diffxmc1$V3/mc1$V1
vhmc2=diffxmc2$V3/mc2$V1
#######################################################3
#####
#Vertical Velocity

vvmc0=diffymc0$V4/mc0$V1
vvmc1=diffymc1$V4/mc1$V1
vvmc2=diffymc2$V4/mc2$V1
#############################################################
#######Slope angle of the tangent
mmc0=atan(session_0mc$V4/session_0mc$V3)
mmc1=atan(session_1mc$V4/session_1mc$V3)
mmc2=atan(session_2mc$V4/session_2mc$V3)

####################################################################33
######################3
#Curvature
cmc0=diff(mmc0)/diff(lengthofcurvesmc0)
cmc1=diff(mmc1)/diff(lengthofcurvesmc1)
cmc2=diff(mmc2)/diff(lengthofcurvesmc2)

################################333
####Aggregate the Horizontal Velocity
hv0nmc=cbind(vhmc0,mc0)
hv1nmc=cbind(vhmc1,mc1)
hv2nmc=cbind(vhmc2,mc2)
################################
HVmean0mc=aggregate( vhmc0~ sub , FUN =mean, data=hv0nmc)
HVmean1mc=aggregate(vhmc1 ~ sub , FUN =mean, data=hv1nmc)
HVmean2mc=aggregate(vhmc2 ~ sub , FUN =mean, data=hv2nmc)
#######################################################3
#####
#Vertical Velocity
vv0nmc=cbind(vvmc0,mc0)
vv1nmc=cbind(vvmc1,mc1)
vv2nmc=cbind(vvmc2,mc2)
################################
vvmean0mc=aggregate( vvmc0~ sub , FUN =mean, data=vv0nmc)
vVmean1mc=aggregate(vvmc1 ~ sub , FUN =mean, data=vv1nmc)
vVmean2mc=aggregate(vvmc2 ~ sub , FUN =mean, data=vv2nmc)
#######################################################
#######Slope angle of the tangent


mmc0n=cbind(mmc0,session_0mc)
mmc1n=cbind(mmc1,session_1mc)
mmc2n=cbind(mmc2,session_2mc)
########################################
mcmmean0=aggregate(mmc0 ~  sub, FUN =mean, data=mmc0n)
mcmmean1=aggregate(mmc1 ~ sub  , FUN =mean, data=mmc1n)
mcmmean2=aggregate(mmc2 ~ sub  , FUN =mean, data=mmc2n)
#####################################################333
cmc0n=cbind(cmc0,lengthofcurvesmc0n)
cmc1n=cbind(cmc1,lengthofcurvesmc1n)
cmc2n=cbind(cmc2,lengthofcurvesmc2n)
####################################################
cmcmin0=aggregate(cmc0 ~  sub, FUN =min, data=cmc0n)
cmcmin1=aggregate(cmc1 ~ sub  , FUN =min, data=cmc1n)
cmcmin2=aggregate(cmc2 ~ sub  , FUN =min, data=cmc2n)



#######################################################################
####Aggregate the speed
speedmean0mc=aggregate(speedmc0 ~ sub , FUN =mean, data=speedmc0n)
speedmean1mc=aggregate(speedmc1 ~ sub , FUN =mean, data=speedmc1n)
speedmean2mc=aggregate(speedmc2 ~ sub , FUN =mean, data=speedmc2n)
#aggregate length
lengthmean0mc=aggregate(lengthofcurvesmc0~ sub , FUN =mean, data=lengthofcurvesmc0n)
lengthmean1mc=aggregate(lengthofcurvesmc1 ~ sub , FUN =mean, data=lengthofcurvesmc1n)
lengthmean2mc=aggregate(lengthofcurvesmc2 ~ sub , FUN =mean, data=lengthofcurvesmc2n)
#aggregate acceleration
accmean0mc=aggregate(Accelerationmc0~ sub , FUN =mean, data=Accelerationmc0n)
accmean1mc=aggregate(Accelerationmc1 ~ sub , FUN =mean, data=Accelerationmc1n)
accmean2mc=aggregate(Accelerationmc2 ~ sub , FUN =mean, data=Accelerationmc2n)


mouse.clickfeaturesb0=cbind( scalexmc0,scaleymc0, scaletmc0,standarddevmcx0,standarddevmcy0,standarddevmct0, meandifftmc0, meandiffxmc0, meandiffymc0,maxtmc0,maxxmc0,maxymc0,mintmc0,minxmc0,minymc0,meantmc0,meanxmc0,meanymc0,sumtmc0,
                             sumxmc0,sumymc0 ,speedmean0mc,lengthmean0mc,accmean0mc ,HVmean0mc,vvmean0mc,mcmmean0, cmcmin0         )

mouse.clickfeaturesb0=mouse.clickfeaturesb0[,-c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55)]
colnames(mouse.clickfeaturesb0)[2] <- "scalex_mc"
colnames(mouse.clickfeaturesb0)[3] <- "scaley_mc"
colnames(mouse.clickfeaturesb0)[4] <- "scalet_mc"
colnames(mouse.clickfeaturesb0)[5] <- "SD_X_mc"
colnames(mouse.clickfeaturesb0)[6] <- "SD_y_mc"
colnames(mouse.clickfeaturesb0)[7] <- "SD_t_mc"
colnames(mouse.clickfeaturesb0)[8] <- "Mean_diff_T_Mc"
colnames(mouse.clickfeaturesb0)[9] <- "Mean_diff_x_Mc"
colnames(mouse.clickfeaturesb0)[10] <- "Mean_diff_y_Mc"
colnames(mouse.clickfeaturesb0)[11] <- "max_t_mc"
colnames(mouse.clickfeaturesb0)[12] <- "max_x_mc"
colnames(mouse.clickfeaturesb0)[13] <- "max_y_mc"
colnames(mouse.clickfeaturesb0)[14] <- "min_t_mc"
colnames(mouse.clickfeaturesb0)[15] <- "min_x_mc"
colnames(mouse.clickfeaturesb0)[16] <- "min_y_mc"
colnames(mouse.clickfeaturesb0)[17] <- "mean_t_mc"
colnames(mouse.clickfeaturesb0)[18] <- "mean_x_mc"
colnames(mouse.clickfeaturesb0)[19] <- "mean_y_mc"
colnames(mouse.clickfeaturesb0)[20] <- "Sum_t_mc"
colnames(mouse.clickfeaturesb0)[21] <- "Sum_x_mc"
colnames(mouse.clickfeaturesb0)[22] <- "Sum_y_mc"
colnames(mouse.clickfeaturesb0)[23] <- "speed_mc"
colnames(mouse.clickfeaturesb0)[24] <- "length_mc"
colnames(mouse.clickfeaturesb0)[25] <- "acceleration_mc"
colnames(mouse.clickfeaturesb0)[26] <- "Horizontal_speed_mc"
colnames(mouse.clickfeaturesb0)[27] <- "Vertical_speed_mc"
colnames(mouse.clickfeaturesb0)[28] <- "tangent_angle_mc"
colnames(mouse.clickfeaturesb0)[29] <- "Curvature_mc"

write.csv(mouse.clickfeaturesb0,file = paste("mouse.clickfeaturesb0", i, ".csv") ,sep = " ")

#################################################################################3
##################################3333
#########session 1




mouse.clickfeaturesb1=cbind( scalexmc1,scaleymc1, scaletmc1,standarddevmcx1,standarddevmcy1,standarddevmct1, meandifftmc1, meandiffxmc1, meandiffymc1,maxtmc1,maxxmc1,maxymc1,mintmc1,minxmc1,minymc1,meantmc1,meanxmc1,meanymc1,sumtmc1,
                             sumxmc1,sumymc1,speedmean1mc,lengthmean1mc,accmean1mc ,HVmean1mc,vVmean1mc,mcmmean1, cmcmin1                      )

mouse.clickfeaturesb1=mouse.clickfeaturesb1[,-c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55)]
colnames(mouse.clickfeaturesb1)[2] <- "scalex_mc"
colnames(mouse.clickfeaturesb1)[3] <- "scaley_mc"
colnames(mouse.clickfeaturesb1)[4] <- "scalet_mc"
colnames(mouse.clickfeaturesb1)[5] <- "SD_X_mc"
colnames(mouse.clickfeaturesb1)[6] <- "SD_y_mc"
colnames(mouse.clickfeaturesb1)[7] <- "SD_t_mc"
colnames(mouse.clickfeaturesb1)[8] <- "Mean_diff_T_Mc"
colnames(mouse.clickfeaturesb1)[9] <- "Mean_diff_x_Mc"
colnames(mouse.clickfeaturesb1)[10] <- "Mean_diff_y_Mc"
colnames(mouse.clickfeaturesb1)[11] <- "max_t_mc"
colnames(mouse.clickfeaturesb1)[12] <- "max_x_mc"
colnames(mouse.clickfeaturesb1)[13] <- "max_y_mc"
colnames(mouse.clickfeaturesb1)[14] <- "min_t_mc"
colnames(mouse.clickfeaturesb1)[15] <- "min_x_mc"
colnames(mouse.clickfeaturesb1)[16] <- "min_y_mc"
colnames(mouse.clickfeaturesb1)[17] <- "mean_t_mc"
colnames(mouse.clickfeaturesb1)[18] <- "mean_x_mc"
colnames(mouse.clickfeaturesb1)[19] <- "mean_y_mc"
colnames(mouse.clickfeaturesb1)[20] <- "Sum_t_mc"
colnames(mouse.clickfeaturesb1)[21] <- "Sum_x_mc"
colnames(mouse.clickfeaturesb1)[22] <- "Sum_y_mc"
colnames(mouse.clickfeaturesb1)[23] <- "speed_mc"
colnames(mouse.clickfeaturesb1)[24] <- "length_mc"
colnames(mouse.clickfeaturesb1)[25] <- "acceleration_mc"
colnames(mouse.clickfeaturesb1)[26] <- "Horizontal_speed_mc"
colnames(mouse.clickfeaturesb1)[27] <- "Vertical_speed_mc"
colnames(mouse.clickfeaturesb1)[28] <- "tangent_angle_mc"
colnames(mouse.clickfeaturesb1)[29] <- "Curvature_mc"


write.csv(mouse.clickfeaturesb1,file = paste("mouse.clickfeaturesb1", i, ".csv") ,sep = " ")

#session 2
#############################################################################################
####################################################################################

mouse.clickfeaturesb2=cbind( scalexmc2,scaleymc2, scaletmc2,standarddevmcx2,standarddevmcy2,standarddevmct2, meandifftmc2, meandiffxmc2, meandiffymc2,maxtmc2,maxxmc2,maxymc2,mintmc2,minxmc2,minymc2,meantmc2,meanxmc2,meanymc2,sumtmc2,
                             sumxmc2,sumymc2 ,speedmean1mc,lengthmean1mc,accmean1mc ,HVmean2mc,vVmean2mc,mcmmean2, cmcmin2              )

mouse.clickfeaturesb2=mouse.clickfeaturesb2[,-c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55)]
colnames(mouse.clickfeaturesb2)[2] <- "scalex_mc"
colnames(mouse.clickfeaturesb2)[3] <- "scaley_mc"
colnames(mouse.clickfeaturesb2)[4] <- "scalet_mc"
colnames(mouse.clickfeaturesb2)[5] <- "SD_X_mc"
colnames(mouse.clickfeaturesb2)[6] <- "SD_y_mc"
colnames(mouse.clickfeaturesb2)[7] <- "SD_t_mc"
colnames(mouse.clickfeaturesb2)[8] <- "Mean_diff_T_Mc"
colnames(mouse.clickfeaturesb2)[9] <- "Mean_diff_x_Mc"
colnames(mouse.clickfeaturesb2)[10] <- "Mean_diff_y_Mc"
colnames(mouse.clickfeaturesb2)[11] <- "max_t_mc"
colnames(mouse.clickfeaturesb2)[12] <- "max_x_mc"
colnames(mouse.clickfeaturesb2)[13] <- "max_y_mc"
colnames(mouse.clickfeaturesb2)[14] <- "min_t_mc"
colnames(mouse.clickfeaturesb2)[15] <- "min_x_mc"
colnames(mouse.clickfeaturesb2)[16] <- "min_y_mc"
colnames(mouse.clickfeaturesb2)[17] <- "mean_t_mc"
colnames(mouse.clickfeaturesb2)[18] <- "mean_x_mc"
colnames(mouse.clickfeaturesb2)[19] <- "mean_y_mc"
colnames(mouse.clickfeaturesb2)[20] <- "Sum_t_mc"
colnames(mouse.clickfeaturesb2)[21] <- "Sum_x_mc"
colnames(mouse.clickfeaturesb2)[22] <- "Sum_y_mc"
colnames(mouse.clickfeaturesb2)[23] <- "speed_mc"
colnames(mouse.clickfeaturesb2)[24] <- "length_mc"
colnames(mouse.clickfeaturesb2)[25] <- "acceleration_mc"
colnames(mouse.clickfeaturesb2)[26] <- "Horizontal_speed_mc"
colnames(mouse.clickfeaturesb2)[27] <- "Vertical_speed_mc"
colnames(mouse.clickfeaturesb2)[28] <- "tangent_angle_mc"
colnames(mouse.clickfeaturesb2)[29] <- "Curvature_mc"


write.csv(mouse.clickfeaturesb2,file = paste("mouse.clickfeaturesb2", i, ".csv") ,sep = " ")


mouseclick_sessions=rbind(mouse.clickfeaturesb0,mouse.clickfeaturesb1,mouse.clickfeaturesb2)
write.csv(mouseclick_sessions,file = paste("allmouseclick_sessions", i, ".csv") ,sep = " ")
########################################################################################################
##########################################

lengthofcurvesg=sqrt((diffxg$V3)^2+((diffyg$V4)^2))

