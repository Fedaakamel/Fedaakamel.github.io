library(stringr)
library(zoo)
library(car)
library(lattice)
library(wavelets)
datafolder="C:/Users/fedaaelderdesawe/Dropbox/fedaalaptop/thesis/dataset_a/labeled/"

files=list.files(datafolder)
files
listforX=list()

#scale
for(i in 1:length(files)){
  thissubject= read.table(paste0(datafolder,files[i]),header=FALSE)
  alldata=str_sub(files,start=-4) 
  data=cbind( thissubject[i], alldata)
  listforX[[i]]=thissubject
}

alldata

data= read.table(paste0(datafolder,files[1]),header=FALSE, sep="\t")
write.csv(data, file = paste("data",  ".csv") ,sep = " " ) 
name = st
rsplit(files[1], "_")
data$name <- files[1]
data$name1 <- name[[1]][1]
data$name2 <- name[[1]][2]
data$name3 <- name[[1]][3]
data$name4 <- name[[1]][4]

for(i in 2:length(files)){
  newdata = read.table(paste0(datafolder,files[i]),header=FALSE)
  name = strsplit(files[i], "_")
  newdata$name <- files[i]
  newdata$name1 <- name[[1]][1]
  newdata$name2 <- name[[1]][2]
  newdata$name3 <- name[[1]][3]
  newdata$name4 <- name[[1]][4]
  data = rbind(data,newdata)
}

fin=data[,c(-5,-6,-7)]
write.csv(fin, file = paste("subjects", i, ".csv") ,sep = " ")
subject_m=fin[fin[,2]=="M",]
fin=read.csv("C:/Users/fedaaelderdesawe/Google Drive/subjects 81 .csv")
plot.ts()
#timexcor=subject_m[,c(V1,V3,name4)]
#subject.x=zoo(subject_m$V3,order.by=subject_m$V1)
#subjectts=as.data.frame(subject.x)

#DTWforx=dwt(subject_m$V3, filter="la8", n.levels=2, boundary="periodic", fast=TRUE)
#fft|(features2)
library(data.table)
library(clusterSim)
setDT(fin)

subject_mc=fin[fin[,3]=="MC",]
subject_G=fin[fin[,3]=="G",]
subject_m=fin[fin[,3]=="M",]


plot.ts(subject_mc$V3)
plot.ts(subject_mc$V4)
##################
#mouse
setDT(subject_m)
#####scale 
scaley=subject_m[, .( V4 =data.Normalization(V4,type="n1")), by = .(name4)]
scalex=subject_m[, .( V3 =data.Normalization(V3,type="n1")), by = .(name4)]
scalet=subject_m[, .( V1 =data.Normalization(V3,type="n1")), by = .(name4)]
##eye
setDT(subject_G)
subject_G=fin[fin[,2]=="G",]
scalexg=subject_G[, .( V3 =data.Normalization(V3,type="n1")), by = .(name4)]
scaleyg=subject_G[, .( V4 =data.Normalization(V4,type="n1")), by = .(name4)]
scaletg=subject_G[, .( V1 =data.Normalization(V1,type="n1")), by = .(name4)]

scalexmc=subject_mc[, .( V3 =data.Normalization(V3,type="n1")), by = .(name4)]
scaleymc=subject_mc[, .( V4 =data.Normalization(V4,type="n1")), by = .(name4)]
scaletmc=subject_mc[, .( V1 =data.Normalization(V1,type="n1")), by = .(name4)]
######diff
setDT(subject_m)
mm=subject_m[, .( V1 =diff(V1)), by = .(name4)]
diffx=subject_m[, .( V3 =diff(V3)), by = .(name4)]
diffy=subject_m[, .( V4 =diff(V4)), by = .(name4)]
####
setDT(subject_G)
gg=subject_G[, .( V1 =diff(V1)), by = .(name4)]
diffxg=subject_G[, .( V3 =diff(V3)), by = .(name4)]
diffyg=subject_G[, .( V4 =diff(V4)), by = .(name4)]
#######
setDT(subject_mc)
mc=subject_mc[, .( V1 =diff(V1)), by = .(name4)]
diffxmc=subject_mc[, .( V3 =diff(V3)), by = .(name4)]
diffymc=subject_mc[, .( V4 =diff(V4)), by = .(name4)]
###
standarddevmy=subject_m[, .(V4 =sd(V4)), by = .(name4)]
standarddevmx=subject_m[, .(V4 =sd(V3)), by = .(name4)]

standarddevgy=subject_G[, .(V4 =sd(V4)), by = .(name4)]
standarddevgx=subject_G[, .(V4 =sd(V3)), by = .(name4)]

standarddevmcy=subject_mc[, .(V4 =sd(V4)), by = .(name4)]
standarddevmcx=subject_mc[, .(V4 =sd(V3)), by = .(name4)]

dataaggxmeanmc=aggregate(V3 ~ name4 , FUN =mean, data=subject_mc)
dataaggymeanmc=aggregate(V4 ~ name4 , FUN =mean, data=subject_mc)
dataaggtmeanmc=aggregate(V1 ~ name4 , FUN =mean, data=subject_mc)

dataaggxmeanm=aggregate(V3 ~ name4 , FUN =mean, data=subject_m)
dataaggymeanm=aggregate(V4 ~ name4 , FUN =mean, data=subject_m)
dataaggtmeanm=aggregate(V1 ~ name4 , FUN =mean, data=subject_m)

dataaggxmeang=aggregate(V3 ~ name4 , FUN =mean, data=subject_G)
dataaggymeang=aggregate(V4 ~ name4 , FUN =mean, data=subject_G)
dataaggtmeang=aggregate(V1 ~ name4 , FUN =mean, data=subject_G)

dataaggyrange0=aggregate(V4 ~ name4 , FUN =range, data=scaley0)


########################################################################################
dataaggt=aggregate(V1 ~ name4 , FUN =diff, data=subject_m)
##########diff t
ggplot(diffx,aes(x=name4,y=V3))+geom_point(aes(color="mouse delta X "))+
  geom_point(data=diffxg,aes(color="eye delta X"))+geom_point(data=diffxmc,aes(color="mouse click delta X"))+
  labs(x=" subjects", y="   Difference X coordinates ", title="Difference  X coordinates for all subjects")

#########diff x
ggplot(diffx,aes(x=name4,y=V3))+geom_point(aes(color="mouse delta X "))+
  geom_point(data=diffxg,aes(color="eye delta X"))+geom_point(data=diffxmc,aes(color="mouse click delta X"))+
  labs(x=" subjects", y="   Difference X coordinates ", title="Difference  X coordinates for all subjects")

###################diff y
ggplot(diffy,aes(x=name4,y=V4))+geom_point(aes(color="mouse delta y "))+
  geom_point(data=diffyg,aes(color="eye delta y"))+geom_point(data=diffymc,aes(color="mouse click delta y"))+
  labs(x=" subjects", y="   Difference y coordinates ", title="Difference  y coordinates movements for all subjects")


ggplot(mm,aes(x=name4,y=V1))+geom_point(aes(color="Mouse Time difference"))+
  geom_point(data=gg,aes(color="Eye Time difference"))+geom_point(data=mc,aes(color="Mouse click Time difference"))+
  labs(x=" subjects", y="   Difference movements time  ", title="Difference time movements   for all subjects")


ggplot(scalet,aes(x=name4,y=V1))+geom_point(aes(color="Mouse Time movements"))+
  geom_point(data=scaletg,aes(color="Eye Time movements"))+geom_point(data=scaletmc,aes(color="Mouse click Time difference"))+
  labs(x=" subjects", y="  Time movements   ", title=" Time movements   for all subjects")
#######standard deviation for y
ggplot(standarddevmy,aes(x=name4,y=V4))+geom_point(aes(color="Average Standard deviation for mouse"))+
  geom_point(data=standarddevgy,aes(color="Average standard deviation for eye "))+geom_point(data=standarddevmcy,aes(color="Average standard deviation for mouse click "))+
  labs(x=" subjects", y=" Average standard deviation for y coordinates   ", title=" Average standard deviation for y coordinates movements for all subjects")
#########average time 
ggplot(dataaggtmeanm,aes(x=name4,y=V1))+geom_point(aes(color="Average time for mouse movements"))+
  geom_point(data=dataaggtmeang,aes(color="Average time for eye  movements"))+geom_point(data=dataaggtmeanmc,aes(color="Average time for mouse click  movements"))+
  labs(x=" subjects", y="  Average time movements   ", title=" Average time movements for all subjects")

######average x coordinates
ggplot(dataaggxmeanm,aes(x=name4,y=V3))+geom_point(aes(color="The average Mouse  X coordinates movements"))+
  geom_point(data=dataaggxmeang,aes(color="The average eye  X coordinates movements"))+geom_point(data=dataaggxmeanmc,aes(color="The average mouse click X coordinates movements"))+
  labs(x=" subjects", y="   The average X coordinates movements ", title="The average  X coordinates movements  for all subjects")
########## average y coordinates 
ggplot(dataaggxmeanm,aes(x=name4,y=V3))+geom_point(aes(color="The average Mouse  X coordinates movements"))+
  geom_point(data=dataaggxmeang,aes(color="The average eye  X coordinates movements"))+geom_point(data=dataaggxmeanmc,aes(color="The average mouse click X coordinates movements"))+
  labs(x=" subjects", y="   The average X coordinates movements ", title="The average  X coordinates movements  for all subjects")
##########################################


#setDT(subject_G)
#
#scaleing
deltax=subject_m$V3-subject_G$V3
deltay=subject_m$V4-subject_G$V4
distance=sqrt((deltax)^2+((deltay)^2))
session_0m= subset( session_0m, select = -5 )
#session_0m=subject_m[,. (V1=V1,V3=V3,V4=V4,( name3 =0)), by = .(name4)]
session_0m=subject_m[subject_m[,5]=="0",]
session_1m=subject_m[subject_m[,5]=="1",]
session_2m=subject_m[subject_m[,5]=="2",]
#session_1m=subject_m[,. (V1=V1,V3=V3,V4=V4,( name3 =1)), by = .(name4)]
session_1m= subset( session_1m, select = -5 )
#session_2m=subject_m[,. (V1=V1,V3=V3,V4=V4,( name3 =2)), by = .(name4)]
session_2m= subset( session_2m, select = -5 )

#######################################
################################
########plots
library(ggplot2)
matplot(cbind(session_0m$V3,session_1m$V3,session_2m$V3),type="l",lwd=2,col=c("blue", "red","pink",xlab ="time",ylab = " mouse x coordinates between the three sessions  ",main="mouse x coordinates between the three sessions"))
df1<-session_0m
df2<-session_1m
df3<-session_2m

ggplot(df1,aes(x=V3,y=V4))+geom_line(aes(color="session 0"))+
  geom_line(data=df2,aes(color="session 1"))+geom_line(data=df3,aes(color="session 2"))+
  labs(x="X coordinates ", y=" y coordinates ", title="mouse x coordinates vs. y coordinates for different sessions ")

ggplot(session_0m, aes(x=V3,y=V4), color="steelblue"+ geom_smooth(aes(x=V3, y=V4)))
ggplot(session_0m) + geom_point(aes(x=name4, y=V3, color=cut)) + geom_smooth(aes(x=name4, y=V3))
library(plotly)
p <- ggplot(data =session_0m, aes(x = V3, y =V4)) +
  geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
  geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)

(gg <- ggplotly(p))

#plots
# from the plot of the differnt sessions for y coordiantes we can say that there is similarty for them
matplot(,cbind(session_0m$V4,session_1m$V4,session_2m$V4),type="l",col=c(rainbow(3)),lwd=4, xlab ="Time", ylab = "y coordinates for all subjects",main=" The different of mouse  y coordinates between three sessions ")

matplot(,cbind(session_0m$V3,session_1m$V3,session_2m$V3),type="l",col=c(rainbow(3)),lwd=4, xlab ="Time", ylab = "y coordinates for all subjects",main=" The different of mouse  X coordinates between three sessions ")   
matplot(,cbind(session_0m$V1,session_1m$V1,session_2m$V1),type="l",col=c(rainbow(3)),lwd=2, xlab ="index", ylab = "time for all subjects",main=" The different of mouse times between three sessions ")   

###################################
#mouse vs. eye 

matplot(,cbind(subject_m$V3,subject_G$V3,subject_mc$V3),type="l",col=c("blue", "red", "brown"),lwd=3, xlab ="index", ylab = "X coordinates ",main="The different behavior of mouse and eye in x coordinates ")   
matplot(,cbind(subject_m$V3,subject_G$V3),type="l",col=c("blue", "red", "brown"),lwd=2, xlab ="index", ylab = "X coordinates ",main="The different behavior of mouse and eye in x coordinates ")   


matplot(subject_m$name4,cbind(subject_m$V3,subject_G$V3),type="l",col=c("blue", "red", "brown"),lwd=3, xlab ="index", ylab = "X coordinates ",main="The different behavior of mouse and eye in x coordinates ")   

plot(session_0m$V3,,col=c(rainbow(1)),lwd=2, xlab ="Time", ylab = "X coordinates for all subjects",main=" Mouse  X coordinates for the first session ")

plot(session_1m$V3,,col=c(rainbow(1)),lwd=2, xlab ="Time", ylab = "X coordinates for all subjects",main=" Mouse  X coordinates for the second session ")
plot(session_2m$V3,,col=c(rainbow(1)),lwd=2, xlab ="Time", ylab = "X coordinates for all subjects",main=" Mouse  X coordinates for the Third session ")


#################################################################################################################333
#normalization
setDT(session_0m)
scalex0=session_0m[, .( V3 =data.Normalization(V3,type="n1")), by = .(name4)]

scalex00=session_0m[, .( V3 =data.Normalization(V3,type="n4")), by = .(name4)]
setDT(session_1m)
scalex1=session_1m[, .( V3 =data.Normalization(V3,type="n1")), by = .(name4)]
scalex11=session_1m[, .( V3 =data.Normalization(V3,type="n4")), by = .(name4)]
setDT(session_2m)
scalex2=session_2m[, .( V3 =data.Normalization(V3,type="n1")), by = .(name4)]
scalex22=session_2m[, .( V3 =data.Normalization(V3,type="n4")), by = .(name4)]
#colnames(session_0m) <- c("sub","time", "xm","ym","session")
names(session_0m)
setDT(session_0m)
scaley0=session_0m[, .(V4=data.Normalization(V4,type="n1")), by = .(name4)]
scaley00=session_0m[, .(V4=data.Normalization(V4,type="n4")), by = .(name4)]
setDT(session_1m)
scaley1=session_0m[, .( V4  =data.Normalization(V4,type="n1")), by = .(name4)]
scaley11=session_0m[, .( V4  =data.Normalization(V4,type="n4")), by = .(name4)]
setDT(session_2m)
scaley2=session_0m[, .( V4  =data.Normalization(V4,type="n1")), by = .(name4)]
scaley22=session_0m[, .( V4  =data.Normalization(V4,type="n4")), by = .(name4)]
#################################################################################33
###############################3333
###############plots


 plot(scalex0$V3,scaley0$V4,lwd=2, xlab ="scaled x coordinates", ylab = " scaled Y coordinates ",main=" Mouse  X coordinates vs.Y coordinates  for the first session ",col="green")
 
 plot(scalex1$V3,scaley1$V4,lwd=2, xlab ="scaled x coordinates", ylab = " scaled Y coordinates ",main=" Mouse  X coordinates vs.Y coordinates  for the second session ",col="blue") 
 plot(scalex2$V3,scaley2$V4,lwd=2, xlab ="scaled x coordinates", ylab = " scaled Y coordinates ",main=" Mouse  X coordinates vs.Y coordinates  for the third  session ",col="blue") 
 
matplot(,cbind(scalex0$V3,scalex1$V3,scalex2$V3),type="l",col=c(rainbow(4)),lwd=3, xlab ="index", ylab = "Mouse  X coordinates",main=" The different of mouse X coordinates  between three sessions ")  
#############################
#time standarization
names(session_0m)
setDT(session_0m)
scalet0=session_0m[, .(V1=data.Normalization(V4,type="n1")), by = .(name4)]
scalet00=session_0m[, .(V1=data.Normalization(V4,type="n4")), by = .(name4)]
setDT(session_1m)
scalet1=session_0m[, .( V1  =data.Normalization(V4,type="n1")), by = .(name4)]
scalet11=session_0m[, .( V1  =data.Normalization(V4,type="n4")), by = .(name4)]
setDT(session_2m)
scalet2=session_0m[, .( V1  =data.Normalization(V4,type="n1")), by = .(name4)]
scalet22=session_0m[, .( V1  =data.Normalization(V4,type="n4")), by = .(name4)]

matplot(,cbind(scalet0$V1,scalet1$V1,scalet2$V1),type="l",col=c("blue", "red", "green"),lwd=3, xlab ="index", ylab = "time for all subjects",main=" The different of mouse times between three sessions ")  
####################################################3333
####################

sclaedlog0=aggregate(V3 ~ name4 , FUN =diff, data=scalex00)


##################
#aggregation 
#x
scalexm0=aggregate(V3 ~ name4 , FUN =mean, data=scalex0)
scalexm1=aggregate(V3 ~ name4 , FUN =mean, data=scalex1)
scalexm2=aggregate(V3 ~ name4 , FUN =mean, data=scalex2)
################################################################3333
###################
#######plots
matplot(,cbind(scalexm0$V3,scalexm1$V3,scalexm2$V3),type="l",col=c("blue", "red", "green"),lwd=3, xlab ="Subjects", ylab = " Mean of  Scaled Mouse  X coordinates for each subject",main="The mean of the scaled x coordinates for the three sessions ")  
matplot(,cbind(scaleym0$V4,scaleym1$V4,scaleym2$V4),type="p",col=c("blue", "red", "green"),lwd=3, xlab ="Subjects", ylab = " Mean of  Scaled Mouse  y coordinates for each subject",main=" The mean of the scaled y coordinates for the three sessions ")  
#########
scalexm00=aggregate(V3 ~ name4 , FUN =mean, data=scalex00)
scalexm11=aggregate(V3 ~ name4 , FUN =mean, data=scalex11)
scalexm22=aggregate(V3 ~ name4 , FUN =mean, data=scalex22)

matplot(,cbind(scalexm00$V3,scalexm11$V3,scalexm22$V3),type="l",col=c("blue", "red", "green"),lwd=3, xlab ="Subjects", ylab = " Mean of  Scaled Mouse  X coordinates for each subject",main="The mean of the scaled x coordinates for the three sessions ")  
#y
scaleym0=aggregate(V4 ~ name4 , FUN =mean, data=scaley0)
scaleym1=aggregate(V4 ~ name4 , FUN =mean, data=scaley1)
scaleym2=aggregate(V4 ~ name4 , FUN =mean, data=scaley2)

scaleym00=aggregate(V4 ~ name4 , FUN =mean, data=scaley00)
scaleym11=aggregate(V4 ~ name4 , FUN =mean, data=scaley11)
scaleym22=aggregate(V4 ~ name4 , FUN =mean, data=scaley22)
#time
scaletm00=aggregate(V1 ~ name4 , FUN =mean, data=scalet00)
scaletm11=aggregate(V1 ~ name4 , FUN =mean, data=scalet11)
scaletm22=aggregate(V1 ~ name4 , FUN =mean, data=scalet22)

standrization=cbind(scalexm00,scalexm11,scalexm22,scaleym00,scaleym11,scaleym22,scaletm00,scaletm11,scaletm22)

write.csv(standrization, file = paste("normalx_y_t", i, ".csv") ,sep = " ")
############33333
setDT(subject_m)
scaley=subject_m[, .( V4 =data.Normalization(V4,type="n1")), by = .(name4)]
scalex=subject_m[, .( V3 =data.Normalization(V3,type="n1")), by = .(name4)]

plot(scalex$V3,scaley$V4,lwd=1, xlab ="scaled x coordinates", ylab = " scaled Y coordinates ",main=" Scaled Mouse  X coordinates vs.Y coordinates ",col="blue")
 plot(scalex$V3, ylab = " scaled X coordinates ",main=" scaled Mouse  X coordinates  ",col="red")

#####################################

 eculid=cbind(scaley,scalex,distance,deltax,deltay)

###########################################################################
#standard deviation
setDT(session_0m)
standarddevm0=session_0m[, .( V3 =sd(V3)), by = .(name4)]
setDT(session_1m)
standarddevm1=session_1m[, .( V3 =sd(V3)), by = .(name4)]
setDT(session_2m)
standarddevm2=session_2m[, .( V3 =sd(V3)), by = .(name4)]

matplot(,cbind(standarddevm0$V3,standarddevm1$V3,standarddevm2$V3),type="l",col=c("blue", "red", "green"),lwd=3, xlab ="Subjects", ylab = " standard deviation   of  Mouse  X coordinates for each subject",main="The standard deviation of  x coordinates for the three sessions ")  

matplot(,cbind(subject_m$v3,subject_G$V3),type="l",col=c("blue", "red", "green"),lwd=3, xlab ="Subjects", ylab = " standard deviation   of  Mouse  X coordinates for each subject",main="The Euclidian distance between gaze and mouse  ")  
 plot(eculid$distance,col=c("blue"),lwd=3, xlab ="Subjects", ylab = " The Euclidian distance",main="The Euclidian distance between gaze and mouse  ")  
 plot(eculid$distance,col="blue", ylab = " The ec",main="The Euclidian distance between gaze and mouse  ")
 plot(eculid$deltax,col="red", ylab = "  X coordinates difference",main="X coordinates difference between gaze and mouse  ")
 plot(eculid$deltay,col="green", ylab = "  Y coordinates difference",main="Y coordinates difference between gaze and mouse  ")
 #######################
 ggplot( eculid,aes(x=name4,y=distance))+geom_point(aes(color="distnce"))+
  geom_point(data=ecluid,aes(color="delta X",y=deltax)))+geom_point(data=eculid,aes(color="delta Y",y=deltay))+
  labs(x=" subjects", y=" distance ", title="The standard deviation of  x coordinates for the three sessions")

ggplot(standarddevm0,aes(x=name4,y=V3))+geom_point(aes(color="session 0"))+
  geom_point(data=standarddevm1,aes(color="session 1"))+geom_point(data=standarddevm2,aes(color="session 2"))+
  labs(x=" subjects", y=" standard deviation of X coordinates ", title="The standard deviation of  x coordinates for the three sessions")

#########
#ym
setDT(session_0m)
standarddevmy0=session_0m[, .(V4 =sd(V4)), by = .(name4)]
setDT(session_1m)
standarddevmy1=session_1m[, .( V4 =sd(V4)), by = .(name4)]
setDT(session_2m)
standarddevmy2=session_2m[, .( V4=sd(V4)), by = .(name4)]

mousefeatures0=cbind(session_0m$V1,session_0m$V3,session_0m$V4,scalexm0,scaleym0,standarddevm0,standarddevmy0,dataaggmxmax0,)
###########################################################################3333
##############################################
#######################33plots
ggplot(standarddevmy0,aes(x=name4,y=V4))+geom_point(aes(color="session 0"))+
  geom_point(data=standarddevmy1,aes(color="session 1"))+geom_point(data=standarddevmy2,aes(color="session 2"))+
  labs(x=" subjects", y=" standard deviation of y coordinates ", title="The standard deviation of  y coordinates for the three sessions")

############################################################################
#anglepersub=subject_m[, .( V3 =angle(V3,V4)), by = .(name4)]

#################################### angle
listforangle=list()
angle <- function(x,y){
  dot.prod <- x%*%y 
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))
  as.numeric(theta)
}

##########################################################33
######## Angle session 0
for(i in 1:nrow(session_0m)){
  
  anglexy0= angle(session_0m$V3,session_0m$V4)
corrleation0=cor(session_0m$V3,session_0m$V4)
  
  listforangle[[i]]=anglexy0

} 
########################################3
######### angle session1

for(i in 1:nrow(session_0m)){
  
  anglexy= angle(session_1m$xm,session_0m$ym)
  corrleation=cor(session_0m$xm,session_0m$ym)
  
  listforangle[[i]]=anglexy
  
} 
anglexy=as.vector(listforangle)
write.csv(anglexy, file = paste("angle", i, ".csv") ,sep = " ")
anglexy=as.vector(anglexy)
print(anglexy)
print(corrleation)
###############
####################################################333
####################33
#eye

subject_G=fin[fin[,2]=="G",]
setDT(subject_G)

session_0g=subject_G[subject_G[,5]=="0",]
session_1g=subject_G[subject_G[,5]=="1",]
session_2g=subject_G[subject_G[,5]=="2",]
session_0g= subset( session_0g, select = -5 )


################################
#######################
#plots

matplot(cbind(session_0g$V3,session_1g$V3,session_2g$V3),type="l",lwd=3,col=c("blue", "red","pink",xlab ="time",ylab = " Eye x coordinates between the three sessions  ",main="Eye x coordinates between the three sessions"))

ggplot(session_0g,aes(x=name4,y=V3))+geom_point(aes(color="session 0"))+
  geom_point(data=session_1g,aes(color="session 1"))+geom_point(data=session_2g,aes(color="session 2"))+
  labs(x=" subjects", y="  X coordinates ", title="Eye  x coordinates for the three sessions")

###################################
###################################################
ggplot(session_0g,aes(x=name4,y=V4))+geom_point(aes(color="session 0"))+
  geom_point(data=session_1g,aes(color="session 1"))+geom_point(data=session_2g,aes(color="session 2"))+
  labs(x=" subjects", y="  Y coordinates ", title="Eye  Y coordinates for the three sessions")

ggplot(session_0g,aes(x=V1,y=V3))+geom_point(aes(color="session 0"))+
  geom_point(data=session_1g,aes(color="session 1"))+geom_point(data=session_2g,aes(color="session 2"))+
  labs(x=" time", y="  X coordinates ", title="Eye  X coordinates for the three sessions")


ggplot(session_0g,aes(x=V1,y=V4))+geom_point(aes(color="session 0"))+
  geom_point(data=session_1g,aes(color="session 1"))+geom_point(data=session_2g,aes(color="session 2"))+
  labs(x=" Time", y="  Y coordinates ", title="Eye  Y coordinates for the three sessions")

ggplot(session_0g,aes(x=V3,y=V4))+geom_point(aes(color="session 0"))+
  geom_point(data=session_1g,aes(color="session 1"))+geom_point(data=session_2g,aes(color="session 2"))+
  labs(x=" X coordinates", y="  Y coordinates ", title="Eye   X coorinates vs. Y coordinates for the three sessions")



#session_0g=subject_G[,. (V1=V1,V3=V3,V4=V4,( name3 =0)), by = .(name4)]
#session_1g=subject_G[,. (V1=V1,V3=V3,V4=V4,( name3 =1)), by = .(name4)]
session_1g= subset( session_1g, select = -5 )
#session_2g=subject_G[,. (V1=V1,V3=V3,V4=V4,( name3 =2)), by = .(name4)]
session_2g= subset( session_2g, select = -5 )
setDT(session_0g)
scalexg0=session_0g[, .( V3 =data.Normalization(V3,type="n1")), by = .(name4)]
scalexg00=session_0g[, .( V3 =data.Normalization(V3,type="n4")), by = .(name4)]
setDT(session_1g)
scalexg1=session_1g[, .( V3 =data.Normalization(V3,type="n1")), by = .(name4)]
scalexg11=session_1g[, .( V3 =data.Normalization(V3,type="n4")), by = .(name4)]
setDT(session_2g)
scalexg2=session_2g[, .( V3 =data.Normalization(V3,type="n1")), by = .(name4)]
scalexg22=session_2g[, .( V3 =data.Normalization(V3,type="n4")), by = .(name4)]
#############y scaling
setDT(session_0g)
scaleyg0=session_0g[, .( V4 =data.Normalization(V4,type="n1")), by = .(name4)]
scaleyg00=session_0g[, .( V4 =data.Normalization(V4,type="n4")), by = .(name4)]
setDT(session_1g)
scaleyg1=session_1g[, .( V4 =data.Normalization(V4,type="n1")), by = .(name4)]
scaleyg11=session_1g[, .( V4 =data.Normalization(V4,type="n4")), by = .(name4)]
setDT(session_2g)
scaleyg2=session_2g[, .( V4 =data.Normalization(V4,type="n1")), by = .(name4)]
scaleyg22=session_2g[, .( V4 =data.Normalization(V4,type="n4")), by = .(name4)]
###time
setDT(session_0g)
scaletg00=session_0g[, .( V1 =data.Normalization(V4,type="n4")), by = .(name4)]
setDT(session_1g)
scaletg11=session_1g[, .( V1 =data.Normalization(V4,type="n4")), by = .(name4)]
setDT(session_2g)
scaletg22=session_2g[, .( V1 =data.Normalization(V4,type="n4")), by = .(name4)]
#################################################################################
############
########scales plots
ggplot(scalexg0,aes(x=name4,y=V3))+geom_point(aes(color="session 0"))+
  geom_point(data=scalexg1,aes(color="session 1"))+geom_point(data=scalexg2,aes(color="session 2"))+
  labs(x=" subjects", y="  scaled X coordinates ", title=" Scaled Eye  x coordinates for the three sessions")

matplot(,cbind(scalexg00$V3,scalexg11$V3,scalexg22$V3),type="l",col=c("blue", "red", "green"),lwd=3, xlab ="index", ylab = "Eye X coordinates ",main="Eye  x coordinates for the three sessions ")  

matplot(,cbind(scaleyg0$V4,scaleyg1$V4,scaleyg2$V4),type="l",col=c("blue", "red", "green"),lwd=3, xlab ="index", ylab = "Eye Y coordinates ",main="Eye Y coordinates for the three sessions ")  

matplot(,cbind(scaletg00$V1,scaletg11$V1,scaletg22$V1),type="l",col=c("blue", "red", "green"),lwd=3, xlab ="index", ylab = "Time for eye gaze  ",main="Time gaze for  the three sessions ")  
#standarization aggregation

scalexg00=aggregate(V3 ~ name4 , FUN =mean, data=scalexg00)
scalexg11=aggregate(V3 ~ name4 , FUN =mean, data=scalexg11)
scalexg22=aggregate(V3 ~ name4 , FUN =mean, data=scalexg22)

#y
scaleyg00=aggregate(V4 ~ name4 , FUN =mean, data=scaleyg00)
scaleyg11=aggregate(V4 ~ name4 , FUN =mean, data=scaleyg11)
scaleyg22=aggregate(V4 ~ name4 , FUN =mean, data=scaleyg22)
###################
#time
scaletg00=aggregate(V1 ~ name4 , FUN =mean, data=scaletg00)
scaletg11=aggregate(V1 ~ name4 , FUN =mean, data=scaletg11)
scaletg22=aggregate(V1 ~ name4 , FUN =mean, data=scaletg22)


ggplot(scalexg00,aes(x=name4,y=V3))+geom_point(aes(color="session 0"))+
  geom_point(data=scalexg1,aes(color="session 1"))+geom_point(data=scalexg2,aes(color="session 2"))+
  labs(x=" subjects", y="  scaled X coordinates ", title=" Scaled Eye  x coordinates for the three sessions")

standarizedx_y_t=cbind(scalexg00,scalexg11,scalexg22,scaleyg00,scaleyg11,scaleyg22,scaletg00,scaletg11,scaletg22)
write.csv(standarizedx_y_t, file = paste("standarizedx_y_t_g", i, ".csv") ,sep = " ")


scaleyg=subject_G[, .( V4 =data.Normalization(V4,type="n1")), by = .(name4)]
#################
scalexg=subject_G[, .( V3 =data.Normalization(V3,type="n1")), by = .(name4)]
scaleyg=subject_G[, .( V4 =data.Normalization(V4,type="n1")), by = .(name4)]
scaletg=subject_G[, .( V1 =data.Normalization(V1,type="n1")), by = .(name4)]

#############plots

ggplot(scalexg,aes(x=name4,y=V3))+geom_point(aes(color="Gaze X coordinates"))+
  geom_point(data=scalex,aes(color="mouse X coordinates"))+geom_point(data=scalexmc,aes(color="mouse click X coordinates"))+
labs(x=" subjects", y="  scaled X coordinates ", title=" Scaled x coordinates for eye and mouse ")

ggplot(scaleyg,aes(x=name4,y=V4))+geom_point(aes(color="Gaze Y coordinates"))+
  geom_point(data=scaley,aes(color="mouse y coordinates"))+geom_point(data=scaleymc,aes(color="mouse click Y coordinates"))+
  labs(x=" subjects", y="  scaled Y coordinates ", title=" Scaled Y coordinates for eye and mouse ")



##################
################################################################33
###############################
#mouse click scaling
subject_mc=fin[fin[,2]=="MC",]
setDT(subject_mc)
session_0mc=subject_mc[subject_mc[,5]=="0",]
session_1mc=subject_mc[subject_mc[,5]=="1",]
session_2mc=subject_mc[subject_mc[,5]=="2",]
#session_0mc=subject_mc[,. (V1=V1,V3=V3,V4=V4,( name3 =0)), by = .(name4)]
session_0mc= subset( session_0mc, select = -5 )
#session_1mc=subject_mc[,. (V1=V1,V3=V3,V4=V4,( name3 =1)), by = .(name4)]
session_1mc= subset( session_1mc, select = -5 )
#session_2mc=subject_mc[,. (V1=V1,V3=V3,V4=V4,( name3 =2)), by = .(name4)]
session_2mc= subset( session_2mc, select = -5 )

########scaling

#x
setDT(session_0mc)
scalex0mc=session_0mc[, .( V3 =data.Normalization(V3,type="n1")), by = .(name4)]
scalex00mc=session_0mc[, .( V3 =data.Normalization(V3,type="n4")), by = .(name4)]

setDT(session_1mc)
scalex1mc=session_1mc[, .( V3 =data.Normalization(V3,type="n1")), by = .(name4)]
scalex11mc=session_1mc[, .( V3 =data.Normalization(V3,type="n4")), by = .(name4)]
setDT(session_2mc)
scalex2mc=session_2mc[, .( V3 =data.Normalization(V3,type="n1")), by = .(name4)]
scalex22mc=session_2mc[, .( V3 =data.Normalization(V3,type="n4")), by = .(name4)]
###
setDT(session_0mc)
scaley0mc=session_0mc[, .( V4 =data.Normalization(V4,type="n1")), by = .(name4)]
scaley00mc=session_0mc[, .( V4 =data.Normalization(V4,type="n4")), by = .(name4)]
setDT(session_1mc)
scaley1mc=session_1mc[, .( V4 =data.Normalization(V4,type="n1")), by = .(name4)]
scaley11mc=session_1mc[, .( V4 =data.Normalization(V4,type="n4")), by = .(name4)]
setDT(session_2mc)
scaley2mc=session_2mc[, .( V4 =data.Normalization(V4,type="n1")), by = .(name4)]
scaley22mc=session_2mc[, .( V4 =data.Normalization(V4,type="n4")), by = .(name4)]
scalex0mc,scaley0m
#############
setDT(subject_mc)
scalexmc=subject_mc[, .( V3 =data.Normalization(V3,type="n4")), by = .(name4)]
scaleymc=subject_mc[, .( V4 =data.Normalization(V4,type="n4")), by = .(name4)]
scaletmc=subject_mc[, .( V1 =data.Normalization(V4,type="n4")), by = .(name4)]
timemouse=subject_m$V1
write.csv(timemouse, file = paste("timemouse", i, ".csv") ,sep = " ")
##################################
###aggregate mouse click scaling

scalexmc0=aggregate(V3 ~ name4 , FUN =mean, data=scalex0mc)
scalexmc1=aggregate(V3 ~ name4 , FUN =mean, data=scalex1mc)
scalexmc2=aggregate(V3 ~ name4 , FUN =mean, data=scalex2mc)

scalexmc00=aggregate(V3 ~ name4 , FUN =mean, data=scalex00mc)
scalexmc11=aggregate(V3 ~ name4 , FUN =mean, data=scalex11mc)
scalexmc22=aggregate(V3 ~ name4 , FUN =mean, data=scalex22mc)

matplot(,cbind(scalexmc0$V3,scalexmc1$V3,scalexmc2$V3),type="l",col=c("blue", "red", "green"),lwd=3, xlab ="Subjects", ylab = "Mouse click scaled X coordinates ",main="Mouse click  x coordinates for the three sessions ")  
############3
scaleymc0=aggregate(V4 ~ name4 , FUN =mean, data=scaley00mc)
scaleymc1=aggregate(V4 ~ name4 , FUN =mean, data=scaley11mc)
scaleymc2=aggregate(V4 ~ name4 , FUN =mean, data=scaley22mc)

scaleymc00=aggregate(V4 ~ name4 , FUN =mean, data=scaley00mc)
scaleymc11=aggregate(V4 ~ name4 , FUN =mean, data=scaley11mc)
scaleymc22=aggregate(V4 ~ name4 , FUN =mean, data=scaley22mc)

standarizedx_y_mc=cbind(scalexmc00,scalexmc11,scalexmc22, scaleymc00,scaleymc11,scaleymc22)
write.csv(standarizedx_y_mc, file = paste("standarizedx_y_mc", i, ".csv") ,sep = " ")
#####################
###########aggregation mouse
dataaggxmax=aggregate(V3 ~ name4 , FUN =max, data=subject_m)
dataaggmxmax0=aggregate(V3 ~ name4 , FUN =max, data=session_0m)
dataaggmxmax1=aggregate(V3 ~ name4 , FUN =max, data=session_1m)
dataaggmxmax2=aggregate(V3 ~ name4 , FUN =max, data=session_2m)


#########################################################################3
########max mouse y
dataaggymax0=aggregate(V4 ~ name4 , FUN =max, data=session_0m)
dataaggymax1=aggregate(V4 ~ name4 , FUN =max, data=session_1m)
dataaggymax2=aggregate(V4 ~ name4 , FUN =max, data=session_2m)
####################################################################
#############min x
dataaggxmin=aggregate(V3 ~ name4 , FUN =min, data=subject_m)
dataaggxmin0=aggregate(V3 ~ name4 , FUN =min, data=session_0m)
dataaggxmin1=aggregate(V3 ~ name4 , FUN =min, data=session_1m)
dataaggxmin2=aggregate(V3 ~ name4 , FUN =min, data=session_2m)
################33333333333333
#######################y
dataaggymin=aggregate(V4 ~ name4 , FUN =min, data=subject_m)

dataaggymin0=aggregate(V4 ~ name4 , FUN =min, data=session_0m)
dataaggymin1=aggregate(V4 ~ name4 , FUN =min, data=session_1m)
dataaggymin2=aggregate(V4 ~ name4 , FUN =min, data=session_2m)
####################################################################################
###########################
#Time 
averagetime=aggregate(V1 ~ name4 , FUN =mean, data=subject_m)

averagetime0=aggregate(V1 ~ name4 , FUN =mean, data=session_0m)
averagetime1=aggregate(V1 ~ name4 , FUN =mean, data=session_1m)
averagetime2=aggregate(V1 ~ name4 , FUN =mean, data=session_2m)
#############################################################################
###################################
#mean of diff time
meandifft0=aggregate(V1 ~ name4 , FUN =mean, data=mm0)
meandifft1=aggregate(V1 ~ name4 , FUN =mean, data=mm1)
meandifft2=aggregate(V1 ~ name4 , FUN =mean, data=mm2)

###################################
#aggregation x &y
meandiffx0=aggregate(V3 ~ name4 , FUN =mean, data=diffxm0)
meandiffx1=aggregate(V3 ~ name4 , FUN =mean, data=diffxm1)
meandiffx2=aggregate(V3 ~ name4 , FUN =mean, data=diffxm2)


#########y
meandiffy0=aggregate(V4 ~ name4 , FUN =mean, data=diffym0)
meandiffy1=aggregate(V4 ~ name4 , FUN =mean, data=diffym1)
meandiffy2=aggregate(V4 ~ name4 , FUN =mean, data=diffym2)

###################################3
#sum 
sumxm0=aggregate(V3 ~ name4 , FUN =sum, data=session_0m)
sumxm1=aggregate(V3 ~ name4 , FUN =sum, data=session_1m)
sumxm2=aggregate(V3 ~ name4 , FUN =sum, data=session_2m)
#sum y
sumym0=aggregate(V4 ~ name4 , FUN =sum, data=session_0m)
sumym1=aggregate(V4 ~ name4 , FUN =sum, data=session_1m)
sumym2=aggregate(V4 ~ name4 , FUN =sum, data=session_2m)
######################################
meanxm0=aggregate(V3 ~ name4 , FUN =mean, data=session_0m)
meanxm1=aggregate(V3 ~ name4 , FUN =mean, data=session_1m)
meanxm2=aggregate(V3 ~ name4 , FUN =mean, data=session_2m)
#sum y
meanym0=aggregate(V4 ~ name4 , FUN =mean, data=session_0m)
meanym1=aggregate(V4 ~ name4 , FUN =mean, data=session_1m)
meanym2=aggregate(V4 ~ name4 , FUN =mean, data=session_2m)

averagexy=cbind(meanxm0,meanym0,meanxm1,meanym1,meanxm2,meanym2)
write.csv(averagexy, file = paste("averagexy", i, ".csv") ,sep = " ")
#########################
#mouse
mousefeatures0=cbind(scalexm0,scaleym0,
 standarddevm0,standarddevmy0,dataaggmxmax0,
dataaggymax0,dataaggxmin0,dataaggymin0,averagetime0,dataaggxrange0,
dataaggyrange0,sumxm0,sumym0,meandifft0,meandiffx0,meandiffy0,lengthmean0,speedmean0,accmean0)

write.csv(mousefeatures0, file = paste("mousefeatures0", i, ".csv") ,sep = " ")
#############################################3
#######mouse1
mousefeatures1=cbind(scalexm1,scaleym1,
                     standarddevm1,standarddevmy1,dataaggmxmax1,
                     dataaggymax1,dataaggxmin1,dataaggymin1,averagetime1,dataaggxrange1,
                     dataaggyrange1,sumxm1,sumym1,meandifft1,meandiffx1,meandiffy1,lengthmean1,speedmean1,accmean1)
  write.csv(mousefeatures1, file = paste("mousefeatures1", i, ".csv") ,sep = " ")
#############################################3
#######mouse2
mousefeatures2=cbind(scalexm2,scaleym2,
                     standarddevm2,standarddevmy2,dataaggmxmax2,
                     dataaggymax2,dataaggxmin2,dataaggymin2,averagetime2,dataaggxrange2,
                     dataaggyrange2,sumxm2,sumym2,meandifft2,meandiffx2,meandiffy2,lengthmean2,speedmean2,accmean2)
write.csv(mousefeatures2, file = paste("mousefeatures2", i, ".csv") ,sep = " ")
##########################################
averagetimeg0=aggregate(V1 ~ name4 , FUN =mean, data=session_0g)
averagetimeg1=aggregate(V1 ~ name4 , FUN =mean, data=session_1g)
averagetimeg2=aggregate(V1 ~ name4 , FUN =mean, data=session_2g)
averagetimeg=aggregate(V1 ~ name4 , FUN =mean, data=subject_G)
#standaridzing
###x
#dataaggxstandard=aggregate(V3 ~ name4 , FUN =range, data=scalex)
dataaggxrange0=aggregate(V3 ~ name4 , FUN =range, data=scalex0)
dataaggxrange1=aggregate(V3 ~ name4 , FUN =range, data=scalex1)
dataaggxrange2=aggregate(V3 ~ name4 , FUN =range, data=scalex2)

###y
dataaggystandard=aggregate(ym ~ name4 , FUN =range, data=scaley)
dataaggyrange0=aggregate(V4 ~ name4 , FUN =range, data=scaley0)
dataaggyrange1=aggregate(V4~ name4 , FUN =range, data=scaley1)
dataaggyrange2=aggregate(V4 ~ name4 , FUN =range, data=scaley2)

###########
###########eye

#xrangeg=aggregate(V3 ~ name4 , FUN =range, data=scalexg)
#dataaggx=aggregate(V3 ~ name4 , FUN =range, data=scalex)

dataaggxrange0=aggregate(V3 ~ name4 , FUN =range, data=scalexg0)
dataaggxrange1=aggregate(V3 ~ name4 , FUN =range, data=scalexg1)
dataaggxrangeg2=aggregate(V3 ~ name4 , FUN =range, data=scalexg2)
################y
dataaggyrange0=aggregate(V4 ~ name4 , FUN =range, data=scaleyg0)
dataaggyrange1=aggregate(V4 ~ name4, FUN =range, data=scaleyg1)
dataaggyrangeg2=aggregate(V4 ~ name4 , FUN =range, data=scaleyg2)


ystandardg=aggregate(V4 ~ name4 , FUN =range, data=scaleyg)
#########################################################################3
#aggregation 

scalexg0=aggregate(V3 ~ name4 , FUN =mean, data=scalexg0)
scalexg1=aggregate(V3 ~ name4 , FUN =mean, data=scalexg1)
scalexg2=aggregate(V3 ~ name4 , FUN =mean, data=scalexg2)

#y
scaleyg0=aggregate(V4 ~ name4 , FUN =mean, data=scaleyg0)
scaleyg1=aggregate(V4 ~ name4 , FUN =mean, data=scaleyg1)
scaleyg2=aggregate(V4 ~ name4 , FUN =mean, data=scaleyg2)



scalex0mc,scaley0m

################
###############################################33
#########mouse click
#x
dataaggxrangemc0=aggregate(V3 ~ name4 , FUN =range, data=scalex0mc)
dataaggxrangemc1=aggregate(V3 ~ name4 , FUN =range, data=scalex1mc)
dataaggxrangemc2=aggregate(V3 ~ name4 , FUN =range, data=scalex2mc)
#y
dataaggyrangemc0=aggregate(V4 ~ name4 , FUN =range, data=scaley0mc)
dataaggyrangemc1=aggregate(V4 ~ name4, FUN =range, data=scaley1mc)
dataaggyrangemc2=aggregate(V4 ~ name4 , FUN =range, data=scaley2mc)
########
xstandardmc=aggregate(V3 ~ name4 , FUN =range, data=scalexmc)
ystandardmc=aggregate(V4 ~ name4 , FUN =range, data=scaleymc)

stanadardall=cbind(dataaggxstandard,dataaggystandard,xstandardg,ystandardg,xstandardmc,ystandardmc,averagetime,averagetimeg)
write.csv(stanadardall, file = paste("stanadardall", i, ".csv") ,sep = " ")
##################
dataaggysd=aggregate(V4 ~ name4 , FUN =sd, data=subject_m)
mousemax=cbind(dataaggxmax,dataaggymax,dataaggxmin,dataaggymin,dataaggysd)
write.csv(mousemax, file = paste("mousemax", i, ".csv") ,sep = " ")

#Gaze direction 

subject_G=fin[fin[,2]=="G",]
dataaggxg=aggregate(V3 ~ name4 , FUN =sum, data=subject_G)
# sum x eye
sumxg0=aggregate(V3 ~ name4 , FUN =sum, data=session_0g)
sumxg1=aggregate(V3 ~ name4 , FUN =sum, data=session_1g)
sumxg2=aggregate(V3 ~ name4 , FUN =sum, data=session_2g)

sumyg0=aggregate(V4 ~ name4 , FUN =sum, data=session_0g)
sumyg1=aggregate(V4 ~ name4 , FUN =sum, data=session_1g)
sumyg2=aggregate(V4 ~ name4 , FUN =sum, data=session_2g)
####################################################################3
# sum x eye
averagexg0=aggregate(V3 ~ name4 , FUN =mean, data=session_0g)
averagexg1=aggregate(V3 ~ name4 , FUN =mean, data=session_1g)
averagexg2=aggregate(V3 ~ name4 , FUN =mean, data=session_2g)

###############################################################
averageyg0=aggregate(V4 ~ name4 , FUN =mean, data=session_0g)
averageyg1=aggregate(V4 ~ name4 , FUN =mean, data=session_1g)
averageyg2=aggregate(V4 ~ name4 , FUN =mean, data=session_2g)



##########################################################################333
dataaggyg=aggregate(V4 ~ name4 , FUN =sum, data=subject_G)

#########
#time eye
sumtimeg0=aggregate(V1 ~ name4 , FUN =sum, data=session_0g)
sumtimeg1=aggregate(V1 ~ name4 , FUN =sum, data=session_1g)
sumtimeg2=aggregate(V1 ~ name4 , FUN =sum, data=session_2g)

meantimeg0=aggregate(V1 ~ name4 , FUN =mean, data=session_0g)
meantimeg1=aggregate(V1 ~ name4 , FUN =mean, data=session_1g)
meantimeg2=aggregate(V1 ~ name4 , FUN =mean, data=session_2g)
################################################################
dataaggtg=aggregate(V1 ~ name4 , FUN =sum, data=subject_G)
write.csv(dataaggtg,file = paste("timesumgaze", i, ".csv") ,sep = " ")
dataaggtgm=aggregate(V1 ~ name4 , FUN =mean, data=subject_G)
###########
####mean eye
dataaggxmeang=aggregate(V3 ~ name4 , FUN =mean, data=subject_G)
meanxg0=aggregate(V3 ~ name4 , FUN =mean, data=session_0g)
meanxg1=aggregate(V3 ~ name4 , FUN =mean, data=session_1g)
meanxg2=aggregate(V3 ~ name4 ,FUN =mean, data=session_2g)

meanyg0=aggregate(V4 ~ name4 , FUN =mean, data=session_0g)
meanyg1=aggregate(V4 ~ name4 , FUN =mean, data=session_1g)
meanyg2=aggregate(V4 ~ name4 , FUN =mean, data=session_2g)


############################
###################################################333
#standarddev eye
dataaggymeang=aggregate(V4 ~ name4 , FUN =mean, data=subject_G)

xsdg0=aggregate(V3 ~ name4 , FUN =sd, data=session_0g)
xsdg1=aggregate(V3 ~ name4 , FUN =sd, data=session_1g)
xsdg2=aggregate(V3 ~ name4 , FUN =sd, data=session_2g)

ysdg0=aggregate(V4 ~ name4 , FUN =sd, data=session_0g)
ysdg1=aggregate(V4 ~ name4 , FUN =sd, data=session_1g)
ysdg2=aggregate(V4 ~ name4 , FUN =sd, data=session_2g)

dataaggysd=aggregate(V4 ~ name4 , FUN =sd, data=subject_G)
dataaggxsd=aggregate(V3 ~ name4 , FUN =sd, data=subject_G)
###########################################################################
##### max and min x and y eye 

maxxg0=aggregate(V3 ~ name4 , FUN =max, data=session_0g)
maxxg1=aggregate(V3 ~ name4 , FUN =max, data=session_1g)
maxxg2=aggregate(V3 ~ name4 , FUN =max, data=session_2g)
#y
maxyg0=aggregate(V4 ~ name4 , FUN =max, data=session_0g)
maxyg1=aggregate(V4 ~ name4 , FUN =max, data=session_1g)
maxyg2=aggregate(V4 ~ name4 , FUN =max, data=session_2g)

##################################################################3
#########min
minxg0=aggregate(V3 ~ name4 , FUN =min, data=session_0g)
minxg1=aggregate(V3 ~ name4 , FUN =min, data=session_1g)
minxg2=aggregate(V3 ~ name4 , FUN =min, data=session_2g)
#y
minyg0=aggregate(V4 ~ name4 , FUN =min, data=session_0g)
minyg1=aggregate(V4 ~ name4 , FUN =min, data=session_1g)
minyg2=aggregate(V4 ~ name4 , FUN =min, data=session_2g)

######################################################################################
############################################################333
#eye features

eyefeatures0=cbind(scalexg0,scaleyg0,
                     dataaggmxmax0,
                     dataaggymax0,dataaggxmin0,dataaggymin0,averagetime0,averagexg0,averageyg0,dataaggxrange0,
                     dataaggyrange0,
                   meantimeg0,meanxg0,meanyg0,xsdg0,ysdg0,sumtimeg0,sumxg0,sumyg0,
                   
                   meantg0,meanxdiffg0,meanydiffg0,lengthmeang0,speedmeang0,accmeang0)
######################################3
eyefeatures1=cbind(scalexg1,scaleyg1,
                   dataaggmxmax1,
                   dataaggymax1,dataaggxmin1,dataaggymin1,averagetime1,averagexg1,averageyg1,dataaggxrange1,
                   dataaggyrange1,
                   meantimeg1,meanxg1,meanyg1,xsdg1,ysdg1,sumtimeg1,sumxg1,sumyg1,
                   
                   meantg1,meanxdiffg1,meanydiffg1,lengthmeang1,speedmeang1,accmeang1)
#################################################33
eyefeatures2=cbind(scalexg2,scaleyg2,
                   dataaggmxmax2,
                   dataaggymax2,dataaggxmin2,dataaggymin2,averagetime2,averagexg2,averageyg2,dataaggxrange2,
                   dataaggyrange2,
                   meantimeg2,meanxg2,meanyg2,xsdg2,ysdg2,sumtimeg2,sumxg2,sumyg2,
                   
                   meantg2,meanxdiffg2,meanydiffg2,lengthmeang2,speedmeang2,accmeang2)
################################################3


write.csv(eyefeatures0, file = paste("eyefeatures0", i, ".csv") ,sep = " ")

write.csv(eyefeatures1, file = paste("eyefeatures1", i, ".csv") ,sep = " ")
write.csv(eyefeatures2, file = paste("eyefeatures2", i, ".csv") ,sep = " ")
##########################################################333
dataaggxgmax=aggregate(V3 ~ name4 , FUN =max, data=subject_G)
dataaggygmax=aggregate(V4 ~ name4 , FUN =max, data=subject_G)
dataaggxgmin=aggregate(V3 ~ name4 , FUN =min, data=subject_G)
dataaggygmin=aggregate(V4 ~ name4 , FUN =min, data=subject_G)
gazeagg=cbind(dataaggxg,dataaggyg,dataaggxmeang,dataaggymeang,dataaggysd,dataaggxsd,dataaggxgmax,dataaggygmax,dataaggxgmin,dataaggygmin)
write.csv(gazeagg,file = paste("allgaze", i, ".csv") ,sep = " ")

##################
#mouseclick 
subject_mc=fin[fin[,2]=="MC",]
#time
sumtimemc0=aggregate(V1 ~ name4 , FUN =sum, data=session_0mc)
sumtimemc1=aggregate(V1 ~ name4 , FUN =sum, data=session_1mc)
sumtimemc2=aggregate(V1 ~ name4 , FUN =sum, data=session_2mc)
#x
sumxmc0=aggregate(V3 ~ name4 , FUN =sum, data=session_0mc)
sumxmc1=aggregate(V3 ~ name4 , FUN =sum, data=session_1mc)
sumxmc2=aggregate(V3 ~ name4 , FUN =sum, data=session_2mc)
#y
sumymc0=aggregate(V4 ~ name4 , FUN =sum, data=session_0mc)
sumymc1=aggregate(V4 ~ name4 , FUN =sum, data=session_1mc)
sumymc2=aggregate(V4 ~ name4 , FUN =sum, data=session_2mc)

dataaggxmc=aggregate(V3 ~ name4 , FUN =sum, data=subject_mc)
dataaggymc=aggregate(V4 ~ name4 , FUN =sum, data=subject_mc)
dataaggtmc=aggregate(V1 ~ name4 , FUN =sum, data=subject_mc)
sumtimemc0,sumxmc0,sumymc0,meantimemc0,meanxmc0,meanymc0
#########
write.csv(dataaggtg,file = paste("timesumgaze", i, ".csv") ,sep = " ")
dataaggtgm=aggregate(V1 ~ name4 , FUN =mean, data=subject_mc)
###########
##############################################################3
#############mean
meantimemc0=aggregate(V1 ~ name4 , FUN =mean, data=session_0mc)
meantimemc1=aggregate(V1 ~ name4 , FUN =mean, data=session_1mc)
meantimemc2=aggregate(V1 ~ name4 , FUN =mean, data=session_2mc)
#x
meanxmc0=aggregate(V3 ~ name4 , FUN =mean, data=session_0mc)
meanxmc1=aggregate(V3 ~ name4 , FUN =mean, data=session_1mc)
meanxmc2=aggregate(V3 ~ name4 , FUN =mean, data=session_2mc)
#y
meanymc0=aggregate(V4 ~ name4 , FUN =mean, data=session_0mc)
meanymc1=aggregate(V4 ~ name4 , FUN =mean, data=session_1mc)
meanymc2=aggregate(V4 ~ name4 , FUN =mean, data=session_2mc)

dataaggxmc=aggregate(V3 ~ name4 , FUN =sum, data=subject_mc)
dataaggymc=aggregate(V4 ~ name4 , FUN =sum, data=subject_mc)
dataaggtmc=aggregate(V1 ~ name4 , FUN =sum, data=subject_mc)
###########################################################################
#####################################33
################3
#standard deviation
#############mean
#time
sdtimemc0=aggregate(V1 ~ name4 , FUN =sd, data=session_0mc)
sdtimemc1=aggregate(V1 ~ name4 , FUN =sd, data=session_1mc)
sdtimemc2=aggregate(V1 ~ name4 , FUN =sd, data=session_2mc)
#x
sdxmc0=aggregate(V3 ~ name4 , FUN =sd, data=session_0mc)
sdxmc1=aggregate(V3 ~ name4 , FUN =sd, data=session_1mc)
sdxmc2=aggregate(V3 ~ name4 , FUN =sd, data=session_2mc)
#y
sdymc0=aggregate(V4 ~ name4 , FUN =sd, data=session_0mc)
sdymc1=aggregate(V4 ~ name4 , FUN =sd, data=session_1mc)
sdymc2=aggregate(V4 ~ name4 , FUN =sd, data=session_2mc)

##################################

################3
#max&min x&y

maxxmc0=aggregate(V3 ~ name4 , FUN =max, data=session_0mc)
maxxmc1=aggregate(V3 ~ name4 , FUN =max, data=session_1mc)
maxxmc2=aggregate(V3 ~ name4 , FUN =max, data=session_2mc)
#y
maxymc0=aggregate(V4 ~ name4 , FUN =max, data=session_0mc)
maxymc1=aggregate(V4 ~ name4 , FUN =max, data=session_1mc)
maxymc2=aggregate(V4 ~ name4 , FUN =max, data=session_2mc)

##################################################################3
#########min
minxmc0=aggregate(V3 ~ name4 , FUN =min, data=session_0mc)
minxmc1=aggregate(V3 ~ name4 , FUN =min, data=session_1mc)
minxmc2=aggregate(V3 ~ name4 , FUN =min, data=session_2mc)
#y
minymc0=aggregate(V4 ~ name4 , FUN =min, data=session_0mc)
minymc1=aggregate(V4 ~ name4 , FUN =min, data=session_1mc)
minymc2=aggregate(V4 ~ name4 , FUN =min, data=session_2mc)



dataaggxmc=aggregate(V3 ~ name4 , FUN =sum, data=subject_mc)
dataaggymc=aggregate(V4 ~ name4 , FUN =sum, data=subject_mc)
dataaggtmc=aggregate(V1 ~ name4 , FUN =sum, data=subject_mc)


dataaggxmeanmc=aggregate(V3 ~ name4 , FUN =mean, data=subject_mc)
dataaggymeanmc=aggregate(V4 ~ name4 , FUN =mean, data=subject_mc)
dataaggtmeanmc=aggregate(V1 ~ name4 , FUN =mean, data=subject_mc)

dataaggysdmc=aggregate(V4 ~ name4 , FUN =sd, data=subject_mc)
dataaggxsdmc=aggregate(V3 ~ name4 , FUN =sd, data=subject_mc)
dataaggxmcmax=aggregate(V3 ~ name4 , FUN =max, data=subject_mc)
dataaggymcmax=aggregate(V4 ~ name4 , FUN =max, data=subject_mc)
dataaggxmcmin=aggregate(V3 ~ name4 , FUN =min, data=subject_mc)
dataaggymcmin=aggregate(V4 ~ name4 , FUN =min, data=subject_mc)
mouseclickagg=cbind(dataaggxmc,dataaggymc,dataaggtmc,dataaggxmeanmc,dataaggymeanmc,dataaggtmeanmc,dataaggysdmc,dataaggxsdmc,dataaggxmcmax,dataaggymcmax,dataaggxmcmin,dataaggymcmin)
write.csv(mouseclickagg,file = paste("allmouseclick", i, ".csv") ,sep = " ")

###################################################################################################
#############################

library(data.table)
setDT(subject_mc)
setDT(session_0mc)
#timediff
mc0=session_0mc[, .( V1 =diff(V1)), by = .(name4)]
setDT(session_1mc)
mc1=session_1mc[, .( V1 =diff(V1)), by = .(name4)]
setDT(session_2mc)
mc2=session_2mc[, .( V1 =diff(V1)), by = .(name4)]
#mean of diff time
meantmc0=aggregate(V1 ~ name4 , FUN =mean, data=mc0)
meantmc1=aggregate(V1 ~ name4 , FUN =mean, data=mc1)
meantmc2=aggregate(V1 ~ name4 , FUN =mean, data=mc2)
#####################################################################################3
########
#diff x
setDT(session_0mc)
diffxmc0=session_0mc[, .( V3 =diff(V3)), by = .(name4)]
setDT(session_1mc)
diffxmc1=session_1mc[, .( V3 =diff(V3)), by = .(name4)]
setDT(session_2mc)
diffxmc2=session_2mc[, .( V3 =diff(V3)), by = .(name4)]
##########
#diffy
setDT(session_0mc)
diffymc0=session_0mc[, .( V4=diff(V4)), by = .(name4)]
setDT(session_1mc)
diffymc1=session_1mc[, .( V4=diff(V4)), by = .(name4)]
setDT(session_2mc)
diffymc2=session_2mc[, .( V4=diff(V4)), by = .(name4)]
###mean of diffx
meandiffxmc0=aggregate(V3 ~ name4 , FUN =mean, data=diffxmc0)
meandiffxmc1=aggregate(V3 ~ name4 , FUN =mean, data=diffxmc1)
meandiffxmc2=aggregate(V3 ~ name4 , FUN =mean, data=diffxmc2)
##############################333
###mean of difft
meandifftmc0=aggregate(V1 ~ name4 , FUN =mean, data=mc0)
meandifftmc1=aggregate(V1 ~ name4 , FUN =mean, data=mc1)
meandifftmc2=aggregate(V1 ~ name4 , FUN =mean, data=mc2)
####mean of diffy
meandiffymc0=aggregate(V4 ~ name4 , FUN =mean, data=diffymc0)
meandiffymc1=aggregate(V4 ~ name4 , FUN =mean, data=diffymc1)
meandiffymc2=aggregate(V4 ~ name4 , FUN =mean, data=diffymc2)

meandiffxmc=aggregate(V3 ~ name4 , FUN =mean, data=diffxmc)
diffymc=subject_mc[, .( V4=diff(V4)), by = .(name4)]
dataaggdiymc=aggregate(V4 ~ name4 , FUN =mean, data=diffymc)
######################################################################
################


#length of the curve

lengthofcurvesmc0=sqrt((diffxmc0$V3)^2+((diffymc0$V4)^2))
lengthofcurvesmc1=sqrt((diffxmc1$V3)^2+((diffymc1$V4)^2))
lengthofcurvesmc2=sqrt((diffxmc2$V3)^2+((diffymc2$V4)^2))

lengthofcurvesmc=sqrt((diffxmc$V3)^2+((diffymc$V4)^2))
##############
speedmc0=lengthofcurvesmc0/mc0$V1
speedmc1=lengthofcurvesmc1/mc1$V1
speedmc2=lengthofcurvesmc2/mc2$V1
##################
Accelerationmc0=speedmc0/mc0$V1
Accelerationmc1=speedmc1/mc1$V1
Accelerationmc2=speedmc2/mc2$V1
#######################333
############aggregate speed
lengspeedmc0=cbind(mc0,speedmc0,Accelerationmc0,lengthofcurvesmc0)
lengspeedmc1=cbind(mc1,speedmc1,Accelerationmc1,lengthofcurvesmc1)
lengspeedmc2=cbind(mc2,speedmc2,Accelerationmc2,lengthofcurvesmc2)
###############################3
speedmeanmc0=aggregate(speedmc0 ~ name4 , FUN =mean, data=lengspeedmc0)
speedmeanmc1=aggregate(speedmc1 ~ name4 , FUN =mean, data=lengspeedmc1)
speedmeanmc2=aggregate(speedmc2 ~ name4 , FUN =mean, data=lengspeedmc2)
#aggregate length
lengthmeanmc0=aggregate(lengthofcurvesmc0~ name4 , FUN =mean, data=lengspeedmc0)
lengthmeanmc1=aggregate(lengthofcurvesmc1 ~ name4 , FUN =mean, data=lengspeedmc1)
lengthmeanmc2=aggregate(lengthofcurvesmc2 ~ name4 , FUN =mean, data=lengspeedmc2)
#aggregate acceleration
accmeanmc0=aggregate(Accelerationmc0~ name4 , FUN =mean, data=lengspeedmc0)
accmeanmc1=aggregate(Accelerationmc1 ~ name4 , FUN =mean, data=lengspeedmc1)
accmeanmc2=aggregate(Accelerationmc2 ~ name4 , FUN =mean, data=lengspeedmc2)


#####################################################################
#mouse click features

mouseclickfeatures0=cbind(scalexmc0,scaleymc0,
                  dataaggxrangemc0,
                   dataaggyrangemc0,
                   meantimemc0,sumtimemc0,sumxmc0,sumymc0,meantimemc0,meanxmc0,meanymc0
                  ,sdtimemc0,sdxmc0,sdymc0,maxxmc0,maxymc0,
                  minxmc0,minymc0,meandiffxmc0,meandiffymc0,meandifftmc0
                  ,lengthmeanmc0,speedmeanmc0,accmeanmc0)
######################################3
mouseclickfeatures1=cbind(scalexmc1,scaleymc1,
                          dataaggxrangemc1,
                          dataaggyrangemc1,
                          meantimemc1,sumtimemc1,sumxmc1,sumymc1,meantimemc1,meanxmc1,meanymc1
                          ,sdtimemc1,sdxmc1,sdymc1,maxxmc1,maxymc1,
                          minxmc1,minymc1,meandiffxmc1,meandiffymc1,meandifftmc1
                          ,lengthmeanmc1,speedmeanmc1,accmeanmc1)
######################################3
#################################################33
mouseclickfeatures2=cbind(scalexmc2,scaleymc2,
                          dataaggxrangemc2,
                          dataaggyrangemc2,
                          meantimemc2,sumtimemc2,sumxmc2,sumymc2,meantimemc2,meanxmc2,meanymc2
                          ,sdtimemc2,sdxmc2,sdymc2,maxxmc2,maxymc2,
                          minxmc2,minymc2,meandiffxmc2,meandiffymc2,meandifftmc2
                          ,lengthmeanmc2,speedmeanmc2,accmeanmc2)
######################################3
################################################3

write.csv(mouseclickfeatures0,file = paste("mouseclickfeatures0", i, ".csv") ,sep = " ")
write.csv(mouseclickfeatures1,file = paste("mouseclickfeatures1", i, ".csv") ,sep = " ")
write.csv(mouseclickfeatures2,file = paste("mouseclickfeatures2", i, ".csv") ,sep = " ")

#####################################################################################333
speedallmc=lengthofcurvesmc/mc$V1
speedallmc=lengthofcurvesmc/mc$V1
Accelerationallmc=speedallmc/mc$V1
alldiffmc=cbind(subject_mc$V1,subject_mc$V3,subject_mc$V4,mc,diffxmc,diffymc,lengthofcurvesmc,speedallmc,Accelerationallmc,scalexmc,scaleymc)
write.csv(alldiffmc,file = paste("importantfeatmc", i, ".csv") ,sep = " ")
lengthofcuvesmcagg=aggregate(lengthofcurvesmc ~ name4 , FUN =mean, data=alldiffmc)

speedmeanmc=aggregate(speedallmc ~ name4 , FUN =mean, data=alldiffmc)
accelerationmeanmc=aggregate(Accelerationallmc ~ name4 , FUN =mean, data=alldiffmc)
aggallmc=cbind(dataagtmc,dataagdixmc,dataaggdiymc,lengthofcuvesmcagg,speedmeanmc,accelerationmeanmc)

write.csv(aggallmc,file = paste("aggallmc", i, ".csv") ,sep = " ")
###################################
library(data.table)
setDT(subject_G)
setDT(subject_m)

#timediff
setDT(session_0m)
mm0=session_0m[, .( V1 =diff(V1)), by = .(name4)]
#####
setDT(session_1m)
mm1=session_1m[, .( V1 =diff(V1)), by = .(name4)]
############
setDT(session_2m)
mm2=session_2m[, .( V1 =diff(V1)), by = .(name4)]
#########################################################
##########diffx&y mouse
setDT(session_0m)
diffxm0=session_0m[, .( V3 =diff(V3)), by = .(name4)]
setDT(session_1m)
diffxm1=session_1m[, .( V3 =diff(V3)), by = .(name4)]
setDT(session_2m)
diffxm2=session_2m[, .( V3 =diff(V3)), by = .(name4)]
##############
setDT(subject_m)
###mouse
diffxm=subject_m[, .( V3 =diff(V3)), by = .(name4)]
diffym=subject_m[, .( V4 =diff(V4)), by = .(name4)]
difftm=subject_m[, .( V1 =diff(V1)), by = .(name4)]

lengthofcurvesm=sqrt((diffxm$V3)^2+((diffym$V4)^2))
speedm=lengthofcurvesm/difftm$V1
accelerationm=speedm/difftm$V1
lengspeedm=cbind(difftm,speedm,accelerationm,lengthofcurvesm)
spmagg=aggregate(speedm ~ name4 , FUN =mean, data=lengspeedm)
colnames(lengspeedm)[3] <- "speed"
colnames(lengspeedm)[4] <- "acc"
colnames(lengspeedm)[5] <- "length"

#############
######eye
setDT(subject_G)
diffxg=subject_G[, .( V3 =diff(V3)), by = .(name4)]
diffyg=subject_G[, .( V4 =diff(V4)), by = .(name4)]
difftg=subject_G[, .( V1 =diff(V1)), by = .(name4)]
lengthofcurvesg=sqrt((diffxg$V3)^2+((diffyg$V4)^2))
speedg=lengthofcurvesg/difftg$V1
accelerationg=speedg/difftg$V1
lengspeedg=cbind(difftg,speedg,accelerationg,lengthofcurvesg)
spgagg=aggregate(speedg ~ name4 , FUN =mean, data=lengspeedg)
colnames(lengspeedg)[3] <- "speed"
colnames(lengspeedg)[4] <- "acc"
colnames(lengspeedg)[5] <- "length"
#########
#mouse click
setDT(subject_mc)
diffxmc=subject_mc[, .( V3 =diff(V3)), by = .(name4)]
diffymc=subject_mc[, .( V4 =diff(V4)), by = .(name4)]
difftmc=subject_mc[, .( V1 =diff(V1)), by = .(name4)]
lengthofcurvesmc=sqrt((diffxmc$V3)^2+((diffymc$V4)^2))
speedmc=lengthofcurvesmc/difftmc$V1
accelerationmc=speedmc/difftmc$V1
lengspeedmc=cbind(difftmc,speedmc,accelerationmc,lengthofcurvesmc)
spmcagg=aggregate(speedmc ~ name4 , FUN =mean, data=lengspeedmc)
colnames(lengspeedmc)[3] <- "speed"
colnames(lengspeedmc)[4] <- "acc"
colnames(lengspeedmc)[5] <- "length"
#############################333
############3
#######
#aggregation for acceleration
accm=aggregate(accelerationm ~ name4 , FUN =mean, data=lengspeedm)
accg=aggregate(accelerationg ~ name4 , FUN =mean, data=lengspeedg)
accmc=aggregate(accelerationmc ~ name4 , FUN =mean, data=lengspeedmc)
colnames(accm)[2] <- "acc"
colnames(accg)[2] <- "acc"
colnames(accmc)[2] <- "acc"
#######################33
colnames(spmcagg)[2] <- "speed"
colnames(spmagg)[2] <- "speed"
colnames(spgagg)[2] <- "speed"
names(spmcagg)
############plots 
ggplot(diffxm,aes(x=name4,y=V3))+geom_point(aes(color="mouse"))+
  geom_point(data=diffxg,aes(color="eye"))+geom_point(data=diffxmc,aes(color="mouse click"))+
  labs(x=" subjects", y="  difference X coordinates ", title=" Difference of   x coordinates for eye and mouse directions")
#speed
ggplot(spmagg,aes(x=name4,y=speed))+geom_point(aes(color="mouse"))+
  geom_point(data=spgagg,aes(color="eye"))+geom_point(data=spmcagg,aes(color="mouse click"))+
  labs(x=" subjects", y="  speed ", title=" The average speed for mouse ,eye and mouse click for each subject")
###acceleration
ggplot(accm,aes(x=name4,y=acc))+geom_point(aes(color="mouse"))+
  geom_point(data=accg,aes(color="eye"))+geom_point(data=accmc,aes(color="mouse click"))+
  labs(x=" subjects", y="  acceleration ", title=" The average acceleration for mouse ,eye and mouse click for each subject")
###########################################
ggplot(lengspeedm,aes(x=name4,y=acc))+geom_point(aes(color="mouse"))+
  geom_point(data=lengspeedg,aes(color="eye"))+geom_point(data=lengspeedmc,aes(color="mouse click"))+
  labs(x=" subjects", y="  acceleration ", title=" The  acceleration for mouse ,eye and mouse click for each subject")
###################
ggplot(lengspeedm,aes(x=name4,y=V1))+geom_point(aes(color="mouse"))+
  geom_point(data=lengspeedg,aes(color="eye"))+geom_point(data=lengspeedmc,aes(color="mouse click"))+
  labs(x=" subjects", y="Time Difference  ", title="Time Difference   for mouse ,eye and mouse click for each subject")
###########################33
ggplot(lengspeedm,aes(x=name4,y=speed))+geom_point(aes(color="mouse"))+
  geom_point(data=lengspeedg,aes(color="eye"))+geom_point(data=lengspeedmc,aes(color="mouse click"))+
  labs(x=" subjects", y="speed  ", title="Speed  for mouse ,eye and mouse click for each subject")


##################################
matplot(cbind(accelerationm,accelerationg,accelerationmc),type="l",lwd=3,col=c("blue", "red","pink",xlab ="time",ylab = " Eye x coordinates between the three sessions  ",main="Eye x coordinates between the three sessions"))

#y
setDT(session_0m)
diffym0=session_0m[, .( V4 =diff(V4)), by = .(name4)]
setDT(session_1m)
diffym1=session_1m[, .( V4 =diff(V4)), by = .(name4)]
setDT(session_2m)
diffym2=session_2m[, .( V4 =diff(V4)), by = .(name4)]
##############################################################################

mm=subject_m[, .( V1 =diff(V1)), by = .(name4)]
#################################################################
setDT(session_0m)
mm0=session_0m[, .( V1 =diff(V1)), by = .(name4)]
setDT(session_1m)
mm1=session_1m[, .( V1 =diff(V1)), by = .(name4)]
setDT(session_2m)
mm2=session_2m[, .( V1 =diff(V1)), by = .(name4)]
####################33
#length of the curve mouse
lengthofcurvesm=sqrt((diffxm$V3)^2+((diffym$V4)^2))

lengthofcurvesm0=sqrt((diffxm0$V3)^2+((diffym0$V4)^2))
lengthofcurvesm1=sqrt((diffxm1$V3)^2+((diffym1$V4)^2))
lengthofcurvesm2=sqrt((diffxm2$V3)^2+((diffym2$V4)^2))
################################################
#################################
#speed &accleleration 
speedm0=lengthofcurvesm0/mm0$V1
speedm1=lengthofcurvesm1/mm1$V1
speedm2=lengthofcurvesm2/mm2$V1
###########
accelerationm0=speedm0/mm0$V1
accelerationm1=speedm1/mm1$V1
accelerationm2=speedm2/mm2$V1
############################
lengspeedm0=cbind(mm0,speedm0,accelerationm0,lengthofcurvesm0)
lengspeedm1=cbind(mm1,speedm1,accelerationm1,lengthofcurvesm1)
lengspeedm2=cbind(mm2,speedm2,accelerationm2,lengthofcurvesm2)
##############################################3
#aggregate speed

speedmean0=aggregate(speedm0 ~ name4 , FUN =mean, data=lengspeedm0)
speedmean1=aggregate(speedm1 ~ name4 , FUN =mean, data=lengspeedm1)
speedmean2=aggregate(speedm2 ~ name4 , FUN =mean, data=lengspeedm2)
#aggregate length
lengthmean0=aggregate(lengthofcurvesm0~ name4 , FUN =mean, data=lengspeedm0)
lengthmean1=aggregate(lengthofcurvesm1 ~ name4 , FUN =mean, data=lengspeedm1)
lengthmean2=aggregate(lengthofcurvesm2 ~ name4 , FUN =mean, data=lengspeedm2)
#aggregate acceleration
accmean0=aggregate(accelerationm0~ name4 , FUN =mean, data=lengspeedm0)
accmean1=aggregate(accelerationm1 ~ name4 , FUN =mean, data=lengspeedm1)
accmean2=aggregate(accelerationm2 ~ name4 , FUN =mean, data=lengspeedm2)


lengthofcurvesm=sqrt((diffx$V3)^2+((diffy$V4)^2))
speedallm=lengthofcurvesm/mm$V1
Accelerationallm=speedallm/mm$V1
importantfeatm=cbind(subject_m$V3,subject_m$V4,mm,lengthofcurvesm,speedallm,Accelerationallm,scalex,scaley,diffx,diffy)
write.csv(importantfeatm,file = paste("importantfeatm", i, ".csv") ,sep = " ")


diffxm=subject_m[, .( V3 =diff(V3)), by = .(name4)]


speedall=lengthofcurves/mm$V1
########################################################33333

##########################################################333

dataaggt=aggregate(V1 ~ name4 , FUN =mean, data=mm)


###
dataaggtg=aggregate(V1 ~ name4 , FUN =mean, data=gg)
write.csv(dataaggt,file = paste("meandifftime", i, ".csv") ,sep = " ")
aggallmc=cbind(dataaggtg,dataaggdixg,dataaggdiyg,lengthofcuvesg,speedmeang,accelerationmeang)
###########
#mean of diff x cooridinates 
diffx=subject_m[, .( V3 =diff(V3)), by = .(name4)]
diffy=subject_m[, .( V4 =diff(V3)), by = .(name4)]

dataaggdix=aggregate(V3 ~ name4 , FUN =mean, data=diffx)
write.csv(dataaggdix,file = paste("meandiffx", i, ".csv") ,sep = " ")
##################################################################################################
############################################
####Gaze  different x eye 
setDT(session_0g)
gg0=session_0g[, .( V1 =diff(V1)), by = .(name4)]
diffxg0=session_0g[, .( V3 =diff(V3)), by = .(name4)]
diffyg0=session_0g[, .( V4 =diff(V4)), by = .(name4)]
#############################################
setDT(session_1g)
gg1=session_1g[, .( V1 =diff(V1)), by = .(name4)]
diffxg1=session_1g[, .( V3 =diff(V3)), by = .(name4)]
diffyg1=session_1g[, .( V4 =diff(V4)), by = .(name4)]
########################
setDT(session_2g)
gg2=session_2g[, .( V1 =diff(V1)), by = .(name4)]
diffxg2=session_2g[, .( V3 =diff(V3)), by = .(name4)]
diffyg2=session_2g[, .( V4 =diff(V4)), by = .(name4)]
###################################33
################################
#aggergation

#mean of diff time
meantg0=aggregate(V1 ~ name4 , FUN =mean, data=gg0)
meantg1=aggregate(V1 ~ name4 , FUN =mean, data=gg1)
meantg2=aggregate(V1 ~ name4 , FUN =mean, data=gg2)

###################################
#aggregation x &y
meanxdiffg0=aggregate(V3 ~ name4 , FUN =mean, data=diffxg0)
meanxdiffg1=aggregate(V3 ~ name4 , FUN =mean, data=diffxg1)
meanxdiffg2=aggregate(V3 ~ name4 , FUN =mean, data=diffxg2)

meantg0,meanxg0,meanyg0
#########y
meanydiffg0=aggregate(V4 ~ name4 , FUN =mean, data=diffyg0)
meanydiffg1=aggregate(V4 ~ name4 , FUN =mean, data=diffyg1)
meanydiffg2=aggregate(V4 ~ name4 , FUN =mean, data=diffyg2)
################################################################
#length of the curve eye
lengthofcurvesg0=sqrt((diffxg0$V3)^2+((diffyg0$V4)^2))
lengthofcurvesg1=sqrt((diffxg1$V3)^2+((diffyg1$V4)^2))
lengthofcurvesg2=sqrt((diffxg2$V3)^2+((diffyg2$V4)^2))
################################################
#################################
#speed &accleleration 
speedg0=lengthofcurvesg0/gg0$V1
speedg1=lengthofcurvesg1/gg1$V1
speedg2=lengthofcurvesg2/gg2$V1
###########
accelerationg0=speedg0/gg0$V1
accelerationg1=speedg1/gg1$V1
accelerationg2=speedg2/gg2$V1
############################
write.csv(allg,file = paste("eyefeaturs", i, ".csv") ,sep = " ")
#################################################################################
lengthspeedg0=cbind(gg0,lengthofcurvesg0,speedg0,accelerationg0)

lengthspeedg1=cbind(gg1,lengthofcurvesg1,speedg1,accelerationg1)

lengthspeedg2=cbind(gg2,lengthofcurvesg2,speedg2,accelerationg2)

################################################################
#aggregate speed

speedmeang0=aggregate(speedg0 ~ name4 , FUN =mean, data=lengthspeedg0)
speedmeang1=aggregate(speedg1 ~ name4 , FUN =mean, data=lengthspeedg1)
speedmeang2=aggregate(speedg2 ~ name4 , FUN =mean, data=lengthspeedg2)
#aggregate length
lengthmeang0=aggregate(lengthofcurvesg0~ name4 , FUN =mean, data=lengthspeedg0)
lengthmeang1=aggregate(lengthofcurvesg1 ~ name4 , FUN =mean, data=lengthspeedg1)
lengthmeang2=aggregate(lengthofcurvesg2 ~ name4 , FUN =mean, data=lengthspeedg2)
#aggregate acceleration
accmeang0=aggregate(accelerationg0~ name4 , FUN =mean, data=lengthspeedg0)
accmeang1=aggregate(accelerationg1 ~ name4 , FUN =mean, data=lengthspeedg1)
accmeang2=aggregate(accelerationg2 ~ name4 , FUN =mean, data=lengthspeedg2)




gg=subject_G[, .( V1 =diff(V1)), by = .(name4)]

diffxg=subject_G[, .( V3 =diff(V3)), by = .(name4)]
dataaggdixg=aggregate(V3 ~ name4 , FUN =mean, data=diffxg)
diffyg=subject_G[, .( V4=diff(V4)), by = .(name4)]
dataaggdiyg=aggregate(V4 ~ name4 , FUN =mean, data=diffyg)
lengthofcurvesg=sqrt((diffxg$V3)^2+((diffyg$V4)^2))

lengthofcuvesg=aggregate(lengthofcurvesg ~ name4 , FUN =mean, data=alldiffg)
speedallg=lengthofcurvesg/gg$V1
Accelerationall=speedallg/gg$V1
allg=cbind(subject_G$V1,subject_G$V3,subject_G$V4,gg,diffxg,diffyg,lengthofcurvesg,speedallg,Accelerationall,scalexg,scaleyg)

aggall=cbind(dataaggtg,dataaggdixg,dataaggdiyg,lengthofcuvesg,speedmeang,accelerationmeang)

speedmeang=aggregate(speedallg ~ name4 , FUN =mean, data=alldiffg)
accelerationmeang=aggregate(Accelerationall ~ name4 , FUN =mean, data=alldiffg)
write.csv(dataaggdix,file = paste("meandiffx", i, ".csv") ,sep = " ")
write.csv(alldiffg,file = paste("alldiffg", i, ".csv") ,sep = " ")
write.csv(aggall,file = paste("allaggg", i, ".csv") ,sep = " ")
###############
#mean of diff y cooridinates 
diffy=subject_m[, .( V4=diff(V4)), by = .(name4)]
dataaggdiy=aggregate(V4 ~ name4 , FUN =mean, data=diffy)
write.csv(dataaggdiy,file = paste("meandiffy", i, ".csv") ,sep = " ")
xydiff=cbind(diffx,diffy,speedall,mm)
speedmean=aggregate(speedall ~ name4 , FUN =mean, data=xydiff)
write.csv(speedmean,file = paste("speed", i, ".csv") ,sep = " ")
Accelerationall=xydiff$speedall/xydiff$V1
alldiff=cbind(xydiff,Accelerationall)
accelerationmean=aggregate(Accelerationall ~ name4 , FUN =mean, data=alldiff)
write.csv(accelerationmean,file = paste("acceleration", i, ".csv") ,sep = " ")
#####
#length of curves
lengthofcurves=sqrt((xydiff$V3)^2+((xydiff$V4)^2))
lengthfors=cbind(lengthofcurves,xydiff)

lengthofcuvesall=aggregate(lengthofcurves ~ name4 , FUN =mean, data=lengthfors)
write.csv(lengthofcuvesall,file = paste("meanlengthofcurve", i, ".csv") ,sep = " ")
#speed
speedall=lengthofcuvesall$lengthofcurves/dataaggt$V1
write.csv(speedall,file = paste("speed", i, ".csv") ,sep = " ")
#speed
###########
dataaggxsd=aggregate(V3 ~ name4 , FUN =sd, data=subject_m)
dataaggxmean=aggregate(V3 ~ name4 , FUN =mean, data=subject_m)
dataaggymean=aggregate(V4 ~ name4 , FUN =mean, data=subject_m)


################################################
write.csv(dataaggymean,file = paste("meany", i, ".csv") ,sep = " ")
write.csv(dataaggxmean,file = paste("meanx", i, ".csv") ,sep = " ")
dataaggxsd=aggregate(V3 ~ name4 , FUN =sd, data=subject_m)
#######################################################################################################
write.csv(dataaggxsd,file = paste("standardx", i, ".csv") ,sep = " ")
s16m=subject_m[subject_m[,6]=="s016",]
s6m=subject_m[subject_m[,6]=="s006",]
s7m=subject_m[subject_m[,6]=="s007",]
s8m=subject_m[subject_m[,6]=="s008",]
s11m=subject_m[subject_m[,6]=="s011",]
s12m=subject_m[subject_m[,6]=="s012",]
s14m=subject_m[subject_m[,6]=="s014",]
s15m=subject_m[subject_m[,6]=="s015",]
s16m=subject_m[subject_m[,6]=="s016",]
s17m=subject_m[subject_m[,6]=="s017",]
s18m=subject_m[subject_m[,6]=="s018",]
s001m=subject_m[subject_m[,6]=="s001",]


x01m=mean(s001m$V3)
x6mm=mean(s6m$V3)
x6s=sum(s6m$V3)
x6d=mean(diff(s6m$V3))

x7d=mean(diff(s7m$V3))
svmm=mean(s7m$V3)
svmy=mean(s7m$V4)

write.csv(x01d,file = paste("xcoordinatediff", i, ".csv") ,sep = " ")
#plotting y coordinates for each subject 
plot(s16m$V4,type="l",col=c(rainbow(3)),lwd=3, xlab ="Time", ylab = "y coordinates")
#x vs.y coordinates
plot(s16m$V3,s16m$V4,type="l",col=c(rainbow(3)),lwd=3, xlab ="x coordinates", ylab = "y coordinates",Main="X vs.y coordinate for subject16")
plot(s6m$V4,type="l",col="green",lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s6m$V3,s6m$V4,type="l",col="green",lwd=3, xlab ="x coordinates", ylab = "y coordinates")
plot(s7m$V4,type="l",col="blue",lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s7m$V3,s7m$V4,type="l",col="blue",lwd=3, xlab ="x coordinates", ylab = "y coordinates")
plot(s8m$V4,type="l",col="blue",lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s8m$V3,s8m$V4,type="l",col="blue",lwd=3, xlab ="x coordinates", ylab = "y coordinates")
plot(s11m$V4,type="l",col=c(rainbow(6)),lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s11m$V3,s11m$V4,type="l",col="blue",lwd=3, xlab ="x coordinates", ylab = "y coordinates")
plot(s12m$V4,type="l",col=c(rainbow(5)),lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s12m$V3,s12m$V4,type="l",col=c(rainbow(5)),lwd=3, xlab ="x coordinates", ylab = "y coordinates")
matplot(,cbind(s16m$V4,s6m$V4,s7m$V4,s8m$V4,s11m$V4,s12m$V4),type="l",col=c(rainbow(3)),lwd=3, xlab ="Time", ylab = "y coordinates")
matplot(,cbind(s16m$V4,s6m$V4,s7m$V4),type="l",col=c(rainbow(3)),lwd=3, xlab ="Time", ylab = "y coordinates")

matplot(,cbind(s16m$V3,s6m$V3,s7m$V3,s8m$V3,s11m$V3,s12m$V3),type="l",col=c(rainbow(3)),lwd=3, xlab ="Time", ylab = "X coordinates")

###############################################################################################

#######################################################################################33
#length of the curve 
lengthofcurve1<- function(x) {
  sqrt(diff(subject_m$V3)^2+(diff(subject_m$V4)^2))
 
}
lengthofcurvef=sqrt(diff(subject_m$V3)^2+(diff(subject_m$V4)^2))
lengthofcurve= lapply(subject_m,lengthofcurve1)
plot(lengthofcurve$V4,col=c(rainbow(4)),lwd=3, xlab ="time", ylab = "length of curve for all subjects")

# length for each subject

lengthofcurve16<- function(x) {
  sqrt(diff(s16m$V3)^2+(diff(s16m$V4)^2))
}
lengthofcurve116= lapply(s16m,lengthofcurve16)
plot(leng thofcurve116$V3,type="p",col="red",lwd=2, ylab = "length of curve for  subject 16")

#1
lengthofcurve1<- function(x) {
  sqrt(diff(s001m$V3)^2+(diff(s001m$V4)^2))
}
lengthofcurve1= lapply(s001m,lengthofcurve1)
times1=diff(s001m$V1)
speed1=lengthofcurve1$V3/times1
print(mean(lengthofcurve1$V3))
print(mean(times1))
print(mean(speed1))
Acceleration1=speed1/times1
print(mean(Acceleration1))
#16
times16=diff(s16m$V1)
speed16=lengthofcurve116$V3/times16
plot(speed16,col="red", xlab ="time", ylab = "speed for  subject 16")


#6
lengthofcurve6<- function(x) {
  sqrt(diff(s6m$V3)^2+(diff(s6m$V4)^2))
}
times6=diff(s6m$V1)
lengthofcurve6= lapply(s6m,lengthofcurve6)
speed6=lengthofcurve6$V3/times6

Acceleration1=speed6/times6
print(mean(lengthofcurve6$V3))
print(mean(times6))
print(mean(speed6))
Acceleration6=speed6/times6
print(mean(Acceleration6))
print(sd(s6m$V3))

plot(speed6,col="blue", xlab ="time", ylab = "speed for  subject 6")



#7
lengthofcurve7<- function(x) {
  sqrt(diff(s7m$V3)^2+(diff(s7m$V4)^2))
}
times7=diff(s7m$V1)
lengthofcurve7= lapply(s7m,lengthofcurve7)
speed7=lengthofcurve7$V3/times7
Acceleration7=speed7/times7
print(mean(lengthofcurve7$V3))
print(mean(times7))
print(mean(speed7))
Acceleration7=speed7/times7
print(mean(Acceleration7))
print(sd(s7m$V3))

plot(speed7,col="green", xlab ="time", ylab = "speed for  subject7")
#8
lengthofcurve8<- function(x) {
  sqrt(diff(s8m$V3)^2+(diff(s8m$V4)^2))
}
times8=diff(s8m$V1)
lengthofcurve8= lapply(s8m,lengthofcurve8)
speed8=lengthofcurve8$V3/times8
plot(speed8,col="pink", xlab ="time", ylab = "speed for  subject 8")

#speed

timesall=diff(subject_m$V1)
averagespeed=lengthofcurve$V3/timesall
 plot(averagespeed,col=c(rainbow(3)),lwd=2,ylab = "average speed for the all subjects ")
 matplot(cbind(speed16,speed6,speed7,speed8),type="l",lwd=1,col=c("blue", "red", "yellow","pink",ylab = "average speed for the four subjects "))

#x1 = final[which(final$name4 == data$name4  & data$name3 == files[i] ),]
# x coordinate
dataaggx=aggregate(V3 ~ name4 , FUN =sum, data=fin)

names(dataaggx$V3)=dataaggx$name4
xpersub=as.vector(dataaggx$V3)
subjects=as.vector(dataaggx$name4)
names(xpersub)=subjects
plot(xpersub, xlab = "subjects",ylab = "x coordinates ",col=c("blue", "red", "yellow","green"),lwd=2,main = "Aggregated x coordinates for each subject")

dataaggxsd=aggregate(V3 ~ name4 , FUN =sd, data=fin)
#Plot for sum of x coordinates vs.standard deviation for all subjects
plot(xpersub,dataaggxsd$V3,xlab = "sum of x coordinates",ylab = "standard deviation of x coordinates ", col=c("blue", "red", "yellow","green"),lwd=3)
# standard deviation for x coordinates
plot(dataaggxsd$V3, xlab = "subjects",ylab = " x coordinates ",col=c("blue", "red", "yellow","green"),lwd=2,main="Aggregated standard deviation for x coordinate for each subject")

##################################################################################################

#Time 

dataaggt=aggregate(V1 ~ name4 , FUN =diff, data=fin)
dataaggt=aggregate(V1 ~ name4 , FUN =diff, data=subject_m)

#wrong #plot(dataaggt$v1,type="l",col=c(rainbow(9)),lwd=2, xlab ="Time", ylab = "average speed for the two subjects ")
attach(dataaggt)
sub_1=data.frame(dataaggt$V1[[1]])
sub_6=data.frame(dataaggt$V1[[2]])
sub_7=data.frame(dataaggt$V1[[3]])
sub_8=data.frame(dataaggt$V1[[4]])
sub_11=data.frame(dataaggt$V1[[5]])
sub_12=data.frame(dataaggt$V1[[6]])

ts.plot(sub_1)
plot(cbind(zoo(sub_1),zoo(sub_6),zoo(sub_7),zoo(sub_8),zoo(sub_11),zoo(sub_12)),type="l",col=c(rainbow(6)),lwd=3, xlab ="subjects", ylab = "Time difference ")
 
#error#scatterplot.matrix(~sub_1+sub_6+sub_7+sub_8+sub_11+sub_12)
#pairs(~ sub_1+sub_6+sub_7+sub_8+sub_11+sub_12,data=dataaggt)

#sub1_6=dataaggt[2,]
#dataaggtt=as.data.frame(unlist(dataaggt$V1))
#dataaggt[2]$name4

#Aggregated y coordinates

dataaggy=aggregate(V4 ~ name4 , FUN =sum, data=fin)

ypersub=as.vector(dataaggy$V4)

#pairs(~ ypersub+xpersub ,data=fin,col=c(rainbow(4)),lwd=3)
plot(ypersub, xlab = "subjects",ylab = "y coordinates ",col=c("blue", "red", "yellow","green"),lwd=3)


# plot x coordinates vs. y coordinates 
plot(xpersub,ypersub,xlab = "x coordinates",ylab = "y coordinates ",col=c("blue", "red", "yellow","green"),lwd=3)



#standard deviation 
dataaggysd=aggregate(V4 ~ name4 , FUN =sd, data=fin)


plot(zoo(dataaggysd$V4) ,col=c(rainbow(4)),lwd=3,xlab = "subjects",ylab = "standard deviation for Y coordinates" )

plot(~ cbind(zoo(dataaggysd$V4),zoo(dataaggy$V4)),lwd=3,col=c("blue", "red", "yellow"),ylab = "sum of y coordinates vs standard deviation of y coordinates")

#difference between pair of observations for all subjects 
dataaggydiff=aggregate(V4 ~ name4 , FUN =diff, data=fin)
dataaggydiff=as.list(dataaggydiff)
dataaggydiff1=as.data.frame(unlist(dataaggydiff$V4))

 plot(dataaggydiff1)
 
 ysub_1=data.frame(dataaggydiff$V4[[1]])
 ysub_6=data.frame(dataaggydiff$V4[[2]])
 ysub_7=data.frame(dataaggydiff$V4[[3]])
 ysub_8=data.frame(dataaggydiff$V4[[4]])
 ysub_11=data.frame(dataaggydiff$V4[[5]])
ysub_12=data.frame(dataaggydiff$V4[[6]])
 
 plot(cbind(zoo(ysub_1),zoo(ysub_6),zoo(ysub_7),zoo(ysub_8),zoo(sub_11),zoo(ysub_12)),type="l",col=c(rainbow(6)),lwd=3, xlab ="subjects", ylab = "y coordinates  difference ")


#newlist=listFromMlist(dataaggydiff,dataaggydiff$V4)

#spam = do.call("rbind", sapply(dataaggydiff, "[[",4 ))
#unique(spam[,4])
#spam=as.matrix(spam)
 #plot(spam)
 

# x coordinates vs. y coordinates 
matplot(,cbind(dataaggx$V3,dataaggy$V4),type="l",col=c(rainbow(3)),lwd=2, xlab ="subject", ylab = "x coordinates vs. y coordinates ")

#standard deviation for x coordinates vs. y coordinates 
matplot(,cbind(dataaggxsd$V3,dataaggysd$V4),type="l",col=c(rainbow(3)),lwd=2, xlab ="subject", ylab = "x coordinates vs. y coordinates ")

plot(dataaggx$V3~factor(dataaggx$name4), lwd=2, xlab="subject",ylab ="x coordinates ", main="subject verses  sum of x coordinates",col=c(rainbow(3)))

plot(dataaggx$V3,dataaggy$V4,lwd=3, xlab="x coordinates",ylab ="y coordinates ", main="  sum of x coordinates vs. sum of y coordinates",col=c(rainbow(5)))


dataaggt=aggregate(V1 ~ name4 , FUN =sum, data=fin)

dataaggp=aggregate(V3~ name4 + V2 , FUN =sum, data=fin)

dataaggm=dataaggp[which(dataaggp$V2 == "M"),]
dataaggG=dataaggp[which(dataaggp$V2 == "G"),]

###############################################################################################

#Gaze direction 

subject_G=fin[fin[,2]=="G",]

s16G=subject_G[subject_G[,6]=="s016",]
s6G=subject_G[subject_G[,6]=="s006",]
s7G=subject_G[subject_G[,6]=="s007",]
s8G=subject_G[subject_G[,6]=="s008",]
s11G=subject_G[subject_G[,6]=="s011",]
s12G=subject_G[subject_G[,6]=="s012",]


#ploting Y coordinates 
plot(s16G$V4,type="l",col=c(rainbow(3)),lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s6G$V4,type="l",col="blue",lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s7G$V4,type="l",col=c(rainbow(3)),lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s8G$V4,type="l",col=c(rainbow(3)),lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s11G$V4,type="l",col=c(rainbow(3)),lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s12G$V4,type="l",col=c(rainbow(4)),lwd=3, xlab ="Time", ylab = "y coordinates")

#############################################################################################

plot(s16G$V3,s16G$V4,type="l",col=c(rainbow(3)),lwd=3, xlab ="x coordinates", ylab = "y coordinates",Main="X vs.y coordinate for subject16")
plot(s6G$V4,type="l",col="green",lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s6G$V3,s6G$V4,type="l",col="green",lwd=3, xlab ="x coordinates", ylab = "y coordinates")
plot(s7G$V4,type="l",col="blue",lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s7G$V3,s7G$V4,type="l",col="blue",lwd=3, xlab ="x coordinates", ylab = "y coordinates")
plot(s8G$V4,type="l",col="blue",lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s8G$V3,s8G$V4,type="l",col="blue",lwd=3, xlab ="x coordinates", ylab = "y coordinates")
plot(s11G$V4,type="l",col=c(rainbow(6)),lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s11G$V3,s11G$V4,type="l",col="blue",lwd=3, xlab ="x coordinates", ylab = "y coordinates")
plot(s12G$V4,type="l",col=c(rainbow(5)),lwd=3, xlab ="Time", ylab = "y coordinates")
plot(s12G$V3,s12G$V4,type="l",col=c(rainbow(5)),lwd=3, xlab ="x coordinates", ylab = "y coordinates")
matplot(,cbind(s16G$V4,s6G$V4,s7G$V4,s8G$V4,s11G$V4,s12G$V4),type="l",col=c(rainbow(3)),lwd=3, xlab ="Time", ylab = "y coordinates")
matplot(,cbind(s16G$V4,s6G$V4,s7G$V4),type="l",col=c(rainbow(3)),lwd=3, xlab ="Time", ylab = "y coordinates")

matplot(,cbind(s16G$V3,s6G$V3,s7G$V3,s8G$V3,s11G$V3,s12G$V3),type="l",col=c(rainbow(3)),lwd=3, xlab ="Time", ylab = "X coordinates")

############################
# mouse vs. Gaze direction


matplot(,cbind(s16m$V3,s16G$V3),type="l",col=c(rainbow(3)),lwd=3, xlab ="time index", ylab = "x coordinates",Main="X  coordinate for subject16 for both mouse and eye direction")

matplot(,cbind(s16m$V4,s16G$V4),type="l",col=c(rainbow(3)),lwd=3, xlab ="time index", ylab = "Y coordinates", Main="Y  coordinate for subject16 for both mouse and eye direction")

matplot(,cbind(s6m$V4,s6G$V4),type="l",col=c(rainbow(3)),lwd=3, xlab ="Time", ylab = "y coordinates",Main="Y  coordinate for subject6 for both mouse and eye direction")

matplot(,cbind(s7m$V4,s7G$V4),type="l",col=c(rainbow(3)),lwd=3, xlab ="Time", ylab = "y coordinates")
subject_MC=fin[fin[,2]=="MC",]
#################################################################################
###################################################
#######################################
#####################################################################
#######################################
######################################################
#Decision tree
library(party)
#####################
mousefeaturss=read.csv("C:/Users/fedaaelderdesawe/Google Drive/featuresM.csv")
eyefeaturss= read.csv("C:/Users/fedaaelderdesawe/Google Drive/featuresG.csv")
mouseclickfeaturss=read.csv("C:/Users/fedaaelderdesawe/Google Drive/featuresMC.csv")
#allfeaturs27=rbind(mousefeaturss,eyefeaturss,mouseclickfeaturss)
featuresall <- read.csv("C:/Users/fedaaelderdesawe/Google Drive/featuresallsubjects.csv")

features2 <- read.csv("C:/Users/fedaaelderdesawe/Google Drive/features2.csv")
features= read.csv("C:/Users/fedaaelderdesawe/Google Drive/allfeaturesallclasses.csv")
features=features[,-1]
ct <- ctree(name4 ~ ., data=allasessions,)  
              controls = ctree_control(minsplit=30, minbucket=10, maxdepth=5)
pClassId <- predict(ct)
print(pClassId )
summary(ct)
summary(controls)
allasessions=read.csv("C:/Users/fedaaelderdesawe/Google Drive/sessions/sessions.csv")
#############################################3
require(ISLR)
require(tree)
attach(allasessions)
subjects=features2[,1]
allmousefeaturss=read.csv("C:/Users/fedaaelderdesawe/Google Drive/allimportantfeaturesmouse.csv")
###############################################
allfeaturesallclasses <- read.csv("C:/Users/fedaaelderdesawe/Google Drive/allfeaturesallclasses.csv")
#mouse tree 

tree.subjectsm = tree(name4~ .-name4 , data =allasessions)
summary(tree.subjectsm)
plot(tree.subjectsm)
text(tree.subjectsm, pretty = 0)
#importance(tree.subjectsm)
cv.model=cv.tree(tree.subjectsm ,FUN=prune.misclass)
plot(cv.model$size,cv.model$dev,type="b")
tree(formula = name4 ~ ., data =allfeaturesallclasses)

#choosing some features 

mousetree.subjects = tree(name4~Accelerationallm +scaledx+scaledy+timem+logx+logy+lengthofcurvesm+speedallm , data =allmousefeaturss)
summary(mousetree.subjects)
cv.model=cv.tree(mousetree.subjects ,FUN=prune.misclass)
plot(cv.model$size,cv.model$dev,type="b")

#mouse with 27 observations
mousetreepersub=tree(subjects~ .-subjects , data =mousefeaturss)
summary(mousetreepersub)
plot(mousetreepersub)
text(mousetreepersub, pretty = 0)

#######################################################################################
#mouse and eye

names(eyefeaturss)=names(mousefeaturss)
names(eyefeaturss)
mouse_eye=rbind(mousefeaturss,eyefeaturss)
mouse_eyetree = tree(subjects~ .-subjects , data =mouse_eye)
summary(mouse_eyetree)
plot(mouse_eyetree)
text(mouse_eyetree, pretty = 0)

###########################################################
#mouse ,eye and mouse click 

mouse_eye_clicktree=tree(subjects~ .-subjects , data =featuresall)
summary(mouse_eye_clicktree)
plot(mouse_eye_clicktree)
text(mouse_eye_clicktree, pretty = 0)

# tree for all observations for all classes 
bigtree = rpart(name4~ .-name4 , data =allasessions,method = "class") #too much run time i didnt got the result
summary(bigtree)
printcp(bigtree)
##################

library(rpart)

# grow tree 
#subjects=features2$subjects
#features=cbind(subjects,features)
fitall <- rpart(name4 ~ ., method="class", data=allasessions )
summary(fitall)
printcp(fitall)
########mouse fit
fitm <- rpart(subjects ~ .-subjects, method="class", data=mousefeaturss )
printcp(fitm)
printcp(fit) # display the results 
plotcp(fitm) # visualize cross-validation results 
summary(fitm) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE,main="Classification Tree for mouse features")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "c:/tree.ps", 
     title = "Classification Tree for mouse features")
##########################################################


#######################################
attach(allasessions)
require(sampling)
library(dplyr)
library(devtools)
sample=strata(allasessions,68,method =c("s001", "s006","s007","s008",
"s011","s012","s014","s015","s016","s017","s018","s019",
"s020","s021","s023" ,"s024","s026","s028","s031","s032","s033","s034","s036","s042" ,"s043" ,"s044","s045"))
set.seed(100)
strata(allasessions, method = "name4", size = 54)
allasessions %>%
  group_by(name4) %>%
  sample_n(68,replace=TRUE)
#####################################33
library(caTools)
allasessions=read.csv("C:/Users/fedaaelderdesawe/Google Drive/sessions/sessions.csv")
train_rows = sample.split(allasessions$name4, SplitRatio=0.7)
test_rows=!train_rows
test1=allasessions[test_rows,]
train =allasessions[ train_rows,]
test  = allasessions[!train_rows,]
write.csv(train, "train.csv")
write.csv(test, "test.csv")
trainset=read.csv("C:/Users/fedaaelderdesawe/Google Drive/sessions/train.csv")
testset=read.csv("C:/Users/fedaaelderdesawe/Google Drive/sessions/test.csv")
testset=testset[,-1]
####
rpart.tree1 <- rpart(name4  ~ .-name4,data=allasessions ,subset=trainset)
rpart.tree1
summary(rpart.tree1)
predictions <- predict(rpart.tree1, testset, type="class")
mean(predictions!=name4)
table(test$name4, predictions)
##########################
alpha     <- 0.67 # percentage of training set
inTrain   <- sample(1:nrow(allasessions), alpha * nrow(allasessions))
train.set <-allasessions[inTrain,]
test.set  <- allasessions[-inTrain,]
subjects=allasessions$name4

rpart.tree <- rpart(name4  ~ .-name4, data=train)
plot(rpart.tree, uniform=TRUE, branch=0.6, margin=0.05)
text(rpart.tree, all=TRUE, use.n=TRUE)
title("Training Set's Classification Tree")
predictions <- predict(rpart.tree, test, type="class")
mean(predictions!=subjects)
table(test$name4, predictions)
summary(rpart.tree)
cv.model=cv.tree(rpart.tree ,FUN=prune.misclass)
############################################################################
## random forest:

library("randomForest")
library(randomForest)
library(stratification)
set.seed(5)
subjects=c("s001", "s006","s007","s008","s011","s012","s014","s015","s016","s017","s018","s019","s020","s021","s023" ,"s024","s026","s028","s031","s032","s033","s034","s036","s042" ,"s043" ,"s044","s045")

######################3
######### random forest with all data
fitrandom <- randomForest(name4  ~ .-name4,   data=allasessions,importance=TRUE,mtry=50,ntree=1000,nodesize = 1)
print(fitrandom)
print.table(fitrandom$confusion)
write.csv(fitrandom$confusion)
write.csv(fitrandom$confusion, file = paste("confusion matrix", ".csv") ,sep = " ")
#accuracy
sum(diag(fitrandom$confusion))/81 #accuracy =.7283

#sample_n(allasessions,68, group_by(subjects, lc) )

mySampSize=ceiling(table(allasessions$name4) * 0.632)
mySampSize=train_rows[allasessions,]
train1=allasessions[mySampSize,]
test1= floor(table(allasessions$name4) * 0.368)
testn=allasessions[-mySampSize,]
stratified=strata(allasessions,size=c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2))

#fit random forest with samplesize
fitrandom1 <- randomForest(name4 ~ .-name4,strata=allasessions$name4 ,subset=train_rows,importance=TRUE,mtry=75,nodesize = 1,ntree=1000 ,  data=allasessions)
fitrandom1
write.csv(fitrandom1$confusion, file = paste("fitrandom1", ".csv") ,sep = " ")
#accuracy
sum(diag(fitrandom1$confusion))/81 #accuracy =.6419

#test error
predict <- predict(fitrandom1, test, type="class")
tt=table(predict,test$name4)
tt
mean(predict!=test$name4)
sum(diag(tt))/27

#fit the random forest with train data
allsub=randomForest(name4  ~ .-name4 ,data=allasessions,subset =train_rows ,importance=TRUE,nodesize = 1,keep.forest=TRUE)
allsub
sum(diag(allsub$confusion))/54 #training accuracy is too low
predictallsub <- predict(allsub, test)
allsubt=table(predictallsub,test$name4)
allsubt
sum(diag(allsubt))/27 #accuracy .9629

try=randomForest(name4~.,data=train,proximity=TRUE, xtest=test[,names(test)!='name4'],ytest=test[,'name4'],nodesize = 1,ntree=1000,keep.forest=TRUE)

try

try1=randomForest(name4~.,data=train,nodesize = 1,ntree=1000,keep.forest=TRUE)
try1
sum(diag(try$confusion))/54
predicttrian <- predict(try, train, type="class")
trainacc=table(predicttrian ,train$name4)
trainacc
write.csv(try$confusion, file = paste("high_confusion matrix", ".csv") ,sep = " ")
write.csv(try, file = paste("try", ".csv") ,sep = " ")
write.csv(trainacc, file = paste("prediction_train", ".csv") ,sep = " ")
sum(diag(trainacc))/sum(trainacc) #training accuracy 100%
#test
predicttry <- predict(try, test)
result=table(predicttry,test$name4)
result
write.csv(result, file = paste("prediction_test", ".csv") ,sep = " ")
sum(diag(result))/sum(result)

summary(fitrandom)# view results 
importance(fitrandom) # importance of each predictor
cv.carseats=cv.tree(rpart.tree,FUN=prune.misclass)
###############################################################################################
mySampSize <- ceiling(table(allasessions$name4) * 0.632)
sam=ceiling(table(allasessions[,names(allasessions)=='name4']*0.7))

r <- randomForest(name4 ~.-name4, data=allasessions,strata=allasessions$name4,sampsize=c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), mtry=40,importance=TRUE, ntree=500 ,nodesize = 1)

r
sampsize=c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
tr=as.data.frame(allasessions[sampsize,],rep=(2))
print(r)
r
plot(r)

predictionsr <- predict(r, test)
conf=table(test$name4, predictionsr)
conf
error=1-sum(diag(conf))/sum(conf)
error
mean(predictionsr==name4)
#################################################################33
#suppprot vector machine
library(e1071)
svmfit=svm(name4~.-name4,  data = train,kernel="radial",  gamma=1, cost=1)
plot(svmfit)
summary(svmfit)
svm.pred <- predict(svmfit, test)
svm=table(test$name4, svm.pred)
sum(diag(svm))/sum(svm)
##################################################################################
allasessions=read.csv("C:/Users/fedaaelderdesawe/Google Drive/sessions/sessions.csv")
trainset=read.csv("C:/Users/fedaaelderdesawe/Google Drive/sessions/train.csv")
testset=read.csv("C:/Users/fedaaelderdesawe/Google Drive/sessions/test.csv")
Ma = matrix(nrow = nrow(allasessions), ncol = nrow(allasessions))
n=list(allasessions)
all.list <- split(allasessions, seq(nrow(allasessions)))
require(dtw)
for(i in 1:nrow(allasessions)){
 
  n1=allasessions[i,]
  
  for(j in 2:length(allasessions)){
    n2= allasessions[j,]
    
    x1=n1
    x2=n2
    # x2= scaledlist[[j]]
    print(x1)
    print(x2)
    d = dtw(x1,x2,keep=TRUE)
    cat(i)
    cat(j)
    Ma[i,j] = d$distance
  }}
length=allasessions$lengthofcurvesm0
speed=allasessions$speedm0
time=allasessions$timeaveragem

subjects=allasessions$name4
k=knn(allasessions[,-1], ,subjects, k = 1, prob = TRUE)
predictions=subjects[k]
predictions
confusionmatrix=table(predictions,subjects)
confusionmatrix
