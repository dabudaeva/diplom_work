##########################################################################################
######### Визуализация

groupmeanEth <- analysis %>% group_by(ID_school) %>% summarise(meanEth=mean(as.numeric(Eth_n))-1)
groupmeanSex <- analysis %>% group_by(ID_school) %>% summarise(meanSex=mean(as.numeric(Sex_n))-1)
groupmeanAge <- analysis %>% group_by(ID_school) %>% summarise(meanAge=mean(Age^2))
groupmeanRel <- analysis %>% group_by(ID_school) %>% summarise(meanRel=mean(Rel))
groupmeanCBV <- analysis %>% group_by(ID_school) %>% summarise(meanCBV=mean(as.numeric(CBV))-1)
groupmeanCBP <- analysis %>% group_by(ID_school) %>% summarise(meanCBP=mean(as.numeric(CBP))-1)
groupmeanAGR <- analysis %>% group_by(ID_school) %>% summarise(meanAGR=mean(Agr))

mean_sch <- merge(groupmeanEth,groupmeanSex, by="ID_school")
mean_sch <- merge(mean_sch,groupmeanAge, by="ID_school")
mean_sch <- merge(mean_sch,groupmeanRel, by="ID_school")
mean_sch <- merge(mean_sch,groupmeanCBV, by="ID_school")
mean_sch <- merge(mean_sch,groupmeanCBP, by="ID_school")
mean_sch <- merge(mean_sch,groupmeanAGR, by="ID_school")


rm(groupmeanEth,groupmeanAge,groupmeanCBP,groupmeanCBV,groupmeanRel,groupmeanSex,groupmeanAGR)
rownames(mean_sch) <- mean_sch$ID_school

groupmeanCBV <- analysis %>% group_by(ID_school) %>% summarise(meanCBV=mean(as.numeric(CBV))-1)
sch <- readxl::read_excel("~/Desktop/диплом/диплом_анализ/data/school_data.xlsx")
sch <- select(sch,ID_school,locationStatus2,school_status)
names(sch) <- c("ID_school","type","status")
sch$ID_school <- as.character(sch$ID_school)
sch$type.город <- ifelse(sch$type=="городское поселение",1,0)
sch$type.село <- ifelse(sch$type=="сельское поселение",1,0)

sch$status[sch$status=="лиц"] <- c("лиц&гим")
sch$status[sch$status=="гимназия"] <- c("лиц&гим")

sch$status.оош <- ifelse(sch$status=="оош",1,0)
sch$status.сош <- ifelse(sch$status=="сош",1,0)
sch$status.элитные <- ifelse(sch$status=="лиц&гим",1,0)

mean_sch <- merge(groupmeanCBV,sch,by="ID_school")
rm(groupmeanCBV,sch)

boxplot(mean_sch$meanCBV~mean_sch$status)


write.csv(mean_sch,"~/Desktop/sent.csv")


##########################################################################################
######### графики по школам
names(mean_sch)

ggplot(mean_sch) + geom_point(aes(x=meanCBP,y=meanCBV)) + 
  geom_abline(intercept=lm(meanCBV~meanCBP,mean_sch)$coefficients[1],
              slope=lm(meanCBV~meanCBP,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="Mean CB perpetration",y="Mean CB victimization")


ggplot(mean_sch) + geom_point(aes(x=size,y=meanCBV)) + 
  geom_abline(intercept=lm(meanCBV~size,mean_sch)$coefficients[1],
              slope=lm(meanCBV~size,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="Size of schools",y="CB victimization")

ggplot(mean_sch) + geom_boxplot(aes(x=type,y=meanCBV)) + 
  # geom_abline(intercept=lm(meanCBV~size,mean_sch)$coefficients[1],
  #             slope=lm(meanCBV~size,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="Type of schools",y="CB victimization")

ggplot(mean_sch) + geom_boxplot(aes(x=status,y=meanCBV)) + 
  # geom_abline(intercept=lm(meanCBV~size,mean_sch)$coefficients[1],
  #             slope=lm(meanCBV~size,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="Status of schools",y="CB victimization")


ggplot(mean_sch) + geom_point(aes(x=size,y=meanCBP)) + 
  geom_abline(intercept=lm(meanCBP~size,mean_sch)$coefficients[1],
              slope=lm(meanCBP~size,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="Size of schools", y="Mean CB perpetration")





ggplot(mean_sch) + geom_point(aes(x=meanEth,y=meanCBV)) + 
  # geom_abline(intercept=lm(meanCBV~meanEth,mean_sch)$coefficients[1],
  #             slope=lm(meanCBV~meanEth,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="Ethnicity", y="CB victimization")

ggplot(mean_sch) + geom_point(aes(x=meanAge,y=meanCBV)) + 
  # geom_abline(intercept=lm(meanCBV~meanAge,mean_sch)$coefficients[1],
  #             slope=lm(meanCBV~meanAge,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="Age", y="CB victimization")

ggplot(mean_sch) + geom_point(aes(x=meanSex,y=meanCBV)) + 
  # geom_abline(intercept=lm(meanCBV~meanSex,mean_sch)$coefficients[1],
  #             slope=lm(meanCBV~meanSex,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="Gender", y="CB victimization")



ggplot(mean_sch) + geom_point(aes(x=meanRel,y=meanCBV)) + 
  # geom_abline(intercept=lm(meanCBV~meanRel,mean_sch)$coefficients[1],
  #             slope=lm(meanCBV~meanRel,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="Relationship with peers", y="Mean CB victimization")


ggplot(mean_sch) + geom_point(aes(x=mean_sch$privacy,y=meanCBV)) + 
  # geom_abline(intercept=lm(meanCBV~meanRel,mean_sch)$coefficients[1],
  #             slope=lm(meanCBV~meanRel,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="Agression (online)", y="CB victimization")

ggplot(mean_sch) + geom_point(aes(x=meanAGR,y=meanCBV)) + 
  geom_abline(intercept=lm(meanCBV~meanAGR,mean_sch)$coefficients[1],
              slope=lm(meanCBV~meanAGR,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="Agression (online)", y="CB victimization")


ggplot(mean_sch) + geom_point(aes(x=log(size),y=meanCBV)) + 
  geom_abline(intercept=lm(meanCBV~log(size),mean_sch)$coefficients[1],
              slope=lm(meanCBV~log(size),mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="log(size)", y="Mean CB victimization")
dev.off()


ggplot(mean_sch) + geom_point(aes(x=meanAge,y=meanCBV)) + 
  geom_abline(intercept=lm(meanCBV~meanAge,mean_sch)$coefficients[1],
              slope=lm(meanCBV~meanAge,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="",y="",title="Связь возраста и КБ")

ggplot(mean_sch) + geom_point(aes(x=meanSex,y=meanCBV)) + 
  geom_abline(intercept=lm(meanCBV~meanSex,mean_sch)$coefficients[1],
              slope=lm(meanCBV~meanSex,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="",y="",title="Связь пола и КБ")

ggplot(mean_sch) + geom_point(aes(x=meanEth,y=meanCBV)) + 
  geom_abline(intercept=lm(meanCBV~meanEth,mean_sch)$coefficients[1],
              slope=lm(meanCBV~meanEth,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="",y="",title="Связь национальности и КБ")

ggplot(mean_sch) + geom_point(aes(x=meanRel,y=meanCBV)) + 
  geom_abline(intercept=lm(meanCBV~meanRel,mean_sch)$coefficients[1],
              slope=lm(meanCBV~meanRel,mean_sch)$coefficients[2], col="lightblue3") +
  labs(x="",y="",title="Связь взаимоотношений со сверстниками и КБ")




##########################################################################################
######### Графики по классам
names(mean_sch)
plot(mean_sch$size,mean_sch$meanCBV,ylab ="Cyberbullying",xlab="Size") # 
plot(mean_sch$type,mean_sch$meanCBV,ylab ="Cyberbullying",xlab="Type")
plot(mean_sch$status,mean_sch$meanCBV,ylab ="Cyberbullying",xlab="Status")
plot(mean_sch$meanRel,mean_sch$meanCBV,ylab ="Cyberbullying",xlab="Relation")
plot(mean_sch$meanAGR,mean_sch$meanCBV,ylab ="Cyberbullying",xlab="Agression")
plot(mean_sch$meanRel,mean_sch$meanAGR,ylab ="Cyberbullying",xlab="Agression")

plot(mean_sch$meanAge,mean_sch$meanCBV,ylab ="Cyberbullying",xlab="Age")
plot(mean_sch$meanSex,mean_sch$meanCBV,ylab ="Cyberbullying",xlab="Gender")
plot(mean_sch$meanEth,mean_sch$meanCBV,ylab ="Cyberbullying",xlab="Agression")

plot(mean_sch$meanRel,mean_sch$meanCBV)

##########################################################################################
######### Графики на индивидуальном ур-не




##########################################################################################
woc <- select(analysis,CBV,Eth,Eth_n,ID_school)
woc <- na.omit(woc)

m6 <- glmer(CBV ~ Age + type_n + (Age|ID_school), data=woc, family=binomial(link="logit"))
woc$pred <- predict(m6)

fixed <- fixef(m6)
random <- ranef(m6)$ID_school
param <- cbind((random[1]+fixed[1]),(random[2]+fixed[2]))
param$ID_school <- row.names(param)
param <- merge(param,analysis,by="ID_school")
param <- param[,1:7]
param <- unique(param)

param$color <- recode(as.character(param$type_n),`0` = "midnightblue", `1` = "lightseagreen", `2` = "yellow2")
ggplot() + 
  geom_line(data=woc,aes(x=as.numeric(Sex),y=pred,col=ID_school)) + 
  theme(legend.position="none") + 
  scale_color_manual(values = param$color) +
  labs(y="CB Victimization",x="Ethnicity",title = "Random Effects")

param0 <- param[param$type_n==0,]
param1 <- param[param$type_n==1,]
param2 <- param[param$type_n==2,]

ggplot(data.frame()) + geom_point(aes()) + 
  xlim(-3, 3) + ylim(min(woc$pred), max(woc$pred)) + 
  geom_abline(intercept=param0$`(Intercept)`,slope=param0$Age.x,col='midnightblue') +
  geom_abline(intercept=param1$`(Intercept)`,slope=param1$Age.x,col='lightseagreen') +
  geom_abline(intercept=param2$`(Intercept)`,slope=param2$Age.x,col='yellow2') +
  geom_abline(intercept=fixed[1],slope=fixed[2],col="red") +
plot(param$`(Intercept)`,param$Eth_n1)


#Scatterplot of Intercepts and Slopes
plot(param)

#Multilevel Graphing using Lattice
groups <- unique(wocfinal$ID_school)[sample(1:160,20)]
subset<- wocfinal[wocfinal$ID_school%in%groups,]

xyplot(cb_victim~ses |as.factor(ID_school), subset, col.line = 'black')
xyplot(cb_victim~ses |as.factor(ID_school), subset, col.line = 'black', type = c("p", "r"))
suppressWarnings(xyplot(cb_victim~ses |as.factor(ID_school), subset, col.line = 'black', type = c("p", "smooth")))


#Grouped Data on Same Plot
groups <- unique(wocfinal$ID_school)[sample(1:160,5)]
subset<- wocfinal[wocfinal$ID_school%in%groups,]

xyplot(cb_victim~ses,subset, type = c("p", "smooth"), group = wocfinal$ID_school)

#QQplots
qqmath(ranef(model, condVar = TRUE), strip = FALSE)







# ggplot(data = wocfinal,aes(x=relations,y=cb_victim,col=ID_school)) + geom_point() + theme(legend.position="none")


