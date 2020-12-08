#Read data
#https://www.ecuadorencifras.gob.ec/salud-salud-reproductiva-y-nutricion/
data<-read.csv("DataBAse(4).csv")

#Select variables of interest
keeps <- c("area", "prov", "upm", "id_viv","id_hogar", "id_per", "persona",
           "f2_s1_100_1", "f2_s1_100_2","f2_s1_100_3", "f2_s1_101","f2_s2_200", 
           "f2_s2_201", "f2_s2_206", "f2_s2_207",  "f2_s2_216_1", "f2_s2_216_2", 
           "f2_s2_217_1","f2_s2_217_2","f2_s2_217_3", "f2_s6_600_1", "f2_s6_601_1",
           "f2_s6_602_1", "f2_s6_603_1","f2_s6_600_2", "f2_s6_601_2","f2_s6_602_2",
           "f2_s6_603_2","f2_s6_600_3", "f2_s6_601_3","f2_s6_602_3", "f2_s6_603_3",
           "f2_s6_600_4", "f2_s6_601_4","f2_s6_602_4", "f2_s6_603_4","f2_s6_600_5",
           "f2_s6_601_5","f2_s6_602_5", "f2_s6_603_5","f2_s6_600_6", "f2_s6_601_6",
           "f2_s6_602_6", "f2_s6_603_6","f2_s6_600_7", "f2_s6_601_7","f2_s6_602_7",
           "f2_s6_603_7","f2_s6_600_8", "f2_s6_601_8","f2_s6_602_8", "f2_s6_603_8",
           "f2_s6_600_9", "f2_s6_601_9","f2_s6_602_9", "f2_s6_603_9", "f2_s6_600_10",
           "f2_s6_601_10","f2_s6_602_10", "f2_s6_603_10","f2_s6_600_11", "f2_s6_601_11",
           "f2_s6_602_11", "f2_s6_603_11","f2_s6_600_12", "f2_s6_601_12","f2_s6_602_12",
           "f2_s6_603_12","f2_s6_627", "f2_s6_628","f2_s6_629", "f2_s6_630","f2_s6_631", 
           "f2_s6_632","f2_s6_633","f2_s8_800a", "f2_s8_801a","f2_s8_800b", "f2_s8_801b",
           "f2_s8_800c", "f2_s8_801c","f2_s8_800d", "f2_s8_801d","f2_s8_800e",
           "f2_s8_801e","f2_s8_800f", "f2_s8_801f","f2_s8_800g", "f2_s8_801g",
           "f2_s8_800h", "f2_s8_801h","f2_s8_800i", "f2_s8_801i", "f2_s8_802",
           "f2_s8_803", "f2_s8_804","f2_s8_805", "f2_s8_806","f2_s8_807", "f2_s8_808",
           "f2_s8_809","f2_s8_810", "f2_s8_811", "f2_s8_812","f2_s8_813","f2_s8_814", 
           "f2_s8_815","f2_s8_816", "f2_s8_817","f2_s8_818", "f2_s8_819","f2_s8_820", 
           "f2_s8_821","f2_s8_821_1", "f2_s8_822","f2_s8_823",
           "f2_s8_824", "f2_s8_825","f2_s8_826", "f2_s8_827", "f2_s8_828",
           "f2_s8_829", "f2_s8_830","f2_s8_831", "f2_s8_832_dias","f2_s8_832_semanas",
           "f2_s8_832_meses","f2_s8_832_anios", "f2_s8_833", "f2_s8_834", "f2_s8_835",
           "f2_s8_836", "f2_s8_837","f2_s8_838_1", "f2_s8_838_2","f2_s8_838_3",
           "f2_s8_838_4","f2_s8_839", "f2_s8_840","f2_s8_841", "f2_s8_842","f2_s8_844",
           "f2_s8_845","f2_s9_900", "f2_s9_901","f2_s9_902","f2_s9_905", 
           "f2_s10_1000","region", "etnia", "edadanios","gedad_anios", "nivins",
           "fexp", "estrato")

#remove the rest of variables
data<- subset(data, select = keeps)

#create a new variable to group ages into 4 groups (10-14, 15-18, 19-24, 15-49)
age_groups <- rep(NA, 48700)


for(i in 1:48700){
    if(data$f2_s1_101[i]<=14){
        age_groups[i]<-"10-14"
    }else if((data$f2_s1_101[i]<=18)&&(data$f2_s1_101[i]>=15)){
        age_groups[i]<-"15-18"
    }else if((data$f2_s1_101[i]<=24)&&(data$f2_s1_101[i]>=19)){
        age_groups[i]<-"19-24"
    }else{
        age_groups[i]<-"25-49"
    }
}
    
data<-cbind(data, age_groups)

#New variable for women who have ever been pregnant
ever_preg <- rep(NA, 48700)

for(i in 1:48700){
    if(data$f2_s2_200[i] == "si"|| data$f2_s2_207[i] == "si"){
        ever_preg[i]<-1
    }else{
        ever_preg[i]<-0
    }
}

data<-cbind(data, ever_preg)

#new variable to  categorize which have never had sex, 
#which have had sex but no pregnancies, and which have ever been pregnant.
sex_preg <- rep(NA, 48700)

for(i in 1:48700){
    if(data$f2_s8_803[i]=="no"||data$f2_s8_803[i]=="no desea contestar"){
        sex_preg[i]<-"No sex"
    }else if(data$f2_s8_803[i]=="si" && data$f2_s8_812[i]=="no"){
        sex_preg[i]<-"Sex/No preg."
    }else if(data$f2_s8_803[i]=="si" && data$f2_s8_812[i]=="si"){
        sex_preg[i]<-"Pregnancy"
    }else{
         sex_preg[i]<-NA
    }
}
    
data<-cbind(data, sex_preg)

#New variable
#Only for 10-24
#No sex, sex & no pregnant, first pregnancy before 18, after 18


age_fst_preg <- rep(NA, 48700)

for(i in 1:48700){
    if(data$f2_s8_803[i]=="no"||data$f2_s8_803[i]=="no desea contestar"){
        age_fst_preg[i]<-"No sex"
    }else if(data$f2_s8_803[i]=="si" && data$f2_s8_812[i]=="no"){
        age_fst_preg[i]<-"Sex/No preg."
    }else if(data$f2_s8_803[i]=="si" && data$f2_s8_812[i]=="si" && data$f2_s8_813[i]<=18){
        age_fst_preg[i]<-"Pregnant before 18"
    }else if(data$f2_s8_803[i]=="si" && data$f2_s8_812[i]=="si" && data$f2_s8_813[i]>18){
        age_fst_preg[i]<-"Pregnant after 18"
    }else{
        age_fst_preg[i]<-NA
    }
}
    

data<-cbind(data, age_fst_preg)

#NEW VARIABLE
#Use of (modern) contraceptive method first time
#1 is yes; 0 is no

use_contra_fst <- rep(NA, 48700)

for(i in 1:48700){
    if(!is.na(data$f2_s8_808[i]) && data$f2_s8_808[i] == 2){
        use_contra_fst[i]<-0
    }else if(!is.na(data$f2_s8_808[i]) && (data$f2_s8_808[i] == 1)&&(data$f2_s8_809[i] == "billings (moco cervical)" || data$f2_s8_809[i] == "coito interrumpido (retiro)" ||
            data$f2_s8_809[i] == "no sabe/ no responde" || data$f2_s8_809[i] == "otro, cuál?" ||
            data$f2_s8_809[i] == "ritmo, calendario"||data$f2_s8_807[i] == "fue sin consentimiento?")){
        use_contra_fst[i]<-0
    }else if(!is.na(data$f2_s8_808[i]) && (data$f2_s8_808[i] == 1)&&(data$f2_s8_809[i] == "condón" || data$f2_s8_809[i] == "implante(implanon, jadelle)" ||
       data$f2_s8_809[i] == "inyección anticonceptiva" || data$f2_s8_809[i] == "métodos vaginales" ||
      data$f2_s8_809[i] == "pastilla de emergencia (del día después)" || 
      data$f2_s8_809[i] == "pastillas anticonceptivas")){
        use_contra_fst[i]<-1
    }else{
        use_contra_fst[i]<-NA
    }   
}

data<-cbind(data, use_contra_fst)

#NEW VARIABLE
#Do they use or have ever used a (modern) contraceptive method
#1 if yes
#0 if no

use_contra_mthd <- rep(NA, 48700)

for(i in 1:48700){
    if(data$f2_s6_602_1[i] == 'si'|| data$f2_s6_603_1[i] == 'si'||
      data$f2_s6_602_2[i] == 'si'|| data$f2_s6_603_2[i] == 'si'||
      data$f2_s6_602_3[i] == 'si'|| data$f2_s6_603_3[i] == 'si'||
      data$f2_s6_602_4[i] == 'si'|| data$f2_s6_603_4[i] == 'si'||
      data$f2_s6_602_5[i] == 'si'|| data$f2_s6_603_5[i] == 'si'||
      data$f2_s6_602_6[i] == 'si'|| data$f2_s6_603_6[i] == 'si'||
      data$f2_s6_602_7[i] == 'si'|| data$f2_s6_603_7[i] == 'si'||
      data$f2_s6_602_8[i] == 'si'|| data$f2_s6_603_8[i] == 'si'||
      data$f2_s6_602_9[i] == 'si'|| data$f2_s6_603_9[i] == 'si'){
        use_contra_mthd[i] <- 1
    }else{
        use_contra_mthd[i] <- 0
    }
}

data<-cbind(data, use_contra_mthd)

#NEW VARIABLE
#Do they know of (modern) contraceptive methods
#1 if yes
#0 if no


#Si en la 600 dijo que si, en la 601 missing

know_contra_mthd <- rep(NA, 48700)


for(i in 1:48700){
    #If they know of the modern contraceptive methods
    #Top of mind
    if(data$f2_s6_600_1[i] == 1 || data$f2_s6_600_2[i] == 1||
      data$f2_s6_600_3[i] == 1 || data$f2_s6_600_4[i] == 1||
      data$f2_s6_600_5[i] == 1 || data$f2_s6_600_6[i] == 1||
      data$f2_s6_600_7[i] == 1 || data$f2_s6_600_8[i] == 1||
      data$f2_s6_600_9[i] == 1){
        
        know_contra_mthd[i] <-1
        
    #If they know modern contraceptive method
    #Not top of mind (induced)
    }else if((data$f2_s6_600_1[i] == 2 || data$f2_s6_600_2[i] == 2||
      data$f2_s6_600_3[i] == 2 || data$f2_s6_600_4[i] == 2||
      data$f2_s6_600_5[i] == 2 || data$f2_s6_600_6[i] == 2||
      data$f2_s6_600_7[i] == 2 || data$f2_s6_600_8[i] == 2||
      data$f2_s6_600_9[i] == 2 )
             &&(data$f2_s6_601_1[i] == "si" || data$f2_s6_601_2[i] == "si"||
      data$f2_s6_601_3[i] == "si" || data$f2_s6_601_4[i] == "si"||
      data$f2_s6_601_5[i] == "si" || data$f2_s6_601_6[i] == "si"||
      data$f2_s6_601_7[i] == "si" || data$f2_s6_601_8[i] == "si"||
      data$f2_s6_601_9[i] == "si" )){
        
        know_contra_mthd[i] <-1
    
    #If they dont know of modern contraceptive method
    }else{
        know_contra_mthd[i] <- 0
    }
}

data<-cbind(data, know_contra_mthd)

#NEW VARIABLE
#IF YOU WERE A STUDENT WHEN FIRST PREGNANT

stud_fst_preg<-rep(NA, 48700) 

for(i in 1:48700){
    if(data$f2_s8_820[i]=="si"){
        stud_fst_preg[i]<-1
    }else if(data$f2_s8_820[i]=="no" || data$f2_s8_820[i]=="nunca estudió"){
        stud_fst_preg[i]<-0
    }else{
        stud_fst_preg[i]<-NA
    }
}

data<-cbind(data,stud_fst_preg)

#NEW VARIABLE
#Only for 10-24
# 1- Teenage pregnancy if first pregnancy before 20
#0- Not teenage pregnancy if first pregnant after 20 or not at all

teen_preg <- rep(NA, 48700)

for(i in 1:48700){
    if(data$f2_s8_803[i]=="si" && data$f2_s8_812[i]=="si" && data$f2_s8_813[i]<=19){
        teen_preg[i]<-1
    }else if(data$f2_s8_803[i]=="no"||data$f2_s8_803[i]=="no desea contestar"||
            (data$f2_s8_803[i]=="si" && data$f2_s8_812[i]=="no")||
             (data$f2_s8_803[i]=="si" && data$f2_s8_812[i]=="si" && data$f2_s8_813[i]>19)){
        teen_preg[i]<-0
    }else{
         teen_preg[i]<-NA
    }
}
    

data<-cbind(data, teen_preg)

#NEW VARIABLE
preg_bef_21 <- rep(NA, 48700)

for(i in 1:48700){
    if(data$f2_s8_803[i]=="si" && data$f2_s8_812[i]=="si" && data$f2_s8_813[i]<=21){
        preg_bef_21[i]<-1
    }else if(data$f2_s8_803[i]=="no"||data$f2_s8_803[i]=="no desea contestar"||
            (data$f2_s8_803[i]=="si" && data$f2_s8_812[i]=="no")||
             (data$f2_s8_803[i]=="si" && data$f2_s8_812[i]=="si" && data$f2_s8_813[i]>21)){
        preg_bef_21[i]<-0
    }else{
         preg_bef_21[i]<-NA
    }
}
    

data<-cbind(data, preg_bef_21)

#NEW VARIABLE
#Pregnant before 21 if they have had sex

sx_preg_bef_21 <- rep(NA, 48700)

for(i in 1:48700){
    if(data$f2_s8_803[i]=="si" && data$f2_s8_812[i]=="si" && data$f2_s8_813[i]<=21){
        sx_preg_bef_21[i]<-1
    }else if((data$f2_s8_803[i]=="si" && data$f2_s8_812[i]=="no")||
             (data$f2_s8_803[i]=="si" && data$f2_s8_812[i]=="si" && data$f2_s8_813[i]>21)){
        sx_preg_bef_21[i]<-0
    }else{
         sx_preg_bef_21[i]<-NA
    }
}
    

data<-cbind(data, sx_preg_bef_21)

#NEW VARIABLE
#10-24 asked
#Age of first sexual relation
#before 15
#15-17
#18+

fst_sex_reltn<-rep(NA, 48700)

for(i in 1:48700){
    if(!is.na(data$f2_s8_804[i])&&data$f2_s8_804[i]<15){
        fst_sex_reltn[i]<-'<15'
    }else if(!is.na(data$f2_s8_804[i])&&data$f2_s8_804[i]>=15 && data$f2_s8_804[i]<=17){
        fst_sex_reltn[i]<-'15-17'
    }else if(!is.na(data$f2_s8_804[i])&&data$f2_s8_804[i]>=18 && data$f2_s8_804[i]<=24){
        fst_sex_reltn[i]<-'18+'
    }else{
        fst_sex_reltn[i]<-NA
    }
        
}
    
data<-cbind(data, fst_sex_reltn)

#New variable 
#First time at 14 or less
#1 is yes
#0 is no
#2 if who don't remember or don't want to answer -->to be removed from database after

fst_sex_14_less <- rep(NA, 48700)

for(i in 1:48700){
    if(!is.na(data$f2_s8_804[i])&&data$f2_s8_804[i]<15){
       fst_sex_14_less[i]<-1 
    }else if(!is.na(data$f2_s8_804[i])&&data$f2_s8_804[i]>=15 && data$f2_s8_804[i]<=24){
        fst_sex_14_less[i]<-0
    }else if(!is.na(data$f2_s8_804[i])&&(data$f2_s8_804[i]==88)){
        fst_sex_14_less[i]<-0
    }else if(!is.na(data$f2_s8_804[i])&&data$f2_s8_804[i]==99){
        fst_sex_14_less[i]<-2
    }else{
        fst_sex_14_less[i]<-NA
    }
}

    
data<-cbind(data, fst_sex_14_less)

#Can a woman get pregnant 1st time.
#1 if they think yes
#0 if they think no

can_preg_fst <- rep(NA, 48700)

for(i in 1:48700){
    if(data$f2_s8_845[i]=='si'){
        can_preg_fst[i]<-1
    }else if(data$f2_s8_845[i]=='no'||data$f2_s8_845[i]=='no sabe/ no responde'){
        can_preg_fst[i]<-0
    }else{
        can_preg_fst[i]<-NA
    }
}

data<-cbind(data, can_preg_fst)

#NEW VARIABLE
#MARRIED/UNION OR NOT
#1-married/union
#0-divorced/single/separated

marital_status <- rep(NA, 48700)

for(i in 1:48700){
    if(data$f2_s9_900[i]=='casado?'||data$f2_s9_900[i]=='unión de hecho?'||
       data$f2_s9_900[i]=='unión libre?'){
        marital_status[i]<-1
    }else if(data$f2_s9_900[i]=='divorciado?'||data$f2_s9_900[i]=='separado?'||
             data$f2_s9_900[i]=='viudo?'||data$f2_s9_900[i]=='soltero?'){
        marital_status[i]<-0
    }else{
        marital_status[i]<-NA
    }
}


        
data<-cbind(data, marital_status)

#NEW VARIABLE
#Level of education
#Basic, mediam, higher

education_lvl <- rep(NA, 48700)

for(i in 1:48700){
    if(data$nivins[i]=='Educación Básica' || data$nivins[i]=='Ninguno o Centro de Alfabetización'){
        education_lvl[i] <- 'Basic Ed.'
    }else if(data$nivins[i]=='Educación Media/Bachillerato'){
        education_lvl[i] <- 'Mid-level Ed.'
    }else if(data$nivins[i]=='Superior'){
        education_lvl[i] <- 'Higher Ed.'
    }else{
        education_lvl[i] <- NA
    }
}
    
data<-cbind(data, education_lvl)

#New (smaller) Dataframe
#This dataframe has only women ages 10-24 who have had sex

newdata<- data[ which(data$f2_s8_803=='si'), ]
dim(newdata)

#Delete missing data which I don't have info about pregnancy
#This is a data cleaning issue
#Also delete the cases where they did not answer about their age on their first sexual experience
#This is because the whole section is missing
#i.e. data$fst_sex_14_less[i]==2

newdata <- newdata[!is.na(newdata$teen_preg), ]

dim(newdata)

newdata <- newdata[!is.na(newdata$use_contra_fst), ]

dim(newdata)

#Convert to factors
newdata$can_preg_fst <- factor(newdata$can_preg_fst)
newdata$ever_preg <- factor(newdata$ever_preg)
newdata$use_contra_fst <- factor(newdata$use_contra_fst)
newdata$use_contra_mthd <- factor(newdata$use_contra_mthd)
newdata$know_contra_mthd <- factor(newdata$know_contra_mthd)
newdata$stud_fst_preg <- factor(newdata$stud_fst_preg)
newdata$teen_preg <- factor(newdata$teen_preg)
newdata$sx_preg_bef_21 <- factor(newdata$sx_preg_bef_21)
newdata$fst_sex_14_less <- factor(newdata$fst_sex_14_less)
newdata$marital_status <- factor(newdata$marital_status)
newdata$preg_bef_21 <- factor(newdata$preg_bef_21)

mymodel <- glm(preg_bef_21 ~ use_contra_fst + area + fst_sex_14_less + marital_status 
                 + know_contra_mthd + education_lvl + can_preg_fst
               , data=newdata, family = "binomial")
summary(mymodel)

#It is very important to note what is significant and what isn't
#Two of my hypotheses have been disproved
#Rural area and knowledge of modern contraceptive methods are not significant predictors

#run model again without those two variables

mymodel <- glm(preg_bef_21 ~ use_contra_fst + fst_sex_14_less + marital_status 
                  + education_lvl 
               , data=newdata, family = "binomial")
summary(mymodel)

#Make a new dataframe that will include the Predicted Probabilities 
newdata2 <- cbind(newdata, predict(mymodel, newdata, type="link", se=TRUE))

newdata2<- within(newdata2, {PredictedProb <- plogis(fit)})

#Plot the Predicted Probability vs Pregnancy before 20 

plot(newdata2$preg_bef_21, newdata2$PredictedProb,
     ylab="Predicted Probability", xlab="Pregnagncy 21 or Younger",
    main="Predicted Probability vs  Pregnancy at 21 Years of Age or Younger")

#I will crossvalidate my model (Explain)
#Run the model 10 times in training data and check it in the test data
library('caret')

crossValSettings <- trainControl(method="repeatedcv", number=20,
                                savePredictions=TRUE)

crossVal <- train(as.factor(preg_bef_21) ~ use_contra_fst + fst_sex_14_less + marital_status 
                  + education_lvl, data=newdata, family="binomial", method="glm",
                 trControl=crossValSettings, na.action=na.exclude)

crossVal

#Another way to check accuracy of the model
#Confusion matrix
#False positives and false negatives

#takes our crossvalidated model and apply it to the original data
pred <- predict(crossVal, newdata = newdata)
confusionMatrix(data=pred, newdata$preg_bef_21)

#The matrix shows my model has more false positives than false negatives
#I have better specificity than sensitivity 
#Sensitivity is the ability to accurately predict a pregnancy when they were actually pregnant
#Specificity is the ability to only categorize those who were pregnant among those who were pregnant
#6651 correctly predicted cases (out of 8574)
#Model is 77.57% accurate 

#I have identified predictors of teen pregnancy
#I need to expand this to include other variables like socioeconomic status and charasteristics of the household
#I will do so for my thesis

#GRAPHS

#sex_preg
mytable <-table(data$sex_preg, data$age_groups)
tble <-prop.table(mytable, 2)*100
tble_for_plot<-as.matrix(tble[,-4])
tble_for_plot

b<-barplot(tble_for_plot, horiz=TRUE, col=c("lightgreen", "darkred", "yellow"),
       beside=TRUE, xlim=c(0,110), xlab="Percentage (%)", ylab="Age Groups (Years)",
           main="Sexual Activity & Pregnancy Ages 10-24")

legend("topright", c("no sex","pregnancy", "sex/no preg."), pch=15, 
       col=c("lightgreen", "darkred", "yellow"), bty="n")

a<-c(99.1, 0.4, 0.6,
     70.5, 17.8, 11.8,
     17.9, 65.5, 16.6)

text(y=b, x=a, pos=4,labels=a)

#age_fst_preg
mytable <-table(data$age_fst_preg, data$age_groups)
tble <-prop.table(mytable, 2)*100
tble_for_plot<-as.matrix(tble[,-4])
tble_for_plot

b<-barplot(tble_for_plot, horiz=TRUE, col=c("lightgreen", "orange", "darkred", "yellow"),
       beside=TRUE, xlim=c(0,110), xlab="Percentage (%)", ylab="Age Groups (Years)",
           main="Sexual Activity & FIRST Pregnancy Ages 10-24")

legend("topright", c("no sex","pregnancy after 18", "pregnancy before 18", "sex/no preg."), pch=15, 
       col=c("lightgreen", "orange", "darkred", "yellow"), bty="n")

a<-c(99.1, NA, 0.4, 0.6,
     70.5, NA, 17.8, 11.7,
     17.9, 24.2, 41.2, 16.6)

text(y=b, x=a, pos=4,labels=a)

#age_fst_preg
mytable <-table(data$age_fst_preg, data$region)
tble <-prop.table(mytable, 2)*100
colnames(tble) = c("Amazon", "Coast", "Islands", "Highlands")
tble

b<-barplot(tble, horiz=TRUE, col=c("lightgreen", "orange", "darkred", "yellow"),
       beside=TRUE, xlim=c(0,110), xlab="Percentage (%)", ylab="Regions",
           main="Sexual Activity & FIRST Pregnancy by Regions (women ages 10-24)")

legend("topright", c("no sex","pregnancy after 18", "pregnancy before 18", "sex/no preg."), pch=15, 
       col=c("lightgreen", "orange", "darkred", "yellow"), bty="n")

a<-c(61.3,7.6, 21, 10.2,
     57.9, 9.6, 23.1, 9.3,
     69.6, 5.8, 10.9, 13.7,
    61.5, 10.1, 18.8, 9.6)

text(y=b, x=a, pos=4,labels=a)

#age_fst_preg
mytable <-table(data$age_fst_preg, data$etnia)
tble <-prop.table(mytable, 2)*100
colnames(tble) = c("Afro-ecuadorian", "White", "Indigenous", "Mestizo", "Montubio")
tble

b<-barplot(tble, horiz=TRUE, col=c("lightgreen", "orange", "darkred", "yellow"),
       beside=TRUE, xlim=c(0,110), xlab="Percentage (%)", ylab="Ethnicity",
           main="Sexual Activity & FIRST Pregnancy by Ethnicity (women ages 10-24)")

legend("topright", c("no sex","pregnancy after 18", "pregnancy before 18", "sex/no preg."), pch=15, 
       col=c("lightgreen", "orange", "darkred", "yellow"), bty="n")

a<-c(55.4, 9.6, 24.3, 10.7,
     56.5, 11.2, 20.1, 12.3,
     65.1, 7.4, 20, 7.5,
    60.1, 9.4, 20.2, 10.2,
    52.2, 11.3, 29.3, 7.2)

text(y=b, x=a, pos=4,labels=a)

#use_contra_fst
mytable <-table(data$use_contra_fst, data$age_groups)
tble <-prop.table(mytable, 2)*100
tble_for_plot<-as.matrix(tble[,-4])
tble_for_plot

b<-barplot(tble_for_plot, horiz=TRUE, col=c("darkred", "lightgreen"),
       beside=TRUE, xlim=c(0,110), xlab="Percentage (%)", ylab="Age Groups (Years)",
           main="Use of a Modern Contraceptive Method for First Sexual Experience")

legend("topright", c("No","Yes"), pch=15, 
       col=c("darkred", "lightgreen"), bty="n")


a<-c(70.97, 29.03, 
     61.78, 38.22, 
     62.49, 37.51)

text(y=b, x=a, pos=4,labels=a)

#know_contra_mthd
mytable <-table(data$know_contra_mthd, data$age_groups)
tble <-prop.table(mytable, 2)*100
tble_for_plot<-as.matrix(tble[,-4])
tble_for_plot

b<-barplot(tble_for_plot, horiz=TRUE, col=c("darkred", "lightgreen"),
       beside=TRUE, xlim=c(0,110), xlab="Percentage (%)", ylab="Age Groups (Years)",
           main="Knowledge of Modern Contraceptive Methods")

legend("bottomright", c("No","Yes"), pch=15, 
       col=c("darkred", "lightgreen"), bty="n")


a<-c(37.2, 62.8, 
     4.8, 95.2, 
     1.6, 98.4)

text(y=b, x=a, pos=4,labels=a)

#preg_bef_21
mytable <-table(data$preg_bef_21, data$age_groups)
tble <-prop.table(mytable, 2)*100
tble_for_plot<-as.matrix(tble[,-4])
tble_for_plot

b<-barplot(tble_for_plot, horiz=TRUE, col=c("darkred", "lightgreen"),
       beside=TRUE, xlim=c(0,110), xlab="Percentage (%)", ylab="Age Groups (Years)",
           main="First Pregnancy Before 21")

legend("topright", c("No","Yes"), pch=15, 
       col=c("darkred", "lightgreen"), bty="n")


a<-c(99.6, 0.4, 
     82.2, 17.8, 
     38.1, 61.9)

text(y=b, x=a, pos=4,labels=a)

#fst_sex_14_less
mytable <-table(data$fst_sex_14_less, data$age_groups)
tble <-prop.table(mytable, 2)*100
tble_for_plot<-as.matrix(tble[,-4])
tble_for_plot

b<-barplot(tble_for_plot, horiz=TRUE, col=c("darkred", "lightgreen", "yellow"),
       beside=TRUE, xlim=c(0,110), xlab="Percentage (%)", ylab="Age Groups (Years)",
           main="First Sexual Experience Before 14")

legend("topright", c("No","Yes", "Chose not to answer"), pch=15, 
       col=c("darkred", "lightgreen", "yellow"), bty="n")


a<-c(1.4, 97.3, 1.4, 
     69.7, 28.9, 1.4,
     85.4, 12.4, 2.2)

text(y=b, x=a, pos=4,labels=a)

#can_preg_fst
mytable <-table(data$can_preg_fst, data$age_groups)
tble <-prop.table(mytable, 2)*100
tble_for_plot<-as.matrix(tble[,-4])
tble_for_plot

b<-barplot(tble_for_plot, horiz=TRUE, col=c("darkred", "lightgreen"),
       beside=TRUE, xlim=c(0,110), xlab="Percentage (%)", ylab="Age Groups (Years)",
           main="Belief it is Possible to Get Pregnant on the First Time")

legend("topright", c("No","Yes"), pch=15, 
       col=c("darkred", "lightgreen"), bty="n")


a<-c(53, 47, 
     27.8, 72.2, 
     21.3, 78.7)

text(y=b, x=a, pos=4,labels=a)


