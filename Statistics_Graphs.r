#GRAPHS AND STATISTICS

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


