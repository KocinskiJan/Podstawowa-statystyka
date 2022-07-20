#"C:\Program Files\R\R-4.1.1\bin\R.exe" CMD BATCH --vanilla "--args Dane.csv" rpis_projekt.R
#install.packages("ggplot2", repos="http://cran.rstudio.com/", dependencies=TRUE)
library(ggplot2)
#install.packages("Hmisc", repos="http://cran.rstudio.com/", dependencies=TRUE)
library(Hmisc)
#install.packages("dplyr", repos="http://cran.rstudio.com/", dependencies=TRUE)
library(dplyr)
#install.packages("car", repos="http://cran.rstudio.com/", dependencies=TRUE)
library(car)
#install.packages("ggpubr", repos="http://cran.rstudio.com/", dependencies=TRUE)
library(ggpubr)
#install.packages("dunn.test", repos="http://cran.rstudio.com/", dependencies=TRUE)
library(dunn.test)
#install.packages("FSA", repos="http://cran.rstudio.com/", dependencies=TRUE)
library(FSA)
#install.packages("DescTools", repos="http://cran.rstudio.com/", dependencies=TRUE)
library(DescTools)
#install.packages("mice", repos="http://cran.rstudio.com/", dependencies=TRUE)
library(mice)
#install.packages("stargazer", repos="http://cran.rstudio.com/", dependencies=TRUE)
library(stargazer)

#args = commandArgs(trailingOnly=TRUE)
#if (length(args)==0)
#{
 # stop("Nalezy podac conajmniej jeden argument wejsciowy")
#}

#1.loading data and feedback about missing data___________________________________________________________
#loaded<- read.csv2(file=args[1], header=TRUE, sep = ";" )
loaded<- read.csv2("Dane.csv" , TRUE, sep = ";")
#loaded<- read.csv2("Dane_2g.csv" , header=TRUE, sep = ";")
nr_miss <- sum(is.na(loaded))
cat('\n In file', nr_miss, 'records with no data.\n')


# filling up missing data and feedback about it____________________________________________________________
FillingupData<-function(){
nowy<<-colnames(loaded)
i=1
j=1

while (i<=length(loaded)) {

  if(is.numeric(loaded[1] )){
    wektor<-c(which( is.na(loaded)))
 
  while(j<=length(wektor)){
    cat("Dane uzupe³niono w kolumnie:", nowy[i],"wiersz:" ,wektor[j],'\n\n')
    print(loaded[j,])
    cat("\n\n")
    loaded[wektor[j],i]<<- mean(loaded[which(loaded[,1]==loaded[wektor[j],1]), i], na.rm=TRUE)
    j=j+1
  }
  j=1 
  }
  i=i+1
}
}
FillingupData()
group=split(loaded, loaded[[1]])

  for(i in length(group)){
   newData <-complete(mice(loaded, m=2, maxit = 2,
                          method = 'mean', seed = 999)) 
  }
    
    
  
  for(r in 1:nrow(group[[1]])){
    for(c in 1:ncol(group[[1]])){
      if(is.na(group[[1]][r,c])){
        cat(group[[1]][[1]][1],"Missing data in column:", colnames(group[[1]])[c], "in raw",r ,"added value", newData[r,c],"\n")
      }}
  }
  
 

finalData <- newData

write.csv2(finalData, 'filled_data.csv')

png(filename='NA-chart.png',
    width = 750, height = 450)
NA_pattern <- md.pattern(loaded)
dev.off()

#detect outliers__________________________________________________________________________________________

outliers <- lapply(finalData %>% select_if(is.numeric),
                   function(x){boxplot(x, plot = FALSE)$out})
for(outliername in names(outliers))
{
  #if outliers exists- print them, else print no outliers.
  if(length(outliers[[outliername]])){
    cat("In column", outliername,"there is", length(outliers[[outliername]]), "outliers:", 
        paste(outliers[[outliername]], sep = ","), "\n")
  }else{
    cat("In column", outliername,"there is no outliers. \n")}
}


#2.Characteristic for given groups______________________________________________________________________

summariseAllData <- function() {
  raturn <- list()
  groups <- unique(finalData[[1]])
  columns <- as.name(colnames(finalData)[1])
  for(gr in groups) {
    filtered <- finalData %>% group_by(!!columns) %>% subset(.[[1]] == gr)
    raturn[[gr]] <- list()
    for(attr in colnames(filtered %>% as.data.frame %>% select_if(is.numeric))) {
      numerics <- filtered %>% select_if(is.numeric)
      nm <- as.name(attr)
      raturn[[gr]][[attr]] <-
        numerics %>% summarise(
          median = median(!!nm),
          mean = mean(!!nm),
          min = min(!!nm),
          max = max(!!nm),
          sd = sd(!!nm)
        )
    }
  }
  write.csv2(raturn,file="charakterystyka_grup.csv")
  return(raturn)
}
summariseAllData()
nameee<-colnames(finalData) 
potrzebne <- group_by(finalData,finalData[, 1])
potrzebne <<- data.frame(do.call(rbind, potrzebne)) # pionowa tabela
#3.Test___________________________________________________________________________________________________
Test<-function(){
  m=1
  nonparametric<-list()
  parametric<- list()
  Group<-c()
  Name<-c()
  compatibility<-c()
  
  
  while(m<=length(finalData)){
    if(is.numeric(finalData[,m])){
      normality=0
      
      ShapiroTest <- group_by(finalData, finalData[,1]) %>%
        summarise(
          statistic = shapiro.test(.data[[nameee[m]]])$statistic,
          p.value = shapiro.test(.data[[nameee[m]]])$p.value
        )
      
      cat('\n\n',nameee[m],'\n')
      counter=1
      for(i in 1:length(ShapiroTest$p.value)){
        
        if(ShapiroTest$p.value[i] < 0.05){
          cat("\n", ShapiroTest$p.value[i], "< 0.05 - you can't assume compatibility with normal distribution\n")
          Group<-append(Group,as.character(ShapiroTest[counter,1]))
          Name<-append(Name,m)
          compatibility<-append(compatibility,"NIE")
        }else{
          cat("\n", ShapiroTest$p.value[i], "> 0.05 - you can assume compatibility with normal distribution\n")
          normality=normality+1
          Group<-append(Group,as.character(ShapiroTest[counter,1]))
          Name<-append(Name,m)
          compatibility<-append(compatibility,"TAK")
        }
        counter=counter+1
      }
      gg<- ggdensity(finalData, x = nameee[m],
                     color = "grupa", fill = "grupa",
                     palette = c("#B19DB9", "#9DB4B9", "#94A987"),
                     ylab = "gêstoœc",
                     xlab = nameee[m]
      ) + facet_wrap(~ finalData[,1], scales = "free")
      ggsave(paste("Gestosc_",nameee[m],".png"),gg)
      
      Levene_wynik=0
      if(normality==length(potrzebne)){ # if given parametr matches normal distribution
        leveneTestResult <- leveneTest(finalData[,m]~ factor(finalData[,1]),  finalData)
        cat( "\nLevene: ",leveneTestResult$"Pr(>F)"[1])
        if(leveneTestResult$"Pr(>F)"[1]>0.05){
          Levene_wynik=1
        }
      }
      
      if((normality!= length(potrzebne)||Levene_wynik==0)&&length(potrzebne)>2){ #KRUSKAL
        nonparametric<-append(nonparametric,m)
        Dunn<-list()
        cat("\nKRUSKAL\n")
        pvalueKWtest <- kruskal.test(finalData[,m]~ finalData[,1], data = finalData)$p.value
        
if(pvalueKWtest<=0.0001)
  print("****")
else if(pvalueKWtest<=0.001)
  print("***")
else if(pvalueKWtest<=0.01)
  print("**")
else if(pvalueKWtest<=0.05)
  print("*")
else if(pvalueKWtest>0.05)
  print("NS")        
        
        if(round(pvalueKWtest,3)< 0.05){
          
          cat(pvalueKWtest, "< 0.05 - there are differences between groups\n")
          Dunn<-dunnTest(finalData[,m]~ factor(finalData[,1]))
          cat("\nDunn Test: \n ")
          print(Dunn)
          df_Dunn<- data.frame(Dunn[["res"]])# creats data frame
          
          df=1
          while(df<=length(df_Dunn[,4])){
            if(df_Dunn[df,4]<0.05){
              cat("differences existi between groups: ",df_Dunn[df,1]," for parametr: ",nameee[m]," : ",df_Dunn[df,4],"\n")
            }
            df=df+1
          }
          
          jpeg( paste("Dunn_",nameee[m],".jpg"), width = 800 , height = 500)
          barplot(df_Dunn[,4]~ df_Dunn[,1],
                  data = finalData,
                  main=paste("Differences in groups for ",nameee[m]),
                  xlab="Grupy",
                  ylab="p.value",
                  col=c("#9898E9","#C3E998","#98CEE9")
          )
          abline( h=0.05, col=" red " )
          dev.off()
          
        }else{
          cat(pvalueKWtest, "> 0.05 - there are no differences between groups")
        }
      }
      
      if(Levene_wynik==1&&normality==length(potrzebne)&&length(potrzebne)>2){
        parametric<-append(parametric,m)
        cat("\nANOVA: \n")
        pvalueAOVtestMCHC <- summary(aov(finalData[,m]~finalData[,1], data = finalData))[[1]][["Pr(>F)"]][[1]]

        if(round(pvalueAOVtestMCHC) < 0.05){
          cat(pvalueAOVtestMCHC, "< 0.05 - there are differences between groups")
          group<-finalData[,1]
          Tukey<-TukeyHSD(aov(finalData[,m] ~ group, data = finalData))
          cat("\nTukey: \n")
          print(Tukey)
          
          jpeg( paste("Tukey_",nameee[m],".jpg"), width = 800 , height = 500)
          plot(Tukey,col="red")
          dev.off()
          df_Tukey<-as.data.frame(Tukey[1:1])
          
          df_t=1
          n_wiersz<-rownames(df_Tukey)
          
          while(df_t<=length(df_Tukey[,4])){
            if(df_Tukey[df_t,4]<0.05){
              cat("Differences existi between groups: ",n_wiersz[df_t]," for parametr: ",nameee[m]," value: ",df_Tukey[df_t,4],'\n')
            }
            df_t=df_t+1
          }
          
        }else{
          cat(pvalueAOVtestMCHC, "> 0.05 - there are no differences between groups")
        }
      }
      
      
      
#Test for two groups_______________________________________________________
      
      if(normality!=2&&length(potrzebne)==2){
        nonparametric<-append(nonparametric,m)
        Wilcox <- wilcox.test(finalData[,m] ~ finalData[,1], data = finalData)$p.value
        cat("WILCOX: ",Wilcox,'\n')
        if(Wilcox < 0.05){
          cat("There are differences between groups\n")
        }else{
          cat("There are no differences between groups\n")
        }
      }
      
      if(Levene_wynik==0&&normality==length(potrzebne)&&length(potrzebne)==2){
        testWelcha<-t.test(finalData[,m] ~ finalData[,1], data = finalData, var.equal = FALSE)$p.value
        cat("TEST WELCHA")
        print(testWelcha)
        if(testWelcha< 0.05){
          cat("There are differences between groups\n")
        }else{
          cat("There are no differences between groups\n")
        }
      }
      
      if(Levene_wynik==1&&normality==length(potrzebne)&&length(potrzebne)==2){
        parametric<-append(parametric,m)
        Ttest <- t.test(finalData[,m] ~ finalData[,1], data = finalData, var.equal = TRUE)$p.value
        cat("\nT-Student: ",Ttest)
        if(Ttest < 0.05){
          cat("There are differences between groups\n")
        }else{
          cat("There are no differences between groups\n")
        }
      }
    }else{
      if(m!=1){
        pvalueChisq <- chisq.test(finalData[,1], finalData[,m])$p.value
        jpeg(paste("chisq_",nameee[m],".jpg"))
        barplot(table(finalData[,m], finalData[,1]),
                main=paste("p.value= ",pvalueChisq),
                ylim = c(0,20),
                beside = TRUE,
                col = c("#CCE5FF", "#CCCCFF"),
                xlab = "grupa",
                ylab = nameee[m],
                legend =unique(finalData[,m]))
        dev.off()
        
        
      }
    }
    m=m+1
    
  }
  
  distribution<<-data.frame(Group,Name,compatibility)
}





#4corealtions_________________________________________________________________________________________________

corelation<-function(){
  Gr<-c()
  Parametr<-c()
  Test<-c()
  PValue<-c()
  wspolczynnik<-c()
  direction_strength<-c()
  
  distribution<-arrange(distribution,Group)
  
  for(g in 1:length(potrzebne)){
    gr <- distribution%>% filter(distribution[,1] == potrzebne[1,g])
    gr_calculation <- finalData %>% filter(finalData[,1] == potrzebne[1,g])
    par1<-1
    while(par1 <length(finalData)){
      par2<-par1+1
      while(par2<=length(gr[,3])){
        p_or_s=0
        
        if(gr[par1,3]=="YES"&&gr[par2,3]=="YES"){
          p_or_s=p_or_s+1
        }  
        
        h1<-gr[par1,2]
        h2<-gr[par2,2]
        cor=0
        
        if(p_or_s==1){
          CorelationPS <- cor.test(gr_calculation[,h1], gr_calculation[,h2], method = "pearson")
          
          if(round (CorelationPS$p.value,3) <0.05){
            Test<-append(Test,"PEARSON")
            PValue<-append(PValue,CorelationPS$p.value)
            cor=1
            cat("Corelation: ",nameee[h1]," - ",nameee[h2]," for group: ",potrzebne[1,g],"\n")
            
            gg<<-ggscatter(gr_calculation, x = nameee[h1], y = nameee[h2], 
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = "pearson",
                           color = "grupa", fill = "grupa",
                           palette = c("#99cc00"),
                           ylab = nameee[h2], 
                           xlab = nameee[h1]
            )
            ggsave(paste("Corelation_",nameee[h1],"_",nameee[h2],"_PEARSON",".png"),gg)
          }
          
 ''       }else{
          
          CorelationPS <- cor.test(gr_calculation[,h1], gr_calculation[,h2], method = "spearman")
          
          if(round(CorelationPS$p.value,3) <0.05){
            cat("There is corelation\n ")
            Test<-append(Test,"SPEARMAN")
            PValue<-append(PValue,CorelationPS$p.value)
            cor=1
            cat("Corelation: ",nameee[h1]," - ",nameee[h2]," for group: ",potrzebne[1,g],"\n")
            gg<<- ggscatter(gr_calculation, x = nameee[h1], y = nameee[h2], 
                            conf.int = TRUE, 
                            cor.coef = TRUE, cor.method = "spearman",
                            color = "grupa", fill = "grupa",
                            palette = c("#9999FF"),
                            ylab = nameee[h1], 
                            xlab = nameee[h2]
            )
            ggsave(paste("Corelation_",nameee[h1],"_",nameee[h2],"_SPEARMAN",".png"),gg)
            
          }
        }
        
        if(cor==1){
          Gr<-append(Gr,gr[1,1])
          Parametr<-append(Parametr,paste(nameee[gr[par1,2]]," ",nameee[gr[par2,2]]))
          
          if(-1<CorelationPS$estimate&& CorelationPS$estimate<(-0.7)){
            wspolczynnik<-append(wspolczynnik,round(CorelationPS$estimate,4))
            direction_strength<-append(direction_strength,"Very strong negative corelation")
            cat("Very strong negative corelation\n")
          }
          if(-0.7<CorelationPS$estimate&& CorelationPS$estimate<(-0.5)){
            wspolczynnik<-append(wspolczynnik,round(CorelationPS$estimate,4))
            direction_strength<-append(direction_strength,"Strong negative corelation")
            cat("Strong negative corelation\n")
          }
          if(-0.5<CorelationPS$estimate&& CorelationPS$estimate<(-0.3)){
            wspolczynnik<-append(wspolczynnik,round(CorelationPS$estimate,4)) 
            direction_strength<-append(direction_strength,"Middle negative corelation")
            cat("Corelation ujemna o srednim natezeniu\n")
          }
          if(-0.3<CorelationPS$estimate&& CorelationPS$estimate<(-0.2)){
            wspolczynnik<-append(wspolczynnik,round(CorelationPS$estimate,4))
            direction_strength<-append(direction_strength,"Weak negative corelation")
            cat("Weak negative corelation\n")
          }
          if(-0.2<CorelationPS$estimate&& CorelationPS$estimate<0.2){
            wspolczynnik<-append(wspolczynnik,round(CorelationPS$estimate,4))
            direction_strength<-append(direction_strength,"No corelation")
            cat("No corelation\n")
          }
          if(0.2<CorelationPS$estimate&& CorelationPS$estimate<0.3){
            wspolczynnik<-append(wspolczynnik,round(CorelationPS$estimate,4))
            direction_strength<-append(direction_strength,"Weak positive corelation")
            cat("Weak positive corelation\n")
          }
          
          if(0.3<CorelationPS$estimate&& CorelationPS$estimate<0.5){
            wspolczynnik<-append(wspolczynnik,round(CorelationPS$estimate,4))
            direction_strength<-append(direction_strength,"Medium positive corelation")
            cat("Medium positive corelation\n")
          }
          if(0.5<CorelationPS$estimate&& CorelationPS$estimate<0.7){
            wspolczynnik<-append(wspolczynnik,round(CorelationPS$estimate,4))
            direction_strength<-append(direction_strength,"Strong positive corelation")
            cat("Strong positive corelation\n")
          }
          if(0.7<CorelationPS$estimate&& CorelationPS$estimate<1){
            wspolczynnik<-append(wspolczynnik,round(CorelationPS$estimate,4))
            direction_strength<-append(direction_strength,"Very strong positive corelation")
            cat("Very strong positive corelation\n")
          }
        }
        
        par2=par2+1
      }  
      par1=par1+1
    }
    
  }
  Corelation<<-data.frame(Gr,Parametr,Test,PValue,wspolczynnik,direction_strength)
  write.csv2(Corelation,file="Corelation.csv")
}


Test()
corelation()



