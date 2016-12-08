setwd('C:/Users/joos_/Desktop/LaptopOleStuff/Advanced stat + ML/20161129/project')
getwd()

rm(list=ls())

budgetdata = read.table("Budget.txt", sep = ",")
budgetdata
budgetdata[,1]
budgetdata2=budgetdata[2:12]
rownames(budgetdata2) = budgetdata[,1]
budgetdata2
names(budgetdata2) = c("authorities", "agriculture", "tradesandcompanies", "work", "accomodations", "education", "socialaction", "veterans", "defense", "debtrefund", "various")

names(budgetdata) = c("year", "authorities", "agriculture", "tradesandcompanies", "work", "accomodations", "education", "socialaction", "veterans", "defense", "debtrefund", "various")
#WRONG# names(budgetdata) = {year, authorities, agriculture, tradesandcompanies, work, accomodations, education, socialaction, veterans, defense, debtrefund}
#WRONG# names(budgetdata) = {"year", "authorities", "agriculture", "tradesandcompanies", "work", "accomodations", "education", "socialaction", "veterans", "defense", "debtrefund"}

budgetdata2
budgetmatrix=as.matrix(budgetdata2)
budgetmatrix

#####DESCRIPTIVES####
means=apply(budgetmatrix, 2, mean)
stdevs=apply(budgetmatrix, 2, sd)
mins=apply(budgetmatrix, 2, min)
maxs=apply(budgetmatrix, 2, max)

means
stdevs
mins
maxs

varnames=colnames(budgetmatrix)
varnames

###TABLE OF MEAN, STDEV, MIN, MAX
explore=cbind(means, stdevs, mins, maxs) #cbind binds columns, rbind binds rows
explore
X11()

#CONVERT MATRIX TO DATA FRAME FOR THE HIST FUNCTION
budgetframe=as.data.frame(budgetmatrix)
budgetframe
#names(budgetframe)=c("authorities", "agriculture", "tradesandcompanies", "work", "accomodations", "education", "socialaction", "veterans", "defense", "debtrefund", "various")


#FUNCTION FOR CREATING MANYH HISTOGRAMS
#myfunction<-function(amatrix){
#  
#  for (i in (1 : length(names(amatrix)))){
#    X11()
#    hist(amatrix[,i], main=(names(amatrix))[i])
#    }
#}

#my_f <-function(mydataset){
#  for (i in 1:length(mydataset)){
#    X11()
#    hist(mydataset[,i], main=paste("Plot",i))
#  }
#}

#myfunction(budgetframe)
#length()
#n=names(budgetframe)

###This one works:
#my_f2 <-function(mydataset){
#  for (i in 1:length(mydataset)){
#    X11()
#    hist(mydataset[,i], main=(names(mydataset))[i])
#  }
#}

my_f2(budgetframe)

graphics.off()
#TRYING IN ONE WINDOW: WORKS!
my_f3 <-function(mydataset){
  par(mar = rep(2, 4)) #set the margin to 2, so it fits in the window
  par(mfrow=c(4,3)) #make 4 rows and 3 colums in the plotting window
  for (i in 1:length(mydataset)){
    hist(mydataset[,i], main=(names(mydataset))[i],freq=FALSE)
  }
}
my_f3(budgetframe)

#apply(budgetmatrix, 2, myfunction(budgetmatrix, varnames))
#budgetframe
#myfunction(budgetmatrix)
#myfunction(budgetframe)

#?hist

#?plot


#?qqnorm

#FUNCTION TO GET QQPLOTS: WORKS!
my_qqplots <-function(mydataset){
  par(mar = rep(2, 4)) #set the margin to 2, so it fits in the window
  par(mfrow=c(4,3)) #make 4 rows and 3 colums in the plotting window
  for (i in 1:length(mydataset)){
    qqnorm(mydataset[,i], main=(names(mydataset))[i])
    qqline(mydataset[,i])
  }
}
my_qqplots(budgetframe)


#BOXPLOTS
?boxplot
budgetmatrix=as.matrix(budgetframe)
budgetframe
budgetmatrix
par(mfrow=c(1,1))
boxplot(budgetmatrix)

#Looking at evolution through time
year=budgetdata[,1]
length(y)
plot(budgetdata[-1]~budgetdata[1], type='l')
budgetdata[-1]
xrange=seq(1850,2000,50)
yrange=seq(0,100,10)

?plot



?par
###################LIENGRAPH TO SHOW CHANGES OVER TIME#############


graphics.off()

par(mar=c(4, 4, 4, 10),xpd=TRUE)

my_lines <-function(xvariable,yvariables){
  par(mfrow=c(1,1)) #make 4 rows and 3 colums in the plotting window
  par(new=TRUE)
  plot(c(min(xvariable),max(xvariable)), c(0, 25), type='n', xlab='year', ylab='Percent', main='Line graph to show changes over time')
  for (i in 1:length(colnames(yvariables))){
    lines(yvariables[,i]~xvariable, main='Lines graph budget', type='l', col=palette()[i])
  }
}

my_lines(year,budgetmatrix)

legend(x=1980,y=25,legend=(colnames(budgetmatrix)), cex=0.8, lty=1, col=(c(palette(), palette()[1:4])), title="My Title")


###################


colnames(budgetmatrix)
budgetmatrix
palette()
?lines
palette()[1]
warnings()
#NOT WORKING YET!!
##legend(1900, 14, legend=names(budgetmatrix),col=c('red'), lty=1:12, cex=0.8)



#my_legend(budgetmatrix)
#?legend
#legend(x=1900,y=12,legend=(colnames(budgetmatrix)), cex=0.8, col=colors, title="My Title")
#(c(palette(), palette()[1:4]))




#############BOXPLOT PER YEAR#########################
budgetmatrix=as.matrix(budgetframe)
budgetframe
budgetmatrix
par(mfrow=c(1,1))
boxplot(budgetmatrix)

yearbudgetmatrix=t(budgetmatrix)
boxplot(yearbudgetmatrix)
#############DOESNT SEEM VERY USEFUL#######################





#############CORRELATION####################
cor(budgetmatrix)
#cor(yearbudgetmatrix)


### HEATMAP #####################
install.packages('ggplot2') #joos
library(ggplot2) #joos
install.packages('reshape2')
library(reshape2)


?ggplot
?ggplot2 #joos
?cormat #joos
?round #joos
?melt #joos
?reshape2 #joos

budgetmatrix
cormat<-round(cor(budgetmatrix),2)
#cormat<-get_upper_tri(cormat)
cormat #joos
cormat<-melt(cormat) #makes combination of correlation (different format, not really like a matrix)
cormat

ggplot(data=data.frame(cormat),aes(fill=value,x=Var1, y=Var2))+geom_tile(color="white")+
  scale_fill_gradient2(low = "blue", high = "blue", mid = "orange", midpoint = 0, limit = c(-1,1), space = "Lab",name="Pearson\nCorrelation")

?aes

?geom_tile
?scale_fill_gradient2
str(cormat)


################heatmap for years#################
coryears=cor(yearbudgetmatrix)
cormatrixyear=melt(coryears)
cormatrixyear

ggplot(data=data.frame(cormatrixyear),aes(fill=value,x=Var1, y=Var2))+geom_tile(color="white")+
  scale_fill_gradient2(low = "black", high = "black", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab",name="Pearson\nCorrelation")

###################maybe good for later#################



###########PRINCOMP###########
budgetframe
PCA=princomp(budgetframe, cor=TRUE)
names(PCA)

PCA
summary(PCA)
#Kaiser criterion: 3 components
#>80%: 4 components
#>90%: 5 components

plot(PCA, type='l') # 2 or 3 components

PCA$sdev
PCA$loadings
PCA$center
PCA$scale
PCA$n.obs
PCA$scores
PCA$call

plot(PCA$scores[,1], PCA$scores[,2], xlab='Component 1', ylab='Component 2', type='n') #empty plot
text(PCA$scores[,1], PCA$scores[,2], labels=(rownames(PCA$scores)))
abline(h=0, v=0)

#component 1: (eg 1872 and 1968)
# education
# social action

#component 2: (eg 1923 and 1947)
# authorities
# work
# trades and companies



#comp 3?: no idea
plot(PCA$scores[,1], PCA$scores[,3], xlab='Component 1', ylab='Component 3', type='n') #empty plot
text(PCA$scores[,1], PCA$scores[,3], labels=(rownames(PCA$scores)))
abline(h=0, v=0)

PCA$loadings
#comp 1:
# agriculture
# tradesandcompanies
# accomodations
# education
# social action

#vs

# defense
# debtrefund


#comp 2:
# authorities
# work

#vs

# veterans


rownames(PCA$scores)


######HIERARCHICAL



scale(budgetframe) #joos
dsjoos=dist(scale(budgetframe))
hclustjoos <- hclust(dsjoos,method="ward.D2")
plot(hclustjoos)
#TWO OR THREE GROUPS





#########KMEANS#################

?kmeans
budgetframe
kmeansjoos=kmeans(budgetframe, 2)
color=kmeansjoos$cluster
plot(PCA$scores[,1], PCA$scores[,2], type='n')
text(PCA$scores[,1], PCA$scores[,2], labels=(rownames(PCA$scores)), col=color)
abline(h=0,v=0)

plot(PCA$scores[,1], PCA$scores[,3], type='n')
text(PCA$scores[,1], PCA$scores[,3], labels=(rownames(PCA$scores)), col=color)

kmeansjoos

plot(budgetframe, col = kmeansjoos$cluster) #THIS IS NOT A VERY USEFUL PLOT


##WITH 3 GROUPS
kmeansjoos=kmeans(budgetframe, 3)
color=kmeansjoos$cluster
plot(PCA$scores[,1], PCA$scores[,2], type='n')
text(PCA$scores[,1], PCA$scores[,2], labels=(rownames(PCA$scores)), col=color)
abline(h=0,v=0)

plot(PCA$scores[,1], PCA$scores[,3], type='n')
text(PCA$scores[,1], PCA$scores[,3], labels=(rownames(PCA$scores)), col=color)
