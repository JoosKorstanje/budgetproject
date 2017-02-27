setwd('G:/LaptopOleStuff/Advanced stat + ML/20161129/project')
getwd()

rm(list=ls())

#Getting the data
budgetdata = read.table("Budget.txt", sep = ",")
budgetdata

#The first column of the budgetdata are years. Creating one data frame with the years and one without.
budgetdata[,1]
budgetdata2=budgetdata[2:12]
rownames(budgetdata2) = budgetdata[,1]
budgetdata2

#Add column names to the two data frames.
names(budgetdata2) = c("authorities", "agriculture", "tradesandcompanies", "work", "accomodations", "education", "socialaction", "veterans", "defense", "debtrefund", "various")
names(budgetdata) = c("year", "authorities", "agriculture", "tradesandcompanies", "work", "accomodations", "education", "socialaction", "veterans", "defense", "debtrefund", "various")

#Creating a matrix out of the data frame.
budgetdata2
budgetmatrix=as.matrix(budgetdata2)
budgetmatrix

####DESCRIPTIVE STATISTICS####
#Calculating several descriptive statistics
means=apply(budgetmatrix, 2, mean)
stdevs=apply(budgetmatrix, 2, sd)
mins=apply(budgetmatrix, 2, min)
maxs=apply(budgetmatrix, 2, max)

#Printing the descriptive statistcs
means
stdevs
mins
maxs

#Adding the variable names to the matrix.
varnames=colnames(budgetmatrix)
varnames

#Putting all the descriptives in one table.
explore=cbind(means, stdevs, mins, maxs)
explore


#graphics.off()

#Function to create a histogram per variable in one plot window
my_f3 <-function(mydataset){
  par(mar = rep(2, 4))
  par(mfrow=c(4,3))
  for (i in 1:length(mydataset)){
    hist(mydataset[,i], main=(names(mydataset))[i],freq=FALSE)
  }
}
my_f3(budgetdata2)


#Function to create a qq-plot per variable in one plot window
my_qqplots <-function(mydataset){
  par(mar = rep(2, 4))
  par(mfrow=c(4,3))
  for (i in 1:length(mydataset)){
    qqnorm(mydataset[,i], main=(names(mydataset))[i])
    qqline(mydataset[,i])
  }
}
my_qqplots(budgetdata2)


#Creating boxplots
par(mfrow=c(1,1))
boxplot(budgetdata2)

#Function to create a line graph per variable in one plot window

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



#Creating boxpltos per year instead of per variable
#graphics.off()
par(mfrow=c(1,1))
yearbudgetmatrix=t(budgetmatrix)
boxplot(yearbudgetmatrix)


#Checking for correlation
cor(budgetmatrix)
cor(yearbudgetmatrix)


#Creating a heatmap of correlations
install.packages('ggplot2')
library(ggplot2)
install.packages('reshape2')
library(reshape2)

#Creating input for ggplot heatmap
cormat<-round(cor(budgetmatrix),2)
cormat<-melt(cormat) #makes combination of correlation (different format, not really like a matrix)

#Creating ggplot heatmap
ggplot(data=data.frame(cormat),aes(fill=value,x=Var1, y=Var2))+geom_tile(color="white")+
  scale_fill_gradient2(low = "blue", high = "blue", mid = "orange", midpoint = 0, limit = c(-1,1), space = "Lab",name="Pearson\nCorrelation")


#Creating heatmap for years
coryears=cor(yearbudgetmatrix)
cormatrixyear=melt(coryears)

ggplot(data=data.frame(cormatrixyear),aes(fill=value,x=Var1, y=Var2))+geom_tile(color="white")+
  scale_fill_gradient2(low = "black", high = "black", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab",name="Pearson\nCorrelation")


####Principal Components Analysis####
PCA=princomp(budgetdata2, cor=TRUE)

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



#comp 3: no clear interpretation
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

####Clustering####
#Decide on k for k-means based on hclust
dsjoos=dist(scale(budgetdata2))
hclustjoos <- hclust(dsjoos,method="ward.D2")
plot(hclustjoos)
#Decision: two or three clusters


#k-means
kmeansjoos=kmeans(budgetdata2, 2)
color=kmeansjoos$cluster

#With two clusters:
#Plot first factorial plane
plot(PCA$scores[,1], PCA$scores[,2], type='n')
text(PCA$scores[,1], PCA$scores[,2], labels=(rownames(PCA$scores)), col=color)
abline(h=0,v=0)

#Plot second factorial plane
plot(PCA$scores[,1], PCA$scores[,3], type='n')
text(PCA$scores[,1], PCA$scores[,3], labels=(rownames(PCA$scores)), col=color)

#Look at output
kmeansjoos



#With 3 clusters
kmeansjoos=kmeans(budgetdata2, 3)
color=kmeansjoos$cluster

#Plot first factorial plane
plot(PCA$scores[,1], PCA$scores[,2], type='n')
text(PCA$scores[,1], PCA$scores[,2], labels=(rownames(PCA$scores)), col=color)
abline(h=0,v=0)

#Plot second factorial plane
plot(PCA$scores[,1], PCA$scores[,3], type='n')
text(PCA$scores[,1], PCA$scores[,3], labels=(rownames(PCA$scores)), col=color)
abline(h=0,v=0)
