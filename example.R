
head(iris)
dim(iris)
plot(iris[1:150,-c(5)])

matrixIris=iris[-5]
class(matrixIris)

matrixIris=matrix(as.numeric(unlist(matrixIris)), nrow=nrow(matrixIris))
class(matrixIris)

#subract that mean and center it on 0
x=scale(matrixIris, center=TRUE, scale=TRUE)

#scale false?
x2=scale(matrixIris, scale=FALSE)

#look at head
head(x)
head(x2)

#covariance matrix
coIris=(t(x)%*%x/(nrow(x)-1))
head(coIris)

#correlation is standard covariance!
head(cor(matrixIris))

out=svd(coIris)
names(out)
out

save4l8er=diag(out$d)
save4l8er
sum(save4l8er)
print(save4l8er)

#now project our data on our new coordinates
PCs=(x%*%out$v)#project data onto new axes
dim(PCs)
cov(PCs)


#total covariance
sum(out$d)
#4 because it is normalized

#how much of each dataset variability is represented by each PC
fracVariance=out$d/sum(out$d)
#72.9%, 22.9%, 3.7% and .52%


#save for later one
sum(save4l8er)
out$d/sum(out$d)
#[1] 0.729624454 0.228507618 0.036689219 0.005178709


#make x brackt for PCA's
x1=c(1,2,3,4)

#plot
ggplot()+geom_point(aes(x=x1, y=fracVariance))+ggtitle("Fraction of Variance in Each PCA")+xlab("PCA")+ylab("Percent Variance")




#data frame with the original data, and PCs
df = cbind(as.data.frame(PCs),iris[1:nrow(x),]) 

class(df)
#and eigenvectors with names


df2 = cbind(as.data.frame(out$v),names(iris[,-c(5)])) 
df2

dim(df)
dim(df2)
df2

names(df2)[5]="names"


#lets setup the length of our arrows as a variable
arrowScale=5

#as a data.frame, it converts them to v1, v2 and v3, etc.. 

#and make an awesome plot, we're going to compare PC1 and PC2

iris$Species

ggplot(df, aes(x = V1, y= V2))+geom_point(aes(colour=Species),size=6)+
  #first just plotting points and colouring them by the cut
  
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  
  #then we'll plot some 0 lines for our reference
  coord_fixed(xlim=c(-5,5),ylim=c(-5,5))+
  
  #and set the scale
  geom_segment(data=df2,aes(x=0,y=0,xend = V1*arrowScale, yend = V2*arrowScale))+
  
  #Now we'll add lines that correspond to our eigen vectors
  geom_text(data=df2,aes(x = V1*arrowScale, y = V2*arrowScale,label=names))
#and label those lines



