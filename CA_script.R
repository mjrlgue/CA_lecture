

#########################
#### chi test examples
###########################


### please check the importance of N = number of data

N=1
table=matrix(c(.8,.3,.4,.9)*N,nrow=2)
table
chisq.test(table)

N=10
table=matrix(c(.8,.3,.4,.9)*N,nrow=2)
table
chisq.test(table)

N=100
table=matrix(c(.8,.3,.4,.9)*N,nrow=2)
table
chisq.test(table)

###################################
###### example code 1 ###
###################################

### create data

observed.table=matrix(c(4,3,2,4,1,4,8,3,11,12,10,
               9,24,1,8,18,5,24,1,1,6,5,1,5,
               6,0,2,3,1,11,4,3,5,2,3,10,23,
               6,7,26,11,20,51,43,8,70,19,66),
             nrow=8,byrow = T)
rownames(observed.table)=c("Canada","France","Germany","Itale","Japan","Russia","UK","USA")
colnames(observed.table)=c("Chemistry","Economics","Literature","Medicine","Peace","Physics")

observed.table

### perform independence test
chisq.test(observed.table) ### test for overall independence
# p-value < 0.05, there is enough evidence to say that these two variables are not independent. 
# i.e. there is some sort of dependence in the data


#########################################################
##### some code to understand the dynamics of CA ####
#########################################################

### Turn table into probability table
total.obs = sum(observed.table)
observed.probabilities = observed.table / total.obs
observed.probabilities = round(observed.probabilities,3)
observed.probabilities

#############################################################
### create expected probability tables under independence

### row and column marginal probabilities
row.probabilities = apply(observed.probabilities,1,sum)
col.probabilities = apply(observed.probabilities,2,sum)

### expected probability table under independence
expected.probabilities = matrix(NA,ncol=ncol(observed.probabilities),nrow=nrow(observed.probabilities))
for( i  in 1:length(row.probabilities)){
  for(j in 1:length(col.probabilities)){
    expected.probabilities[i,j]= row.probabilities[i]*col.probabilities[j]
  }
}
## or
expected.probabilities = matrix(apply(expand.grid(row.probabilities,col.probabilities),1,prod),nrow = nrow(observed.probabilities))



#################################################
###### observing deviations from independence ##
###################################################

### RAW or CRUDE deviations from expected values
(observed.probabilities - expected.probabilities) *100

### chi distances from mean profile
## row profile
(row_1 =  sum(1/col.probabilities * (observed.probabilities[1,]/row.probabilities[1] - col.probabilities)^2))
(row_2 =  sum(1/col.probabilities * (observed.probabilities[2,]/row.probabilities[2] - col.probabilities)^2))
(row_3 =  sum(1/col.probabilities * (observed.probabilities[3,]/row.probabilities[3] - col.probabilities)^2))
(row_4 =  sum(1/col.probabilities * (observed.probabilities[4,]/row.probabilities[4] - col.probabilities)^2))
(row_5 =  sum(1/col.probabilities * (observed.probabilities[5,]/row.probabilities[5] - col.probabilities)^2))
(row_6 =  sum(1/col.probabilities * (observed.probabilities[6,]/row.probabilities[6] - col.probabilities)^2))
(row_7 =  sum(1/col.probabilities * (observed.probabilities[7,]/row.probabilities[7] - col.probabilities)^2))
(row_8 =  sum(1/col.probabilities * (observed.probabilities[8,]/row.probabilities[8] - col.probabilities)^2))

### which is the most distant from the mean country in the table

## total inertia through rows
row.probabilities*c(row_1,row_2,row_3,row_4,row_5,row_6,row_7,row_8)
sum(row.probabilities*c(row_1,row_2,row_3,row_4,row_5,row_6,row_7,row_8))


## col profile
(col_1 =  sum(1/row.probabilities * (observed.probabilities[,1]/col.probabilities[1] - row.probabilities)^2))
(col_2 =  sum(1/row.probabilities * (observed.probabilities[,2]/col.probabilities[2] - row.probabilities)^2))
(col_3 =  sum(1/row.probabilities * (observed.probabilities[,3]/col.probabilities[3] - row.probabilities)^2))
(col_4 =  sum(1/row.probabilities * (observed.probabilities[,4]/col.probabilities[4] - row.probabilities)^2))
(col_5 =  sum(1/row.probabilities * (observed.probabilities[,5]/col.probabilities[5] - row.probabilities)^2))
(col_6 =  sum(1/row.probabilities * (observed.probabilities[,6]/col.probabilities[6] - row.probabilities)^2))

## which is the most distant nobel price from the mean profile?

## total inertia through cols
sum(col.probabilities*c(col_1,col_2,col_3,col_4,col_5,col_6))




#####################################
##### row and columns profiles #####
#####################################

### Very important to understand CA results

row.profiles = round(observed.probabilities/row.probabilities,3)*100
rbind(row.profiles,round(col.probabilities,3)*100)

col.profiles = round(t(observed.probabilities)/col.probabilities,3)*100
t(rbind(col.profiles,round(row.probabilities,3)*100))


##################################################################
################# Run example ##############################
##################################################################
require(FactoMineR)

res.ca = CA(observed.table)
plot(res.ca)
plot(res.ca,axes = c(3,4))
summary(res.ca)

#### number of dimensions
barplot(res.ca$eig[,2])

####################################
##### axis interpretation help #####
####################################

#### rows
## first axis
barplot(res.ca$row$contrib[,1])
abline(mean(res.ca$row$contrib[,1]),0)
## second axis
barplot(res.ca$row$contrib[,2])
abline(mean(res.ca$row$contrib[,2]),0)
## third axis
barplot(res.ca$row$contrib[,3])
abline(mean(res.ca$row$contrib[,3]),0)
## fourth axis
barplot(res.ca$row$contrib[,4])
abline(mean(res.ca$row$contrib[,4]),0)

#### columns
## first axis
barplot(res.ca$col$contrib[,1])
abline(mean(res.ca$col$contrib[,1]),0)
## second axis
barplot(res.ca$col$contrib[,2])
abline(mean(res.ca$col$contrib[,2]),0)
## third axis
barplot(res.ca$col$contrib[,3])
abline(mean(res.ca$col$contrib[,3]),0)
## fourth axis
barplot(res.ca$col$contrib[,4])
abline(mean(res.ca$col$contrib[,4]),0)


###############################################################
##### quality of representation between profiles and axis #####
###############################################################

barplot(res.ca$row$cos2[,1])
barplot(res.ca$row$cos2[,2])
barplot(res.ca$row$cos2[,3])
barplot(res.ca$row$cos2[,4])

barplot(res.ca$col$cos2[,1])
barplot(res.ca$col$cos2[,2])
barplot(res.ca$col$cos2[,3])
barplot(res.ca$col$cos2[,4])


plot(res.ca, cex=.8,selectCol = "cos2 0.6")
plot(res.ca, cex=.8,selectCol = "cos2 0.5",axes = c(3,4))



#######################################
###### Questions #############
######################################

### which is the country that is best represented in the first two axis?

### which is the country that most contributed to the first and second axis

### are italy and literature well represented in the first two axis?

### what can you tell me about the UK?

### what can you tell me about germany and japan?




##### cluster cols and columns in groups
## by columns
res.hcpc = HCPC(res.ca,cluster.CA = "columns")
## by rows
res.hcpc = HCPC(res.ca,cluster.CA = "rows")






###########################################################
########## Your turn ############################
####################################################

### first lets work with housetasks dataset
link="C:/Users/iosu/Dropbox/Anglet/teaching/lectures/AFC/Data/"
load(paste0(link,"housetasks.Rdata"))

### second Olympique games dataset
data(JO)

### third wine description dataset
link="C:/Users/iosu/Dropbox/Anglet/teaching/lectures/AFC/Data/"
load(paste0(link,"CA_wine_description.Rdata"))




##### example #####
housetasks

### create row - col profiles
total.obs = sum(housetasks)
observed.probabilities = housetasks / total.obs
observed.probabilities = round(observed.probabilities,4)
observed.probabilities

row.probabilities = apply(observed.probabilities,1,sum)
col.probabilities = apply(observed.probabilities,2,sum)

row.profiles = round(observed.probabilities/row.probabilities,3)*100
rbind(row.profiles,round(col.probabilities,3)*100)

col.profiles = round(t(observed.probabilities)/col.probabilities,3)*100
t(rbind(col.profiles,round(row.probabilities,3)*100))


res.ca = CA(housetasks)
plot(res.ca,axes = c(3,4))
summary(res.ca)

#### number of dimensions
barplot(res.ca$eig[,2])

####################################
##### axis interpretation help #####
####################################

## first axis
par(mfrow=c(2,1))
idx=which(res.ca$row$contrib[,1]>mean(res.ca$row$contrib[,1]))
barplot(res.ca$row$contrib[idx,1],main="contribution")

idx=which(res.ca$row$cos2[,1]>mean(res.ca$row$cos2[,1]))
barplot(res.ca$row$cos2[idx,1],main="cos2")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
idx=which(res.ca$col$contrib[,1]>mean(res.ca$row$contrib[,1]))
barplot(res.ca$col$contrib[idx,1],main="contribution")

idx=which(res.ca$col$cos2[,1]>mean(res.ca$row$cos2[,1]))
barplot(res.ca$col$cos2[idx,1],main="cos2")
par(mfrow=c(1,1))


## second axis
par(mfrow=c(2,1))
idx=which(res.ca$row$contrib[,2]>mean(res.ca$row$contrib[,2]))
barplot(res.ca$row$contrib[idx,2],main="contribution")

idx=which(res.ca$row$cos2[,2]>mean(res.ca$row$cos2[,2]))
barplot(res.ca$row$cos2[idx,2],main="cos2")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
idx=which(res.ca$col$contrib[,2]>mean(res.ca$row$contrib[,2]))
barplot(res.ca$col$contrib[idx,2],main="contribution")

idx=which(res.ca$col$cos2[,2]>mean(res.ca$row$cos2[,2]))
barplot(res.ca$col$cos2[idx,2],main="cos2")
par(mfrow=c(1,1))





