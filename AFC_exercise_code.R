
#### test of independence
### the importance of N
table=matrix(c(8,3,4,7),nrow=2)
table
chisq.test(table)

table=matrix(c(8,3,4,7) * 10 ,nrow=2)
table
chisq.test(table)



require(FactoMineR)

link ="C:/Users/iosu/Dropbox/Anglet/teaching/lectures/AFC/Data/"
data <- read.csv2(paste0(link,"AnaDo_JeuDonnees_UEemploi.csv"),fileEncoding="latin1",row.names = 1)
head(data)
model <- CA(data)
summary(model)
dimdesc(model)

plot(model, cex=.8)

####  plot the rows amd columns with a cos2 bigger than 0.8
plot(model, invisible="col",cex=.8,selectRow = "cos2 0.8")
plot(model, invisible="row",cex=.8,selectCol = "cos2 0.8")

####  plot together the rows amd columns with a cos2 bigger than 0.7
plot(model, shadowtext = TRUE, cex=.8, selectRow = "cos2 0.7", selectCol = "cos2 0.7")

#### plot the 4 rows and columns that contribute most to the cA
plot(model, shadowtext = TRUE, cex=.8, selectRow = "contrib 4", selectCol = "contrib 4")

#### plot other axes
plot(model, shadowtext = TRUE, cex=.8, axes = c(2,3))

##### cluster cols and columns in groups
## by columns
res.hcpc = HCPC(model,cluster.CA = "columns")
## by rows
res.hcpc = HCPC(model,cluster.CA = "rows")
