#1
st <- data.frame(state.x77)
#2
head(st)
tail(st)
str(st)
#3
colnames(st) <- make.names(colnames(st))
#4
rownames(st)
#5
ncol(st)
nrow(st)
#6
str(st)
#7
apply(st,1,sum())
#8.
apply(st,2,sum())
st[,"Income"]
#9.
st["Florida",]
st["Texas","Area"]
#10.
st["Ohio",c("Population","Income")]
#11.
subset(st, Population >=5000)
#12.
subset(st, Income >=4500)[c("Income","Population","Area")]
#13.
sum(st$Income>4500)
#14.
subset(st, Area>=100000 & st$Frost >=120)

#15.
apply(subset(st,Illiteracy >= 2.0)["Income"],2,mean)

#16.
a <- apply(subset(st,Illiteracy >= 2.0)["Income"],2,mean)
b <- apply(subset(st,Illiteracy < 2.0)["Income"],2,mean)
abs(a-b)

#17.
row.names(st[match(max(st$Life.Exp),st$Life.Exp),])

#18. 
subset(st,st$Income > st["Pennsylvania","Income"])
st["Pennsylvania","Income"]
