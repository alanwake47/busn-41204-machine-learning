library(tree)

nbc <- read.csv("nbc.csv")

genre <- as.factor(nbc$Genre)
GRP <- nbc$GRP
PE <- nbc$PE

Show <- nbc[,1]
nbc <- nbc[,-c(1,58:62)]

genretree <- tree(genre ~ ., data=cbind(nbc, genre), mincut=1)
plot(genretree, col=8, type="uniform", lwd=2); text(genretree, cex=0.5)

gp <- predict(genretree, newdata=nbc, type="class")
disagree <- genre != gp
rbind(show=as.character(Show[disagree]),
      data=as.character(genre[disagree]), pred=as.character(gp[disagree]))


x <- model.matrix(PE ~ genre + GRP)[,-1]
x <- as.data.frame(x)
names(x) <- c("reality", "comedy", "GRP")

nbctree <- tree(PE ~ ., data=x, mincut=1)

newgrp <- seq(1,3000,length=1000)
pd <- predict(nbctree, newdata=data.frame(GRP=newgrp, drama=1, comedy=0, reality=0))
pc <- predict(nbctree, newdata=data.frame(GRP=newgrp, drama=0, comedy=1, reality=0))
pr <- predict(nbctree, newdata=data.frame(GRP=newgrp, drama=0, comedy=0, reality=1))

par(mfrow=c(1,2)); plot(nbctree, type="uniform", col=8); text(nbctree, cex=0.75)
plot(PE ~ GRP, data=nbc, bty="n", col=c(4,2,3)[genre], pch=20, ylim=c(45,90))
lines(newgrp, pd, col=4); lines(newgrp, pc, col=3); lines(newgrp, pr, col=2)
legend("bottomright", c("comedy", "drama", "reality"), col=4:2, bty="n", lty=1)




