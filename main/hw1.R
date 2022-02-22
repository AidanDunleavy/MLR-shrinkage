# homework 1

library(ISLR)
College

which(College$Top10perc > 70)
College[115,]

s = floor(0.7*dim(College)[1])

set.seed(123)
train_ind <- sample(seq_len(nrow(College)), size = s)
train_ind

CTrain <- College[train_ind,]
head(CTrain)

attach(CTrain)

y = Enroll

model2 = glm(y ~ Apps + Accept + Outstate + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate)

summary(model2)


cor(as.matrix(CTrain[,-c(1,5,6,7,8)]))

cor(PhD, Terminal)


model = glm(y ~ Accept + Outstate + Room.Board + Books + Personal + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate)

summary(model)


predict.glm(model, data.frame(Accept =  c(1400,1500), Outstate = c(9000,1000), Room.Board = 3000, Books = 300, Personal = 2000, Terminal = 80, S.F.Ratio = 12, perc.alumni = 22, Expend = 5000, Grad.Rate = 60))

CTest = College[-train_ind,]

preds = predict.glm(model, data.frame(Accept =  CTest$Accept, Outstate = CTest$Outstate, Room.Board = CTest$Room.Board, Books = CTest$Books, Personal = CTest$Personal,
                              Terminal = CTest$Terminal, S.F.Ratio = CTest$S.F.Ratio, perc.alumni = CTest$perc.alumni, Expend = CTest$Expend, Grad.Rate = CTest$Grad.Rate))
head(preds)
res = CTest$Enroll - preds
head(res)
mean(res)
var(res)
plot(res)
