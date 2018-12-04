mydata=read.csv(file="E:/Real.csv") # 

View(mydata)

colnames(mydata)

hist(mydata$Metro_distance)

mydata$No=NULL
mydata$X1.transaction.date=NULL

colnames(mydata)[1]="house_age"
colnames(mydata)[2]="Metro_distance"
colnames(mydata)[3]="no_of_stores"
colnames(mydata)[4]="latitude_house"
colnames(mydata)[5]="longitude_house"
colnames(mydata)[6]="house_price"

pairs(mydata)

set.seed(30)
rand = sample(1:nrow(mydata),380)

train=mydata[rand,]
test=mydata[-rand,]
View(test)

train_1=train[sample(1:nrow(train),20),]

m1=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7),train_1)
summary(m1)

#PLOTTING THE MODEL OVER THE DATA
plot(train_1$Metro_distance,train_1$house_price, pch=19, cex=0.5)
lines(sort(train1$Metro_distance), fitted(m)[order(train1$Metro_distance)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
train_error1=sum(m1$residuals^2)
pred = predict(m1, newdata=test)
test_error1=sum((pred-test$house_price)^2)

train_2=train[sample(1:nrow(train),30),]

m2=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7),train_2)
summary(m2)

#PLOTTING THE MODEL OVER THE DATA
plot(train_2$Metro_distance,train_2$house_price, pch=19, cex=0.5)
lines(sort(train_2$Metro_distance), fitted(m2)[order(train_2$Metro_distance)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
train_error2=sum(m2$residuals^2)
pred = predict(m2, newdata=test)
test_error2=sum((pred-test$house_price)^2)

train_3=train[sample(1:nrow(train),50),]

m3=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7),train_3)
summary(m2)

#PLOTTING THE MODEL OVER THE DATA
plot(train_3$Metro_distance,train_3$house_price, pch=19, cex=0.5)
lines(sort(train_3$Metro_distance), fitted(m3)[order(train_3$Metro_distance)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
train_error3=sum(m3$residuals^2)
pred = predict(m3, newdata=test)
test_error3=sum((pred-test$house_price)^2)

train_4=train[sample(1:nrow(train),70),]

m4=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7),train_4)
summary(m4)

#PLOTTING THE MODEL OVER THE DATA
plot(train_4$Metro_distance,train_4$house_price, pch=19, cex=0.5)
lines(sort(train_4$Metro_distance), fitted(m4)[order(train_4$Metro_distance)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
train_error4=sum(m4$residuals^2)
pred = predict(m4, newdata=test)
test_error4=sum((pred-test$house_price)^2)

train_5=train[sample(1:nrow(train),100),]

m5=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7),train_5)
summary(m4)

#PLOTTING THE MODEL OVER THE DATA
plot(train_5$Metro_distance,train_5$house_price, pch=19, cex=0.5)
lines(sort(train_5$Metro_distance), fitted(m5)[order(train_5$Metro_distance)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
train_error5=sum(m5$residuals^2)
pred = predict(m5, newdata=test)
test_error5=sum((pred-test$house_price)^2)


train_6=train[sample(1:nrow(train),200),]

m6=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7),train_6)
summary(m6)

#PLOTTING THE MODEL OVER THE DATA
plot(train_6$Metro_distance,train_6$house_price, pch=19, cex=0.5)
lines(sort(train_6$Metro_distance), fitted(m6)[order(train_6$Metro_distance)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
train_error6=sum(m6$residuals^2)
pred = predict(m6, newdata=test)
test_error6=sum((pred-test$house_price)^2)

train_7=train[sample(1:nrow(train),300),]

m7=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7),train_7)
summary(m7)

#PLOTTING THE MODEL OVER THE DATA
plot(train_7$Metro_distance,train_7$house_price, pch=19, cex=0.5)
lines(sort(train_7$Metro_distance), fitted(m7)[order(train_7$Metro_distance)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
train_error7=sum(m7$residuals^2)
pred = predict(m7, newdata=test)
test_error7=sum((pred-test$house_price)^2)

train_8=train[sample(1:nrow(train),350),]

m8=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7),train_8)
summary(m8)

#PLOTTING THE MODEL OVER THE DATA
plot(train_8$Metro_distance,train_8$house_price, pch=19, cex=0.5)
lines(sort(train_8$Metro_distance), fitted(m8)[order(train_8$Metro_distance)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
train_error8=sum(m8$residuals^2)
pred = predict(m8, newdata=test)
test_error8=sum((pred-test$house_price)^2)


x<-c(length(train_1$house_price),length(train_2$house_price),length(train_3$house_price),length(train_4$house_price),length(train_5$house_price),length(train_6$house_price),length(train_7$house_price),length(train_8$house_price))

y1<-c(train_error1,train_error2,train_error3,train_error4,train_error5,train_error6,train_error7,train_error8)

y2<-c(test_error1,test_error2,test_error3,test_error4,test_error5,test_error6,test_error7,test_error8)


plot(x,y1,type='l',col='red')
par(new='TRUE')
plot(x,y2,type='l',col='purple')


s1=sqrt((test_error1)/(length(test$house_price)))
s2=sqrt(test_error2/length(test$house_price))
s3=sqrt(test_error3/length(test$house_price))
s4=sqrt(test_error4/length(test$house_price))
s5=sqrt(test_error5/length(test$house_price))
s6=sqrt(test_error6/length(test$house_price))
s7=sqrt(test_error7/length(test$house_price))
s8=sqrt(test_error8/length(test$house_price))
     
RMSE<-c(s1,s2,s3,s4,s5,s6,s7,s8)
dev.new(width=50, height=20)
plot( RMSE~x , type="o" , bty="o" ,main ="TEST ERROR VS SAMPLE SIZE FOR POLYNOMIAL REGRESSION ORDER 7 FOR METRO DISTANCE" ,xlab=" TRAINING SAMPLE SIZE" , ylab="RESIDUALS OF TEST",col=rgb(0.1,0.2,0.3,0.7),pch=19,cex=0.8)
legend("topright",
legend = "RSS FOR EACH",
col = c(rgb(0.2,0.4,0.1,0.7),
rgb(0.8,0.4,0.1,0.7)),
pch = c(17,19),
bty = "o",
pt.cex = 2,
cex = 0.6,
text.col = "black",
horiz = F ,
inset = c(0.1, 0.1))




