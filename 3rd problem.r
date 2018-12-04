mydata=read.csv(file="E:/Real.csv")

mydata$No=NULL
mydata$X1.transaction.date=NULL

colnames(mydata)[1]="house_age"
colnames(mydata)[2]="Metro_distance"
colnames(mydata)[3]="no_of_stores"
colnames(mydata)[4]="latitude_house"
colnames(mydata)[5]="longitude_house"
colnames(mydata)[6]="house_price"

set.seed(40)

test=mydata[sample(1:nrow(mydata),50),]

set.seed(39)

train1=mydata[sample(1:nrow(mydata),20),]
train2=mydata[sample(1:nrow(mydata),20),]
train3=mydata[sample(1:nrow(mydata),20),]
train4=mydata[sample(1:nrow(mydata),20),]

n1m1=lm(house_price~Metro_distance,train1)
n2m1=lm(house_price~Metro_distance,train2)
n3m1=lm(house_price~Metro_distance,train3)
n4m1=lm(house_price~Metro_distance,train4)

#TRAIN AND TEST ACCURACY------RMSE FOR MODEL 1
sum(m1$residuals^2)
pred1=predict(n1m1, newdata=test)
pred2=predict(n2m1, newdata=test)
pred3=predict(n3m1, newdata=test)
pred4=predict(n4m1, newdata=test)
rmse_testn1m1=sqrt(sum((pred1-test$house_price)^2)/length(test$house_price))
rmse_testn2m1=sqrt(sum((pred2-test$house_price)^2)/length(test$house_price))
rmse_testn3m1=sqrt(sum((pred3-test$house_price)^2)/length(test$house_price))
rmse_testn4m1=sqrt(sum((pred4-test$house_price)^2)/length(test$house_price))


#ORDER 2 POLYNOMIAL
n1m2=lm(house_price~Metro_distance+I(Metro_distance^2),train1)
n2m2=lm(house_price~Metro_distance+I(Metro_distance^2),train2)
n3m2=lm(house_price~Metro_distance+I(Metro_distance^2),train3)
n4m2=lm(house_price~Metro_distance+I(Metro_distance^2),train4)

#TRAIN AND TEST ACCURACY------RMSE FOR MODEL 2
sum(m2$residuals^2)
pred1=predict(n1m2, newdata=test)
pred2=predict(n2m2, newdata=test)
pred3=predict(n3m2, newdata=test)
pred4=predict(n4m2, newdata=test)

rmse_testn1m2=sqrt(sum((pred1-test$house_price)^2)/length(test$house_price))
rmse_testn2m2=sqrt(sum((pred2-test$house_price)^2)/length(test$house_price))
rmse_testn3m2=sqrt(sum((pred3-test$house_price)^2)/length(test$house_price))
rmse_testn4m2=sqrt(sum((pred4-test$house_price)^2)/length(test$house_price))


#ORDER 5 POLYNOMIAL
n1m3=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5),train1)
n2m3=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5),train2)
n3m3=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5),train3)
n4m3=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5),train4)

#TRAIN AND TEST ACCURACY------RMSE FOR MODEL 3
sum(m3$residuals^2)
pred1=predict(n1m3, newdata=test)
pred2=predict(n2m3, newdata=test)
pred3=predict(n3m3, newdata=test)
pred4=predict(n4m3, newdata=test)
rmse_testn1m3=sqrt(sum((pred1-test$house_price)^2)/length(test$house_price))
rmse_testn2m3=sqrt(sum((pred2-test$house_price)^2)/length(test$house_price))
rmse_testn3m3=sqrt(sum((pred3-test$house_price)^2)/length(test$house_price))
rmse_testn4m3=sqrt(sum((pred4-test$house_price)^2)/length(test$house_price))

#ORDER 6 POLYNOMIAL
n1m4=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6),train1)
n2m4=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6),train2)
n3m4=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6),train3)
n4m4=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6),train4)

#TRAIN AND TEST ACCURACY------RMSE FOR MODEL 4
sum(m4$residuals^2)
pred1=predict(n1m4, newdata=test)
pred2=predict(n2m4, newdata=test)
pred3=predict(n3m4, newdata=test)
pred4=predict(n4m4, newdata=test)

rmse_testn1m4=sqrt(sum((pred1-test$house_price)^2)/length(test$house_price))
rmse_testn2m4=sqrt(sum((pred2-test$house_price)^2)/length(test$house_price))
rmse_testn3m4=sqrt(sum((pred3-test$house_price)^2)/length(test$house_price))
rmse_testn4m4=sqrt(sum((pred4-test$house_price)^2)/length(test$house_price))

#ORDER 7 POLYNOMIAL
n1m5=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7),train1)
n2m5=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7),train2)
n3m5=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7),train3)
n4m5=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7),train4)



#TRAIN AND TEST ACCURACY------RMSE FOR MODEL 5
sum(m5$residuals^2)
pred1=predict(n1m5, newdata=test)
pred2=predict(n2m5, newdata=test)
pred3=predict(n3m5, newdata=test)
pred4=predict(n4m5, newdata=test)
rmse_testn1m5=sqrt(sum((pred1-test$house_price)^2)/length(test$house_price))
rmse_testn2m5=sqrt(sum((pred2-test$house_price)^2)/length(test$house_price))
rmse_testn3m5=sqrt(sum((pred3-test$house_price)^2)/length(test$house_price))
rmse_testn4m5=sqrt(sum((pred4-test$house_price)^2)/length(test$house_price))

#ORDER 8 POLYNOMIAL
n1m6=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7)+I(Metro_distance^8),train1)
n2m6=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7)+I(Metro_distance^8),train2)
n3m6=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7)+I(Metro_distance^8),train3)
n4m6=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7)+I(Metro_distance^8),train4)

#TRAIN AND TEST ACCURACY------RMSE FOR MODEL 6
sum(m6$residuals^2)
pred1=predict(n1m6, newdata=test)
pred2=predict(n2m6, newdata=test)
pred3=predict(n3m6, newdata=test)
pred4=predict(n4m6, newdata=test)
rmse_testn1m6=sqrt(sum((pred1-test$house_price)^2)/length(test$house_price))
rmse_testn2m6=sqrt(sum((pred2-test$house_price)^2)/length(test$house_price))
rmse_testn3m6=sqrt(sum((pred3-test$house_price)^2)/length(test$house_price))
rmse_testn4m6=sqrt(sum((pred4-test$house_price)^2)/length(test$house_price))


x<-c(1,2,5,6,7,8)
y1<-c(rmse_testn1m1,rmse_testn1m2,rmse_testn1m3,rmse_testn1m4,rmse_testn1m5,rmse_testn1m6)
y2<-c(rmse_testn2m1,rmse_testn2m2,rmse_testn2m3,rmse_testn2m4,rmse_testn2m5,rmse_testn2m6)
y3<-c(rmse_testn3m1,rmse_testn3m2,rmse_testn3m3,rmse_testn3m4,rmse_testn3m5,rmse_testn3m6)
y4<-c(rmse_testn4m1,rmse_testn4m2,rmse_testn4m3,rmse_testn4m4,rmse_testn4m5,rmse_testn4m6)

dev.new(width=50, height=50)
par(mfrow=c(2,2))
plot(x,log(y1),type='o',pch=19,cex=0.8,xlab="ORDER OF POLYNOMIAL",ylab="TEST RMSE",main="SAMPLE 1",col="red")
plot(x,log(y2),type='o',pch=19,cex=0.8,xlab="ORDER OF POLYNOMIAL",ylab="TEST RMSE",main="SAMPLE 2",col="blue4")
plot(x,log(y3),type='o',pch=19,cex=0.8,xlab="ORDER OF POLYNOMIAL",ylab="TEST RMSE",main="SAMPLE 3",col="orangered4")
plot(x,log(y4),type='o',pch=19,cex=0.8,xlab="ORDER OF POLYNOMIAL",ylab="TEST RMSE",main="SAMPLE 4",col="chartreuse")
mtext(expression(bold("TEST RMSE VS COMPLEXITY FOR RANDOM SAMPLES OF SIZE 20")), side = 3, line = -2, outer = TRUE)

















