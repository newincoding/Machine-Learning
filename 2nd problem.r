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

rand = sample(1:nrow(mydata),300)

train=mydata[rand,]
test=mydata[-rand,]

m1=lm(house_price~Metro_distance,train)
m2=lm(house_price~Metro_distance+I(Metro_distance^2),train)
m3=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3),train)

m4=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4),train)
m5=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5),train)
m6=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6),train)
m7=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7),train)
m8=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7)+I(Metro_distance^8),train)
m9=lm(house_price~Metro_distance+I(Metro_distance^2)+I(Metro_distance^3)+I(Metro_distance^4)+I(Metro_distance^5)+I(Metro_distance^6)+I(Metro_distance^7)+I(Metro_distance^8)+I(Metro_distance^9),train)


dev.new(width=50, height=20)
plot(train$Metro_distance,train$house_price, pch=19, cex=0.5,xlab="METRO DISTANCE(METRES)",ylab="HOUSE PRICE($)",main="MODEL REPRESENTATION FOR DIFFERENT ORDER POLYNOMIAL REGRESSION OF SAMPLE SIZE 300")
help(plot)
lines(sort(train$Metro_distance), fitted(m1)[order(train$Metro_distance)], col='red', type='l')
lines(sort(train$Metro_distance), fitted(m2)[order(train$Metro_distance)], col='green', type='l')
lines(sort(train$Metro_distance), fitted(m3)[order(train$Metro_distance)], col='blue', type='l')
legend("topright", legend=c("ORDER 1", "ORDER 2","ORDER 3"),
       col=c("red", "green","blue"), lty=1:2, cex=0.8,text.font=2, box.lwd=2)

dev.new(width=50, height=20)
plot(train$Metro_distance,train$house_price, pch=19, cex=0.5,xlab="METRO DISTANCE(METRES)",ylab="HOUSE PRICE($)",main="MODEL REPRESENTATION FOR DIFFERENT ORDER POLYNOMIAL REGRESSION OF SAMPLE SIZE 300")
lines(sort(train$Metro_distance), fitted(m4)[order(train$Metro_distance)], col='red', type='l')
lines(sort(train$Metro_distance), fitted(m5)[order(train$Metro_distance)], col='green', type='l')
lines(sort(train$Metro_distance), fitted(m6)[order(train$Metro_distance)], col='blue', type='l')
legend("topright", legend=c("ORDER 4", "ORDER 5","ORDER 6"),
       col=c("red", "green","blue"), lty=1:2, cex=0.8,text.font=2, box.lwd=2)

dev.new(width=50, height=20)
plot(train$Metro_distance,train$house_price, pch=19, cex=0.5,xlab="METRO DISTANCE(METRES)",ylab="HOUSE PRICE($)",main="MODEL REPRESENTATION FOR DIFFERENT ORDER POLYNOMIAL REGRESSION OF SAMPLE SIZE 300")
lines(sort(train$Metro_distance), fitted(m7)[order(train$Metro_distance)], col='red', type='l')
lines(sort(train$Metro_distance), fitted(m8)[order(train$Metro_distance)], col='green', type='l')
lines(sort(train$Metro_distance), fitted(m9)[order(train$Metro_distance)], col='blue', type='l')
legend("topright", legend=c("ORDER 7", "ORDER 8","ORDER 9"),
       col=c("red", "green","blue"), lty=1:2, cex=0.8,text.font=2, box.lwd=2)















