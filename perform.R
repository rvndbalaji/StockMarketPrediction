msft = c(98.6,98.9,97.9,89.7,71.2)
ibm = c(99.4,99.5,99.4,99.2,99.3)
mmm = c(99.8,99.3,99.4,95,85.84)
ko =  c(99.5,99.6,99.4,99.4,99.2)
mcd = c(98.7,99,98.8,96.4,90.8)
intc = c(98.8,99.1,99.2,87.4,79.1)
nke = c(99.2,98.4,99.3,99,97.4)
aapl = c(97,99,98.8,98.6,90.1)
csco = c(97.9,98.4,99.3,99.2,90.9)
dis = c(97.9,99,98,97.5,98.9)

results = data.frame(msft,ibm,mmm,ko,mcd,intc,nke,aapl,csco,dis)


one = mean(as.numeric(results[1,]))
ten = mean(as.numeric(results[2,]))
thirty = mean(as.numeric(results[3,]))
sixty = mean(as.numeric(results[4,]))
ninety = mean(as.numeric(results[5,]))



C = c("red","lightblue","yellow","orange","darkcyan","pink","green","brown","purple","blue") 

thick = 2
x = 1


pall = function()
{
  plot(msft,type='b',col=C[1],lwd=thick,pch=x,xlab = "Range in days",ylab = "Accuracy %",main="Individual Predictive Performance",xaxt="n")
  axis(1, at=1:5, labels=c("1","10","30","60","90"))
  points(ibm,type='b',col=C[2],lwd=thick,pch=x)
  points(mmm,type='b',col=C[3],lwd=thick,pch=x)
  points(ko,type='b',col=C[4],lwd=thick,pch=x)
  points(mcd,type='b',col=C[5],lwd=thick,pch=x)
  points(intc,type='b',col=C[6],lwd=thick,pch=x)
  points(nke,type='b',col=C[7],lwd=thick,pch=x)
  points(aapl,type='b',col=C[8],lwd=thick,pch=x)
  points(csco,type='b',col=C[9],lwd=thick,pch=x)
  points(dis,type='b',col=C[10],lwd=thick,pch=x)
  
  names = c("Microsoft","IBM","3M","Coca Cola","McDonald's","Intel","Nike","Apple","Cisco","Disney")
  legend('bottomleft',names,fill = C)
}

pavg = function()
{

  plot(c(1:5),c(one,ten,thirty,sixty,ninety),type='b',col=C[1],lwd=thick,pch=x,xlab = "Range in days",ylab = "Accuracy %",main="Overall Performance",xlim = c(1,5),ylim = c(80,100),xaxt="n")
  axis(1, at=1:5, labels=c("1","10","30","60","90"))
  legend('bottomleft',"Accuracy  ",fill = 'red')
}






