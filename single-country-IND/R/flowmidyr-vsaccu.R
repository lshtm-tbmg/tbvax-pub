never = function(t, A=1, k=NULL){
  A * exp(-k*t)  
}
vac = function(t, A=1, k=NULL){
  A*(1 - exp(-k*t))  
}

r=0.494
never(0,A=85.1, k=r)
never(0.5,A=85.1, k=r)
never(1,A=85.1, k=r)

vac(0,A=85.1, k=r)
vac(0.5,A=85.1, k=r)
vac(0.8,A=85.1, k=r)
vac(1,A=85.1, k=r)

r  * never(t=0.5,A=85.1, k=r)
r  * never(t=0.8,A=85.1, k=r)

r=(1:30)/10
plot(x=r,y=vac(t=1,A=100,k=r),type="b",col="red",ylim=c(0,100))
lines(x=r,y=r*never(t=0.5,A=100,k=r),type="b",col="blue")


-log((1-exp(-r))/r)/r
