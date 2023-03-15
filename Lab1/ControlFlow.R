####################
#
# MDLE, 2019-2023
#
####################
#### Ex1 ####
x<-0
for (i in seq(0,10,1)) 
{
  x<-x+i  
}

#### Ex2 ####
for (i in rep(0,10)) 
{
  print(i)  
}

#### Ex3 ####
v<-c(1,2,3,4,5,6,7,8,9)
while(length(v)>0) 
{
  v<-v[-1]
  print(v)
  if( (6 %in% v) == FALSE)
    break
}