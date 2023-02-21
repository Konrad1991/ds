import numpy as np
import math# number of data points to generate
size = 400# data points generated using this func
# given X, radius R return Y = SquareRoot(R^2 - X^2)
def circle(x,r):
 return math.sqrt(r**2-x**2)# Generate 400 random datapoints for X between [-3.0,3.0] 
X = np.random.uniform(low=-3.0,high=3.0,size=size)
Y = []# Generate 400 random labels = (1,0)
O = np.random.randint(low=0,high=2,size=size) # [low,high)# Find Y for each given X
for x,o in zip(X,O): 
 if o == 1 :
  Y.append(circle(x,3.5))  
 else: 
  Y.append(circle(x,4.0)) 

data = []
for x,y,o in zip(X,Y,O):
 data.append([x,y,int(o)])
 
np.savetxt("foo.csv", data, delimiter=",")
