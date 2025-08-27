import numpy as np
import matplotlib.pyplot as plt

x1 = np.linspace(-10, 10, 400)

x2_values = [0, 1, 2]

for x2 in x2_values:
    J = (x1 + x2 - 2)**2           
    plt.plot(x1, J, label=f"x2 = {x2}")  

plt.xlabel("x1")                   
plt.ylabel("J(x1, x2)")            
plt.title("Plot of J = (x1 + x2 - 2)^2 vs x1 for fixed x2 values")  
plt.legend()                       
plt.grid(True)                     

plt.show()                         
