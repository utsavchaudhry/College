import numpy as np

x = np.array([-2, -5, -3,  0, -8, -2,  1,  5, -1,  6])
y = np.array([ 1, -4,  1,  3, 11,  5,  0, -1, -3,  1])

x = x.reshape(-1, 1)
y = y.reshape(-1, 1)

ones = np.ones_like(x)       
X = np.hstack((ones, x))     

XT_X = X.T @ X               
XT_y = X.T @ y               
inv_XT_X = np.linalg.inv(XT_X)
theta = inv_XT_X @ XT_y      

intercept, slope = theta.flatten()
print(f"Intercept (theta_0): {intercept:.4f}")
print(f"Slope     (theta_1): {slope:.4f}")
