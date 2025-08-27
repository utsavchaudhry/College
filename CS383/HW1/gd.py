#!/usr/bin/env python3
import numpy as np
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt

data = np.loadtxt("x06Simple.csv", delimiter=',', skiprows=1)[:,1:]  
X, y = data[:,:-1], data[:,-1].reshape(-1,1)

X_train, X_test, y_train, y_test = train_test_split(
    X, y, train_size=2/3, random_state=0, shuffle=True        
)

# Standardize
mu, sigma = X_train.mean(axis=0), X_train.std(axis=0)         
X_train = (X_train - mu) / sigma
X_test  = (X_test  - mu) / sigma

# Add bias
m, n = X_train.shape
Xb_train = np.hstack([np.ones((m,1)), X_train])
Xb_test  = np.hstack([np.ones((X_test.shape[0],1)), X_test])

# Setup GD
np.random.seed(0)                                              
theta = 2*np.random.rand(n+1,1) - 1                            
lr = 0.01                                                      

# GD loop
prev_rmse = float('inf')
train_errors, test_errors = [], []

for i in range(1, 1001):
    # Predictions & gradient
    y_hat = Xb_train @ theta
    grad = (2/m) * Xb_train.T @ (y_hat - y_train)               
    theta -= lr * grad

    # Compute RMSEs
    rmse_tr = np.sqrt(np.mean((y_train - Xb_train@theta)**2))  
    rmse_te = np.sqrt(np.mean((y_test  - Xb_test @theta)**2))  
    train_errors.append(rmse_tr)
    test_errors.append(rmse_te)

    # Check convergence
    pct = abs((prev_rmse - rmse_tr)/prev_rmse)                  
    if pct < 2**-23:
        print(f"Converged at iteration {i}")                   
        break
    prev_rmse = rmse_tr

print("Final θ:", theta.flatten())
print(f"Test RMSE: {test_errors[-1]:.4f}")

plt.figure()  
plt.plot(train_errors, label='Train RMSE')   
plt.plot(test_errors,  label='Test RMSE')    

plt.xlabel('Iteration')                      
plt.ylabel('RMSE')                           
plt.title('Training vs. Testing RMSE per Iteration')  
plt.legend(loc='best')                       
plt.grid(True)                               
plt.tight_layout()                           

plt.show()                                   
