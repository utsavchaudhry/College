import numpy as np
from sklearn.model_selection import train_test_split

data = np.loadtxt("x06Simple.csv", delimiter=',', skiprows=1)[:,1:]
X, y = data[:,:-1], data[:,-1]

X_train, X_test, y_train, y_test = train_test_split(
    X, y, train_size=2/3, random_state=0, shuffle=True
)

# Add bias term
Xb_train = np.hstack([np.ones((X_train.shape[0],1)), X_train])
Xb_test  = np.hstack([np.ones((X_test.shape[0],1)),  X_test])

# Container for predictions
y_pred = np.zeros_like(y_test)

# Locally-weighted for each test point
for i, x_star in enumerate(X_test):
    # L1 distances
    d = np.abs(X_train - x_star).sum(axis=1)
    # Weights (k=1)
    w = np.exp(-d)  
    W = np.diag(w)
    # Solve weighted normal equations
    XtWX = Xb_train.T @ W @ Xb_train
    XtWy = Xb_train.T @ (w[:,None] * y_train.reshape(-1,1))
    theta = np.linalg.inv(XtWX) @ XtWy
    # Predict
    y_pred[i] = np.array([1, *x_star]) @ theta

rmse = np.sqrt(np.mean((y_test - y_pred)**2))
print(f"Test RMSE (locally-weighted): {rmse:.4f}")
