import numpy as np
from sklearn.linear_model import LinearRegression

x = np.array([-2, -5, -3,  0, -8, -2,  1,  5, -1,  6]).reshape(-1, 1)
y = np.array([ 1, -4,  1,  3, 11,  5,  0, -1, -3,  1])

model = LinearRegression()       
model.fit(x, y)                  

print(f"Intercept: {model.intercept_:.4f}")
print(f"Slope    : {model.coef_[0]:.4f}")
