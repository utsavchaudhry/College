#!/usr/bin/env python3
import numpy as np
import pandas as pd

def load_data(path):
    df = pd.read_csv(path, header=0, index_col=False)            
    data = df.values[:, 1:]                                      
    return data  

def split_train_test(data, seed=0):
    np.random.seed(seed)                                         
    np.random.shuffle(data)                                      
    N = data.shape[0]
    n_train = int(np.ceil(2/3 * N))                              
    train, test = data[:n_train], data[n_train:]
    return train, test

def standardize(train_X, test_X):
    mu = train_X.mean(axis=0)                                    
    sigma = train_X.std(axis=0)                                  
    train_std = (train_X - mu) / sigma
    test_std  = (test_X  - mu) / sigma
    return train_std, test_std

def fit_closed_form(X, y):
    # add bias column of ones
    ones = np.ones((X.shape[0], 1))                              
    Xb = np.hstack((ones, X))                                    
    # (X^T X)^{-1} X^T y
    XtX = Xb.T @ Xb                                              
    Xty = Xb.T @ y                                               
    theta = np.linalg.inv(XtX) @ Xty                             
    return theta

def predict(X, theta):
    ones = np.ones((X.shape[0], 1))
    Xb = np.hstack((ones, X))
    return Xb @ theta                                           

def rmse(y_true, y_pred):
    err = y_true - y_pred
    return np.sqrt(np.mean(err**2))                              

def k_fold_cv(data, k=5):
    np.random.seed(0)
    np.random.shuffle(data)
    N = data.shape[0]
    folds = np.array_split(data, k)
    rmses = []
    for i in range(k):
        val = folds[i]
        train = np.vstack([f for j,f in enumerate(folds) if j!=i])
        X_train, y_train = train[:, :-1], train[:, -1].reshape(-1,1)
        X_val,   y_val   = val[:, :-1],   val[:, -1].reshape(-1,1)
        # Standardize using training
        X_t_std, X_v_std = standardize(X_train, X_val)
        theta = fit_closed_form(X_t_std, y_train)
        y_pred = predict(X_v_std, theta)
        rmses.append(rmse(y_val, y_pred))
    return np.array(rmses)

def main():
    data = load_data("x06Simple.csv")
    
    train, test = split_train_test(data, seed=0)
    X_train, y_train = train[:, :-1], train[:, -1].reshape(-1,1)
    X_test,  y_test  = test[:, :-1],  test[:, -1].reshape(-1,1)
    
    X_tr_std, X_te_std = standardize(X_train, X_test)
    
    theta = fit_closed_form(X_tr_std, y_train)
    print("Model coefficients (θ):", theta.flatten())
    
    y_pred = predict(X_te_std, theta)
    
    test_rmse = rmse(y_test, y_pred)
    print(f"Test RMSE: {test_rmse:.4f}")
    
    cv_rmses = k_fold_cv(data, k=5)                              
    print(f"Cross-Validation RMSEs: {cv_rmses}")
    print(f"Mean Cross-Validation RMSE: {cv_rmses.mean():.4f}")

if __name__ == "__main__":
    main()
