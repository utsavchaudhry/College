import numpy as np

data = np.loadtxt("spambase.data", delimiter=",")
np.random.seed(0)
np.random.shuffle(data)

X = data[:, :-1]
y = data[:, -1]

N = len(X)
train_size = int(np.ceil(2*N/3))
X_train, X_test = X[:train_size], X[train_size:]
y_train, y_test = y[:train_size], y[train_size:]

feat_means = X_train.mean(axis=0)
feat_stds = X_train.std(axis=0, ddof=0)
feat_stds[feat_stds == 0] = 1e-6 # Avoid division by zero

X_train_std = (X_train - feat_means) / feat_stds
X_test_std  = (X_test  - feat_means) / feat_stds

X_train_spam = X_train_std[y_train == 1]
X_train_ham  = X_train_std[y_train == 0]

# Compute Gaussian parameters for each feature in each class
mu_spam = X_train_spam.mean(axis=0)
sd_spam = X_train_spam.std(axis=0, ddof=0)
mu_ham  = X_train_ham.mean(axis=0)
sd_ham  = X_train_ham.std(axis=0, ddof=0)
sd_spam[sd_spam == 0] = 1e-6
sd_ham[sd_ham == 0] = 1e-6

# priors
p_spam = len(X_train_spam) / train_size
p_ham  = len(X_train_ham)  / train_size

eps = 1e-12
y_pred = []
for x in X_test_std:
    # spam
    loglik_spam = np.log(p_spam + eps)
    loglik_spam += np.sum(
        -0.5 * np.log(2*np.pi*sd_spam**2) 
        - 0.5 * ((x - mu_spam)**2 / (sd_spam**2) + eps)
    )
    # not spam
    loglik_ham = np.log(p_ham + eps)
    loglik_ham += np.sum(
        -0.5 * np.log(2*np.pi*sd_ham**2) 
        - 0.5 * ((x - mu_ham)**2 / (sd_ham**2) + eps)
    )
    y_pred.append(1 if loglik_spam > loglik_ham else 0)
y_pred = np.array(y_pred)

TP = np.sum((y_pred == 1) & (y_test == 1))
TN = np.sum((y_pred == 0) & (y_test == 0))
FP = np.sum((y_pred == 1) & (y_test == 0))
FN = np.sum((y_pred == 0) & (y_test == 1))

precision = TP / (TP + FP) if (TP + FP) > 0 else 0.0
recall    = TP / (TP + FN) if (TP + FN) > 0 else 0.0
accuracy  = (TP + TN) / y_test.shape[0] if y_test.shape[0] > 0 else 0.0
f_measure = 2 * precision * recall / (precision + recall) if (precision + recall) > 0 else 0.0

metrics = [
    ("Precision", precision),
    ("Recall", recall),
    ("F-measure", f_measure),
    ("Accuracy", accuracy),
]

def fmt(x):
    s = f"{x:.4f}"              
    return s.rstrip("0").rstrip(".")

max_name_len = max(len(name) for name, _ in metrics)

print(f"{'Metric':<{max_name_len}} | Value")
print(f"{'-' * max_name_len}-|{'-' * 6}")

for name, val in metrics:
    print(f"{name:<{max_name_len}} | {fmt(val)}")


