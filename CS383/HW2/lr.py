import numpy as np

data = np.loadtxt('spambase.data', delimiter=',')

X = data[:, :-1]  # features
y = data[:, -1].astype(int)  # label (0 or 1)

np.random.seed(0)
indices = np.arange(X.shape[0])
np.random.shuffle(indices)
X = X[indices]
y = y[indices]

train_size = int(np.ceil(2/3 * X.shape[0]))
X_train, X_test = X[:train_size], X[train_size:]
y_train, y_test = y[:train_size], y[train_size:]

mean = X_train.mean(axis=0)
std = X_train.std(axis=0)
std[std == 0] = 1.0 # Avoid division by zero
# Standardize
X_train = (X_train - mean) / std
X_test = (X_test - mean) / std


X_train_bias = np.hstack([X_train, np.ones((X_train.shape[0], 1))])
X_test_bias  = np.hstack([X_test,  np.ones((X_test.shape[0], 1))])
theta = np.random.uniform(-1.0, 1.0, size=X_train_bias.shape[1])

def sigmoid(z):
    return 1.0 / (1.0 + np.exp(-z))

def compute_loss(Xb, y, theta):
    z = Xb.dot(theta)
    preds = sigmoid(z)
    eps = 1e-15 # avoid log(0)
    preds = np.clip(preds, eps, 1 - eps)
    loss = -np.mean(y * np.log(preds) + (1 - y) * np.log(1 - preds))
    return loss

# Batch Gradient Descent
learning_rate = 0.01
max_iterations = 1500
threshold = 2**-23  # convergence threshold for change in loss

prev_loss = compute_loss(X_train_bias, y_train, theta)
for iteration in range(max_iterations):
    predictions = sigmoid(X_train_bias.dot(theta))
    gradient = (X_train_bias.T.dot(predictions - y_train)) / X_train_bias.shape[0]
    theta = theta - learning_rate * gradient
    current_loss = compute_loss(X_train_bias, y_train, theta)
    if abs(prev_loss - current_loss) < threshold:
        break
    prev_loss = current_loss

test_prob = sigmoid(X_test_bias.dot(theta))
y_pred = (test_prob >= 0.5).astype(int)

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

