#!/usr/bin/env python3

import numpy as np
import random
import math
from collections import Counter, namedtuple

def load_and_prepare_data(path='spambase.data', seed=0):
    
    data = np.loadtxt(path, delimiter=',')
    X = data[:, :-1]
    y = data[:, -1].astype(int)

    np.random.seed(seed)
    idx = np.arange(len(X))
    np.random.shuffle(idx)
    X = X[idx]
    y = y[idx]

    n = len(X)
    train_size = int(np.ceil(2 * n / 3))
    X_train, X_test = X[:train_size], X[train_size:]
    y_train, y_test = y[:train_size], y[train_size:]

    mean = X_train.mean(axis=0)
    std  = X_train.std(axis=0, ddof=0)
    std[std == 0] = 1.0
    X_train_std = (X_train - mean) / std
    X_test_std  = (X_test  - mean) / std

    # Binarize each feature by its training median
    medians = np.median(X_train_std, axis=0)
    X_train_bin = (X_train_std  > medians).astype(int)
    X_test_bin  = (X_test_std   > medians).astype(int)

    return X_train_bin, y_train, X_test_bin, y_test

Node = namedtuple('Node', ['feature', 'left', 'right', 'label'])

def entropy(labels):
    total = len(labels)
    if total == 0:
        return 0.0
    counts = Counter(labels)
    ent = 0.0
    for count in counts.values():
        p = count / total
        ent -= p * math.log2(p)
    return ent

def information_gain(X, y, feature):
    parent_ent = entropy(y)
    # split
    mask = (X[:, feature] == 0)
    left_y  = y[mask]
    right_y = y[~mask]
    # weighted avg entropy
    w_left  = len(left_y) / len(y)
    w_right = len(right_y) / len(y)
    gain = parent_ent - (w_left * entropy(left_y) + w_right * entropy(right_y))
    return gain

def majority_label(labels):
    counts = Counter(labels)
    if counts[1] >= counts[0]:
        return 1
    else:
        return 0

def build_tree(X, y, features, rnd):
    # If all labels the same, make a leaf
    if len(set(y)) == 1:
        return Node(feature=None, left=None, right=None, label=y[0])

    # If no features left, make a leaf with majority label
    if not features:
        return Node(feature=None, left=None, right=None, label=majority_label(y))

    # Compute IG for each feature
    gains = [(information_gain(X, y, f), f) for f in features]
    max_gain = max(gains, key=lambda x: x[0])[0]
    # If no positive gain, stop and make majority leaf
    if max_gain <= 0:
        return Node(feature=None, left=None, right=None, label=majority_label(y))

    # Select all features with max_gain, then tie-break randomly
    best_feats = [f for (g, f) in gains if abs(g - max_gain) < 1e-12]
    chosen = rnd.choice(best_feats)

    # Split data on chosen feature
    mask_left  = (X[:, chosen] == 0)
    mask_right = ~mask_left

    left_node  = build_tree(X[mask_left],  y[mask_left],  [f for f in features if f != chosen], rnd)
    right_node = build_tree(X[mask_right], y[mask_right], [f for f in features if f != chosen], rnd)

    return Node(feature=chosen, left=left_node, right=right_node, label=None)

def predict_tree(node, x):
    """Traverse the tree for a single binary feature vector x."""
    if node.feature is None:
        return node.label
    if x[node.feature] == 0:
        return predict_tree(node.left, x)
    else:
        return predict_tree(node.right, x)

def compute_metrics(y_true, y_pred):
    TP = sum((y_pred[i] == 1 and y_true[i] == 1) for i in range(len(y_true)))
    TN = sum((y_pred[i] == 0 and y_true[i] == 0) for i in range(len(y_true)))
    FP = sum((y_pred[i] == 1 and y_true[i] == 0) for i in range(len(y_true)))
    FN = sum((y_pred[i] == 0 and y_true[i] == 1) for i in range(len(y_true)))

    precision = TP / (TP + FP) if (TP + FP) else 0.0
    recall    = TP / (TP + FN) if (TP + FN) else 0.0
    accuracy  = (TP + TN) / len(y_true)
    f1        = (2 * precision * recall / (precision + recall)) if (precision + recall) else 0.0

    return precision, recall, f1, accuracy

def fmt(x):
    s = f"{x:.4f}"              
    return s.rstrip("0").rstrip(".")

def main():

    X_train, y_train, X_test, y_test = load_and_prepare_data('spambase.data', seed=0)

    rnd = random.Random(0)
    features = list(range(X_train.shape[1]))
    tree = build_tree(X_train, y_train, features, rnd)

    y_pred = [predict_tree(tree, x) for x in X_test]

    precision, recall, f1, accuracy = compute_metrics(y_test, y_pred)
    
    metrics = [
        ("Precision", precision),
        ("Recall", recall),
        ("F-measure", f1),
        ("Accuracy", accuracy),
    ]

    max_name_len = max(len(name) for name, _ in metrics)

    print(f"{'Metric':<{max_name_len}} | Value")
    print(f"{'-' * max_name_len}-|{'-' * 6}")

    for name, val in metrics:
        print(f"{name:<{max_name_len}} | {fmt(val)}")

if __name__ == "__main__":
    main()
