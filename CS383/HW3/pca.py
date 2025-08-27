import numpy as np
import matplotlib.pyplot as plt

from sklearn.datasets import fetch_lfw_people
from sklearn.model_selection import train_test_split
from sklearn.neighbors import KNeighborsClassifier

def fit_standardizer(X):
    mean = X.mean(axis=0)
    std = X.std(axis=0, ddof=0)
    std[std == 0] = 1.0  # avoid division by zero
    return mean, std

def apply_standardizer(X, mean, std):
    return (X - mean) / std

class PCA_SVD:
    """
    PCA implemented with SVD on the centered data matrix.
    Optionally performs whitening (unit variance in projected space).
    """
    def __init__(self, n_components, whiten=False):
        self.n_components = n_components
        self.whiten = whiten
        self.mean_ = None
        self.components_ = None
        self.explained_variance_ = None
        self._whitening_factors = None

    def fit(self, X):

        self.mean_ = X.mean(axis=0)
        Xc = X - self.mean_

        U, S, Vt = np.linalg.svd(Xc, full_matrices=False)
        
        n_samples = X.shape[0]
        explained_var = (S**2) / (n_samples - 1)

        k = self.n_components
        self.components_ = Vt[:k, :]
        self.explained_variance_ = explained_var[:k]

        if self.whiten:
            self._whitening_factors = 1.0 / np.sqrt(self.explained_variance_ + 1e-12)
        else:
            self._whitening_factors = np.ones_like(self.explained_variance_)
        return self

    def transform(self, X):
        Xc = X - self.mean_
        Z = Xc @ self.components_.T
        Z = Z * self._whitening_factors
        return Z

    def fit_transform(self, X):
        self.fit(X)
        return self.transform(X)

# --------------------------------------------
# 1-NN (k=1) using SSD (squared Euclidean) only
# --------------------------------------------
def knn1_predict_ssd(X_train, y_train, X_test, batch=256):
    """
    Predict labels for X_test using 1-NN with squared Euclidean distance.
    Uses the ||a-b||^2 = ||a||^2 + ||b||^2 - 2 a·b trick in batches for speed/memory.
    """
    y_pred = np.empty(X_test.shape[0], dtype=y_train.dtype)
    train_norm2 = np.sum(X_train**2, axis=1)  # (n_train,)

    for start in range(0, X_test.shape[0], batch):
        end = min(start + batch, X_test.shape[0])
        B = X_test[start:end]                             # (m, d)
        B_norm2 = np.sum(B**2, axis=1, keepdims=True)     # (m, 1)
        # distances^2 = ||B||^2 + ||train||^2 - 2 * B @ train^T
        d2 = B_norm2 + train_norm2[None, :] - 2.0 * (B @ X_train.T)  # (m, n_train)
        nn_idx = np.argmin(d2, axis=1)
        y_pred[start:end] = y_train[nn_idx]
    return y_pred

def accuracy(y_true, y_pred):
    return (y_true == y_pred).mean()

# ---------------------------
# Pipeline
# ---------------------------
def main():

    people = fetch_lfw_people(min_faces_per_person=20, resize=0.7)  # images: 87x65
    X_all = people.data  # shape (N, 87*65)
    y_all = people.target

    # Mask: up to 50 per unique target
    mask = np.zeros_like(y_all, dtype=bool)
    for t in np.unique(y_all):
        idx = np.where(y_all == t)[0][:50]
        mask[idx] = True
    X = X_all[mask].astype(np.float64) / 255.0  # scale to [0,1]
    y = y_all[mask]

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.25, random_state=0, stratify=y
    )

    # Baseline: sklearn 1-NN on raw pixels
    knn = KNeighborsClassifier(n_neighbors=1, metric="euclidean")
    knn.fit(X_train, y_train)
    base_acc = knn.score(X_test, y_test)
    print(f"[Baseline] 1-NN on raw pixels: accuracy = {base_acc:.3f}")

    mean, std = fit_standardizer(X_train)
    Xtr_std = apply_standardizer(X_train, mean, std)
    Xte_std = apply_standardizer(X_test, mean, std)

    # PCA to 100D (no whitening)
    pca100 = PCA_SVD(n_components=100, whiten=False).fit(Xtr_std)
    Xtr_100 = pca100.transform(Xtr_std)
    Xte_100 = pca100.transform(Xte_std)

    # 1-NN (SSD) on 100D (no whitening)
    y_pred_100 = knn1_predict_ssd(Xtr_100, y_train, Xte_100, batch=512)
    acc_100 = accuracy(y_test, y_pred_100)
    print(f"1-NN on 100D PCA:          accuracy = {acc_100:.3f}")

    # PCA to 100D with whitening
    pca100w = PCA_SVD(n_components=100, whiten=True).fit(Xtr_std)
    Xtr_100w = pca100w.transform(Xtr_std)
    Xte_100w = pca100w.transform(Xte_std)

    y_pred_100w = knn1_predict_ssd(Xtr_100w, y_train, Xte_100w, batch=512)
    acc_100w = accuracy(y_test, y_pred_100w)
    print(f"1-NN on 100D PCA (whitened): accuracy = {acc_100w:.3f}")

    # PCA to 2D for visualization
    #    (Visual-only step; not part of training/eval.)
    mean_all, std_all = fit_standardizer(X)
    X_std_all = apply_standardizer(X, mean_all, std_all)
    pca2 = PCA_SVD(n_components=2, whiten=False).fit(X_std_all)
    X_2d = pca2.transform(X_std_all)

    # Plot
    plt.figure(figsize=(9, 6))
    plt.scatter(X_2d[:, 0], X_2d[:, 1], s=10, alpha=0.6, label="faces")
    plt.legend()
    plt.title("2D PCA Projection of LFW")
    plt.xlabel("PC1")
    plt.ylabel("PC2")
    plt.tight_layout()
    plt.savefig("pca_2d_scatter.png", dpi=150)
    print("Saved 2D scatter to pca_2d_scatter.png")

if __name__ == "__main__":
    main()
