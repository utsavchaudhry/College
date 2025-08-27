import numpy as np
import matplotlib.pyplot as plt

from sklearn.datasets import fetch_lfw_people
from sklearn.model_selection import train_test_split

def fit_standardizer(X):
    mu = X.mean(axis=0)
    sd = X.std(axis=0, ddof=0)
    sd[sd == 0] = 1.0
    return mu, sd

def apply_standardizer(X, mu, sd):
    return (X - mu) / sd

def pca_svd_fit(Xc, n_components=None):
    """
    PCA via SVD on a centered matrix Xc (n x d).
    Returns (components, explained_var, explained_var_ratio, mean_used).
    components: shape (k, d), rows are principal axes.
    """
    mean_used = Xc.mean(axis=0)
    Xc = Xc - mean_used

    U, S, Vt = np.linalg.svd(Xc, full_matrices=False)
    n = Xc.shape[0]
    var = (S**2) / (n - 1)
    total_var = var.sum()
    if n_components is None or n_components > Vt.shape[0]:
        n_components = Vt.shape[0]
    components = Vt[:n_components, :]
    explained_var = var[:n_components]
    explained_ratio = explained_var / total_var
    return components, explained_var, explained_ratio, mean_used

def pca_project(X, mean_used, components):
    Xc = X - mean_used
    return Xc @ components.T  # scores

def pca_reconstruct(scores, mean_used, components):
    # scores: (..., k)
    return scores @ components + mean_used

# ------------- pipeline -------------
def main():

    people = fetch_lfw_people(min_faces_per_person=20, resize=0.7)
    X_all = people.data.astype(np.float64) / 255.0
    y_all = people.target
    images_all = people.images  # for shape only: (87, 65)

    mask = np.zeros_like(y_all, dtype=bool)
    for t in np.unique(y_all):
        mask[np.where(y_all == t)[0][:50]] = True

    X = X_all[mask]
    y = y_all[mask]

    Xtr, Xte, ytr, yte = train_test_split(X, y, test_size=0.25, random_state=0, stratify=y)
    mu_pix, sd_pix = fit_standardizer(Xtr)
    Xtr_std = apply_standardizer(Xtr, mu_pix, sd_pix)
    Xte_std = apply_standardizer(Xte, mu_pix, sd_pix)

    comps, expl_var, expl_ratio, mean_used = pca_svd_fit(Xtr_std, n_components=None)

    # Extremes along PC1 and PC2 (on TRAIN set)
    Ztr = pca_project(Xtr_std, mean_used, comps)
    pc1 = Ztr[:, 0]
    pc2 = Ztr[:, 1]
    i_pc1_min, i_pc1_max = np.argmin(pc1), np.argmax(pc1)
    i_pc2_min, i_pc2_max = np.argmin(pc2), np.argmax(pc2)

    names = people.target_names
    print("PC1 min  -> idx={}, name={}, score={:.3f}".format(i_pc1_min, names[ytr[i_pc1_min]], pc1[i_pc1_min]))
    print("PC1 max  -> idx={}, name={}, score={:.3f}".format(i_pc1_max, names[ytr[i_pc1_max]], pc1[i_pc1_max]))
    print("PC2 min  -> idx={}, name={}, score={:.3f}".format(i_pc2_min, names[ytr[i_pc2_min]], pc2[i_pc2_min]))
    print("PC2 max  -> idx={}, name={}, score={:.3f}".format(i_pc2_max, names[ytr[i_pc2_max]], pc2[i_pc2_max]))

    # Visualize the primary eigenface (PC1, as 87x65)
    h, w = images_all.shape[1], images_all.shape[2]  # 87, 65
    eigenface1 = comps[0].reshape(h, w)
    plt.figure(figsize=(4, 5))
    plt.imshow(eigenface1, cmap="gray")
    plt.title("Primary Principal Component (Eigenface 1)")
    plt.axis("off")
    plt.tight_layout()
    plt.savefig("eigenface_pc1.png", dpi=150)

    # Reconstruct X_train[0] using ONLY PC1, then unstandardize
    x0_std = Xtr_std[0]     # standardized
    z0 = pca_project(x0_std[None, :], mean_used, comps)[0]  # all scores
    x0_hat1_std = pca_reconstruct(z0[:1][None, :], mean_used, comps[:1, :])[0]
    x0_hat1 = x0_hat1_std * sd_pix + mu_pix
    x0_hat1 = np.clip(x0_hat1, 0.0, 1.0)

    plt.figure(figsize=(4, 5))
    plt.imshow(x0_hat1.reshape(h, w), cmap="gray")
    plt.title("Reconstruction with PC1 only")
    plt.axis("off")
    plt.tight_layout()
    plt.savefig("recon_pc1.png", dpi=150)

    # Choose k s.t. cumulative explained variance >= 95%
    cum = np.cumsum(expl_ratio)
    k95 = int(np.searchsorted(cum, 0.95) + 1)
    print(f"k (>=95% variance): {k95}")

    # Reconstruct with top-k components; unstandardize
    x0_hatk_std = pca_reconstruct(z0[:k95][None, :], mean_used, comps[:k95, :])[0]
    x0_hatk = x0_hatk_std * sd_pix + mu_pix
    x0_hatk = np.clip(x0_hatk, 0.0, 1.0)

    plt.figure(figsize=(4, 5))
    plt.imshow(x0_hatk.reshape(h, w), cmap="gray")
    plt.title(f"Reconstruction with top-{k95} PCs (>=95%)")
    plt.axis("off")
    plt.tight_layout()
    plt.savefig("recon_topk.png", dpi=150)

    # Visualize the four extreme faces on PC1/PC2
    figs = [
        ("PC1 min", Xtr[i_pc1_min]), ("PC1 max", Xtr[i_pc1_max]),
        ("PC2 min", Xtr[i_pc2_min]), ("PC2 max", Xtr[i_pc2_max]),
    ]
    plt.figure(figsize=(8, 8))
    for j, (lab, img_flat) in enumerate(figs, 1):
        ax = plt.subplot(2, 2, j)
        ax.imshow(img_flat.reshape(h, w), cmap="gray")
        ax.set_title(lab)
        ax.axis("off")
    plt.tight_layout()
    plt.savefig("pc_extremes.png", dpi=150)

    print("Saved: eigenface_pc1.png, recon_pc1.png, recon_topk.png, pc_extremes.png")

if __name__ == "__main__":
    main()
