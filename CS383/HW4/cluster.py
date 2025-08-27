import numpy as np

# ---------------------- Load & Standardize ----------------------
# Load dataset (9 columns: 8 features and 1 label)
data = np.loadtxt('diabetes.csv', delimiter=',')
X = data[:, 1:]  # features (columns 1-8)
Y = data[:, 0]   # labels (first column, values -1 or 1)

means = X.mean(axis=0)
stds  = X.std(axis=0)
stds[stds == 0] = 1.0  # safety
X_std = (X - means) / stds

print("Data shape:", X_std.shape)        # should be (768, 8)
print("Feature means (after std):", X_std.mean(axis=0))  # ~0
print("Feature std devs (after std):", X_std.std(axis=0))  # ~1)

# ---------------------- Core k-means (final result only) ----------------------
def myKMeans(X, Y, k, epsilon=2**-23, max_iter=1000):
    np.random.seed(0)

    N, d = X.shape
    # Randomly initialize cluster centers by selecting k random points
    initial_idx = np.random.choice(N, size=k, replace=False)
    centers = X[initial_idx].copy()

    def compute_purity(assignments, labels):
        total = len(labels)
        sum_majority = 0
        for j in range(k):
            idx = np.where(assignments == j)[0]
            if idx.size == 0:
                continue  # skip empty cluster
            cluster_labels = labels[idx]
            _, counts = np.unique(cluster_labels, return_counts=True)
            sum_majority += counts.max()  # dominant class in cluster j
        return sum_majority / total

    # Initial assignment (vectorized L2)
    X_norm = np.sum(X**2, axis=1)
    centers_norm = np.sum(centers**2, axis=1)
    dist_matrix = X_norm[:, None] + centers_norm[None, :] - 2 * X.dot(centers.T)
    cluster_assign = np.argmin(dist_matrix, axis=1)

    initial_purity = compute_purity(cluster_assign, Y)
    print(f"Iteration 0: Purity = {initial_purity:.4f}")

    initial_centers = centers.copy()
    initial_assign  = cluster_assign.copy()

    for i in range(1, max_iter+1):
        # Update cluster centers as mean of assigned points
        new_centers = np.zeros((k, d))
        for j in range(k):
            points_idx = np.where(cluster_assign == j)[0]
            if points_idx.size == 0:
                # If a cluster lost all points, reinitialize its center randomly
                rand_idx = np.random.randint(0, N)
                new_centers[j] = X[rand_idx]
            else:
                new_centers[j] = X[points_idx].mean(axis=0)

        # Check movement of centers (L1 norm of shift)
        center_shift = np.sum(np.abs(centers - new_centers))
        centers = new_centers

        # Reassign points based on updated centers
        X_norm = np.sum(X**2, axis=1)
        centers_norm = np.sum(centers**2, axis=1)
        dist_matrix = X_norm[:, None] + centers_norm[None, :] - 2 * X.dot(centers.T)
        cluster_assign = np.argmin(dist_matrix, axis=1)

        # Compute and report purity for this iteration
        purity = compute_purity(cluster_assign, Y)
        print(f"Iteration {i}: Purity = {purity:.4f}")

        if center_shift < epsilon:
            break

    final_centers = centers.copy()
    final_assign  = cluster_assign.copy()
    final_purity  = compute_purity(final_assign, Y)
    iterations    = i
    return initial_centers, initial_assign, final_centers, final_assign, final_purity, iterations

# ---------------------- Try k=2..7 and pick best by purity ----------------------
best_k = None
best_purity = -1.0
best_result = None

for k in range(2, 8):
    print(f"\nRunning k-means for k={k}...")
    init_centers, init_assign, fin_centers, fin_assign, fin_purity, iters = myKMeans(X_std, Y, k)
    print(f"Final purity for k={k} = {fin_purity:.4f} (converged in {iters} iterations)")
    if fin_purity > best_purity:
        best_purity = fin_purity
        best_k = k
        best_result = (init_centers, init_assign, fin_centers, fin_assign, fin_purity, iters)

print(f"\nBest k based on final purity: k={best_k} (purity = {best_purity:.4f})")

# ---------------------- Static plots (initial & final) ----------------------
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D  # noqa: F401

def compute_purity_external(assignments, labels, k):
    total = len(labels)
    majority = 0
    for j in range(k):
        idx = np.where(assignments == j)[0]
        if idx.size == 0:
            continue
        _, counts = np.unique(labels[idx], return_counts=True)
        majority += counts.max()
    return majority / total

def pca_for_viz(X, n_components=3):
    mean = X.mean(axis=0)
    Xc = X - mean
    U, S, Vt = np.linalg.svd(Xc, full_matrices=False)
    comps = Vt[:n_components].T  # shape (d, m)
    return mean, comps

def pca_project(points, mean, comps):
    return (points - mean) @ comps

def plot_state_3d(X, assignments, centers, title, outfile):
    """
    X: (N,d) standardized features (original space)
    assignments: length-N cluster ids
    centers: (k,d) centroids in the same space as X
    """
    d = X.shape[1]
    # Reduce to 3D for plotting (or pad if d<3)
    if d > 3:
        mean, comps = pca_for_viz(X, 3)
        X3 = pca_project(X, mean, comps)
        C3 = pca_project(centers, mean, comps)
        axis_labels = ('PC1', 'PC2', 'PC3')
    elif d == 3:
        X3, C3 = X, centers
        axis_labels = ('X1', 'X2', 'X3')
    else:
        # pad to 3D if needed
        padX = np.zeros((X.shape[0], 3 - d))
        padC = np.zeros((centers.shape[0], 3 - d))
        X3 = np.hstack([X, padX])
        C3 = np.hstack([centers, padC])
        axis_labels = ('X1', 'X2', 'X3')

    colors = ['tab:blue','tab:red','tab:green','tab:orange','tab:purple','tab:brown','tab:pink']
    k = centers.shape[0]

    fig = plt.figure(figsize=(7, 6))
    ax = fig.add_subplot(111, projection='3d')

    for j in range(k):
        idx = np.where(assignments == j)[0]
        if idx.size:
            ax.scatter(X3[idx,0], X3[idx,1], X3[idx,2],
                       marker='x', s=18, alpha=0.9, color=colors[j % len(colors)])
        ax.scatter(C3[j,0], C3[j,1], C3[j,2],
                   marker='o', s=120, edgecolor='black', linewidths=0.8,
                   color=colors[j % len(colors)])

    ax.set_xlabel(axis_labels[0]); ax.set_ylabel(axis_labels[1]); ax.set_zlabel(axis_labels[2])
    ax.set_title(title)
    plt.tight_layout()
    plt.savefig(outfile, dpi=150)
    plt.show()

# --- Render and save plots for the best k ---
init_centers, init_assign, fin_centers, fin_assign, fin_purity, iters = best_result

# Purities for plot titles
init_purity = compute_purity_external(init_assign, Y, best_k)  # initial (before updates)
final_purity = fin_purity

# Filenames
initial_png = f"kmeans_k{best_k}_initial.png"
final_png   = f"kmeans_k{best_k}_final.png"

# Plots
plot_state_3d(
    X_std, init_assign, init_centers,
    title=f"Iteration 0 Purity={init_purity:.5f} (k={best_k})",
    outfile=initial_png
)

plot_state_3d(
    X_std, fin_assign, fin_centers,
    title=f"Iteration {iters} Purity={final_purity:.5f} (k={best_k})",
    outfile=final_png
)

print("Saved figures:", initial_png, "and", final_png)

# ====================== Video ======================

print("Generating videos. Please don't Kill the process. It should take less than a minute")

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D  # noqa: F401
from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
import cv2

def _pca_fit(X, n_components=3):
    mu = X.mean(axis=0)
    Xc = X - mu
    U, S, Vt = np.linalg.svd(Xc, full_matrices=False)
    return mu, Vt[:n_components].T

def _pca_transform(X, mu, comps):
    return (X - mu) @ comps

def myKMeans_with_states(X, Y, k, epsilon=2**-23, max_iter=1000, seed=0):
    np.random.seed(seed)
    N, d = X.shape
    centers = X[np.random.choice(N, size=k, replace=False)].copy()

    def _purity(assignments, labels):
        total = len(labels); maj = 0
        for j in range(k):
            idx = np.where(assignments == j)[0]
            if idx.size == 0: continue
            _, counts = np.unique(labels[idx], return_counts=True)
            maj += counts.max()
        return maj / total

    Xn = np.sum(X**2, axis=1)
    Cn = np.sum(centers**2, axis=1)
    dist = Xn[:, None] + Cn[None, :] - 2 * X.dot(centers.T)
    assign = np.argmin(dist, axis=1)

    states = [(centers.copy(), assign.copy(), 0, _purity(assign, Y))]
    for it in range(1, max_iter+1):
        new_centers = np.zeros_like(centers)
        for j in range(k):
            idx = np.where(assign == j)[0]
            new_centers[j] = X[idx].mean(axis=0) if idx.size else X[np.random.randint(0, N)]
        shift = np.sum(np.abs(centers - new_centers))
        centers = new_centers

        Cn = np.sum(centers**2, axis=1)
        dist = Xn[:, None] + Cn[None, :] - 2 * X.dot(centers.T)
        assign = np.argmin(dist, axis=1)
        states.append((centers.copy(), assign.copy(), it, _purity(assign, Y)))

        if shift < epsilon:
            break
    return states

# ---------- reuse best_k from earlier; compute states only once ----------
best_states = myKMeans_with_states(X_std, Y, best_k)

# ---------- projection (2D if d<=2, else 3D via PCA) ----------
d = X_std.shape[1]
if d > 3:
    mu, comps = _pca_fit(X_std, 3)
    X_plot = _pca_transform(X_std, mu, comps)
    def _proj(C): return _pca_transform(C, mu, comps)
    axis_labels = ('PC1','PC2','PC3')
elif d == 3:
    X_plot = X_std
    def _proj(C): return C
    axis_labels = ('X1','X2','X3')
else:
    X_plot = X_std[:, :2]
    def _proj(C): return C[:, :2]
    axis_labels = ('X1','X2')

FPS = 15
TWEEN_FRAMES = 4
FINAL_HOLD_SEC = 1.0
ROT_DEG_PER_FRAME = 2.0
POINT_SUBSAMPLE = 2
RECOMPUTE_TWEEN_ASSIGN = False

colors = ['tab:blue','tab:red','tab:green','tab:orange','tab:purple','tab:brown','tab:pink']

Xn_full = np.sum(X_std**2, axis=1)
def _assign_to_centers(centers):
    Cn = np.sum(centers**2, axis=1)
    dist = Xn_full[:, None] + Cn[None, :] - 2 * X_std.dot(centers.T)
    return np.argmin(dist, axis=1)

def _render_frame(C, title, A=None, fig_w=800, fig_h=600, dpi=100, azim=45, elev=22):
    Cp = _proj(C)
    k = Cp.shape[0]
    fig = plt.figure(figsize=(fig_w/dpi, fig_h/dpi), dpi=dpi)
    if X_plot.shape[1] == 3:
        ax = fig.add_subplot(111, projection='3d')
        for j in range(k):
            if A is not None:
                idx = np.where(A == j)[0]
                if idx.size:
                    if POINT_SUBSAMPLE > 1: idx = idx[::POINT_SUBSAMPLE]
                    ax.scatter(X_plot[idx,0], X_plot[idx,1], X_plot[idx,2],
                               marker='x', s=12, alpha=0.9, color=colors[j % len(colors)])
            ax.scatter(Cp[j,0], Cp[j,1], Cp[j,2],
                       marker='o', s=80, edgecolor='black', linewidths=0.8,
                       color=colors[j % len(colors)])
        ax.view_init(elev=elev, azim=azim)
        ax.set_xlabel(axis_labels[0]); ax.set_ylabel(axis_labels[1]); ax.set_zlabel(axis_labels[2])
    else:
        ax = fig.add_subplot(111)
        for j in range(k):
            if A is not None:
                idx = np.where(A == j)[0]
                if idx.size:
                    if POINT_SUBSAMPLE > 1: idx = idx[::POINT_SUBSAMPLE]
                    ax.scatter(X_plot[idx,0], X_plot[idx,1],
                               marker='x', s=12, alpha=0.9, color=colors[j % len(colors)])
            ax.scatter(Cp[j,0], Cp[j,1],
                       marker='o', s=80, edgecolor='black', linewidths=0.8,
                       color=colors[j % len(colors)])
        ax.set_xlabel(axis_labels[0]); ax.set_ylabel(axis_labels[1]); ax.grid(True)

    plt.title(title)
    canvas = FigureCanvas(fig)
    canvas.draw()
    buf = np.asarray(canvas.buffer_rgba())
    img_bgr = cv2.cvtColor(buf, cv2.COLOR_RGBA2BGR)
    plt.close(fig)
    return img_bgr

video_path = "K_4.mp4"

C0, A0, it0, p0 = best_states[0]
sample = _render_frame(C0, f"Iteration {it0}  Purity={p0:.5f}  (k={best_k})", A0)
H, W = sample.shape[:2]
writer = cv2.VideoWriter(video_path, cv2.VideoWriter_fourcc(*'mp4v'), FPS, (W, H))

az = 45.0

for idx in range(len(best_states)-1):
    C_a, A_a, it_a, p_a = best_states[idx]
    C_b, A_b, it_b, p_b = best_states[idx+1]

    frame_a = _render_frame(C_a, f"Iteration {it_a}  Purity={p_a:.5f}  (k={best_k})", A_a, fig_w=W, fig_h=H, azim=az)
    writer.write(frame_a); az += ROT_DEG_PER_FRAME

    for s in range(1, TWEEN_FRAMES+1):
        alpha = s/(TWEEN_FRAMES+1)
        C_t = (1.0 - alpha)*C_a + alpha*C_b
        if RECOMPUTE_TWEEN_ASSIGN:
            A_t = _assign_to_centers(C_t)
            title = f"Iter {it_a}→{it_b}  t={alpha:.2f}  (k={best_k})"
        else:
            A_t = A_a
            title = f"Iter {it_a}→{it_b}  t={alpha:.2f}  (fast)  (k={best_k})"
        frame_t = _render_frame(C_t, title, A_t, fig_w=W, fig_h=H, azim=az)
        writer.write(frame_t); az += ROT_DEG_PER_FRAME

C_z, A_z, it_z, p_z = best_states[-1]
frame_z = _render_frame(C_z, f"Iteration {it_z}  Purity={p_z:.5f}  (k={best_k})", A_z, fig_w=W, fig_h=H, azim=az)
writer.write(frame_z); az += ROT_DEG_PER_FRAME

for _ in range(int(FINAL_HOLD_SEC*FPS)):
    frame_hold = _render_frame(C_z, f"Iteration {it_z}  Purity={p_z:.5f}  (k={best_k})", A_z, fig_w=W, fig_h=H, azim=az)
    writer.write(frame_hold); az += ROT_DEG_PER_FRAME

writer.release()
print(f"Saved video to: {video_path}")

