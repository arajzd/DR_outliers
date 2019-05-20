import numpy as np
import random
import pandas as pd
from sklearn.datasets import make_spd_matrix
from sklearn.model_selection import train_test_split
from sklearn.decomposition import PCA, KernelPCA, NMF
from sklearn.manifold import TSNE
from sklearn.cluster import DBSCAN, KMeans
from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import cross_val_score
from sklearn import metrics
from sklearn.preprocessing import scale
import matplotlib.pyplot as plt


def gen_gaussian_data(num_feat=30, num_samples=100, num_class=5):
    means = np.random.normal(loc = 0, scale = 2, size=(num_class, num_feat))
    cov = [make_spd_matrix(num_feat, random_state=None) for i  in range(num_class)]
    x = np.zeros((num_samples*num_class, num_feat))
    y = np.zeros(num_samples*num_class)
    for i in range(num_class):
        x[i*num_samples:(i+1)*num_samples, :] = np.random.multivariate_normal(mean=means[i], cov=cov[i], size=num_samples)
        y[i*num_samples:(i+1)*num_samples] = i

    for i in [2,3]:
        x[:, i] = x[:,i]*5*np.random.rand()

    return x, y


def add_outliers(data, labels, num_outliers=30, num_feat=2,  features=(0, 1), outlier_mult=10):
    data_copy = np.copy(data)
    labels_copy = np.copy(labels)
    sample_ind = [int(x) for x in data.shape[0]*np.random.rand(num_outliers)]
    labels_copy[sample_ind] = max(labels_copy) + 1
    features =[[features[0]]*len(sample_ind), [features[1]]*len(sample_ind)]
    feat_ind = random.sample(range(3, len(data[0])), num_feat)
    for i in range(num_feat-2):
        features.append([feat_ind[i]]*len(sample_ind))
    for i in range(len(features)):
        data_copy[sample_ind, features[i]] = data_copy[sample_ind, features[i]]*outlier_mult

    return data_copy, labels_copy


def dim_reduc(method, data, data_out, data_norm, data_norm_out):
    result = method.fit_transform(data)
    result_out = method.fit_transform(data_out)
    result_norm = method.fit_transform(data_norm)
    result_norm_out = method.fit_transform(data_norm_out)

    return [result, result_out, result_norm, result_norm_out]


def plot_acc_diff(pca, kpca, title_str, fig_num):
    plt.figure(fig_num)
    plt.plot(factors, np.transpose(pca), label='PCA')
    plt.plot(factors, np.transpose(kpca), label='KPCA sigmoid')
    plt.xlabel("Outlier percentage")
    plt.ylabel("Reduced percentage accuracy")
    plt.title(title_str)
    plt.legend()


def visualize_2d(data, labels, labels_outliers, dim_red_str):
    fig, ax = plt.subplots(nrows=len(dim_red_str), ncols=len(data[0]), figsize=(18,9))
    cmap = plt.cm.Accent
    norm = plt.Normalize(vmin=0, vmax=max(labels_outliers))

    for i in range(len(dim_red_str)):
        for j in range(len(data[0])):
            data_set = data[i][j]
            if j==0 or j==2:
                ax[i,j].scatter(data_set[:,0], data_set[:,1], c=cmap(norm((labels))))
            else:
                ax[i,j].scatter(data_set[:,0], data_set[:,1], c=cmap(norm(labels_outliers)))
            title_str = dim_red_str[i] + " with outliers" if j==1 or j==3 else dim_red_str[i]
            title_str = title_str + " normalized" if j==2 or j==3 else title_str
            ax[i,j].set_title(title_str)


pca = PCA(n_components=2, random_state=123)
kpca_sig = KernelPCA(n_components=2, kernel="sigmoid", random_state=123)
kpca_rbf = KernelPCA(n_components=2, kernel="poly", degree=2, random_state=123)
tsne = TSNE(random_state=123)

knn = KNeighborsClassifier(n_neighbors=5, leaf_size=30, p=2)

num_samples = 200
num_it = 100
factors = [0.1, 0.2, 0.3, 0.4, 0.5]

cv_scores = np.zeros((8, len(factors)))
for j in range(len(factors)):
    for i in range(num_it):
        X, y = gen_gaussian_data()
        X_outliers, y_outliers = add_outliers(X, y, num_outliers=int(factors[j]*num_samples))
        X_norm = scale(X)
        X_norm_out = scale(X_outliers)

        proj_data = [[X, X_outliers, X_norm, X_norm_out]]
        proj_data.append(dim_reduc(pca, X, X_outliers, X_norm, X_norm_out))
        proj_data.append(dim_reduc(kpca_sig, X, X_outliers, X_norm, X_norm_out))
        #proj_data.append(dim_reduc(kpca_rbf, X, X_outliers, X_norm, X_norm_out))
        #proj_data.append(dim_reduc(tsne, X, X_outliers, X_norm, X_norm_out))

        cv_scores[0,j] += np.mean(cross_val_score(knn, proj_data[1][0], y, cv=5))
        cv_scores[1,j] += np.mean(cross_val_score(knn, proj_data[1][1], y, cv=5))
        cv_scores[2,j] += np.mean(cross_val_score(knn, proj_data[1][2], y, cv=5))
        cv_scores[3,j] += np.mean(cross_val_score(knn, proj_data[1][3], y, cv=5))
        cv_scores[4,j] += np.mean(cross_val_score(knn, proj_data[2][0], y, cv=5))
        cv_scores[5,j] += np.mean(cross_val_score(knn, proj_data[2][1], y, cv=5))
        cv_scores[6,j] += np.mean(cross_val_score(knn, proj_data[2][2], y, cv=5))
        cv_scores[7,j] += np.mean(cross_val_score(knn, proj_data[2][3], y, cv=5))


dim_red_str = ["original data", "PCA", "KPCA_Sig", "KPCA_RBF", "t-SNE"]
pca_str = ["PCA", "KPCA_Sig"]
cv_scores = cv_scores/num_it

cv_diff = [cv_scores[0,:]-cv_scores[1,:], cv_scores[0,:]-cv_scores[2,:], cv_scores[0,:]-cv_scores[3,:], \
          cv_scores[4,:]-cv_scores[5,:], cv_scores[4,:]-cv_scores[6,:], cv_scores[4,:]-cv_scores[7,:]]

plt.style.use("ggplot")
plot_acc_diff(cv_diff[0], cv_diff[3], "PCA vs. KPCA accuracy when adding outliers", 0)
plot_acc_diff(cv_diff[1], cv_diff[4], "PCA vs. KPCA accuracy after normalization", 1)
plot_acc_diff(cv_diff[2], cv_diff[5], "PCA vs. KPCA accuracy after normalization when adding outliers", 2)

visualize_2d(proj_data[1:3], y, y_outliers, pca_str)

plt.show()
