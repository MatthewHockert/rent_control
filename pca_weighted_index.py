import pandas as pd
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
import numpy as np

def compute_pca_weighted_index(df, score_columns, index_name="rent_control_pca"):
    df_subset = df[score_columns].dropna()

    scaler = StandardScaler()
    scaled_data = scaler.fit_transform(df_subset)

    pca = PCA(n_components=1)
    pca.fit(scaled_data)

    # First principal component loadings
    pc1_loadings = pca.components_[0]
    normalized_weights = pc1_loadings / np.sum(np.abs(pc1_loadings))

    # Compute index: dot product of scores and weights
    df[index_name] = df[score_columns].apply(lambda row: np.dot(row, normalized_weights) if row.notnull().all() else None, axis=1)
    
    return df, dict(zip(score_columns, normalized_weights))