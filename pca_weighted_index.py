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

    pc1_loadings = pca.components_[0]
    normalized_weights = pc1_loadings / np.sum(np.abs(pc1_loadings))

    # Compute raw PCA index
    raw_index = df[score_columns].apply(lambda row: np.dot(row, normalized_weights) if row.notnull().all() else np.nan, axis=1)

    # Min-max scale to 0–1
    min_val, max_val = raw_index.min(), raw_index.max()
    scaled_index = (raw_index - min_val) / (max_val - min_val)
    df[index_name] = scaled_index

    # Unweighted mean
    df["rent_control_unweighted"] = df[score_columns].mean(axis=1)

    # Dominant variable (driver of unweighted score)
    def find_dominant_driver(row):
        if row.isnull().any():
            return None
        values = row[score_columns].values
        max_indices = np.where(values == np.max(values))[0]
        return ', '.join([score_columns[i] for i in max_indices])

    df["rent_control_driver"] = df.apply(find_dominant_driver, axis=1)

    return df, dict(zip(score_columns, normalized_weights))