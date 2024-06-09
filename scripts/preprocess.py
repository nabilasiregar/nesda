import pandas as pd
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer
from sklearn.ensemble import RandomForestRegressor

class Preprocessing:
    @staticmethod
    def perform_imputation(columns, filename, random_state=42):
        """
        Static method to perform imputation on specified columns using IterativeImputer with RandomForestRegressor.
        :param columns: list, columns to perform imputation on
        :param filename: str, path to the CSV file containing the data
        """
        df = pd.read_csv(filename)
        df_filtered = df[columns]

        imputer = IterativeImputer(
            estimator=RandomForestRegressor(random_state=random_state),
            random_state=random_state
        )

        df_filtered_imputed = imputer.fit_transform(df_filtered)
        df_filtered_imputed = pd.DataFrame(df_filtered_imputed, columns=columns)
        df.loc[df.index, columns] = df_filtered_imputed

        output_filename = filename.replace('.csv', '_imputed.csv')
        df.to_csv(f"../data/preprocessed/{output_filename}", index=False)
        print(f"Imputed data saved as '{output_filename}'.")
    
    @staticmethod
    def drop_unnamed_cols(data):
        data.loc[:, ~data.columns.str.contains('^Unnamed')]
    
