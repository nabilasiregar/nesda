import math
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import plotly.express as px
import seaborn as sns

class DataAnalysis:
    @staticmethod
    def plot_missing_values(df):
        """Plots the number of missing values per feature in a DataFrame as a bar chart."""
        sum_missing_values = df.isnull().sum()
        total_missing_values = sum_missing_values[sum_missing_values > 0]

        plt.figure(figsize=(12, 6))
        total_missing_values.plot(kind='bar')
        plt.title('Columns with Missing Data')
        plt.xlabel('Features')
        plt.ylabel('Number of Missing Values')
        plt.xticks(rotation=45, ha='right')
        plt.show()

    @staticmethod
    def plot_missing_heatmap(df):
        """Plots an interactive heatmap of missing values in a DataFrame using Plotly."""
        missing_data = df.isnull()

        fig = px.imshow(missing_data, 
                        labels=dict(x="Features", y="Number of Missing Values", color="Missing Data"),
                        x=df.columns, 
                        color_continuous_scale='Viridis')
        
        fig.update_layout(
            title='Heatmap of Missing Data',
            xaxis_nticks=50, 
            yaxis_nticks=10 
        )
        fig.update_xaxes(tickangle=90)
        fig.show()

    @staticmethod
    def get_list_of_features(df):
        features = df.columns
        return features.tolist()
    
    @staticmethod
    def describe_missingness(df):
        missing_data = df.columns[df.isnull().any()]
        return df[missing_data].describe()
    
    @staticmethod
    def plot_distribution(df, columns):
        """
        Plots the distribution of specified columns.

        :param df: pandas DataFrame containing the filtered data.
        :param columns: list of column names to plot.
        """
        num_cols = len(columns)
        num_rows = math.ceil(num_cols / 3)
        fig, axes = plt.subplots(num_rows, 3, figsize=(15, 5 * num_rows))
        
        for i, col in enumerate(columns):
            row, col_pos = divmod(i, 3)
            ax = axes[row, col_pos] if num_rows > 1 else axes[col_pos]
            
            if df[col].dtype in ['int64', 'float64']:
                df[col].plot(kind='hist', bins=30, ax=ax, title=col)
            else:
                df[col].value_counts().plot(kind='bar', ax=ax, title=col)
            
            ax.set_xlabel(col)
            ax.set_ylabel('Frequency')

        plt.tight_layout()
        plt.show()
        
    @staticmethod
    def plot_missing_data_distribution(df):
        """Plots the distribution for each column with missing data."""
        any_missing_data = df.columns[df.isnull().any()]

        num_cols = 3  
        num_rows = (len(any_missing_data) + num_cols - 1) // num_cols  # Rounds up if not a multiple of num_cols

        fig, axes = plt.subplots(nrows=num_rows, ncols=num_cols, figsize=(15, num_rows * 4))
        axes = axes.flatten()  

        for i, col in enumerate(any_missing_data):
            ax = axes[i]
            df[col].dropna().plot(kind='hist', bins=30, ax=ax, color='blue', alpha=0.7)
            ax.set_title(f'Distribution of {col}')
            ax.set_xlabel(col)
            ax.set_ylabel('Frequency')

        for j in range(i + 1, len(axes)):
            fig.delaxes(axes[j])

        plt.tight_layout()
        plt.show()
    
    @staticmethod
    def plot_correlation_matrix(df, columns, title):
        """
        Plots a correlation matrix for specified columns in a DataFrame.

        Parameters:
        df (pd.DataFrame): The DataFrame to analyze.
        columns (list): List of columns to include in the correlation matrix.
        title (string): Title of the plot
        """
        df_filtered = df[columns]
        corr_matrix = df_filtered.corr()

        plt.figure(figsize=(10, 10))
        sns.heatmap(corr_matrix, annot=True, fmt=".2f", cmap="coolwarm", square=True,
                annot_kws={"size": 14}, cbar_kws={"shrink": 0.8})
        plt.title(f"Correlation Matrix of {title}", fontsize=20, pad=10)
        plt.xticks(rotation=45, ha='right', fontsize=14)
        plt.yticks(rotation=0, fontsize=14)
        plt.subplots_adjust(top=0.9)
        plt.tight_layout()
        plt.show()
    
    @staticmethod
    def plot_outliers(df, columns):
        """
        Plots boxplots for specified columns in a DataFrame to visualize outliers.

        Parameters:
        df (pd.DataFrame): The DataFrame containing the data.
        columns (list): List of columns to be visualized for outliers.
        """
        df_filtered = df[columns]

        plt.figure(figsize=(15, 10))
        sns.boxplot(data=df_filtered, orient='h', palette="Set2")
        plt.title('Outliers Visualization')
        plt.xlabel('Values')
        plt.ylabel('Variables')
        plt.show()
    
    @staticmethod
    def compare_distributions(first_df, first_label, second_df, second_label, columns):
        """
        Plots and compares the distributions of specified columns from two DataFrames.

        Parameters:
        first_df (pd.DataFrame): First DataFrame you want to compare.
        second_df (pd.DataFrame): Second DataFrame you want to compare.
        columns (list): List of columns to be compared, must exist in both dataframes.
        """
        fig, axes = plt.subplots(nrows=len(columns), ncols=2, figsize=(15, 5 * len(columns)))
        axes = axes.flatten()

        for i, col in enumerate(columns):
            # First data distribution
            ax = axes[2*i]
            first_df[col].dropna().plot(kind='hist', bins=30, ax=ax, color='blue', alpha=0.5, label=f"{first_label}")
            ax.set_title(f'{first_label} Distribution of {col}')
            ax.set_xlabel(col)
            ax.set_ylabel('Frequency')

            # Second data distribution
            ax = axes[2*i + 1]
            second_df[col].plot(kind='hist', bins=30, ax=ax, color='red', alpha=0.5, label=f"{second_label}")
            ax.set_title(f'{second_label} Distribution of {col}')
            ax.set_xlabel(col)
            ax.set_ylabel('Frequency')

        for j in range(2*i + 2, len(axes)):
            fig.delaxes(axes[j])

        plt.tight_layout()
        plt.show()
    