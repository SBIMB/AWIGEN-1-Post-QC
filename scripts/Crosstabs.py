import pandas as pd


class Crosstabs:
    def __init__(self, data_frame):
        self.df = data_frame  # site data frame

    def get_percentage_cross_tabs(self, list_of_cols, by_col):
        # list_of_cols is a list of categorical variables
        # by_col is the variable name which group_by is based on.
        for col in list_of_cols:
            print("_____________", col, "____________")
            g = pd.crosstab(self.df[by_col], self.df[col], margins=True, margins_name='Total', normalize='index').round(4)*100
            print(g)
            print("\n\n")

    def get_counts_cross_tabs(self, list_of_cols, by_col):
        for col in list_of_cols:
            print("_____________", col, "____________")
            g = pd.crosstab(self.df[by_col], self.df[col], margins=True, margins_name='Total')
            print(g)
            print("\n\n")

    def plot_histogram(self, columns):
        for col in columns:
                self.df[col].value_counts().plot(kind='bar')