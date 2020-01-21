# This file gives access to processed data and its links
# It returns the link to data and the dataframe as global variables

from pathlib import Path
import pandas as pd


#this a global variable
processed_data_dir = Path("../data/processed")

data_link = processed_data_dir/'awigen_data.csv'

awigen_1_data = pd.read_csv(data_link, sep=';', error_bad_lines=False, low_memory=False)
