import pandas as pd

data_link = "../data/all_sites_v2.5.3.22.csv"
data = pd.read_csv(data_link, sep=',', error_bad_lines=False)
print(data)