import pandas as pd

data = pd.read_csv("../data/all_sites_v2.5.3.22.csv", sep=',', error_bad_lines=False)
print(data)