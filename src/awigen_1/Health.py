import sys
sys.path.append("../src")
from awigen_1.config import data_link

import csv
import pandas as pd
from awigen_1 import QC_Columns


class Health:
    def __init__(self):
        codeBook = QC_Columns.CodeBook()
        list_of_cols = codeBook.get_health_cols()

        data = pd.read_csv(data_link,
                           usecols=list_of_cols,
                           warn_bad_lines=True,
                           error_bad_lines=False,
                           low_memory=False,
                           quoting=csv.QUOTE_NONE)

        df = [x for _, x in data.groupby('site_qc')]

        self.agincourt = df[0]
        self.dikgale = df[1]
        self.nairobi = df[2]
        self.nanoro = df[3]
        self.navrongo = df[4]
        self.soweto = df[5]

    def get_agincourt(self):
        self.agincourt.fillna('Blank')
        return self.agincourt

    def get_dikgale(self):
        self.dikgale.fillna('Blank')
        return self.dikgale

    def get_nairobi(self):
        self.nairobi.fillna('Blank')
        return self.nairobi

    def get_nanoro(self):
        self.nanoro.fillna('Blank')
        return self.nanoro

    def get_navrongo(self):
        self.navrongo.fillna('Blank')
        return self.navrongo

    def get_soweto(self):
        self.soweto.fillna('Blank')
        return self.soweto
