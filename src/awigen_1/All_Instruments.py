import csv
import pandas as pd
from scripts import QC_Columns


class All:
    def __init__(self):
        codeBook = QC_Columns.CodeBook()
        data_link = codeBook.get_data_link()

        self.records = pd.read_csv(data_link, low_memory=False)
        self.data = self.records[self.records.site != 0]

        df = [x for _, x in self.data.groupby('site_qc')]

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

    def get_all_sites(self):
        return self.data

    def get_shape(self):
        print(self.get_all_sites().shape)

all = All()
all.get_shape()