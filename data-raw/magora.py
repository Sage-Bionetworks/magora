from sqlalchemy import create_engine
import pandas as pd
import synapseclient
from pathlib import Path

# reads the csv in ./gene_expressions and returns a list of tuples
def create_file_list(csv_path):
    files = pd.read_csv(csv_path)
    return list(zip(files['id'], files['version']))

# returns the path of each csv
def get_csv(syn, csv):
    return syn.get(csv[0], version=csv[1])['path']

# creates a data frame from the cached csv
def create_data_frame(entity_path):
    return pd.read_csv(entity_path)

# inserts a dataframe into a local database
def insert_data_frame(data_frame, sql_engine):
    data_frame.to_sql('rna_seq_res', sql_engine, if_exists='append')

# any transformations to the data would go in here
def transform_data_frame(data_frame):
    sex = {"Female": 0, "Male": 1}
    data_frame.replace({"Sex": sex}, inplace=True)


def main():
    syn = synapseclient.Synapse()
    syn.login()

    relative_path = Path(__file__).parent / "gene_expressions/data_sources.csv"

    files = create_file_list(relative_path)
    files = [get_csv(syn=syn, csv=file) for file in files] # files is now a list of file locations in the cache

    engine = create_engine('sqlite:///magora.db', echo = True)

    for file in files:
        df = create_data_frame(entity_path=file)
        transform_data_frame(data_frame = df)
        insert_data_frame(data_frame=df, sql_engine=engine)

if __name__ == "__main__":
    main()