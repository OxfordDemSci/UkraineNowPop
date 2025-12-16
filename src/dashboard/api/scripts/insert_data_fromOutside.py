from pathlib import Path
import os
import sys
from sqlalchemy import create_engine, inspect
import psycopg2
import pandas as pd
import dask
dask.config.set({'dataframe.query-planning': True})
import csv 
import psutil

#BASE_DIR = Path("C:/Users/EdithD/Documents/git/UkraineNowPop/src/dashboard/api/")
BASE_DIR = Path(__file__).resolve().parent.parent  # API route
sys.path.append(str(BASE_DIR))  # API route

from app.models import Migration, Population
from sqlalchemy.orm import sessionmaker
import logging


ENV = BASE_DIR.parent.parent.parent.joinpath('.env')
GPKG = BASE_DIR.joinpath("app", "data", "db-data", "GEODATA.gpkg")


POSTGRES_USER = os.environ.get("POSTGRES_USER")
POSTGRES_PASSWORD = os.environ.get("POSTGRES_PASSWORD")
POSTGRES_DB = os.environ.get("POSTGRES_DB")
TABLES_DIR = os.environ.get("DATABASE_TABLES_DIR")
if psutil.virtual_memory().total / (1024**3) < 4:
    CHUNK_SIZE = 2000
else:
    CHUNK_SIZE = 20000

def timer_and_log(func):
    def wrapper(*args, **kwargs):
        import time
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        print(f"Time taken to run {func.__name__}: {end - start} seconds")
        logging.info(f"Time taken to run {func.__name__}: {end - start} seconds")
        return result
    return wrapper


conn = psycopg2.connect(
    database=POSTGRES_DB,
    user=POSTGRES_USER,
    password=POSTGRES_PASSWORD,
    host="15.188.235.225",
    port="5432",
)
engine = create_engine(
    f"postgresql+psycopg2://{POSTGRES_USER}:{POSTGRES_PASSWORD}@15.188.235.225:5432/{POSTGRES_DB}"
)
pg_host = "now_pop_postgres"

@timer_and_log
def add_migration_data(overwrite_existing: bool = False):
    Session = sessionmaker(bind=engine)
    session = Session()
    dummy_migration = GPKG.parent.joinpath("migration_full.csv")
    if not dummy_migration.exists():
        dummy.main_migration(dummy_migration, [1])
    query = session.query(Migration).first()
    if query and overwrite_existing:
        session.query(Migration).delete(synchronize_session=False)
    if not query or overwrite_existing:
        for chunk in pd.read_csv(dummy_migration, chunksize=CHUNK_SIZE):
            chunk['day'] = pd.to_datetime(chunk['day'], dayfirst=True)
            chunk = chunk.astype({
                "country": "string",
                "admin_level": "int8",
                "origin": "string",
                "destination": "string",
                "age_min": "int8",
                "age_max": "int16",
                "sex": "int8",
                "probability": "float32",
                "count": "int32",
            })
            data = []
            for _, row in chunk.iterrows():
                data.append({
                    "country": row["country"],
                    "admin_level": row["admin_level"],
                    "origin": row["origin"],
                    "destination": row["destination"],
                    "day": row["day"],
                    "age_min": row["age_min"],
                    "age_max": row["age_max"],
                    "sex": row["sex"],
                    "probability": row["probability"],
                    "count": row["count"]
                })
            session.bulk_insert_mappings(Migration, data)
    session.commit()

@timer_and_log
def add_pop_data(overwrite_existing: bool = True):
    def parse_pop_posterior(s):
        parts = s.replace('[', '').replace(']', '').split(',')
        return [int(part) for part in parts]
    Session = sessionmaker(bind=engine)
    session = Session()
    dummy_pop = GPKG.parent.joinpath("pop_full.csv")
    if not dummy_pop.exists():
        dummy.main_pop(dummy_pop)
    query = session.query(Population).first()
    if query and overwrite_existing:
        session.query(Population).delete(synchronize_session=False)
    if not query or overwrite_existing:
        for chunk in pd.read_csv(dummy_pop, chunksize=CHUNK_SIZE, encoding='utf-8', quoting=csv.QUOTE_ALL):
            chunk["sex"] = chunk["sex"].replace({"male": 1, "female": 2})
            chunk['day'] = pd.to_datetime(chunk['day'], yearfirst=True)
            chunk = chunk.astype({
                "country": "string",
                "admin_level": "int8",
                "pcode": "string",
                "age_min": "int8",
                "age_max": "int16",
                "pop": "int32",
                "pop_upper": "int32",
                "pop_lower": "int32",
                "sex": "int8",
            })
            chunk['pop_posterior'] = chunk['pop_posterior'].apply(parse_pop_posterior)

            data = []
            for _, row in chunk.iterrows():
                data.append({
                    "country": row["country"],
                    "admin_level": row["admin_level"],
                    "pcode": row["pcode"],
                    "day": row["day"],
                    "age_min": row["age_min"],
                    "age_max": row["age_max"],
                    "pop": row["pop"],
                    "pop_upper": row["pop_upper"],
                    "pop_lower": row["pop_lower"],
                    "sex": row["sex"],
                    "pop_posterior": row["pop_posterior"]
                })
            session.bulk_insert_mappings(Population, data)
    session.commit()


def main():
    add_pop_data(overwrite_existing=True)
    add_migration_data(overwrite_existing=True)


if __name__ == "__main__":
    main()
