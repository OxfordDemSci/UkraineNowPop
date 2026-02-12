from pathlib import Path
import os
import sys
from sqlalchemy import create_engine, inspect
import psycopg2
import pandas as pd
import dask
from tqdm import tqdm
from dotenv import load_dotenv

dask.config.set({"dataframe.query-planning": True})
import csv
import psutil

# BASE_DIR = Path("C:/Users/EdithD/Documents/git/UkraineNowPop/src/dashboard/api/")
BASE_DIR = Path(__file__).resolve().parent.parent  # API route
sys.path.append(str(BASE_DIR))  # API route

from app.models import Migration, Population
from sqlalchemy.orm import sessionmaker
import logging


GPKG = BASE_DIR.joinpath("app", "data", "db-data", "GEODATA.gpkg")
ENV = BASE_DIR.parent.parent.parent.joinpath(".env")
load_dotenv(ENV)

POSTGRES_USER = os.environ.get("POSTGRES_USER")
POSTGRES_PASSWORD = os.environ.get("POSTGRES_PASSWORD")
POSTGRES_HOST = os.environ.get("POSTGRES_HOST")
POSTGRES_DB = os.environ.get("POSTGRES_DB")
TABLES_DIR = os.environ.get("DATABASE_TABLES_DIR")
if psutil.virtual_memory().total / (1024**3) < 4:
    CHUNK_SIZE = 2000
else:
    CHUNK_SIZE = 20000

print("Inserting data for HOST: " + POSTGRES_HOST)


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
    host=POSTGRES_HOST,
    port="5432",
)
engine = create_engine(
    f"postgresql+psycopg2://{POSTGRES_USER}:{POSTGRES_PASSWORD}@{POSTGRES_HOST}:5432/{POSTGRES_DB}"
)
pg_host = "now_pop_postgres"


# --- insert population data ---#
@timer_and_log
def add_pop_data(overwrite_existing: bool = True):
    def parse_pop_posterior(s):
        parts = s.replace("[", "").replace("]", "").split(",")
        return [int(part) for part in parts]

    Session = sessionmaker(bind=engine)
    pop_data = GPKG.parent.joinpath("pop.csv")

    # if overwrite, delete everything first
    if overwrite_existing:
        session = Session()
        try:
            session.query(Population).delete(synchronize_session=False)
            session.commit()
        except Exception:
            session.rollback()
        finally:
            session.close()

    # check for empty table
    session_check = Session()
    try:
        has_any = session_check.query(Population).first() is not None
    finally:
        session_check.close()

    # insert data if table is empty
    if not has_any:
        # open sql session
        session = Session()

        try:
            print("Processing stocks data...")
            for chunk in tqdm(
                pd.read_csv(
                    pop_data,
                    chunksize=CHUNK_SIZE,
                    encoding="utf-8",
                    quoting=csv.QUOTE_ALL,
                ),
                total=28,
            ):
                chunk["sex"] = chunk["sex"].replace({"male": 1, "female": 2})
                chunk["day"] = pd.to_datetime(chunk["day"], format="%Y-%m-%d")
                chunk = chunk.astype(
                    {
                        "country": "string",
                        "admin_level": "int8",
                        "pcode": "string",
                        "age_min": "int8",
                        "age_max": "int16",
                        "pop": "int32",
                        "pop_upper": "int32",
                        "pop_lower": "int32",
                        "sex": "int8",
                    }
                )
                chunk["pop_posterior"] = chunk["pop_posterior"].apply(
                    parse_pop_posterior
                )

                data = []
                for _, row in chunk.iterrows():
                    data.append(
                        {
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
                            "pop_posterior": row["pop_posterior"],
                        }
                    )

                # insert chunk
                session.bulk_insert_mappings(Population, data)

            # commit when all chunks completed
            session.commit()

        except Exception:
            session.rollback()
        finally:
            session.close()


# --- insert migration data ---#
@timer_and_log
def add_migration_data(overwrite_existing: bool = False):
    Session = sessionmaker(bind=engine)
    migration_data = GPKG.parent.joinpath("migration.csv")

    # if overwrite, delete everything first
    if overwrite_existing:
        session = Session()
        try:
            session.query(Migration).delete(synchronize_session=False)
            session.commit()
        except Exception:
            session.rollback()
        finally:
            session.close()

    # check for empty table
    session_check = Session()
    try:
        has_any = session_check.query(Migration).first() is not None
    finally:
        session_check.close()

    # insert data if table is empty
    if not has_any:
        # open sql session
        session = Session()

        try:
            print("Processing migration data...")

            for chunk in tqdm(
                pd.read_csv(migration_data, chunksize=CHUNK_SIZE), total=1912
            ):
                chunk["day"] = pd.to_datetime(chunk["day"], format="%Y-%m-%d")
                chunk = chunk.astype(
                    {
                        "country": "string",
                        "admin_level": "int8",
                        "origin": "string",
                        "destination": "string",
                        "age_min": "int8",
                        "age_max": "int16",
                        "sex": "int8",
                        "proportion": "int16",
                        "count": "int32",
                    }
                )
                data = []
                for _, row in chunk.iterrows():
                    data.append(
                        {
                            "country": row["country"],
                            "admin_level": row["admin_level"],
                            "origin": row["origin"],
                            "destination": row["destination"],
                            "day": row["day"],
                            "age_min": row["age_min"],
                            "age_max": row["age_max"],
                            "sex": row["sex"],
                            "proportion": row["proportion"],
                            "count": row["count"],
                        }
                    )

                # insert chunk
                session.bulk_insert_mappings(Migration, data)

            # commit when all chunks completed
            session.commit()

        except Exception:
            session.rollback()
        finally:
            session.close()


def main():
    add_pop_data(overwrite_existing=True)
    add_migration_data(overwrite_existing=True)


if __name__ == "__main__":
    main()
