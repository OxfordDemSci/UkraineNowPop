from pathlib import Path
import os
import sys
from dotenv import load_dotenv
from alembic import command
from alembic.config import Config
from sqlalchemy import create_engine, inspect
import psycopg2
import geopandas as gpd  # type: ignore
import pandas as pd
import fiona
import dask

dask.config.set({"dataframe.query-planning": True})
import csv
import psutil

BASE_DIR = Path(__file__).resolve().parent.parent  # API route
sys.path.append(str(BASE_DIR))  # API route

from app.models import (
    AdminUnits,
    Languages,
    Countries,
    AdminUnitsMetadata,
    Migration,
    Population,
)
from app.datatypes import CountriesEnum3
from sqlalchemy.orm import sessionmaker
from shapely.geometry import MultiPolygon
import logging

import make_dummy_pop_migration as dummy

logging.basicConfig(level=logging.INFO)

ENV = BASE_DIR.parent.parent.parent.joinpath(".env")
GPKG = BASE_DIR.joinpath("app", "data", "db-data", "GEODATA.gpkg")
DATA = BASE_DIR.parent.parent.parent.joinpath("data/dummy_tables")
load_dotenv(ENV)

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


try:
    conn = psycopg2.connect(
        database=POSTGRES_DB,
        user=POSTGRES_USER,
        password=POSTGRES_PASSWORD,
        host="now_pop_postgres",
        port="5432",
    )
    engine = create_engine(
        f"postgresql+psycopg2://{POSTGRES_USER}:{POSTGRES_PASSWORD}@now_pop_postgres:5432/{POSTGRES_DB}"
    )
    pg_host = "now_pop_postgres"
except psycopg2.OperationalError:
    conn = psycopg2.connect(
        database=POSTGRES_DB,
        user=POSTGRES_USER,
        password=POSTGRES_PASSWORD,
        host="localhost",
        port="5432",
    )
    engine = create_engine(
        f"postgresql+psycopg2://{POSTGRES_USER}:{POSTGRES_PASSWORD}@localhost:5432/{POSTGRES_DB}"
    )
    pg_host = "localhost"


def upgrade_alembic(pg_host: str):
    alembic_cfg = Config(BASE_DIR.joinpath("alembic.ini"))
    alembic_cfg.set_main_option(
        "sqlalchemy.url",
        f"postgresql://{POSTGRES_USER}:{POSTGRES_PASSWORD}@{pg_host}:5432/{POSTGRES_DB}",
    )
    command.upgrade(alembic_cfg, "head")


@timer_and_log
def insert_admin_units(overwrite_existing: bool = False):
    Session = sessionmaker(bind=engine)
    session = Session()
    countries = fiona.listlayers(GPKG)
    for country in countries:
        query = (
            session.query(AdminUnits).filter_by(country=CountriesEnum3[country]).first()
        )
        if query and overwrite_existing:
            query = (
                session.query(AdminUnits)
                .filter_by(country=CountriesEnum3[country])
                .delete(synchronize_session=False)
            )
        if not query or overwrite_existing:
            gdf = gpd.read_file(GPKG, layer=country)
            gdf["geometry"] = gdf["geometry"].apply(
                lambda geom: MultiPolygon([geom])
                if geom.geom_type == "Polygon"
                else geom
            )
            data = []
            for _, row in gdf.iterrows():
                data.append(
                    {
                        "pcode": row["pcode"],
                        "admin_level": row["admin_level"],
                        "country": CountriesEnum3[country],
                        "country_lan2": row["country_lan2"],
                        "country_lan3": row["country_lan3"],
                        "name_en": row["name_en"],
                        "name_lan2": row["name_lan2"],
                        "name_lan3": row["name_lan3"],
                        "geometry": row["geometry"].wkt,
                    }
                )
            session.bulk_insert_mappings(AdminUnits, data)
    session.commit()


@timer_and_log
def add_languages(
    countries: list[dict[str, list]] = [{"UKR": ["Ukrainian", "Russian"]}],
):
    Session = sessionmaker(bind=engine)
    session = Session()
    for country in countries:
        for key, value in country.items():
            query = session.query(Languages).filter_by(country=CountriesEnum3[key])
            if not query.first():
                language = Languages(
                    country=CountriesEnum3[key], lan2=value[0], lan3=value[1]
                )
                session.add(language)
    session.commit()


@timer_and_log
def add_country_access(
    countries: list[dict] = [
        {"UKR": {"National": None, "Oblast": None, "Raion": "READ", "Hromada": "READ"}}
    ],
):
    Session = sessionmaker(bind=engine)
    session = Session()
    for country in countries:
        for key, value in country.items():
            query = session.query(Countries).filter_by(country=CountriesEnum3[key])
            if not query.first():
                admin_names = list(value.keys())
                country = Countries(
                    country=CountriesEnum3[key],
                    adm0_access=value[admin_names[0]],
                    adm1_access=value[admin_names[1]],
                    adm2_access=value[admin_names[2]],
                    adm3_access=value[admin_names[3]],
                    adm1_name=admin_names[1],
                    adm2_name=admin_names[2],
                    adm3_name=admin_names[3],
                )
                session.add(country)
    session.commit()


@timer_and_log
def insert_admin_units_meta_data(overwrite_existing: bool = False):
    csv = BASE_DIR.joinpath("app", "data", "db-data", "global_pcodes.csv")
    Session = sessionmaker(bind=engine)
    session = Session()
    query = session.query(AdminUnitsMetadata).first()
    if query and overwrite_existing:
        session.query(AdminUnitsMetadata).delete(synchronize_session=False)
    if not query or overwrite_existing:
        for chunk in pd.read_csv(csv, skiprows=[1], chunksize=CHUNK_SIZE):
            data = []
            for _, row in chunk.iterrows():
                data.append(
                    {
                        "location": CountriesEnum3[row["Location"]],
                        "admin_level": row["Admin Level"],
                        "pcode": row["P-Code"],
                        "name": row["Name"],
                        "parent_pcode": row["Parent P-Code"],
                        "valid_from": row["Valid from date"],
                    }
                )
            session.bulk_insert_mappings(AdminUnitsMetadata, data)
    session.commit()

def main():
    upgrade_alembic(pg_host)
    insert_admin_units()
    add_languages()
    add_country_access()
    insert_admin_units_meta_data(overwrite_existing=False)


if __name__ == "__main__":
    main()
