from pathlib import Path
import os
import sys
from dotenv import load_dotenv
from alembic import command
from alembic.config import Config
from sqlalchemy import create_engine, inspect
import psycopg2
import geopandas as gpd  # type: ignore
from geoalchemy2 import Geometry, WKBElement
import pandas as pd
import fiona

BASE_DIR = Path(__file__).resolve().parent.parent  # API route
sys.path.append(str(BASE_DIR))  # API route

from app.models import AdminUnits, Languages, Countries, AdminUnitsMetadata
from app.datatypes import CountriesEnum2, CountriesEnum3, UserRoleEnum
from sqlalchemy.orm import sessionmaker
from shapely.geometry import MultiPolygon
from shapely.wkt import loads

ENV = BASE_DIR.parent.joinpath('.env')
GPKG = BASE_DIR.joinpath("app", "data", "db-data", "GEODATA.gpkg")
load_dotenv(ENV)

POSTGRES_USER = os.environ.get("POSTGRES_USER")
POSTGRES_PASSWORD = os.environ.get("POSTGRES_PASSWORD")
POSTGRES_DB = os.environ.get("POSTGRES_DB")
TABLES_DIR = os.environ.get("DATABASE_TABLES_DIR")

try:
    conn = psycopg2.connect(
        database=POSTGRES_DB,
        user=POSTGRES_USER,
        password=POSTGRES_PASSWORD,
        host="ics_postgres",
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


def subset_and_dissolve(
        gdf: gpd.GeoDataFrame,
        columns: list[str],
        adm_level: int
        ) -> gpd.GeoDataFrame:
    gdf = gdf[columns]
    gdf = gdf.set_index([x for x in columns if x not in [f"pcode_{adm_level}", "geometry"]])
    gdf = gdf.dissolve(by=f"pcode_{adm_level}").reset_index()
    gdf["adm_level"] = adm_level
    return gdf


def insert_admin_units(countries: list = ["UKR"]):
    Session = sessionmaker(bind=engine)
    session = Session()
    countries = fiona.listlayers(GPKG)
    for country in countries:
        query = session.query(AdminUnits).filter_by(country=CountriesEnum3[country])
        if query:
            query.delete(synchronize_session=False)
        gdf = gpd.read_file(GPKG, layer=country)
        gdf['geometry'] = gdf['geometry'].apply(lambda geom: MultiPolygon([geom]) if geom.geom_type == 'Polygon' else geom)
        for adm_level in range(0, 4):
            gdf_subset = subset_and_dissolve(
                gdf,
                # TODO Rename the columns in the gdf to match the columns in the AdminUnits table
                [f"pcode_{adm_level}", "adm_", "country_lan2", "country_lan3", "name_en", "name_lan2", "name_lan3", "geometry"],
                adm_level)
            for index, row in gdf_subset.iterrows():
                admin_unit = AdminUnits(
                    pcode=row["pcode"],
                    admin_level=row["admin_level"],
                    country=CountriesEnum3[country],
                    country_lan2=row["country_lan2"],
                    country_lan3=row["country_lan3"],
                    name_en=row["name_en"],
                    name_lan2=row["name_lan2"],
                    name_lan3=row["name_lan3"],
                    geometry=row["geometry"].wkt
                )
                session.add(admin_unit)
    session.commit()


def add_languages(countries: list[dict[str, list]] = [{"UKR": ["Ukrainian", "Russian"]}]):
    Session = sessionmaker(bind=engine)
    session = Session()
    for country in countries:
        for key, value in country.items():
            query = session.query(Languages).filter_by(country=CountriesEnum3[key])
            if not query.first():
                language = Languages(country=CountriesEnum3[key], lan2=value[0], lan3=value[1])
                session.add(language)
    session.commit()


def add_country_access(countries: list[dict] = [{"UKR": {"National": None, "Oblast": None, "Raion": "READ", "Hromada": "READ"}}]):
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
                    adm3_name=admin_names[3]                 
                    )
                session.add(country)
    session.commit()


def insert_admin_units_meta_data():
    csv = BASE_DIR.parent.parent.parent.joinpath("data", "cod-ab", "global_pcodes.csv")
    Session = sessionmaker(bind=engine)
    session = Session()
    query = session.query(AdminUnitsMetadata).all()
    if not query:
        df = pd.read_csv(csv, skiprows=[1])
        for index, row in df.iterrows():
            admin_unit = AdminUnitsMetadata(
                location=CountriesEnum3[row["Location"]],
                admin_level=row["Admin Level"],
                pcode=row["P-Code"],
                name=row["Name"],
                parent_pcode=row["Parent P-Code"],
                valid_from=row["Valid from date"]
            )
            session.add(admin_unit)
    session.commit()




def main():
    # upgrade_alembic(pg_host)
    insert_admin_units()
    # add_languages()
    # add_country_access()
    #insert_admin_units_meta_data()


if __name__ == "__main__":
    main()