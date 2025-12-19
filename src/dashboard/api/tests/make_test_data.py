from pathlib import Path

import geopandas as gpd
import pandas as pd
from shapely.geometry import MultiPolygon
import dotenv
import os

dotenv.load_dotenv()
from app.models import (
        AdminUnits,
        Migration,
        Population,
        User,
        Languages,
        Countries,
)

from app.datatypes import CountriesEnum3, UserRoleEnum
        

BASE = Path(__file__).resolve().parent
TEST_DIR = BASE.parent.joinpath("tests/data")


def insert_test_data(db_session):
    insert_users(db_session)
    insert_admin_units(db_session)
    add_languages(db_session)
    add_country_access(db_session)
    add_migration(db_session)
    add_population(db_session)
    db_session.commit()


def insert_users(db_session):
    admin_user = os.getenv("POSTGRES_USER")
    admin_password = os.getenv("POSTGRES_PASSWORD")
    read_user = os.getenv("POSTGRES_READONLY")
    read_password = os.getenv("POSTGRES_READONLY_PASSWORD")

    admin_user = User(username=admin_user, password=admin_password, role=UserRoleEnum.WRITE)
    read_user = User(username=read_user, password=read_password, role=UserRoleEnum.READ)

    db_session.add(admin_user)
    db_session.add(read_user)


def add_population(db_session):
    def parse_pop_posterior(s):
        parts = s.replace('[', '').replace(']', '').split(',')
        return [int(part) for part in parts]
    df = pd.read_parquet(TEST_DIR.joinpath("pop.parquet"))
    df["sex"] = df["sex"].replace({"male": 1, "female": 2})
    df = df.astype({
        "country": "string",
        "admin_level": "int8",
        "pcode": "string",
        "age_min": "int8",
        "age_max": "int8",
        "pop": "int32",
        "pop_upper": "int32",
        "pop_lower": "int32",
        "sex": "int8",
    })
    df["pop_posterior"] = df["pop_posterior"].apply(parse_pop_posterior)
    data = []
    for _, row in df.iterrows():
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
    db_session.bulk_insert_mappings(Population, data)


def add_migration(db_session):
    df = pd.read_parquet(TEST_DIR.joinpath("migration.parquet"))
    df = df.astype({
                "country": "string",
                "admin_level": "int8",
                "origin": "string",
                "destination": "string",
                "age_min": "int8",
                "age_max": "int8",
                "sex": "int8",
                "proportion": "float32",
                "count": "int32",            
        })
    data = []
    for _, row in df.iterrows():
        data.append({
                    "country": row["country"],
                    "admin_level": row["admin_level"],
                    "origin": row["origin"],
                    "destination": row["destination"],
                    "day": row["day"],
                    "age_min": row["age_min"],
                    "age_max": row["age_max"],
                    "sex": row["sex"],
                    "proportion": row["proportion"],
                    "count": row["count"]
                })
    db_session.bulk_insert_mappings(Migration, data)



def add_country_access(db_session):
    countries = [{"UKR": {"National": None, "Oblast": None, "Raion": "READ", "Hromada": "READ"}}]
    for country in countries:
        for key, value in country.items():
            query = db_session.query(Countries).filter_by(country=CountriesEnum3[key])
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
                db_session.add(country)


def add_languages(db_session):
    countries = [{"UKR": ["Ukrainian", "Russian"]}]
    for country in countries:
        for key, value in country.items():
            language = Languages(country=CountriesEnum3[key], lan2=value[0], lan3=value[1])
            db_session.add(language)


def insert_admin_units(db_session):
    gdf = gpd.read_file(TEST_DIR.joinpath("GEODATA.gpkg"), layer="UKR")
    gdf['geometry'] = gdf['geometry'].apply(
                lambda geom: MultiPolygon([geom]) if geom.geom_type == 'Polygon' else geom
                )
    data = []
    for _, row in gdf.iterrows():
        data.append({
            'pcode': row["pcode"],
            'admin_level': row["admin_level"],
            'country': CountriesEnum3["UKR"],
            'country_lan2': row["country_lan2"],
            'country_lan3': row["country_lan3"],
            'name_en': row["name_en"],
            'name_lan2': row["name_lan2"],
            'name_lan3': row["name_lan3"],
            'geometry': row["geometry"].wkt
        })
    db_session.bulk_insert_mappings(AdminUnits, data)

