from pathlib import Path
import os
import sys
from dotenv import load_dotenv
from alembic import command
from alembic.config import Config
from sqlalchemy import create_engine, inspect
import psycopg2

BASE_DIR = Path(__file__).resolve().parent.parent  # API route
sys.path.append(str(BASE_DIR))  # API route

from app.models import AdminUnits
from app.datatypes import CountriesEnum2, CountriesEnum3

ENV = BASE_DIR.parent.joinpath('.env')
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


def insert_admin_units():
    print(CountriesEnum2.UA.value)


def insert_admin_units_meta_data():
    pass


def main():
    upgrade_alembic(pg_host)
    insert_admin_units()


if __name__ == "__main__":
    main()