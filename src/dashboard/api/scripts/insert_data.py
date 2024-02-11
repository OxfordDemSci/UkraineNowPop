from pathlib import Path
import os
from dotenv import load_dotenv

BASE_DIR = Path(__file__).resolve().parent.parent  # API route
ENV = BASE_DIR.parent.joinpath('.env')
load_dotenv(ENV)


class Configuration:
    POSTGRES_USER: str | None = os.getenv("POSTGRES_USER")
    POSTGRES_PASSWORD: str | None = os.getenv("POSTGRES_PASSWORD")
    POSTGRES_READONLY: str | None = os.getenv("POSTGRES_READONLY")
    POSTGRES_READONLY_PASSWORD: str | None = os.getenv("POSTGRES_READONLY_PASSWORD")
    POSTGRES_DB: str | None = os.getenv("POSTGRES_DB")
    POSTGRES_DB_TEST: str | None = os.getenv("POSTGRES_DB_TEST")
    DATABASE_TABLES_DIR: str | None = os.getenv("DATABASE_TABLES_DIR")
    basedir: str | None = os.getenv("basedir")
    SECRET_KEY: str | None = os.getenv("SECRET_KEY")
    DATABASE_URL: str | None = os.getenv("DATABASE_URL")


def main():
    config = Configuration()


if __name__ == "__main__":
    main()