from geoalchemy2 import Geometry
from sqlalchemy import (
    Column, String, Integer, Enum, SMALLINT, ForeignKey, Date, Numeric, ARRAY
)
from sqlalchemy.ext.declarative import declarative_base

import app

from .datatypes import UserRoleEnum, CountriesEnum2, CountriesEnum3


Base = declarative_base()


class User(Base):  # type: ignore
    __tablename__ = "users"

    id = Column(Integer, primary_key=True, autoincrement=True)
    username = Column(String(255), nullable=False)
    password = Column(String(255), nullable=False)
    role: Column[Enum] = Column(Enum(UserRoleEnum), nullable=False)

    def __init__(self, username, password=None, role=None):
        self.username = username
        if password:
            self.password = app.bcrypt.generate_password_hash(
                password
            ).decode()
        self.role = role


class Population(Base):  # type: ignore
    __tablename__ = "population"

    id = Column(Integer, primary_key=True, autoincrement=True)
    country = Column(String(3), nullable=False)
    admin_level = Column(SMALLINT, nullable=False)
    pcode = Column(String(20), nullable=False)
    day = Column(Date, nullable=False, index=True)
    age_min = Column(SMALLINT, nullable=False)
    age_max = Column(SMALLINT, nullable=False)
    sex = Column(SMALLINT, nullable=False)
    pop = Column(Numeric(precision=10, scale=2), nullable=False)
    pop_upper = Column(Numeric(precision=10, scale=2), nullable=False)
    pop_lower = Column(Numeric(precision=10, scale=2), nullable=False)
    pop_quartiles: Column = Column(ARRAY(Integer), nullable=False)


class AdminUnits(Base):  # type: ignore
    __tablename__ = "admin_units"

    id = Column(Integer, primary_key=True, autoincrement=True)
    country: Column[Enum] = Column(Enum(CountriesEnum3), nullable=False)  # 3 letter ISO code
    pcode_0: Column[Enum] = Column(Enum(CountriesEnum2), nullable=False)
    pcode_1 = Column(String(20), nullable=True)
    pcode_2 = Column(String(20), nullable=True)
    pcode_3 = Column(String(20), nullable=True)
    # adm0 3 languages
    adm0_en = Column(String(255), nullable=False)
    adm0_lan2 = Column(String(255), nullable=True)
    adm0_lan3 = Column(String(255), nullable=True)
    # adm1 3 languages
    adm1_en = Column(String(255), nullable=True)
    adm1_lan2 = Column(String(255), nullable=True)
    adm_lan3 = Column(String(255), nullable=True)
    # adm2 3 languages
    adm2_en = Column(String(255), nullable=True)
    adm2_lan2 = Column(String(255), nullable=True)
    adm2_lan3 = Column(String(255), nullable=True)
    # adm3 3 languages
    adm3_en = Column(String(255), nullable=True)
    adm3_lan2 = Column(String(255), nullable=True)
    adm3_lan3 = Column(String(255), nullable=True)
    # 1 geometry at highest resolution
    geometry: Column[Geometry] = Column(Geometry(geometry_type='MULTIPOLYGON', srid=4326), nullable=False)


class AdminUnitsMetadata(Base):  # type: ignore
    __tablename__ = "admin_units_metadata"

    id = Column(Integer, primary_key=True, autoincrement=True)
    location: Column[Enum] = Column(Enum(CountriesEnum3), nullable=False)
    admin_level = Column(SMALLINT, nullable=False)
    pcode = Column(String(20), nullable=True)
    name = Column(String(255), nullable=True)
    parent_pcode = Column(String(20), nullable=True)
    valid_from = Column(Date, nullable=True)
    

class Languages(Base):  # type: ignore
    __tablename__ = "languages"

    id = Column(Integer, primary_key=True, autoincrement=True)
    country: Column[Enum] = Column(Enum(CountriesEnum3), nullable=False)
    lan2 = Column(String(255), nullable=True)
    lan3 = Column(String(255), nullable=True)


class Migration(Base):  # type: ignore
    __tablename__ = "migration"

    id = Column(Integer, primary_key=True, autoincrement=True)
    country = Column(String(3))
    admin_level = Column(SMALLINT, nullable=False)
    origin = Column(String(20), nullable=False)
    destination = Column(String(20), nullable=False)
    day = Column(Date, nullable=False)
    age_min = Column(SMALLINT, nullable=False)
    age_max = Column(SMALLINT, nullable=False)
    sex = Column(SMALLINT, nullable=False)
    probability = Column(Numeric(precision=5, scale=4), nullable=False)
