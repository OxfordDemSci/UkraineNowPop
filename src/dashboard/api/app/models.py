from flask_bcrypt import Bcrypt
from sqlalchemy import Column, String, Integer, Boolean, Enum
from sqlalchemy.ext.declarative import declarative_base

import app

from .datatypes import UserRoleEnum

Base = declarative_base()


class User(Base):
    __tablename__ = "users"

    id = Column(Integer, primary_key=True, autoincrement=True)
    username = Column(String(255), nullable=False)
    password = Column(String(255), nullable=False)
    role = Column(Enum(UserRoleEnum), nullable=False)

    def __init__(self, username, password, role):
        self.username = username
        self.password = app.bcrypt.generate_password_hash(
            password
        ).decode()
        self.role = role
