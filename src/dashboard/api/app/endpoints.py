from flask import Flask, request, jsonify, make_response
from flask_jwt_extended import (
    JWTManager, create_access_token, jwt_required, get_jwt_identity, get_jwt, decode_token
)
from flask_sqlalchemy import SQLAlchemy
from flask.wrappers import Response
from functools import wraps
from typing import Union

from app.models import User
from app import data_queries as dq
from app import db


def check_scope(required_scope):
    def decorator(f):
        @wraps(f)
        @jwt_required()
        def decorated_function(*args, **kwargs):
            claims = get_jwt()
            user_scopes = claims['scope']

            scope_hierarchy = ['read', 'write']
            if scope_hierarchy.index(user_scopes) >= scope_hierarchy.index(required_scope):
                return f(*args, **kwargs)
            else:
                return "You don't have permission to access this resource", 403
        return decorated_function
    return decorator


def decodetoken(token):
    decoded_token = decode_token(token)
    return decoded_token


def login():
    from app import bcrypt
    data = request.get_json()
    username = data.get('username')
    password = data.get('password')

    if not username or not password:
        return jsonify({'message': 'Missing username or password'}), 400

    user = db.session.query(User).filter_by(username=username).first()
    if not user or not bcrypt.check_password_hash(user.password, password):
        return jsonify({'message': 'Invalid username or password'}), 401
    additional_claims = {"scope": user.role.value}
    access_token = create_access_token(identity=username, additional_claims=additional_claims)
    return jsonify(access_token=access_token), 200


def get_countries() -> list[dict]:
    return dq.get_countries()


def init(country: str):
    return dq.init(country)


@jwt_required(optional=True)
def get_admin_units(
    country: str,
    admin_level: int,
) -> Union[list[dict], Response]:
    if admin_level not in [1, 2, 3]:
        return make_response("Invalid admin level", 400)
    if admin_level > 1:
        current_user = get_jwt_identity()
        if current_user is None:
            return make_response("You need to be logged in to access this resource", 401)
    data = dq.get_geodata(country, admin_level)
    return data

@jwt_required(optional=True)
def get_population(
    country: str,
    admin_level: int,
    date: str,
    admin_id: str | None = None,
    age_min_male: int | None = None,
    age_max_male: int | None = None,
    age_min_female: int | None = None,
    age_max_female: int | None = None,
) -> Union[list[dict], Response]:
    if admin_level not in [1, 2, 3]:
        return make_response("Invalid admin level", 400)
    if admin_level > 1:
        current_user = get_jwt_identity()
        if current_user is None:
            return make_response("You need to be logged in to access this resource", 401)
    data = dq.get_population(
        country,
        admin_level,
        date,
        admin_id,
        age_min_male,
        age_max_male,
        age_min_female,
        age_max_female
        )
    return data

@jwt_required(optional=True)
def get_migration_probabilities(
        country: str,
        admin_level: int,
        date: str,
        admin_id: str | None = None,
        age_min_male: int | None = None,
        age_max_male: int | None = None,
        age_min_female: int | None = None,
        age_max_female: int | None = None,
) -> Union[list[dict], Response]:
    if admin_level not in [1, 2, 3]:
        return make_response("Invalid admin level", 400)
    if admin_level > 1:
        current_user = get_jwt_identity()
        if current_user is None:
            return make_response("You need to be logged in to access this resource", 401)
    data = dq.get_migration_probabilities(
        country,
        admin_level,
        date,
        admin_id,
        age_min_male,
        age_max_male,
        age_min_female,
        age_max_female
    )
    return data


@check_scope("read")
def test_get():
    return "Hello GISRede"


@check_scope("read")
def test_read():
    return "Hello GISRede Reader"


@check_scope("write")
def test_write():
    return "Hello GISRede Writer"
