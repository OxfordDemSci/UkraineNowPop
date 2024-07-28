from functools import wraps
from typing import Any, Dict, List, Union

from flask import Response, current_app, jsonify, make_response, request
from flask_jwt_extended import (create_access_token, decode_token, get_jwt,
                                get_jwt_identity, jwt_required)

from app import data_queries as dq
from app.data_validation import validate_input, validate_dates
from app.datatypes import RankBy
from app.models import User


def check_scope(required_scope):
    def decorator(f):
        @wraps(f)
        @jwt_required()
        def decorated_function(*args, **kwargs):
            claims = get_jwt()
            user_scopes = claims["scope"]

            scope_hierarchy = ["read", "write"]
            if scope_hierarchy.index(user_scopes) >= scope_hierarchy.index(
                required_scope
            ):
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

    try:
        data = request.get_json()
        username = data.get("username")
        password = data.get("password")

        if not username or not password:
            return jsonify({"message": "Missing username or password"}), 400

        user = dq.get_user(username)
        if not user or not bcrypt.check_password_hash(user.password, password):
            return jsonify({"message": "Invalid username or password"}), 401
        additional_claims = {"scope": user.role.value}
        access_token = create_access_token(
            identity=username, additional_claims=additional_claims
        )
    except Exception as e:
        current_app.logger.error(e)
        return make_response("An error occurred while fetching the data", 500)
    return jsonify(access_token=access_token), 200


def get_countries() -> Union[List[Dict], Response]:
    try:
        return dq.get_countries()
    except Exception as e:
        current_app.logger.error(e)
        return make_response("An error occurred while fetching the data", 500)


@validate_input
def init(country: str):
    try:
        return dq.init(country)
    except Exception as e:
        current_app.logger.error(e)
        return make_response("An error occurred while fetching the data", 500)


@validate_input
def get_age_ranges(country: str):
    try:
        return dq.get_age_ranges(country)
    except Exception as e:
        current_app.logger.error(e)
        return make_response("An error occurred while fetching the data", 500)


@validate_input
def get_dates(country: str):
    try:
        return dq.get_dates(country)
    except Exception as e:
        current_app.logger.error(e)
        return make_response("An error occurred while fetching the data", 500)


@validate_input
@jwt_required(optional=True)
def get_admin_units(
    country: str,
    admin_level: int,
) -> Union[Dict[Any, Any], Response]:
    if admin_level not in [1, 2, 3]:
        return make_response("Invalid admin level", 400)
    if admin_level > 1:
        current_user = get_jwt_identity()
        if current_user is None:
            return make_response(
                "You need to be logged in to access this resource", 401
            )
    try:
        data = dq.get_geodata(country, admin_level)
    except Exception as e:
        current_app.logger.error(e)
        return make_response("An error occurred while fetching the data", 500)
    return data


@validate_input
@validate_dates
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
) -> Union[Dict[Any, Any], Response]:
    if admin_level not in [1, 2, 3]:
        return make_response("Invalid admin level", 400)
    if admin_level > 1:
        current_user = get_jwt_identity()
        if current_user is None:
            return make_response(
                "You need to be logged in to access this resource", 401
            )
    try:
        data = dq.get_population(
            country,
            admin_level,
            date,
            admin_id,
            age_min_male,
            age_max_male,
            age_min_female,
            age_max_female,
        )
    except Exception as e:
        current_app.logger.error(e)
        return make_response("An error occurred while fetching the data", 500)
    return data


@validate_input
@validate_dates
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
    rank_by: RankBy = RankBy.COUNT,
    limit: int = 10,
) -> Union[Dict[Any, Any], Response]:
    if admin_level not in [1, 2, 3]:
        return make_response("Invalid admin level", 400)
    if admin_level > 1:
        current_user = get_jwt_identity()
        if current_user is None:
            return make_response(
                "You need to be logged in to access this resource", 401
            )
    try:
        data = dq.get_migration_probabilities(
            country,
            admin_level,
            date,
            admin_id,
            age_min_male,
            age_max_male,
            age_min_female,
            age_max_female,
            rank_by,
            limit,
        )
    except Exception as e:
        current_app.logger.error(e)
        return make_response("An error occurred while fetching the data", 500)
    return data

