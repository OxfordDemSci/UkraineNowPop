from datetime import datetime
from functools import wraps

import app.models as m
from app import db


def validate_input(f):
    @wraps(f)
    def decorated_function(*args, **kwargs):
        if country := kwargs.get("country", None):
            try:
                db.session.query(m.Countries.country).filter(
                    m.Countries.country == country
                ).first()[0].name
            except TypeError:
                return "Invalid country", 400
        if admin_level := kwargs.get("admin_level", None):
            if admin_level not in [1, 2, 3]:
                return "Invalid admin level", 400
        if date := kwargs.get("date", None):
            try:
                datetime.strptime(date, "%Y-%m-%d")
            except ValueError:
                return "Invalid date", 400
        if admin_id := kwargs.get("admin_id", None):
            try:
                db.session.query(m.AdminUnits.pcode).filter(
                    m.AdminUnits.pcode == admin_id
                ).first()[0]
            except TypeError:
                return "Invalid admin_id", 400
        if age_min_male := kwargs.get("age_min_male", None):
            if not isinstance(age_min_male, int):
                return "Invalid age_min", 400
        if age_max_male := kwargs.get("age_max_male", None):
            if not isinstance(age_max_male, int):
                return "Invalid age_max_male", 400
        if age_min_female := kwargs.get("age_min_female", None):
            if not isinstance(age_min_female, int):
                return "Invalid age_min", 400
        if age_max_female := kwargs.get("age_max_female", None):
            if not isinstance(age_max_female, int):
                return "Invalid age_max_female", 400
        return f(*args, **kwargs)

    return decorated_function
