from datetime import datetime
from functools import wraps

import app.models as m
from app.data_queries import get_age_ranges, get_dates


def validate_input(f):
    @wraps(f)
    def decorated_function(*args, **kwargs):
        from app import db
        if country := kwargs.get("country", None):
            try:
                db.session.query(m.Countries.country).filter(
                    m.Countries.country == country
                ).first()[0].name
            except TypeError:
                return  f"Data not available for country: {country}", 400
        if admin_level := kwargs.get("admin_level", None):
            if admin_level not in [1, 2, 3]:
                return  f"Data not available for admin level: {admin_level}", 400
        if admin_id := kwargs.get("admin_id", None):
            try:
                db.session.query(m.AdminUnits.pcode).filter(
                    m.AdminUnits.pcode == admin_id
                ).first()[0]
            except TypeError:
                return f"Data not available for admin id: {admin_id}", 400
        age_ranges = get_age_ranges(country)
        age_mins = [age_range["age_min"] for age_range in age_ranges]
        age_maxs = [age_range["age_max"] for age_range in age_ranges]
        if age_min_male := kwargs.get("age_min_male", None):
            if not isinstance(age_min_male, int) or age_min_male not in age_mins:
                return f"Data not available for this age_min -> Accepted values: {age_mins}", 400
        if age_max_male := kwargs.get("age_max_male", None):
            if not isinstance(age_max_male, int) or age_max_male not in age_maxs:
                return f"Data not available for this age_max_male -> Accepted values: {age_maxs}", 400
        if age_min_female := kwargs.get("age_min_female", None):
            if not isinstance(age_min_female, int) or age_min_female not in age_mins:
                return f"Data not available for this age_min -> Accepted values: {age_mins}", 400
        if age_max_female := kwargs.get("age_max_female", None):
            if not isinstance(age_max_female, int) or age_max_female not in age_maxs:
                return f"Data not available for this age_max_female -> Accepted values: {age_maxs}", 400
        if age_min := kwargs.get("age_min", None):
            if not isinstance(age_min, int) or age_min not in age_mins:
                return f"Data not available for this age_min -> Accepted values: {age_mins}", 400
        if age_max := kwargs.get("age_max", None):
            if not isinstance(age_max, int) or age_max not in age_maxs:
                return f"Data not available for this age_max -> Accepted values: {age_maxs}", 400
        return f(*args, **kwargs)

    return decorated_function


def validate_dates(f):
    @wraps(f)
    def decorated_function(*args, **kwargs):
        country = kwargs.get("country", None)
        dates = {key: [d.strftime("%Y-%m-%d") for d in value] for key, value in get_dates(country).items()}
        if date := kwargs.get("date", None):
            try:
                datetime.strptime(date, "%Y-%m-%d")
            except ValueError:
                return "Invalid date formate -> Must be YYYY-MM-DD", 400
            if f.__name__ == "get_population":
                if date not in dates["dates_pop"]:
                    return "Invalid date -> See /get_dates endpoint for valid dates", 400
            return f(*args, **kwargs)
        if date_start := kwargs.get("date_start", None):
            try:
                datetime.strptime(date_start, "%Y-%m-%d")
            except ValueError:
                return "Invalid date_start formate -> Must be YYYY-MM-DD", 400
        if date_end := kwargs.get("date_end", None):
            try:
                datetime.strptime(date_end, "%Y-%m-%d")
            except ValueError:
                return "Invalid date_end formate -> Must be YYYY-MM-DD", 400
        if (date_start and date_end) and (date_start > date_end):
            return "Invalid date range -> date_start must be before date_end", 400
        if date_start not in dates["dates_pop"] or date_end not in dates["dates_pop"]:
            return "Invalid dates -> See /get_dates endpoint for valid dates", 400
        return f(*args, **kwargs)
    return decorated_function
