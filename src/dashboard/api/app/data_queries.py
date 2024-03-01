from .models import AdminUnits
from app import db


def get_geodata(country: str, admin_level: int) -> dict:
    admin_units = db.session.query(AdminUnits).filter_by(country=country, admin_level=admin_level).all()
    return {
        "type": "FeatureCollection",
        "features": [au.to_geojson for au in admin_units],
    }
