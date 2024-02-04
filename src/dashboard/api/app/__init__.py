import connexion
from flask import Flask

from app.config import app_config


def create_app(config_name: str) -> Flask:
    connexion_app = connexion.FlaskApp(__name__, specification_dir="./")
    connexion_app.add_api("api-config.yaml")
    app = connexion_app.app
    app.config.from_object(app_config[config_name])
    print(app)
    print(app.url_map)
    return app
