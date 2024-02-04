import connexion
from connexion.options import SwaggerUIOptions
from flask import Flask

from app.config import app_config


def create_app(config_name: str) -> Flask:
    options = SwaggerUIOptions(swagger_ui_path="/app")
    connexion_app = connexion.FlaskApp(__name__, swagger_ui_options=options)
    connexion_app.add_api("api-config.yaml")
    app = connexion_app.app
    app.config.from_object(app_config[config_name])
    print(app)
    print(app.url_map)
    print(connexion_app.add_api("api-config.yaml"))
    return app
