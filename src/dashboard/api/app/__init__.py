import connexion
from flask import Flask, request
from flask_cors import CORS
from flask_limiter import Limiter
from flask_limiter.util import get_remote_address
from flask_sqlalchemy import SQLAlchemy
import logging
import os
from pathblib import Path
import socket
from datetime import datetime, timezone

from app.config import app_config


BASE = Path(__file__).resolve().parent


def get_nginx_ip():
    try:
        nginx_ip = socket.gethostbyname("now_pop_nginx")
        return nginx_ip
    except socket.gaierror:
        return None


def is_exempt():
    nginx_ip = get_nginx_ip()
    return request.remote_addr == nginx_ip


db = SQLAlchemy()
limiter = Limiter(
    key_func=get_remote_address,
    default_limits=["60/minute", "1000/hour", "10000/day"],
    strategy="fixed-window-elastic-expiry",
    storage_uri="",  # Set in create_app()
    storage_options={},
    default_limits_exempt_when=is_exempt,
)


def create_app(config_name: str) -> Flask:
    connexion_app = connexion.FlaskApp(__name__, specification_dir="./")
    connexion_app.add_api("api-config.yaml")
    app = connexion_app.app
    app.config.from_object(app_config[config_name])
    app.config["ENV"] = config_name
    db.init_app(app)
    CORS(app, resources={r"/*": {"origins": "*"}})
    if not logging.getLogger().handlers:
        logging.basicConfig(level=logging.INFO)
        app.logger.addHandler(logging.StreamHandler())  # Log to the terminal
        log_path = BASE.parent.joinpath('api_logs.log')
        file_handler = logging.FileHandler(log_path)
        app.logger.addHandler(file_handler)
        app.logger.setLevel(logging.INFO)
    if config_name not in ["local_development", "testing"]:
        limiter._storage_uri = "memcached://ics_memcached:11211"
        limiter.init_app(app)

    @app.before_request
    def before_request_function():
        args = request.args
        app.logger.info(f"time={datetime.now(timezone.utc)}, url={request.url}, endpoint={request.path} params={dict(args.items())}")

    @app.after_request
    def after_request_func(response):
        return response
    return app
