from flask import Flask, request, jsonify
from flask_jwt_extended import JWTManager, create_access_token, jwt_required, get_jwt_identity, get_jwt, decode_token
from flask_sqlalchemy import SQLAlchemy
from functools import wraps

from app.models import User
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


@check_scope("read")
def test_get():
    return "Hello GISRede"


@check_scope("read")
def test_read():
    return "Hello GISRede Reader"


@check_scope("write")
def test_write():
    return "Hello GISRede Writer"
