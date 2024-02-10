from flask import Flask, request, jsonify
from flask_jwt_extended import JWTManager, create_access_token, jwt_required, get_jwt_identity, get_jwt
from flask_sqlalchemy import SQLAlchemy

from app.models import User
from app import db


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

@jwt_required()
def test_get():
    current_user = get_jwt_identity()
    claims = get_jwt()
    print(claims)
    print(current_user)
    return "Hello GISRede"
