from pathlib import Path

from app.endpoints import login
from app.models import User
from app import bcrypt
from app.datatypes import UserRoleEnum

TEST_DIR = Path(__file__).resolve().parent.joinpath("data")


def test_get_countries(session, test_client, app, db):
    response = test_client.get("/api/get_countries")
    assert response.json[0]["country"] == "UKR"
    assert response.status_code == 200


def test_login(app, session, test_client, db):
    # test_user is defined in the conftest
    username = "test_user"
    password = "test_password"
    # Test with valid credentials
    response = test_client.post('/api/login', json={"username": username, "password": password})
    assert response.status_code == 200
    assert "access_token" in response.json

    # Test with missing username

    response = test_client.post('/api/login', json={"password": password})
    assert response.status_code == 400
    assert response.json["message"] == "Missing username or password"

    # Test with missing password
    response = test_client.post('/api/login', json={"username": username})
    assert response.status_code == 400
    assert response.json["message"] == "Missing username or password"

    # Test with invalid username
    response = test_client.post('/api/login', json={"username": "invalid_user", "password": password})
    assert response.status_code == 401
    assert response.json["message"] == "Invalid username or password"

    # Test with invalid password
    response = test_client.post('/api/login', json={"username": username, "password": "invalid_password"})
    assert response.status_code == 401
    assert response.json["message"] == "Invalid username or password"
