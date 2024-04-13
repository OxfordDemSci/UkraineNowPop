from pathlib import Path
import pytest

from app.endpoints import login
from app.models import User
from app import bcrypt
from app.datatypes import UserRoleEnum

TEST_DIR = Path(__file__).resolve().parent.joinpath("data")


def test_get_countries(session, test_client, app, db):
    response = test_client.get("/api/get_countries")
    assert response.json[0]["country"] == "UKR"
    assert response.status_code == 200

def test_init(session, test_client, app, db):
    response = test_client.get("/api/init?country=UKR")
    assert response.status_code == 200
    assert response.json["age_ranges"] == [{'age_min': 0, 'age_max': 4}, {'age_min': 5, 'age_max': 9}]
    assert response.json["admin_names"] == [{'adm1_name': 'Oblast', 'adm2_name': 'Raion', 'adm3_name': 'Hromada'}]
    assert response.json["languages"] == [{'lan1': 'English', 'lan2': 'Ukrainian', 'lan3': 'Russian'}]

@pytest.mark.parametrize("admin_level", [1, 2, 3])
def test_get_admin_units(session, test_client, app, db, admin_level):
    # Log in and get the access token
    username = "test_user"
    password = "test_password"
    response = test_client.post('/api/login', json={"username": username, "password": password})
    assert response.status_code == 200
    token = response.json["access_token"]

    # Test the protected endpoint
    headers = {
        'Authorization': f'Bearer {token}'
    }
    response = test_client.get(f'/api/get_admin_units?country=UKR&admin_level={admin_level}', headers=headers)
    assert response.status_code == 200


@pytest.mark.parametrize("admin_level, expected_status_code",
                         [
                            (1, 200),
                            (2, 401),
                            (3, 401)])
def test_get_admin_units_unauthorized(session, test_client, app, db, admin_level, expected_status_code):
    response = test_client.get(f'/api/get_admin_units?country=UKR&admin_level={admin_level}')
    assert response.status_code == expected_status_code

@pytest.mark.parametrize("admin_level", [1, 2, 3])
def test_get_population_with_auth(session, test_client, app, db, admin_level):
    # Log in and get the access token
    username = "test_user"
    password = "test_password"
    response = test_client.post('/api/login', json={"username": username, "password": password})
    assert response.status_code == 200
    token = response.json["access_token"]

    # Test the protected endpoint
    headers = {
        'Authorization': f'Bearer {token}'
    }
    response = test_client.get(
        f'/api/get_population?country=UKR&admin_level={admin_level}&date=2024-01-01&age_min_male=0&age_max_male=9&age_min_female=0&age_max_female=9',
        headers=headers
    )
    assert response.status_code == 200


@pytest.mark.parametrize("admin_level, expected_status_code",
                         [
                            (1, 200),
                            (2, 401),
                            (3, 401)])
def test_get_population_unauthorized(session, test_client, app, db, admin_level, expected_status_code):
    response = test_client.get(
        f'/api/get_population?country=UKR&admin_level={admin_level}&date=2024-01-01&age_min_male=0&age_max_male=9&age_min_female=0&age_max_female=9',
    )


@pytest.mark.parametrize("admin_level", [1, 2, 3])
def test_get_migration_probabilities_with_auth(session, test_client, app, db, admin_level):
    # Log in and get the access token
    username = "test_user"
    password = "test_password"
    response = test_client.post('/api/login', json={"username": username, "password": password})
    assert response.status_code == 200
    token = response.json["access_token"]

    # Test the protected endpoint
    headers = {
        'Authorization': f'Bearer {token}'
    }
    response = test_client.get(
        f'/api/get_migration_probabilities?country=UKR&admin_level={admin_level}&date=2024-01-01&age_min_male=0&age_max_male=9&age_min_female=0&age_max_female=9&rank_by=count&limit=10',
        headers=headers
    )
    assert response.status_code == 200


@pytest.mark.parametrize("admin_level, expected_status_code",
                         [
                            (1, 200),
                            (2, 401),
                            (3, 401)])
def test_get_migration_probabilities_unauthorized(session, test_client, app, db, admin_level, expected_status_code):
    response = test_client.get(
        f'/api/get_migration_probabilities?country=UKR&admin_level={admin_level}&date=2024-01-01&age_min_male=0&age_max_male=9&age_min_female=0&age_max_female=9&rank_by=count&limit=10',
    )
    assert response.status_code == expected_status_code


def test_login(app, session, test_client, db):
    # test_user is defined in the conftest
    username = "test_user"
    password = "test_password"
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
