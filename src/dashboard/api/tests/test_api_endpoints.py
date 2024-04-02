from pathlib import Path

TEST_DIR = Path(__file__).resolve().parent.joinpath("data")

def test_get_countries(session, app, db):
    with app.test_client() as client:
        response = client.get("/api/get_countries")
        print(response.json)
        assert response.status_code == 200
