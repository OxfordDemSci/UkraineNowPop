# Ukraine Population Nowcasting Dashboard


**NOTE** Environment variables need to be set before running the app. This should be saved to `./.env`. Please seee the `./example_env` on the format to use and variables to set.

Example environment variables. Please do not copy these secrets

```
POSTGRES_USER=admin_user
POSTGRES_PASSWORD=secret_password
POSTGRES_READONLY=readonly_user
POSTGRES_READONLY_PASSWORD=another_secret_password
POSTGRES_DB=now_pop
POSTGRES_DB_TEST=now_pop_test
DATABASE_TABLES_DIR=app/data/db-data
basedir=<PATH_TO_PROJECTS ROOT>
SECRET_KEY="p4+N>*!aXEzeMn,K+ehIkId6@rJ=7V"
DATABASE_URL_LOCAL="postgresql://user:password@localhost:5432/now_pop"
DATABASE_URL_TEST = "postgresql://user:password@localhost:5432/now_pop_test"
DATABASE_URL="postgresql://user:password@now_pop_postgres:5432/now_pop"
DATABASE_URL_READONLY="postgresql://readonly_user:another_secret_password@now_pop_postgres:5432/now_pop"
ENV="dev"  # local, dev, prod, test
JWT_SECRET_KEY="5526BA682F324E7E816C5CBAC9293"
JWT_ACCESS_TOKEN_EXPIRES=2  # Hours before tokens expire
```

**To use any of the python code in this application, the dependencies defined in the requirements.txt file should be installed `pip install -r requirements.txt`. For some reason, the package `psycopg2` AND `psycopg2-binary` was required for the scripts and database migrations, but only `psycopg2-binary` could be installed in the Docker containers. If you have problems in running any of the scripts outside of Docker, you may need to install `psycopg2`, but do not add this to the `requirements.txt` file as this cannot be installed in Docker. This application was written using `Python 3.11.3`**

## Data Preparation
- Data should be help in `./api/app/data/db-data` as `csvs` or `geopackages`.
- There should already be a csv for `global_pcodes.csv` and a geopackage for `GEODATA.gpkg` in the repository.
- The `GEODATA.gpkg` should have a layer for each country in the project, with ALL admin levels in the same table. See `UKR` for example.
- Two additional tables should be added before starting the service: `migration.csv` and `pop.csv`. These should either be generated randomly through the script `./scripts/make_dummy_pop_migration.py`, which will save the output to the `db-data` folder. This table should be added BEFORE launching the database container to prevent memory issues.
- Once the tables are in place, the API can be run as a standalone service, or together with the UI container.

## Running API as a standalone service for debugging and development
- **NOTE** The environment variable for `ENV` should be set to `local` for this to work.
- `cd` into './scripts/postgres_local_dev' and run `docker-compose up --build` to launch the api container
- `cd` back into the api root (`./api`) and run the `scripts/insert_data.py` script - `python scripts/insert_data.py` to add the tables to the database.
- Launch the flask app from the same directory: `python ./wsgi.py`
- Interaction with the API can be carried out from `localhost:8080/api/ui`, where the different endpoints can be tested.


## Running API and UI together to view the dashboard
- **NOTE** The environment variable for `ENV` should be set to `dev` or higher for this to work.
The dashboard frontend and backend are all launched and linked together using Docker and docker-compose (you will need to install these on your computer for this to work). To run the full application, `cd` to the current location in your terminal and run `docker-compose up -d --build` (omit the `-d` to run the application log messages to the terminal). Any changes made to the code will not be reflected in the running app, and the app will need to be stopped and rebuilt for changes to take effect. 

After the app has started and the tables inserted into the database, the frontend can be accessed from `http://127.0.0.1/` and the API documentation/sandbox from `http://localhost:8000/api/ui/` - in this Swagger UI, you can see how queries can be built using different parameters, and test queries with different inputs.


## Authentication
To view data at Administration Levels > 1, users need to be authenticated. This can be done through the `swagger-ui` at `localhost:8080/api/ui`. Post your credentials to the `POST /login` in the ui. If successful, a token will be returned. You can then either authenticate the whole service by clicking the `Authorize` button on the top right of the UI (green with a padlock) and pasting the token into the box, or click on the padlocks beside the individual endpoints in the UI and pasting in the token to authenticate only those endpoints. 

**NOTE That due to the size of some of the json responses, it might be better to do all of this through `POSTMAN` as the `swagger-ui` can crash with larger responses.**

The token will last for the number of hours set in `JWT_ACCESS_TOKEN_EXPIRES` in the environment variables.


## Database migrations - only required if changes made to DB Schema
The application uses `alembic` [https://alembic.sqlalchemy.org/en/latest/](https://alembic.sqlalchemy.org/en/latest/) to apply version control to changes to the database schema. This process only needs to be followed when tables schemas or their fields' data-types change. The database schema is defined in `api/app/models.py`, with a model class for each table. A database should be running (from either of the docker-compose lines mentioned above) for the following process to work.
1. If the database is running locally (independent of the UI container), the environment `ENV` variable should be set to `local`, otherwise `dev`.
2. Make the migration file:\
Make changes to the model/db schema and run `alembic revision --autogenerate -m "<Message-detailing-the-database-change>`
3. Check the generated script in `./api/alembic/versions` - These might need editing
4. Migrate the change to the database:\
Run `alembic upgrade head`


### Unit and end-to-end tests
Tests have been written to test the application's data queries and the endpoints using `pytest` [https://docs.pytest.org/en/7.4.x/](https://docs.pytest.org/en/7.4.x/). A test application and database are defined in `tests/conftest.py` with tables from `tests/test_data` being automatically inserted into the test database for the tests, which is torn down after the tests.\
All tests can be run by running `pytest tests --disable-warnings`

**If the database schema is changed, the test data and unit tests will need to be changed accordingly to reflect this.**


### Linting
Following any changes, linting can be run on the code through the following commands from within the `api` folder:\
1. `python -m black app`
2. `python -m flake8 app`
3. `python -m isort app`
4. `python -m mypy app`

