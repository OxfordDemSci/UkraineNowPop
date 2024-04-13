import pytest
from sqlalchemy import and_

from app.data_queries import (
    get_population,
    get_migration_probabilities,
    get_geodata,
    get_age_ranges,
    get_min_max_age_ranges,
    get_age_sex_population,
    get_prob_count
)
from app.datatypes import RankBy
from app.models import Migration



def test_get_population(app, session, db):
    country = "UKR"
    admin_level = 3
    date = "2024-01-01"
    admin_id = None
    age_min_male = 0
    age_max_male = 9
    age_min_female = 0
    age_max_female = 9

    result = get_population(
        country,
        admin_level,
        date,
        admin_id,
        age_min_male,
        age_max_male,
        age_min_female,
        age_max_female
    )

    assert isinstance(result, dict)
    assert "male_population" in result["pyramids"]
    assert "female_population" in result["pyramids"]
    assert "pyramids" in result
    assert isinstance(result["pyramids"]["male_population"], list)
    assert isinstance(result["pyramids"]["female_population"], list)
    assert isinstance(result["pyramids"], dict)


def test_get_migration_probabilities(app, session, db):
    # Define the input parameters
    country = 'UKR'
    admin_level = 1
    date = '2024-01-01'
    admin_id = "UA73"
    age_min_male = 0
    age_max_male = 9
    age_min_female = 0
    age_max_female = 9
    rank_by = RankBy.COUNT
    limit = 10

    # Call the function with the input parameters
    result = get_migration_probabilities(
        country,
        admin_level,
        date,
        admin_id,
        age_min_male,
        age_max_male,
        age_min_female,
        age_max_female,
        rank_by,
        limit
    )

    # Check if the result is a dictionary
    assert isinstance(result, dict)

    # Check if the dictionary contains the expected keys
    for key in result.keys():
        assert 'destination' in result[key][0]
        assert 'probability' in result[key][0]
        assert 'count' in result[key][0]


@pytest.mark.parametrize("admin_level, expected", [
    (1, 2),
    (2, 4),
    (3, 53)
])
def test_get_geodata(session, test_client, app, db, admin_level, expected):
    # Set up test data
    country = "UKR"
    admin_level = admin_level
    result = get_geodata(country, admin_level)
    assert isinstance(result, dict)

    assert "type" in result
    assert "features" in result

    assert len(result["features"]) == expected


def test_get_age_ranges(session, test_client, app, db):
    ranges = get_age_ranges("UKR")
    assert isinstance(ranges, list)
    assert ranges == [
            {"age_min": 0, "age_max": 4},
            {"age_min": 5, "age_max": 9}        
        ]


def test_get_min_max_age_ranges():
    # Set up test data
    data = [
        {"age_min": 0, "age_max": 4},
        {"age_min": 5, "age_max": 9},
        {"age_min": 10, "age_max": 14},
        {"age_min": 15, "age_max": 19}
    ]
    age_min = 6
    age_max = 12

    # Call the function
    result = get_min_max_age_ranges(data, age_min, age_max)

    # Check the result
    assert result == [
        {"age_min": 5, "age_max": 9},
        {"age_min": 10, "age_max": 14}
    ]


def test_get_age_sex_population(session, test_client, app, db):
    min_max = [{"age_min": 0, "age_max": 4}, {"age_min": 5, "age_max": 9}]
    date = "2024-01-01"
    admin_level = 1
    country = "UKR"
    sex = 1
    admin_id = "UA73"

    pop_posteriors, query, pop_pyramid_query = get_age_sex_population(
        min_max,
        date,
        admin_level,
        country,
        sex,
        admin_id
        )

    assert len(pop_posteriors) == 2
    assert len(pop_posteriors[0]) == 1000
    assert query.all() == [("UA73", 108901), ("UA80", 108854)]
    assert pop_pyramid_query.all() == [(0, 4, 54668), (5, 9, 54233)]


@pytest.mark.parametrize("destination, origin, m_min, m_max, f_min, f_max, expected_count", [
    ("UA01", "UA05", 0, 4, 0, 4, 2645),
    ("UA01", "UA05", 5, 9, 5, 9, 764),
])
def test_get_prob_count(
    session,
    test_client,
    app,
    db,
    destination,
    origin,
    m_min,
    m_max,
    f_min,
    f_max,
    expected_count
):
    admin_level = 1
    conditions = []
    conditions.append(and_(
        Migration.age_min >= m_min,
        Migration.age_min <= m_min + 4,
        Migration.age_max >= m_max - 4,
        Migration.age_max <= m_max,
        Migration.sex == 1
    ))
    conditions.append(and_(
        Migration.age_min >= f_min,
        Migration.age_min <= f_min + 4,
        Migration.age_max >= f_max - 4,
        Migration.age_max <= f_max,
        Migration.sex == 2
    ))
    date = "2024-01-07"
    country = "UKR"

    result = get_prob_count(destination, origin, admin_level, conditions, date, country)

    # Check the result
    assert result["count"] == expected_count