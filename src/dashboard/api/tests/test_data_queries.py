from app.data_queries import get_population, get_migration_probabilities

from app.datatypes import RankBy


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