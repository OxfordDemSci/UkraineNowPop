from sqlalchemy import distinct, func
from collections import defaultdict


from .models import AdminUnits, Population, Migration, Countries, Languages
from app import db


def get_geodata(country: str, admin_level: int) -> dict:
    admin_units = db.session.query(AdminUnits).filter_by(country=country, admin_level=admin_level).all()
    return {
        "type": "FeatureCollection",
        "features": [au.to_geojson for au in admin_units],
    }


def get_age_ranges(country: str) -> list[dict]:
    age_ranges = (
        db.session.query(Population.age_min, Population.age_max)
        .filter(Population.country == country).distinct()
        .all()
    )
    return [{"age_min": age_min, "age_max": age_max} for age_min, age_max in age_ranges]

def init(country: str) -> dict:
    init_data = {}
    age_ranges = get_age_ranges(country)
    init_data["age_ranges"] = age_ranges
    admin_names = (
        db.session.query(Countries.adm1_name, Countries.adm2_name, Countries.adm3_name)
        .filter(Countries.country == country)
        .all()
    )
    init_data["admin_names"] = [{"adm1_name": adm1, "adm2_name": adm2, "adm3_name": adm3} for adm1, adm2, adm3 in admin_names]
    dates = db.session.query(distinct(Population.day)).filter(Population.country == country).all()
    init_data["dates"] = [date for date, in dates]
    languages = db.session.query(Languages.lan2, Languages.lan3).filter(Languages.country == country).all()
    init_data["languages"] = [{"lan1": "English", "lan2": lan2, "lan3": lan3} for lan2, lan3 in languages]
    return init_data


def get_min_max_age_ranges(data: list, age_min: int, age_max: int):
    age_ranges = data
    result = []
    for age_range in age_ranges:
        if age_range["age_min"] <= age_min < age_range["age_max"] or age_range["age_min"] < age_max <= age_range["age_max"]:
            result.append(age_range)
    if len(result) == 1:
        result[1] = result[0]
    return result


def get_age_sex_population(
        min_max: list[dict],
        date: str,
        admin_level: int,
        country: str,
        sex: int,
) -> list[tuple]:
    sub_query = (
        db.session.query(
            Population.id
            # Population.pcode,
            # func.sum(Population.pop).label('population'),
            # func.array_agg(Population.pop_posterior).label('pop_posterior'),
        ).filter(
            Population.age_min >= min_max[0]["age_min"],
            Population.age_min <= min_max[1]["age_min"],
            Population.age_max >= min_max[0]["age_max"],
            Population.age_max <= min_max[1]["age_max"],
            Population.sex == sex,
            Population.admin_level == admin_level,
            Population.day == date,
            Population.country == country
        )#.group_by(Population.pcode)
    )
    pop_posteriors = db.session.query(func.array_agg(Population.pop_posterior)).filter(Population.id.in_(sub_query)).all()[0][0]
    query = db.session.query(Population.pcode, func.sum(Population.pop).label('population')).filter(Population.id.in_(sub_query)).group_by(Population.pcode)
    return pop_posteriors, query



def get_population(
        country: str,
        admin_level: int,
        date: str,
        admin_id: str | None = None,
        age_min_male: int | None = None,
        age_max_male: int | None = None,
        age_min_female: int | None = None,
        age_max_female: int | None = None,
) -> dict:
    age_ranges = get_age_ranges(country)
    male_min_max = get_min_max_age_ranges(age_ranges, age_min_male, age_max_male)
    female_min_max = get_min_max_age_ranges(age_ranges, age_min_female, age_max_female)

    pop = {}

    if male_min_max:
        m_pop_posteriors, male_query = get_age_sex_population(
            male_min_max,
            date,
            admin_level,
            country,
            1                        
            )
        male_results = male_query.all()
        for pcode, population in male_results:
            if pcode in pop:
                pop[pcode]["male_population"] = population
                # pop[pcode]["male_pop_posteriors"] = m_pop_posteriors
            else:
                pop[pcode] = {"male_population": population,} #"male_pop_posteriors": m_pop_posteriors}
    else:
        male_query = []

    if female_min_max:
        f_pop_posteriors, female_query = get_age_sex_population(
            female_min_max,
            date,
            admin_level,
            country,
            2
        )
        female_results = female_query.all()
        for pcode, population in female_results:
            if pcode in pop:
                pop[pcode]["female_population"] = population
                #pop[pcode]["female_pop_posteriors"] = f_pop_posteriors
            else:
                pop[pcode] = {"female_population": population,} #"female_pop_posteriors": f_pop_posteriors}
    else:
        female_results = []
    population_data = {}
    population_data["population_totals"] = {}
    import numpy as np
    concatenated = np.concatenate((f_pop_posteriors, m_pop_posteriors), axis=0)
    population_data["pop_posteriors"] = np.sum(concatenated, axis=0).tolist()
    for (pcode, f_pop), (_, m_pop) in zip(female_results, male_results):
        population_data["population_totals"][pcode] = f_pop + m_pop
        
    pop = {"male_population": [], "female_population": []}

    if male_min_max:
        male_query = (
            db.session.query(Population.age_min, Population.age_max, func.sum(Population.pop).label('population'))
            .filter(
                Population.age_min >= male_min_max[0]["age_min"],
                Population.age_min <= male_min_max[1]["age_min"],
                Population.age_max >= male_min_max[0]["age_max"],
                Population.age_max <= male_min_max[1]["age_max"],
                Population.sex == 1,
                Population.admin_level == admin_level,
                Population.day == date,
                Population.country == country
            )
            .group_by(Population.age_min, Population.age_max)
        )
        if admin_id:
            male_query = male_query.filter(Population.pcode == admin_id)
        male_results = male_query.all()
        for age_min, age_max, population in male_results:
            pop["male_population"].append({"age_min": age_min, "age_max": age_max, "population": population})

    if female_min_max:
        female_query = (
            db.session.query(Population.age_min, Population.age_max, func.sum(Population.pop).label('population'))
            .filter(
                Population.age_min >= female_min_max[0]["age_min"],
                Population.age_min <= female_min_max[1]["age_min"],
                Population.age_max >= female_min_max[0]["age_max"],
                Population.age_max <= female_min_max[1]["age_max"],
                Population.sex == 2,
                Population.admin_level == admin_level,
                Population.day == date,
                Population.country == country
            )
            .group_by(Population.age_min, Population.age_max)
        )
        if admin_id:
            female_query = female_query.filter(Population.pcode == admin_id)
        female_results = female_query.all()
        for age_min, age_max, population in female_results:
            pop["female_population"].append({"age_min": age_min, "age_max": age_max, "population": population})

    # Calculate total population
    total_population_query = (
        db.session.query(func.sum(Population.pop).label('population'))
        .filter(
            Population.admin_level == admin_level,
            Population.day == date,
            Population.country == country
        )
    )
    if admin_id:
        total_population_query = total_population_query.filter(Population.pcode == admin_id)
    total_population = total_population_query.scalar()
    pop["total_population"] = total_population

    pyramid_name = "pyramids" if admin_id is None else f"pyramid_{admin_id}"
    population_data[pyramid_name] = pop
    return population_data
    

def get_migration_probabilities(
        country: str,
        admin_level: int,
        date: str,
        admin_id: str | None = None,
        age_min_male: int | None = None,
        age_max_male: int | None = None,
        age_min_female: int | None = None,
        age_max_female: int | None = None,
) -> dict:
    age_ranges = get_age_ranges(country)
    male_min_max = get_min_max_age_ranges(age_ranges, age_min_male, age_max_male)
    female_min_max = get_min_max_age_ranges(age_ranges, age_min_female, age_max_female)
    
    # FIXME SHOULD PROBABILITIES BE SUMMED OR AVERAGED?
    
    probabilities = defaultdict(list)
    if male_min_max:
        male_query = (
            db.session.query(Migration.origin, Migration.destination, func.sum(Migration.probability).label('probability'))
            .filter(
                Migration.age_min >= male_min_max[0]["age_min"],
                Migration.age_min <= male_min_max[1]["age_min"],
                Migration.age_max >= male_min_max[0]["age_max"],
                Migration.age_max <= male_min_max[1]["age_max"],
                Migration.sex == 0,  # FIXME: This should be 1
                Migration.admin_level == admin_level,
                Migration.day == date,
                Migration.country == country
            )
            .group_by(Migration.origin, Migration.destination)
        )
        if admin_id:
            male_query = male_query.filter(Migration.origin == admin_id)
    if female_min_max:
        female_query = (
            db.session.query(Migration.origin, Migration.destination, func.sum(Migration.probability).label('probability'))
            .filter(
                Migration.age_min >= female_min_max[0]["age_min"],
                Migration.age_min <= female_min_max[1]["age_min"],
                Migration.age_max >= female_min_max[0]["age_max"],
                Migration.age_max <= female_min_max[1]["age_max"],
                Migration.sex == 1,  # FIXME this should be 2
                Migration.admin_level == admin_level,
                Migration.day == date,
                Migration.country == country
            )
            .group_by(Migration.origin, Migration.destination)
        )
        if admin_id:
            female_query = female_query.filter(Migration.origin == admin_id)

    male_results = male_query.all()
    female_results = female_query.all()
    results = male_results + female_results
    if results:
        for origin, destination, probability in male_results:
            probabilities[origin].append({"destination": destination, "probability": probability})
    return dict(probabilities)
