from collections import defaultdict
from typing import List, Dict, Any, Union

import numpy as np
import sqlalchemy
from sqlalchemy import and_, distinct, func, or_
from sqlalchemy.engine import Row

from app import db

from .datatypes import RankBy
from .models import AdminUnits, Countries, Languages, Migration, Population


def get_geodata(country: str, admin_level: int) -> dict:
    admin_units = (
        db.session.query(AdminUnits)
        .filter_by(country=country, admin_level=admin_level)
        .all()
    )
    return {
        "type": "FeatureCollection",
        "features": [au.to_geojson for au in admin_units],
    }


def get_countries() -> list[dict]:
    countries = db.session.query(Countries.country).distinct().all()
    data = [{"country": country.name} for country, in countries]
    for country in data:
        country["centroid"] = db.session.query(
            func.ST_AsText(
                func.ST_Centroid(
                    func.ST_Collect(AdminUnits.geometry).filter(
                        AdminUnits.country == country["country"],
                        AdminUnits.admin_level == 1,
                    )
                )
            )
        ).first()[0]
        country["bounding_box"] = db.session.query(
            func.ST_AsText(
                func.ST_Envelope(
                    func.ST_Collect(AdminUnits.geometry).filter(
                        AdminUnits.country == country["country"],
                        AdminUnits.admin_level == 1,
                    )
                )
            )
        ).first()[0]
    return data


def get_age_ranges(country: str) -> list[dict]:
    age_ranges = (
        db.session.query(Population.age_min, Population.age_max)
        .filter(Population.country == country)
        .distinct()
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
    init_data["admin_names"] = [
        {"adm1_name": adm1, "adm2_name": adm2, "adm3_name": adm3}
        for adm1, adm2, adm3 in admin_names
    ]
    dates: List[Row] = (
        db.session.query(distinct(Population.day))
        .filter(Population.country == country)
        .all()
    )
    init_data["dates"] = [date for date, in dates]
    languages = (
        db.session.query(Languages.lan2, Languages.lan3)
        .filter(Languages.country == country)
        .all()
    )
    init_data["languages"] = [
        {"lan1": "English", "lan2": lan2, "lan3": lan3} for lan2, lan3 in languages
    ]
    return init_data


def get_min_max_age_ranges(data: list, age_min: int, age_max: int):
    age_ranges = data
    result = []
    for age_range in age_ranges:
        if (
            age_range["age_min"] <= age_min < age_range["age_max"]
            or age_range["age_min"] < age_max <= age_range["age_max"]
        ):
            result.append(age_range)
    if len(result) == 1:
        result.append(result[0])
    return result


def get_age_sex_population(
    min_max: list[dict],
    date: str,
    admin_level: int,
    country: str,
    sex: int,
    admin_id: str | None = None,
) -> tuple[list, sqlalchemy.orm.query.Query, sqlalchemy.orm.query.Query]:
    sub_query = db.session.query(Population.id).filter(
        Population.age_min >= min_max[0]["age_min"],
        Population.age_min <= min_max[1]["age_min"],
        Population.age_max >= min_max[0]["age_max"],
        Population.age_max <= min_max[1]["age_max"],
        Population.sex == sex,
        Population.admin_level == admin_level,
        Population.day == date,
        Population.country == country,
    )
    pop_posteriors_query = (
        db.session.query(func.array_agg(Population.pop_posterior))
        .filter(Population.id.in_(sub_query))
        .filter(Population.pcode == admin_id if admin_id else True)
    )
    pop_pyramid_query = (
        db.session.query(
            Population.age_min,
            Population.age_max,
            func.sum(Population.pop).label("population"),
        )
        .filter(
            Population.id.in_(sub_query),
            Population.pcode == admin_id if admin_id else True,
        )
        .group_by(Population.age_min, Population.age_max)
    )
    pop_posteriors = pop_posteriors_query.all()[0][0]
    query = (
        db.session.query(Population.pcode, func.sum(Population.pop).label("population"))
        .filter(Population.id.in_(sub_query))
        .group_by(Population.pcode)
    )
    return pop_posteriors, query, pop_pyramid_query


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
    if age_min_male is not None and age_max_male is not None:
        male_min_max = get_min_max_age_ranges(age_ranges, age_min_male, age_max_male)
    if age_min_female is not None and age_max_female is not None:
        female_min_max = get_min_max_age_ranges(age_ranges, age_min_female, age_max_female)

    pop: Dict[str, Dict[str, Any]] = {}

    if male_min_max:
        m_pop_posteriors, male_query, male_pop_pyramid_query = get_age_sex_population(
            male_min_max, date, admin_level, country, 1, admin_id
        )
        male_results = male_query.all()
        for pcode, population in male_results:
            if pcode in pop:
                pop[pcode]["male_population"] = population
            else:
                pop[pcode] = {"male_population": population}
    else:
        male_results = []

    if female_min_max:
        (
            f_pop_posteriors,
            female_query,
            female_pop_pyramid_query,
        ) = get_age_sex_population(
            female_min_max, date, admin_level, country, 2, admin_id
        )
        female_results = female_query.all()
        for pcode, population in female_results:
            if pcode in pop:
                pop[pcode]["female_population"] = population
            else:
                pop[pcode] = {"female_population": population}
    else:
        female_results = []
    population_data: Dict[str, Union[Dict[str, Any], List[Any]]] = {}
    population_data["population_totals"] = {}
    concatenated = np.concatenate((f_pop_posteriors, m_pop_posteriors), axis=0)
    population_data["pop_posteriors"] = np.sum(concatenated, axis=0).tolist()
    for (pcode, f_pop), (_, m_pop) in zip(female_results, male_results):
        population_data["population_totals"][pcode] = f_pop + m_pop

    pop: Dict[str, List[Any]] = {"male_population": [], "female_population": []}

    if male_min_max:
        male_pyramid_results = male_pop_pyramid_query.all()
        for age_min, age_max, population in male_pyramid_results:
            pop["male_population"].append(
                {"age_min": age_min, "age_max": age_max, "population": population}
            )

    if female_min_max:
        female_pyramid_results = female_pop_pyramid_query.all()
        for age_min, age_max, population in female_pyramid_results:
            pop["female_population"].append(
                {"age_min": age_min, "age_max": age_max, "population": population}
            )

    total_population_query = db.session.query(
        func.sum(Population.pop).label("population")
    ).filter(
        Population.admin_level == admin_level,
        Population.day == date,
        Population.country == country,
    )
    if admin_id:
        total_population_query = total_population_query.filter(
            Population.pcode == admin_id
        )
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
    rank_by: RankBy = RankBy.COUNT,
    limit: int = 10,
) -> dict:
    age_ranges = get_age_ranges(country)
    male_min_max = get_min_max_age_ranges(age_ranges, age_min_male, age_max_male)
    female_min_max = get_min_max_age_ranges(age_ranges, age_min_female, age_max_female)
    rank_col = (
        func.sum(Migration.probability)
        if rank_by == RankBy.PROBABILITY
        else func.sum(Migration.count)
    )
    probabilities = defaultdict(list)

    closest_date_subquery = db.session.query(
        func.min(func.abs(func.date(date) - func.date(Migration.day)))
    ).subquery()

    conditions = []

    if male_min_max:
        conditions.append(
            and_(
                Migration.age_min >= male_min_max[0]["age_min"],
                Migration.age_min <= male_min_max[1]["age_min"],
                Migration.age_max >= male_min_max[0]["age_max"],
                Migration.age_max <= male_min_max[1]["age_max"],
                Migration.sex == 1,  # 1 for male
            )
        )

    if female_min_max:
        conditions.append(
            and_(
                Migration.age_min >= female_min_max[0]["age_min"],
                Migration.age_min <= female_min_max[1]["age_min"],
                Migration.age_max >= female_min_max[0]["age_max"],
                Migration.age_max <= female_min_max[1]["age_max"],
                Migration.sex == 2,  # 2 for female
            )
        )
    sub_query = (
        db.session.query(
            Migration.origin,
            Migration.destination,
            func.sum(Migration.probability).label("probability"),
            func.sum(Migration.count).label("count"),
        )
        .filter(
            or_(*conditions),
            Migration.admin_level == admin_level,
            func.abs(func.date(date) - func.date(Migration.day))
            == closest_date_subquery,
            Migration.country == country,
            Migration.origin == admin_id if admin_id else True,
        )
        .group_by(Migration.origin, Migration.destination)
        .order_by(rank_col.desc())
        .limit(limit)
    )

    results = sub_query.all()
    if results:
        for origin, destination, probability, count in results:
            probabilities[origin].append(
                {"destination": destination, "probability": probability, "count": count}
            )
            probabilities[destination].append(
                get_prob_count(
                    destination, origin, admin_level, conditions, date, country
                )
            )

    return dict(probabilities)


def get_prob_count(
    destination: str,
    origin: str,
    admin_level: int,
    conditions: list,
    date: str,
    country: str,
) -> dict:
    closest_date_subquery = db.session.query(
        func.min(func.abs(func.date(date) - func.date(Migration.day)))
    ).subquery()
    query = db.session.query(
        func.sum(Migration.probability).label("probability"),
        func.sum(Migration.count).label("count"),
    ).filter(
        or_(*conditions),
        Migration.admin_level == admin_level,
        func.abs(func.date(date) - func.date(Migration.day)) == closest_date_subquery,
        Migration.country == country,
        Migration.origin == destination,
        Migration.destination == origin,
    )
    result = query.one()
    return {
        "destination": origin,
        "probability": result.probability,
        "count": result.count,
    }
