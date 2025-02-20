import py_helpers
from py_helpers import logger
import os
from datetime import datetime, timedelta
import pandas as pd
import numpy as np


def agesex_cols(gender, age_min, age_max):
    """
    Return age-sex column names from Meta Marketing API parameters
    :param gender: int list [0, 1, 2]
    :param age_min: int
    :param age_max: int
    :return: list
    """

    gend = ["T", "M", "F"]
    gend = [gend[x] for x in gender]

    result = []
    for g in gend:
        result += [
            g + "_" + "_".join([str(x).zfill(2), str(x + 4).zfill(2)])
            for x in list(range(age_min, age_max, 5))
        ]
        result += [g + "_" + str(age_max) + "Plus"]

    return result


# age-sex columns
def agesex_col_to_query_args(col):
    """
    Convert age-sex column name into arguments for querying the mySocialWatcher API or database
    :param col: str
    :return: dict
    """

    parms = col.split("_")

    if "Plus" in parms[1]:
        parms[1] = parms[1].strip("Plus")
        parms.append("999")

    agesex_key = ["T", "M", "F"]

    result = dict()
    result["gender"] = agesex_key.index(parms[0])
    result["age_min"] = parms[1]
    result["age_max"] = parms[2]

    return result


def baseline_audience(
    base_date,
    country,
    geo_level,
    geo_keys,
    platform="facebook",
    agesex=agesex_cols(gender=[2, 1], age_min=15, age_max=65),
    language_name="all",
    location_types='["recent"]',
):
    """
    Determine baseline social media audience by age and sex for a region and date
    :param base_date: str YYYY-MM-DD
    :param country: str ISO-2
    :param geo_level: str ('countries', 'regions', 'cities')
    :param geo_keys: int list (ignored if geo_level='countries')
    :param platform: str ('facebook', 'instagram')
    :param agesex: str list (e.g. returned from agesex_cols())
    :param language_name: str
    :param location_types: str (e.g. '["home", "recent"]')
    :return: pandas.DataFrame
    """
    # base date to datetime format
    base_date = datetime.strptime(base_date, "%Y-%m-%d")

    # prepare result dataframe
    result = pd.DataFrame()
    result["geo_key"] = np.repeat(geo_keys, len(agesex))
    result["agesex"] = agesex * len(geo_keys)
    result["platform"] = platform
    result["geo_level"] = geo_level
    result["country"] = country
    result["language_name"] = language_name
    result["location_types"] = location_types

    for idx, row in result.iterrows():
        logger.info(": ".join(list(row[["platform", "country", "geo_key", "agesex"]])))

        # API arguments
        args = {
            "platform": row["platform"],
            "country": row["country"],
            "geo_level": row["geo_level"],
            "geo_key": row["geo_key"],
            "language_name": row["language_name"],
            "location_types": row["location_types"],
        }
        args.update(agesex_col_to_query_args(row["agesex"]))
        if args["geo_level"] == "countries":
            args["geo_key"] = None

        # API queries, looping forward until a date returns data
        delta_date = -1
        api_response = pd.DataFrame()
        while len(api_response) == 0 & delta_date < 10:
            delta_date += 1
            query_date = base_date + timedelta(days=delta_date)
            args.update(
                {
                    "date_start": query_date.strftime("%Y-%m-%d"),
                    "date_end": query_date.strftime("%Y-%m-%d"),
                }
            )
            api_response = py_helpers.query_api(endpoint="query_clean", args=args)
        result.at[idx, "dau"] = api_response.iloc[0,]["dau"]
        result.at[idx, "mau_lower"] = api_response.iloc[0,]["mau_lower"]
        result.at[idx, "mau_upper"] = api_response.iloc[0,]["mau_upper"]
        result.at[idx, "date"] = api_response.iloc[0,]["collection_date"]

    return result


def daily_audience(
    date_start,
    date_end,
    country,
    geo_level,
    geo_keys,
    platform="facebook",
    agesex=agesex_cols(gender=[2, 1], age_min=15, age_max=65),
    collection_name=None,
    language_name="all",
    location_types='["recent"]',
):
    """
    Retrieve daily social media audience by age and sex for a region and range of dates
    :param date_start: str YYYY-MM-DD
    :param date_end: str YYYY-MM-DD
    :param country: str ISO-2
    :param geo_level: str ('countries', 'regions', 'cities', 'custom_locations')
    :param geo_keys: int list (ignored if geo_level='countries')
    :param platform: str ('facebook', 'instagram')
    :param agesex: str list (e.g. returned from agesex_cols())
    :param collection_name: str
    :param language_name: str
    :param location_types: str (e.g. '["home", "recent"]')
    :return: pandas.DataFrame
    """

    # base date to datetime format
    date_start = datetime.strptime(date_start, "%Y-%m-%d")
    date_end = datetime.strptime(date_end, "%Y-%m-%d")

    results = []
    for geo_key in geo_keys:
        # geo_key = geo_keys[0]
        for demgroup in agesex:
            # demgroup = agesex[0]

            logger.info(": ".join([platform, country, geo_key, demgroup]))

            # API arguments
            args = {
                "date_start": date_start,
                "date_end": date_end,
                "platform": platform,
                "country": country,
                "geo_level": geo_level,
                "geo_key": geo_key,
                "language_name": language_name,
                "location_types": location_types,
            }
            args.update(agesex_col_to_query_args(demgroup))
            if args["geo_level"] == "countries":
                args["geo_key"] = None
            if collection_name is not None:
                args["collection_name"] = collection_name

            response = py_helpers.query_api(endpoint="query_clean", args=args)
            response["agesex"] = demgroup

            # API query
            results.append(response)

    return pd.concat(results)
