import json
from pathlib import Path

import geopandas as gpd  # type: ignore
import pandas as pd  # type: ignore
import requests


def subset_incident_data_in_buffer(
    edges: gpd.GeoDataFrame,
    incident_data: Path | str,
    incident_out_file: Path | str,
    buffer_distance: int,
    crs: int,
    is_acled: bool = True,
    index_col: str = "event_id_cnty",
) -> gpd.GeoDataFrame:
    """Subset incidents to those within buffer of routes' edges. If ACLED data is used, the function will subset
    columns to those required for analysis.

    Args:
        edges (gpd.GeoDataFrame): Edges dataframe extracted from full network
        incident_data (Path | str): Incident data
        incident_out_file (Path | str): Output location
        buffer_distance (int): Buffer distance in which to subset incidents
        crs (int): CRS
        is_acled (bool, optional): Is the incident data ACLED data. Defaults to True. If True, ACLED columns will
            be subset.
        index_col (str, optional): Index column. Defaults to "event_id_cnty".

    Returns:
        gpd.GeoDataFrame: Geodataframe of incidents within buffer
    """
    if is_acled:
        incident = get_acled_data_from_csv(Path(incident_data), crs)
    else:
        make_incident_data(pd.read_csv(Path(incident_data)), crs)
    incident_buffered = (
        incident.set_index(index_col).copy().buffer(buffer_distance).to_frame()
    )
    incident_join = incident_buffered.sjoin(edges, how="inner", predicate="intersects")
    incident = incident[incident[index_col].isin(incident_join.index)]
    incident[[x for x in incident.columns if x != "geometry"]].to_csv(
        Path(incident_out_file), index=False
    )
    return incident


def get_acled_data_from_csv(
    csv_path: Path | str,
    crs: int,
    outfile: Path | str | None = None,
) -> gpd.GeoDataFrame:
    """Read ACLED data from csv and subset columns

    Args:
        csv_path (Path | str): Path to csv
        crs (int): CRS
        outfile (Path | str | None, optional): Output location if required. Defaults to None.

    Returns:
        gpd.GeoDataFrame: Subset ACLED data
    """
    df = pd.read_csv(Path(csv_path))
    df = df[
        [
            "event_id_cnty",
            "event_date",
            "year",
            "disorder_type",
            "event_type",
            "sub_event_type",
            "latitude",
            "longitude",
            "fatalities",
        ]
    ].copy()
    return make_incident_data(df, crs, outfile=outfile)


def get_acled_data_from_api(
    api_key: str,
    email: str,
    country: str,
    start_date: str,
    end_date: str,
    crs: int,
    accept_acleddata_terms: bool,
    outfile: Path | str | None = None,
) -> gpd.GeoDataFrame:
    """Get ACLED data from the ACLED API in date range. This function will return a geodataframe of the data and
    requires an API key and email address to access the data. The data will be subset to columns required for analysis.

    Args:
        api_key (str): API key for ACLED
        email (str): Email used to access ACLED data
        country (str): Full country name (i.e. "Ukraine" not "UKR")
        start_date (str): Start date in format "YYYY-MM-DD"
        end_date (str): End date in format "YYYY-MM-DD"
        crs (int): Local CRS in which to project points
        accept_acleddata_terms (bool): Indicate acceptance of ACLED terms - See ACLED API documentation
        outfile (Path | str | None, optional): Location to save output if required. Defaults to None.

    Raises:
        ValueError: If no data is returned from the API

    Returns:
        gpd.GeoDataFrame: GeoDataFrame of ACLED point data in specified CRS
    """
    df_list = []
    page = 1
    while True:
        url = (
            f"https://api.acleddata.com/acled/read?terms={accept_acleddata_terms}"
            f"&key={api_key}"
            f"&email={email}"
            f"&country={country}"
            f"&event_date={start_date}|{end_date}&event_date_where=BETWEEN"
            f"&page={page}"
            f"&export_type=csv"
        )
        response = requests.get(url)
        if data := json.loads(response.text).get("data"):
            df_list.append(pd.DataFrame(data))
            page += 1
        else:
            break
    if df_list:
        df = pd.concat(df_list)
        df = df[
            [
                "event_id_cnty",
                "event_date",
                "year",
                "disorder_type",
                "event_type",
                "sub_event_type",
                "latitude",
                "longitude",
                "fatalities",
            ]
        ].copy()
        return make_incident_data(df, crs, outfile=outfile)
    raise ValueError("No data returned from ACLED API")
