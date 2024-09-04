import geopandas as gpd
import pandas as pd
import os
from dotenv import load_dotenv
from pathlib import Path

# Load the .env file
env_path = Path('.') / '.env'
load_dotenv(env_path)
in_dir = Path(os.getenv('in_dir'))
out_dir = Path(os.getenv('out_dir'))

boundaries_oblast = gpd.read_file(in_dir / 'COD-AB' / 'ukr_admbnda_sspe_20230201_SHP'/'ukr_admbnda_adm1_sspe_20230201.shp').rename(
columns={'ADM1_PCODE': 'pcode'}
)
boundaries_oblast = gpd.read_file(in_dir / 'COD-AB' / 'ukr_admbnda_sspe_20230201_SHP'/'ukr_admbnda_adm2_sspe_20230201.shp').rename(
columns={'ADM1_PCODE': 'pcode'}
)



def count_points_in_polygons(gdf_points, gdf_polygons, varname='count', start_date='2022-02-26', end_date='2024-05-15'):
    """
    Count the number of points within each polygon and return a GeoDataFrame with this information.

    Parameters:
    gdf_points (GeoDataFrame): GeoDataFrame containing points.
    gdf_polygons (GeoDataFrame): GeoDataFrame containing polygons.

    Returns:
    GeoDataFrame: A GeoDataFrame containing polygons with a new column 'point_count' showing the number of points within each polygon.
    """
    # Perform a spatial join to count points in each polygon
    gdf_points = gdf_points.to_crs(gdf_polygons.crs)
    joined = gpd.sjoin(gdf_points, gdf_polygons[['pcode', 'geometry']], how='inner', predicate='within')

    # Count points in each polygon
    point_counts = joined.groupby(['pcode', 'time']).size()

    point_counts_df = point_counts.reset_index(name=varname)

    # Generate all combinations of pcode and time
    all_pcodes = gdf_polygons['pcode'].unique()
    all_dates = pd.date_range(start=start_date, end=end_date,
                              freq='D').strftime('%Y-%m-%d')
    all_combinations = pd.MultiIndex.from_product([all_pcodes, all_dates], names=['pcode', 'time'])

    # Reindex point_counts to include all combinations and fill missing with 0
    point_counts_df = point_counts_df.set_index(['pcode', 'time']).reindex(all_combinations,
                                                                                 fill_value=0).reset_index()

    return point_counts_df

# 1. Process ACLED
acled = gpd.read_file(out_dir / 'covariates' / 'raw' / 'acled.gpkg').rename(
columns={'event_date': 'time'}
)

acled_oblast = count_points_in_polygons(acled, boundaries_oblast, 'acled_all')

# Define the filtering conditions in acled and corresponding variable name
conditions = {
    'acled_withfatalities': acled['fatalities'] != '0',
    'acled_disorderPolitical': acled['disorder_type'] == 'Political violence',
    'acled_disorderStrategic': acled['disorder_type'] == 'Strategic developments',
    'acled_disorderDemonstration': acled['disorder_type'] == 'Demonstrations',
    'acled_disorderPoliticalDemonstration': acled['disorder_type'] == 'Political violence; Demonstrations',
    'acled_eventExplosion': acled['event_type'] == 'Explosions/Remote violence',
    'acled_eventBattle': acled['event_type'] == 'Battles',
    'acled_eventStrategic': acled['event_type'] == 'Strategic developments',
    'acled_eventAgainstCivilians': acled['event_type'] == 'Violence against civilians',
    'acled_eventProtest': acled['event_type'] == 'Protests',
    'acled_eventRiot': acled['event_type'] == 'Riots'
}

# Calculate counts for each condition

for key, condition in conditions.items():
    filtered_acled = acled[condition]
    acled_oblast[key] = count_points_in_polygons(filtered_acled, boundaries_oblast)['count']

acled_oblast = pd.DataFrame(acled_oblast)

acled_oblast.to_csv(out_dir / 'covariates' / 'interim' / 'acled_oblast.csv',
                  index=False)


# Combine all summaries in one output

covariates_file_paths = [
    out_dir / 'covariates' / 'interim' / 'acled_oblast.csv'
]
covariates = [pd.read_csv(file) for file in covariates_file_paths]

covariates = pd.concat(covariates, axis=0, ignore_index=True)

covariates.to_csv(out_dir / 'covariates' / 'final' / 'covariates_location.csv',
                  index=False)