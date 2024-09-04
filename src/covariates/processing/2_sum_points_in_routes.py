import geopandas as gpd
import pandas as pd
import net_friction.calculations as calc
import net_friction.data_preparation as prep
import os
from dotenv import load_dotenv
from pathlib import Path
import numpy as np

# Load the .env file
env_path = Path('.') / '.env'
load_dotenv(env_path)
in_dir = Path(os.getenv('in_dir'))
out_dir = Path(os.getenv('out_dir'))

# load data
edges = gpd.read_file(out_dir / 'covariates' / 'network' /'network_edges_oblast.gpkg')
matrix = pd.read_csv(out_dir / 'covariates' / 'network' /'network_matrix_oblast.csv')
matrix['edge_geometries_ids'] = matrix['edge_geometries_ids'].apply(lambda x: np.array(x.split(','), dtype=int))

def sum_points_in_routes(
    matrix, edges, buffer_distance_in_meters, points_file_path, aggregation_functions,
        output_file_path, date_start = '2022-02-26', date_end = '2024-05-15'
):
    # Load the point data data
    points = gpd.read_file(points_file_path)

    # Get incidents in routes
    incidents_in_routes = calc.get_incidents_in_route_sjoin(
        matrix, edges, points, buffer_distance_in_meters
    )

    # Calculate distances to routes
    incidents_in_routes_list = []
    for (from_pcode, to_pcode), group_df in incidents_in_routes.set_index(
            ["from_pcode", "to_pcode"]
        ).groupby(level=[0, 1]):
        incidents_in_routes_list.append(
            calc.get_distances_to_route(group_df, matrix, edges)
        )
    incidents_in_routes_df = pd.concat(incidents_in_routes_list)

    # Aggregate incidents
    df_grouped = (
        incidents_in_routes_df.groupby(["event_date", "from_pcode", "to_pcode"])
        .agg(aggregation_functions)
        .reset_index()
    )

    df_grouped.columns =  [
        f"{col[1]}" if col[1] != '' else f"{col[0]}"
        for col in df_grouped.columns
    ]

    # Fill in missing routes
    df_grouped_filled = prep.fill_missing_routes(
        df_grouped, matrix, date_start=date_start, date_end=date_end
    )

    df_grouped_filled.to_csv(output_file_path, index=False)

    return df_grouped_filled

for buffer_distance_in_meters in [1000, 5000, 10000]:
    df_grouped_filled = sum_points_in_routes(
        matrix, edges,
        buffer_distance_in_meters = buffer_distance_in_meters,
        aggregation_functions={
        'event_id_cnty': [(f'incident_count_{buffer_distance_in_meters}', 'count')],
        'fatalities': [(f'sum_fatalities_{buffer_distance_in_meters}', 'sum')],
        'distance_to_route': [(f'mean_distance_to_route_{buffer_distance_in_meters}','mean')]
    },
        points_file_path=out_dir / 'covariates' / 'interim' / f'acled_edge_oblast_{buffer_distance_in_meters}.gpkg',
        output_file_path = out_dir / 'covariates' / 'interim' / f'acled_routes_oblast_{buffer_distance_in_meters}.csv'
    )

print(df_grouped_filled)

# combine outputs

covariates_file_paths = [
    out_dir / 'covariates' / 'interim' / 'acled_routes_oblast_1000.csv',
    out_dir / 'covariates' / 'interim' / 'acled_routes_oblast_5000.csv',
    out_dir / 'covariates' / 'interim' / 'acled_routes_oblast_10000.csv'
]
covariates_list = [pd.read_csv(file) for file in covariates_file_paths]
covariates = covariates_list[0]
for df in covariates_list[1:]:
    covariates = pd.merge(covariates, df, how='outer')

covariates.to_csv(out_dir / 'covariates' / 'final' / 'covariates_routes_oblast.csv',
                  index=False)