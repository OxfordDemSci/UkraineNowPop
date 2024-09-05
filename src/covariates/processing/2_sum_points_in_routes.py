import geopandas as gpd
import pandas as pd
import net_friction.calculations as calc
import net_friction.data_preparation as prep
import numpy as np


# load data
edges = gpd.read_file(out_dir / 'covariates' / 'network' /'network_edges_oblast.gpkg')
matrix = pd.read_csv(out_dir / 'covariates' / 'network' /'network_matrix_oblast.csv')
matrix['edge_geometries_ids'] = matrix['edge_geometries_ids'].apply(lambda x: np.array(x.split(','), dtype=int))

def aggregate_points(points, aggregation_functions, prefix_col,  date_start = '2022-02-26', date_end = '2024-05-15'):
    # Aggregate incidents
    df_grouped = (
        points.groupby(["event_date", "from_pcode", "to_pcode"])
        .agg(aggregation_functions)
        .reset_index()
    )

    df_grouped.columns = [
        f"{prefix_col}_{col[1]}" if col[1] != '' else f"{col[0]}"
        for col in df_grouped.columns
    ]

    # Fill in missing routes
    df_grouped_filled = prep.fill_missing_routes(
        df_grouped, matrix, date_start=date_start, date_end=date_end
    )
    return df_grouped_filled

def sum_points_in_routes(
        points_file_path, matrix, edges,
        buffer_distance_in_meters,
        aggregation_functions,filtering_conditions,
        output_file_path
):
    # buffer_distance_in_meters = 1000
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

    incidents_grouped_list = []
    for key, condition in filtering_conditions.items():
        filtered_incidents = incidents_in_routes_df.query(condition)
        incidents_grouped_list.append(aggregate_points(filtered_incidents, aggregation_functions, key))

    incidents_grouped = incidents_grouped_list[0]
    for df in incidents_grouped_list[1:]:
        incidents_grouped = pd.merge(incidents_grouped, df, how='outer')

    incidents_grouped = pd.melt(incidents_grouped,
            id_vars=['event_date', 'from_pcode', 'to_pcode'],
            var_name='covariate',
            value_name='value')

    incidents_grouped.to_csv(output_file_path, index=False)

    return incidents_grouped


filtering_conditions = {
    'acled_all': 'from_pcode.notna()',
    'acled_withfatalities': 'fatalities > 0',
     'acled_disorderPolitical': 'disorder_type == "Political violence"',
    'acled_disorderStrategic': 'disorder_type == "Strategic developments"',
    'acled_disorderDemonstration': 'disorder_type == "Demonstrations"',
    'acled_disorderPoliticalDemonstration': 'disorder_type == "Political violence; Demonstrations"',
    'acled_eventExplosion': 'event_type == "Explosions/Remote violence"',
    'acled_eventBattle': 'event_type == "Battles"',
    'acled_eventStrategic': 'event_type == "Strategic developments"',
    'acled_eventAgainstCivilians': 'event_type == "Violence against civilians"',
    'acled_eventProtest': 'event_type == "Protests"',
    'acled_eventRiot': 'event_type == "Riots"'
}

aggregation_functions = {
        'event_id_cnty': [(f'incident_count_{buffer_distance_in_meters}', 'count')],
        'fatalities': [(f'fatalities_sum_{buffer_distance_in_meters}', 'sum')],
        'distance_to_route': [(f'distance_mean_{buffer_distance_in_meters}','mean')]
    }

for buffer_distance_in_meters in [1000, 5000, 10000]:
        df_grouped_filled = sum_points_in_routes(
        points_file_path = out_dir / 'covariates' / 'interim' / f'acled_edge_oblast_{buffer_distance_in_meters}.gpkg',
        matrix=matrix, edges=edges,
        buffer_distance_in_meters = buffer_distance_in_meters,
        aggregation_functions= aggregation_functions,
        filtering_conditions= filtering_conditions,
        output_file_path = out_dir / 'covariates' / 'interim' / f'acled_routes_oblast_{buffer_distance_in_meters}.csv'
    )

print(df_grouped_filled)

# combine all covariates outputs

covariates_file_paths = [
    out_dir / 'covariates' / 'interim' / 'acled_routes_oblast_1000.csv',
    out_dir / 'covariates' / 'interim' / 'acled_routes_oblast_5000.csv',
    out_dir / 'covariates' / 'interim' / 'acled_routes_oblast_10000.csv'
]
covariates_list = [pd.read_csv(file) for file in covariates_file_paths]
covariates = pd.concat(covariates_list)


    covariates_list)[0]
for df in covariates_list[1:]:
    covariates = pd.merge(covariates, df, how='outer')

covariates['directional'] = False

covariates = covariates.sort_values(by=['event_date', 'from_pcode', 'to_pcode'])

covariates.to_csv(out_dir / 'covariates' / 'final' / 'covariates_routes_oblast.csv',
                  index=False)