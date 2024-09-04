import geopandas as gpd
import os
from dotenv import load_dotenv
from pathlib import Path

from net_friction import data_preparation as prep
from net_friction import datatypes as dt
from net_friction import calculations as calc

# Load the .env file
env_path = Path('.') / '.env'
load_dotenv(env_path)

# Access the environment variables
in_dir = Path(os.getenv('in_dir'))
out_dir = Path(os.getenv('out_dir'))

def create_network(roads, weighting_method, population, boundaries, centroid_file_path, edge_file_path, matrix_file_path, crs):
    subset_fields = ["osm_id", "fclass"]  # Fields in OSM data
    subset_categories = ["motorway", "trunk", "primary", "secondary", "tertiary"]  # fclass in OSM data
    roads_gdf = prep.get_roads_data(roads, crs, subset_fields, subset_categories)

    # Crude topology fix
    roads_gdf = prep.fix_topology(roads_gdf, crs, len_segments=1000)

    # Get network object and edges dataframe of full data
    net, edges = prep.make_graph(roads_gdf)

    # Create source/destination points weighted by raster
    src_dst_points = prep.get_source_destination_points(
        boundaries=boundaries,
        weighting_method=weighting_method,
        network=net,
        crs=crs,
        centroids_file=centroid_file_path,
        raster=population
    )

    # Get shortest path nodes between source/destination pairs
    shortest_path_nodes, shortest_path_lengths = calc.calculate_routes_and_route_distances(
        net, src_dst_points
    )
    src_dst_points["shortest_path_nodes"] = shortest_path_nodes
    src_dst_points["shortest_path_lengths"] = shortest_path_lengths

    # Subset 'global' edges to edges between source and destination pairs
    route_geom_ids = prep.get_route_geoms_ids(src_dst_points.copy(), edges)
    edge_ids = route_geom_ids.explode("edge_geometries_ids")["edge_geometries_ids"].unique()
    edges_subset = edges[edges.index.isin(edge_ids)]

    # Save edges as future input for roads for improved performance
    edges_subset.to_file(edge_file_path, driver="GPKG")
    src_dst_points.to_csv(matrix_file_path, index=False)


# Continuous raster for use in weighting
population_ukr = in_dir / 'GISREDE' / 'ukr_ppp_2020_1km_Aggregated.tif'


# Weighting method (CENTROID/WEIGHTED)
weighting_method_ukr = dt.WeightingMethod.WEIGHTED

# Open OSM roads
crs_ukr = 6383
roads_ukr = in_dir / 'GISREDE' / "gis_osm_roads_free_1.shp"

# boundaries
boundaries_oblast = gpd.read_file(in_dir / 'COD-AB' / 'ukr_admbnda_sspe_20230201_SHP'/'ukr_admbnda_adm1_sspe_20230201.shp').rename(
columns={'ADM1_PCODE': 'pcode'}
)

# oblast
create_network(
    roads=roads_ukr,
    weighting_method=weighting_method_ukr,
    population=population_ukr,
    boundaries=boundaries_oblast,
    centroid_file_path= out_dir / 'covariates' / 'network' / 'network_centroidsWeighted_oblast.gpkg',
    edge_file_path=out_dir / 'covariates' / 'network' /'network_edges_oblast.gpkg',
    matrix_file_path=out_dir / 'covariates' / 'network' /'network_matrix_oblast.csv',
    crs=crs_ukr)

# raion
create_network(
    roads=roads_ukr,
    weighting_method=weighting_method_ukr,
    population=population_ukr,
    boundaries=boundaries_ukr,
    centroid_file_path=in_dir / 'GISREDE' / 'L2' / 'centroids_L2.gpkg',
    edge_file_path=out_dir / 'covariates' / 'network' / 'network_edges_raion.gpkg',
    matrix_file_path=out_dir / 'covariates' / 'network' / 'network_matrix_raion.csv',
    crs=crs_ukr)