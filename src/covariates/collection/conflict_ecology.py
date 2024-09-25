import rasterio
import geopandas as gpd
import numpy as np
from shapely.geometry import Point
import pandas as pd
import net_friction.data_preparation as prep

exec(open('py_setup.py').read())


def convert_raster_to_points(raster_path):
    """
    Convert a single raster file to a GeoDataFrame of points with their pixel values.

    :param raster_path: Path to the raster file.
    :return: GeoDataFrame containing points and pixel values.
    """
    with rasterio.open(raster_path) as src:
        raster = src.read(1)  # Read the first band
        transform = src.transform  # Get the transform information
        crs = src.crs  # Coordinate reference system

    # Get indices of all non-NaN pixels
    rows, cols = np.where(~np.isnan(raster))
    values = raster[rows, cols]  # Extract pixel values

    # Convert pixel coordinates to geographic coordinates
    points = [Point(rasterio.transform.xy(transform, row, col)) for row, col in zip(rows, cols)]

    # Create a GeoDataFrame
    gdf = gpd.GeoDataFrame({'value': values}, geometry=points, crs=crs)

    gdf['time'] = pd.to_datetime(gdf['value'], unit= 'ms').dt.strftime('%Y-%m-%d')

    return gdf

raster_dir = in_dir / 'Conflict-Ecology' / 'Ukraine' / 'ukr_damage_all_months_asc_dsc_v102_mmu'
pattern = 'timestamped*'
tif_files = list(raster_dir.glob(pattern))
tif_files = [str(fp) for fp in tif_files]

point_datasets = []

# Process each raster file that matches the pattern
for raster_path in tif_files:
    print(f"Processing {raster_path}")
    points_gdf = convert_raster_to_points(raster_path)
    point_datasets.append(points_gdf)

points_merged = gpd.GeoDataFrame(pd.concat(point_datasets, ignore_index=True))

points_merged.to_file(
    out_dir / 'covariates' / 'raw' / 'conflictEcology_point.gpkg'
)

# Extract longitude and latitude from the geometry column
points_merged['longitude'] = points_merged.geometry.x
points_merged['latitude'] = points_merged.geometry.y

points_merged = points_merged.drop(columns='geometry')
# create index
points_merged = points_merged.reset_index()
points_merged.to_csv(
out_dir / 'covariates' / 'raw' / 'conflictEcology_point.csv',
    index=False
)

# Extract in buffer

def vary_buffer_in_subset(buffer_distance_in_meters):
    # Define the output filenames
    incidents_outfile_csv = out_dir / 'covariates' / 'interim' / f'conflictEcology_edge_oblast_{buffer_distance_in_meters}.csv'

    # Read and process the data
    incident_subset_gdf = prep.subset_incident_data_in_buffer(
        edges = gpd.read_file(out_dir / 'covariates' / 'network' / 'network_edges_oblast.gpkg'),
        incident_data = out_dir / 'covariates' / 'raw' / 'conflictEcology_point.csv',
        incident_out_file = incidents_outfile_csv,
        buffer_distance = buffer_distance_in_meters,
        crs = 6383,
        is_acled=False,
        index_col="index"  # Unique ID field in incidents table
    )


for buffer in [1000, 5000, 10000]:
    # buffer_distance = 1000
    vary_buffer_in_subset(buffer)