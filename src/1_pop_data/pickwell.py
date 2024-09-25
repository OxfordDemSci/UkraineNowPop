import pandas as pd
import folium
from folium.plugins import HeatMap, MarkerCluster
import geopandas as gpd

# Script parameters
drive_path = "K:/DemSci/projects/2023_WHO_Ukraine_Population/"
pickwell = pd.read_csv(drive_path + 'data/Pickwell/doug_offset.csv')  # pd.read_csv
admin = gpd.read_file(drive_path + 'data/COD-AB/ukr_admbnda_sspe_20230201_SHP/ukr_admbnda_adm3_sspe_20230201.shp')
output_path = drive_path + 'output/population_proxy/'

nightime_start = 20 # hour when nighttime starts
nightime_end = 7 # hour when nighttime ends
admin_name = 'ADM3_PCODE' # column name of the admin code
period_aggregate = 'ymdh' # period over which to aggregate the location: ymw, ymd, ymdh [year-month-week-day-hour]

# Define the classification dictionary
classification = {
    'High Wealth': ['apple', 'google', 'samsung', 'oneplus'],
    'Mid Wealth': ['xiaomi', 'huawei', 'vivo', 'oppo', 'asus', 'motorola', 'lenovo', 'realme'],
    'Low Wealth': ['itel', 'tecno', 'zte', 'meizu', 'redmi', 'hmd global']
}

##############################################################################
# 1. Preprocessing

# Round GPS coordinates
pickwell['longitude'] = pickwell['longitude'].apply(lambda x: round(x, 3))
pickwell['latitude'] = pickwell['latitude'].apply(lambda x: round(x, 3))

# Define different period aggregations
pickwell['timestamp'] = pd.to_datetime(pickwell['timestamp'])
pickwell['ymw'] = pickwell['timestamp'] - pd.offsets.Week(weekday=0)
pickwell['ymw'] = pickwell['ymw'].dt.strftime('%Y-%m-%d')
pickwell['ymd'] = pickwell['timestamp'].dt.strftime('%Y-%m-%d')
pickwell['ymdh'] = pickwell['timestamp'].dt.strftime('%Y-%m-%d-%H')

# Define the wealth classification
pickwell['manufacturer'] = pickwell['manufacturer'].str.lower()

# Define the nightime period
pickwell['hour'] = pickwell['timestamp'].dt.hour



def compute_flows_and_stock(df, admin, period_aggregate, output_path=None):    
    """
    Compute stock and flow data for the given device data aggregated by the given period.

    Parameters
    ----------
    df : pandas.DataFrame
        Device data aggregated by the given period.
    admin : geopandas.GeoDataFrame
        Admin boundaries data.
    period_aggregate : str
        Period over which to aggregate the location: ymw, ymd, ymdh [year-month-week-day-hour].

    Returns
    -------
    tuple of pandas.DataFrame
        Stock data by 100m, stock data by admin, flow data between admin.

    """
    ##############################################################################
    period_deltasec_dict = { # Define the difference in seconds between two periods
        'ymw': 604800 ,
        'ymd': 86400,
        'ymdh': 3600
    }

    period_format_dict = {
    'ymw': '%Y-%m-%d',
    'ymd': '%Y-%m-%d',
    'ymdh': '%Y-%m-%d-%H'
    }


    ##############################################################################
    # 2. Compute stock data by 100m 
    df_grouped = df.groupby(['device_aid', period_aggregate])[['longitude', 'latitude']].agg(lambda x: x.mode().iloc[0]).reset_index()

    df_100m = df_grouped.groupby(['longitude', 'latitude', period_aggregate])['device_aid'].count().reset_index()
    df_100m = df_100m.rename(columns={'device_aid': 'count'})

    ##############################################################################
    # 3. Compute stock data by admin

    df_gdf = gpd.GeoDataFrame(
        df_grouped,
        geometry=gpd.points_from_xy(df_grouped.longitude, df_grouped.latitude),
        crs = "EPSG:4326"
    )

    admin = admin.to_crs(df_gdf.crs)
    df_gdf = gpd.sjoin(df_gdf, admin[[admin_name, 'geometry']], how='inner', predicate='covered_by')

    df_admin = df_gdf.groupby([admin_name,period_aggregate])['device_aid'].count().reset_index()
    df_admin = df_admin.rename(columns={'device_aid': 'count'})

    ##############################################################################
    # 4. Compute flows data between admin

    # Filter device IDs that have data for consecutive periods
    # We need backward and forward to get the first and the last period
    df_gdf = df_gdf.sort_values(['device_aid',period_aggregate]).reset_index(drop=True)

    df_gdf[period_aggregate] = pd.to_datetime(df_gdf[period_aggregate], format=period_format_dict[period_aggregate])
    
    consecutive_period_forward = df_gdf.groupby('device_aid')[period_aggregate].apply(lambda x: x[x.diff().dt.total_seconds() == period_deltasec_dict[period_aggregate]]).reset_index()
    consecutive_period_backward = df_gdf.groupby('device_aid')[period_aggregate].apply(lambda x: x[x.diff(-1).dt.total_seconds() == -period_deltasec_dict[period_aggregate]]).reset_index()

    consecutive_period = pd.concat([consecutive_period_forward, consecutive_period_backward])
    consecutive_period = consecutive_period[['device_aid',period_aggregate]].drop_duplicates()
    consecutive_period = consecutive_period.merge(df_gdf[['device_aid', period_aggregate, admin_name]],  on=['device_aid', period_aggregate])

    # Compute the previous admin for each consecutive date
    consecutive_period = consecutive_period.sort_values(['device_aid',period_aggregate]).reset_index(drop=True)
    consecutive_period['previous_' + admin_name ] = consecutive_period.groupby('device_aid')[admin_name].shift(1)
    consecutive_period = consecutive_period[consecutive_period['previous_' + admin_name ].notna()]

    # Sum per admin combination and period to get flows data
    consecutive_period_aggregate = consecutive_period.groupby([period_aggregate, admin_name,'previous_' + admin_name ])['device_aid'].count().reset_index()
    consecutive_period_aggregate = consecutive_period_aggregate.rename(columns={'device_aid': 'count'})
    
    # write output
    if output_path is not None:
        df_100m.to_csv(output_path + 'stock_100m.csv', index=False)
        df_admin.to_csv(output_path + 'stock_admin.csv', index=False)
        consecutive_period_aggregate.to_csv(output_path + 'flow_admin.csv', index=False)

    return {'stock_100m': df_100m, 'stock_admin': df_admin, 'flow_admin': consecutive_period_aggregate}


# Play with parameters: duration of night, period over which to aggregate, and wealth classification
pickwell_filtered_207 = pickwell[(pickwell['hour'] >= 20) | (pickwell['hour'] < 7)]
flows_stocks_total_207_ymw = compute_flows_and_stock(pickwell_filtered_207, admin, period_aggregate = 'ymw', output_path=output_path + 'night207_ymw_')

pickwell_filtered_1810 = pickwell[(pickwell['hour'] >= 18) | (pickwell['hour'] < 10)]
flows_stocks_total_1810_ymw = compute_flows_and_stock(pickwell_filtered_1810, admin, period_aggregate = 'ymdh', output_path=output_path + 'night1810_ymw_')


pickwell_207_rich = pickwell_filtered_207[pickwell_filtered_207['manufacturer'].isin(classification['High Wealth'])]  
pickwell_207_poor = pickwell_filtered_207[pickwell_filtered_207['manufacturer'].isin(classification['Low Wealth'])]  
flows_stocks_poor_207_ymw = compute_flows_and_stock(pickwell_207_poor, admin, period_aggregate = 'ymdh', output_path=output_path + 'night207_ymw_poor_')



# clean manufacturer


# Check that each device ID has exactly one manufacturer (and has it for all the observations)
# manufacturer_counts = pickwell.groupby('device_aid')['manufacturer'].nunique()
# incoherent_devices = manufacturer_counts[manufacturer_counts > 1].index
# if len(incoherent_devices) > 0:
#     print(f"The following device IDs have more than one manufacturer: {incoherent_devices}")

# Visualisation

# Create a map
# m = folium.Map(location=[ 50.4456842, 30.6281897], zoom_start=13)

# # Create a heatmap
# # Create a marker cluster
# marker_cluster = MarkerCluster().add_to(m)

# # Add points to the marker cluster
# for index, row in pickwell_filtered.iterrows():
#     folium.Marker([row['latitude'], row['longitude']]).add_to(marker_cluster)
# m

# m = folium.Map(location=[ 50.4407932,30.4904615], zoom_start=14)
# heat_data = pickwell_filtered[['latitude', 'longitude']].values.tolist()
# HeatMap(heat_data, radius=10).add_to(m)
# m