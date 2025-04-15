import rasterstats
from py_helpers.utils import *
import tarfile
from datetime import datetime
import re
import plotnine as gg

# parameters
country = 'ua'
adminName_col = 'ADM1_PCODE'
output_name = country + '_graced_oblast.csv'
folder = 'graced'

# directories
os.makedirs(out_dir / 'covariates' / 'raw' / folder, exist_ok=True)

# data input
admin_gis_path = repo_dir / 'data' / 'cod-ab' / 'ukr_admbnda_sspe_20230201_SHP'/'ukr_admbnda_adm1_sspe_20230201.shp'
admin_gis = gpd.read_file(admin_gis_path)
master_index = pd.read_csv(out_dir / (country +'_master_index.csv'))
master_index = master_index[[adminName_col, 't', 'i', 't_key', 'i_key', 't_name', 'i_name']].drop_duplicates()
time_index = pd.read_csv(out_dir / (country +'_time_index.csv'))
# Path to your directory containing .nc.tar.gz files
graced_dir = in_dir / 'GRACED'

# 2. Function to extract and read .nc file from .nc.tar.gz archive
def read_nc_from_tar(tar_path):
    with tarfile.open(tar_path, 'r:gz') as tar:
        # Extract the .nc file from the tar
        nc_filename = [name for name in tar.getnames() if name.endswith('.nc')][0]
        nc_filepath = os.path.join(graced_dir, nc_filename)
        if not os.path.exists(nc_filepath):
            tar.extractall(path=graced_dir) 

    # Open the NetCDF file (assuming it's a raster format)
    zonal = rasterstats.zonal_stats(admin_gis, nc_filepath, stats=['sum'], all_touched=True)
    zonal_sum = pd.Series([d['sum'] for d  in zonal]).replace(np.nan, 0)

    return zonal_sum



# 3. Process all the .nc.tar.gz files

def process_graced_type(graced_type):
    cov_name = 'graced_' + graced_type

    files = [os.path.join(graced_dir, filename) for filename in os.listdir(graced_dir) if graced_type in filename]

    admin_sum = {}

    for filename in files:
        if filename.endswith('.nc.tar.gz'):
            tar_path = os.path.join(graced_dir, filename)

            date_str = re.search(r'y(\d{4})_m(\d{2})', tar_path).groups()
            collection_date = str(datetime(year=int(date_str[0]), month=int(date_str[1]), day=1).date())

            # Read and extract values
            zonal_sum = read_nc_from_tar(tar_path)
            admin_sum[collection_date] = zonal_sum

    admin_sum = pd.DataFrame(admin_sum)
    admin_sum[adminName_col] = admin_gis[adminName_col]

    admin_sum_lg = pd.melt(admin_sum, id_vars=[adminName_col], var_name='collection_date', value_name=cov_name)

    admin_sum_lg = admin_sum_lg.merge(
        time_index, how='left', on='collection_date').merge(
            master_index, how='right'
        )

    admin_sum_lg.drop(columns=['collection_date'], inplace=True)	

    admin_sum_lg = admin_sum_lg.melt(
        id_vars=['i', 'i_name', 't', 't_name', 'ADM1_PCODE', 'i_key', 't_key'], 
        var_name='covariate', value_name='value')

    return admin_sum_lg


residential_df = process_graced_type( 'Residential')
commercial_df = process_graced_type('GroundTransportation')
industrial_df = process_graced_type('Industry')

# Bind all the DataFrames together
all_graced_data = pd.concat([residential_df, commercial_df, industrial_df], keys=['Residential', 'Commercial', 'Industrial'])



# Write output
all_graced_data.to_csv(out_dir / 'covariates' / 'raw'/ folder /output_name, index=False)


# Visualise an example

all_graced_data = pd.read_csv(out_dir / 'covariates' / 'raw'/ folder /output_name)
all_graced_data['t_name'] = pd.to_datetime(all_graced_data['t_name']) 

filtered_data = all_graced_data[all_graced_data['i_name'].isin(['Donetska', 'Luhanska', 'Kyiv', 'Lvivska','Zaporizka', 'Zakarpatska'])]

(
    gg.ggplot(filtered_data, gg.aes(x='t_name', y='value', colour='i_name')) +
    #plt.facet_grid(.~covariate)+
    gg.geom_point()+
    gg.theme_minimal()+
    gg.facet_grid('covariate', scales='free_y')+
    gg.theme(plot_background=gg.element_rect(fill='white'))
)

