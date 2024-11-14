# download git code
from pathlib import Path
import shutil
import ee
from py_helpers.utils import *
import matplotlib.pyplot as plt
import rasterstats

if not Path('py_helpers/PWTT').exists():
    !git clone https://github.com/oballinger/PWTT py_helpers/PWTT
from py_helpers.PWTT.code import pwtt

# parameters
ggdrive = Path('H:/My Drive/')
folder = 'pwtt_ukraine'
country = 'ua'

# data input
boundaries_oblast_path = repo_dir / 'data' / 'COD-AB' / 'ukr_admbnda_sspe_20230201_SHP'/'ukr_admbnda_adm1_sspe_20230201.shp'
boundaries_oblast = gpd.read_file(boundaries_oblast_path)
master_index = pd.read_csv(out_dir / (country +'_master_index.csv'))
master_index = master_index[['ADM1_PCODE', 't', 'i', 't_key', 'i_key', 't_name', 'i_name']].drop_duplicates()

time_index = pd.read_csv(out_dir / (country +'_time_index.csv'))

# 1. run pwtt
project_name = 'ee-nowpoplcds'
ee.Authenticate()
ee.Initialize(project=project_name)
ukraine = ee.Geometry.Rectangle([22.0856083513, 44.3614785833, 40.0807890155, 52.3350745713])

war_date = pd.to_datetime('2022-02-22')
end_date = time_index['collection_date'].max()
dates = pd.date_range(war_date+pd.Timedelta(weeks=4), end_date, freq='ME')
dates = [date.replace(day=22).strftime('%Y-%m-%d') for date in dates if date <= end_date]

for month in dates:
    # month = dates[0]
    ukr_damge = pwtt.filter_s1(aoi=ukraine,
                   war_start='2022-02-22',
                   inference_start=month,
                   pre_interval=12,
                   post_interval=2,
                   export=True,
                   export_dir='pwtt_ukraine')         
    task = ee.batch.Export.image.toDrive(
                image=ukr_damge,
                description=month,
                folder='pwtt_ukraine',
                scale=5000,
                fileFormat='GeoTIFF'
            )
    task.start()

ee.batch.Task.list()
ee.data.getTaskStatus('BUV4IWXVW5Z5P3TOJ6BZKNZM')

# 2. transfer the images from google drive to output folder
shutil.copytree( ggdrive / folder,  out_dir / 'covariates' / 'raw'/ folder, dirs_exist_ok=True)

# 3. Extract building damages per admin unit

tiff_list = os.listdir(out_dir / 'covariates' / 'raw'/ folder)
tiff_list = [s for s in tiff_list if s.endswith('.tif')]

oblast_sum = {}

# Iterate over the raster files
for raster_file in tiff_list:
    # raster_file = tiff_list[0]
    raster_path = out_dir / 'covariates' / 'raw'/ folder/ raster_file
    zonal = rasterstats.zonal_stats(boundaries_oblast, raster_path, stats=['sum'])
    oblast_sum[os.path.splitext(raster_file)[0]] = [d['sum'] for d in zonal]

oblast_sum = pd.DataFrame(oblast_sum)
oblast_sum['ADM1_PCODE'] = boundaries_oblast['ADM1_PCODE']

# Transform to long format
oblast_sum_lg = pd.melt(oblast_sum, id_vars='ADM1_PCODE', var_name='collection_date', value_name='pwtt')

oblast_sum_lg = oblast_sum_lg.merge(
    time_index, how='left', on='collection_date').merge(
        master_index, how='right'
    )

oblast_sum_lg.drop(columns=['collection_date'], inplace=True)	

# Write output
oblast_sum_lg.to_csv(out_dir / 'covariates' / 'interim'/ (country+'_pwtt_oblast.csv'), index=False)



# Visualise an example
filtered_data = oblast_sum_lg[oblast_sum_lg['i'].isin([4,21, 23])]

plt.figure(figsize=(12, 6))

for i in filtered_data['i_name'].unique():
    i_data = filtered_data[filtered_data['i_name'] == i]
    i_key = i_data['i'].unique()[0]-4
    plt.scatter(i_data['t'], i_data['pwtt_interpolated'], label=f'i={i} (t)', color=plt.cm.tab20(i_key))

plt.xlabel('t')
plt.ylabel('pwtt')
plt.legend()
plt.show()
