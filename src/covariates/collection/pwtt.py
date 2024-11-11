# download git code
!git clone https://github.com/oballinger/PWTT py_helpers/PWTT

import shutil
from pathlib import Path
import ee
from py_helpers.PWTT.code import pwtt
from py_helpers.utils import *
import rasterio
import rasterio.features


# parameters
ggdrive = Path('H:/My Drive/')
folder = 'pwtt_ukraine'

project_name='ee-nowpoplcds'
ee.Authenticate()
ee.Initialize(project=project_name)
ukraine = ee.Geometry.Rectangle([22.0856083513, 44.3614785833, 40.0807890155, 52.3350745713])

boundaries_oblast = gpd.read_file(in_dir / 'COD-AB' / 'ukr_admbnda_sspe_20230201_SHP'/'ukr_admbnda_adm1_sspe_20230201.shp')
boundaries_oblast = boundaries_oblast.rename(
    columns={'ADM1_PCODE': 'pcode'}
)

# 1. run pwtt

war_date = pd.to_datetime('2022-02-22')
end_date = pd.to_datetime('2024-11-22')
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

oblast_sum = {}

# Iterate over the raster files
for raster_file in tiff_list:
    # Open the raster file
    with rasterio.open(out_dir / 'covariates' / 'raw'/ folder/ raster_file, 'r') as src:
        raster_data = src.read(1)

    # Compute the sum of building damages
    oblast_sum[os.path.splitext(raster_file)[0]]  = boundaries_oblast.apply(lambda row: np.nansum(raster_data[rasterio.features.geometry_mask([row.geometry], raster_data.shape, src.transform, all_touched=True)]), axis=1)

oblast_sum = pd.DataFrame(oblast_sum)
oblast_sum['pcode'] = boundaries_oblast['pcode']

# Transform to long format
oblast_sum_lg = pd.melt(oblast_sum, id_vars='pcode', var_name='collection_date', value_name='pwtt')

# Write output
oblast_sum_lg.to_csv(out_dir / 'covariates' / 'interim'/ 'pwtt_oblast.csv', index=False)
