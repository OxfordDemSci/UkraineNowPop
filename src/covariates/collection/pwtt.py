!git clone https://github.com/oballinger/PWTT pyidp/PWTT

import ee
from pyidp.PWTT.code import pwtt


project_name='ee-nowpoplcds'
ee.Authenticate()
ee.Initialize(project=project_name)

ukraine = ee.Geometry.Rectangle([22.0856083513, 44.3614785833, 40.0807890155, 52.3350745713])

ukr_damge = pwtt.filter_s1(aoi=ukraine,
                   war_start='2022-02-22',
                   inference_start='2024-05-01',
                   pre_interval=12,
                   post_interval=2,
                   export=True,
                   export_dir='pwtt_ukraine')

             
task = ee.batch.Export.image.toDrive(
                image=ukr_damge,
                description='test',
                folder='pwtt_ukraine',
                scale=5000,
                fileFormat='GeoTIFF'
            )
task.start()
ee.batch.Task.list()
ee.data.getTaskStatus('JW7WL2JYACEXW7TL2TYBEOWK')
