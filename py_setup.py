import pandas as pd
import os
from dotenv import load_dotenv
from pathlib import Path

pd.set_option('display.width', 400)
pd.set_option('display.max_columns', 15)

print("Pandas configuration is applied globally!")
# Load the .env file
env_path = Path('.') / '.env'
load_dotenv(env_path)
in_dir = Path(os.getenv('in_dir'))
out_dir = Path(os.getenv('out_dir'))

print("in_dir and out_dir set")