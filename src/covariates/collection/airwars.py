import time
import pandas as pd
from bs4 import BeautifulSoup
import requests
import pickle


OUT_FOLDER = 'K:/DemSci/data/airwars'


def query_airwars_api(url, per_page=10):
    try:
        all_data = []
        page = 1
        while True:
            print(page)
            params = {'per_page': per_page, 'page': page}
            response = requests.get(url, params=params)
            if response.status_code == 200:
                data = response.json()
                if not data:
                    break  # No more data available
                all_data.extend(data)
                page += 1
                time.sleep(1)  # Gentle rate limiting
            else:
                print(f"Error: Status code {response.status_code}")
                break
        return all_data
    except requests.exceptions.RequestException as e:
        print(f"Error: {e}")
        return None

def extract_attributes(row):
    try:
        response = requests.get(row['link'])
        if response.status_code == 200:
            soup = BeautifulSoup(response.content, 'html.parser')

            location = soup.find(class_='meta-block location')
            if location:
                location_text = location.find('h4').next_sibling.strip()
                df_gaza.loc[df_gaza['id'] == row['id'], 'location_text'] = location_text

            else:
                location_text = "Location information not found"

            geolocation = soup.find(class_='Geolocations primary')
            if geolocation:
                geolocation_text = geolocation.find('i').next_sibling.strip()
                df_gaza.loc[df_gaza['id'] == row['id'], 'geolocation_text'] = geolocation_text

            else:
                geolocation_text = "Geolocation information not found"

            killed = soup.select_one('ul.meta-list.summary li.sub div.value')
            if killed:
                killed_text = killed.get_text(strip=True)
                killed_number = list(map(int, re.findall(r'\d+', killed_text)))
                if len(killed_number) == 2:
                    killed_number = (killed_number[0]+killed_number[1])/2
                elif len(killed_number) == 1:
                    killed_number = int(killed_number[0])
                else:
                    killed_number = None
                df_gaza.loc[df_gaza['id'] == row['id'], 'killed_text'] = killed_text
                df_gaza.loc[df_gaza['id'] == row['id'], 'killed_number'] = killed_number

            else:
                killed_text = "Killed information not found"

            time.sleep(1)

            return location_text, geolocation_text, killed_text
        else:
            print(f'Failed to fetch webpage for link: {link}')
            return None, None
    except Exception as e:
        print(f'Error occurred while processing link {link}: {e}')
        return None, None

url = "https://airwars.org/wp-json/wp/v2/civ/"
result = query_airwars_api(url, per_page=10)
df = pd.json_normalize(result)

df.to_pickle(OUT_FOLDER + '/airwars_raw.pkl')

# 878 = UKRAINE
# 459 = Turkey
# 767 = Gaza strip