import os
import time

import requests
from geopy.geocoders import Nominatim
import random
from retrying import retry
from requests.exceptions import ChunkedEncodingError

def get_country_from_coords(latitude, longitude):
    geolocator = Nominatim(user_agent="unosat" + str(random.randint(1, 100)))
    location = geolocator.reverse((latitude, longitude), language='en', exactly_one=True)
    if location:
        address = location.address
        country = address.split(",")[-1].strip()
        return country
    else:
        return "Country not found"

def create_folder(folder_name):
    try:
        os.makedirs(folder_name)
        print(f"Folder '{folder_name}' created successfully")
    except FileExistsError:
        print(f"Folder '{folder_name}' already exists")

def save_file(url, file_name, folder):
    response = requests.get(url)
    if response.status_code == 200:
        with open(os.path.join(folder + file_name), 'wb') as f:
            f.write(response.content)
            print(f"File '{file_name}' saved successfully")
    else:
        print(f"Failed to download file '{file_name}'. Status Code: {response.status_code}")

def write_readme(readme, file_name, folder):
    with open(os.path.join(folder, file_name), "w", encoding='utf8') as f:
        f.write(readme)
        print("Readme file saved successfully")
def process_product(product_id):
    # URL of the website
    url = f'https://unosat.org/our_products/{product_id}'
    url_download = f'https://unosat.org/static/unosat_filesystem/{product_id}/'

    # Make a GET request to the website
    try:
        response = make_request(url)


        content = response.json()
        map_event = content['map_event']

        # Find country
        country = get_country_from_coords(content['latitude'], content['longitude'])
        if country:
            out_folder = f'./data/unosat/{country}/'
            # Create folder based on country name
            create_folder(out_folder)

        readme = f"{product_id}{map_event['title']} {map_event['description']} {map_event['sources']}"
        write_readme(readme, map_event['title'].replace('/', '').replace(':', '') + '.txt', out_folder)

        # Save PDF file
        pdf_name = map_event.get('pdf_name')
        if pdf_name:
            save_file(f'{url_download}{pdf_name}', pdf_name, out_folder)
        else:
            print(f"No PDF found for Product ID: {product_id}, Title: {map_event['title']}")

        # Save GDP file
        gdp_name = map_event.get('gdp_link')
        if gdp_name:
            gdp_name = gdp_name.replace(f'/static/unosat_filesystem/{product_id}', '')
            save_file(f'{url_download}{gdp_name}', gdp_name, out_folder)
        else:
            print(f"No GDP found for Product ID: {product_id}, Title: {map_event['title']}")

        # Save SHP file
        shp_name = map_event.get('shp_link')
        if shp_name:
            shp_name = shp_name.replace(f'/static/unosat_filesystem/{product_id}', '')
            save_file(f'{url_download}{shp_name}', shp_name, out_folder)
        else:
            print(f"No SHP found for Product ID: {product_id}, Title: {map_event['title']}")

    except requests.exceptions.ChunkedEncodingError as e:
        print("Error:", e)

@retry(stop_max_attempt_number=4, wait_fixed=5000)  # Retry 3 times with a 2-second delay between retries
def make_request(url):
    response = requests.get(url)
    response.raise_for_status()  # Raise an error if the response status code is not in the 2xx range
    return response


# List of product IDs
product_ids = range(1, 3857)
# Process each product
for product_id in product_ids:
    print(f"Processing product {product_id}...")
    time.sleep(1)
    process_product(product_id)
product_id = 3831