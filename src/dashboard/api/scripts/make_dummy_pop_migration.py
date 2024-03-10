from pathlib import Path
import pandas as pd
import geopandas as gpd
import numpy as np
import random
from random import uniform
from itertools import product

BASE = Path(__file__).resolve().parent

UKRAINE_POPULATION = 50000000


def divide_population(population, num_groups):
    division = np.random.multinomial(population, np.ones(num_groups)/num_groups)
    return division


def make_dummy_pop_data(admin_units, total_population=50000000):
    data = []
    level1_pop = divide_population(total_population, admin_units['ADM1_PCODE'].nunique())
    days = pd.date_range(start="2024-01-01", end="2024-01-07")
    age_groups = np.arange(0, 80, 5)
    
    for day in days:
        print(day)
        for i, (adm1, pop1) in enumerate(zip(admin_units['ADM1_PCODE'].unique(), level1_pop)):
            age_group_pop = divide_population(pop1, len(age_groups))
            for j, (age_min, pop2) in enumerate(zip(age_groups, age_group_pop)):
                sex_pop = divide_population(pop2, 2)
                for k, (sex, pop3) in enumerate(zip(['male', 'female'], sex_pop)):
                    pop_upper = int(pop3 * (1 + np.random.randint(0, 20)/100))
                    pop_lower = int(pop3 * (1 - np.random.randint(0, 20)/100))
                    pop_posterior = np.random.randint(pop_lower, pop_upper + 1, 1000).tolist()
                    data.append({
                        "admin_level": 1,
                        "pcode": adm1,
                        "day": day,
                        "age_min": age_min,
                        "age_max": age_min + 4,
                        "sex": sex,
                        "pop": pop3,
                        "pop_upper": pop_upper,
                        "pop_lower": pop_lower,
                        "pop_posterior": pop_posterior
                    })
                    
                    level2_units = admin_units[admin_units['ADM1_PCODE'] == adm1]['ADM2_PCODE'].unique()
                    level2_pop = divide_population(pop3, len(level2_units))
                    for l2, pop4 in zip(level2_units, level2_pop):
                        pop_upper = int(pop4 * (1 + np.random.randint(0, 20)/100))
                        pop_lower = int(pop4 * (1 - np.random.randint(0, 20)/100))
                        pop_posterior = np.random.randint(pop_lower, pop_upper + 1, 1000).tolist()
                        data.append({
                            "admin_level": 2,
                            "pcode": l2,
                            "day": day,
                            "age_min": age_min,
                            "age_max": age_min + 4,
                            "sex": sex,
                            "pop": pop4,
                            "pop_upper": pop_upper,
                            "pop_lower": pop_lower,
                            "pop_posterior": pop_posterior
                        })
                        
                        level3_units = admin_units[admin_units['ADM2_PCODE'] == l2]['ADM3_PCODE'].unique()
                        level3_pop = divide_population(pop4, len(level3_units))
                        for l3, pop5 in zip(level3_units, level3_pop):
                            pop_upper = int(pop5 * (1 + np.random.randint(0, 20)/100))
                            pop_lower = int(pop5 * (1 - np.random.randint(0, 20)/100))
                            pop_posterior = np.random.randint(pop_lower, pop_upper + 1, 1000).tolist()
                            data.append({
                                "admin_level": 3,
                                "pcode": l3,
                                "day": day,
                                "age_min": age_min,
                                "age_max": age_min + 4,
                                "sex": sex,
                                "pop": pop5,
                                "pop_upper": pop_upper,
                                "pop_lower": pop_lower,
                                "pop_posterior": pop_posterior
                            })
    df = pd.DataFrame(data)
    df.insert(0, 'country', 'UKR')
    df['admin_level'] = df['admin_level'].astype('category')
    df['sex'] = df['sex'].astype('category')
    return df


def make_dummy_migration_data(df, admin_levels : list[int]):
    days = pd.date_range(start="2024-01-01", end="2024-01-07", freq="W")
    age_groups = np.arange(0, 80, 5)
    sexes = ['male', 'female']

    data = []

    for level in admin_levels:
        admin_units = df[f"ADM{level}_PCODE"].unique().tolist()
        for origin, destination, day, age_min, sex in product(admin_units, admin_units, days, age_groups, sexes):
            if origin != destination:
                probability = round(uniform(0, 0.0001), 8)
                data.append({
                    "country": "UKR",
                    "admin_level": level,
                    "origin": origin,
                    "destination": destination,
                    "day": day,
                    "age_min": age_min,
                    "age_max": age_min + 4,
                    "sex": 0 if sex == 'male' else 1, 
                    "probability": probability,
                    "count": probability * UKRAINE_POPULATION
                })
    df = pd.DataFrame(data)
    return df



def main_migration(
        migration_out_file: str | Path,
        migration_levels: list[int],
        save_to_parquet: bool = False
        ) -> pd.DataFrame:
    adm_units = BASE.joinpath("ADMIN_UNITS.csv")
    df = pd.read_csv(adm_units)
    df_migration = make_dummy_migration_data(df, admin_levels=migration_levels)
    if save_to_parquet:
        df_migration.to_parquet(migration_out_file, index=False)
    else:
        df_migration.to_csv(migration_out_file, index=False)


def main_pop(
        out_file: str | Path,
        pop: int = 50000000,
        save_to_parquet: bool = False 
        ) -> pd.DataFrame:
    adm_units = BASE.joinpath("ADMIN_UNITS.csv")
    df = pd.read_csv(adm_units)
    df_out = make_dummy_pop_data(df, pop)
    if save_to_parquet:
        df_out.to_parquet(out_file, index=False, compression="gzip")
    else:
        df_out.to_csv(out_file, index=False)


if __name__ == "__main__":
    out_file = BASE.parent.parent.parent.parent.joinpath("data", "dummy_tables", "dummy_pop.csv")
    migration_out_file = BASE.parent.parent.parent.parent.joinpath("data", "dummy_tables", "dummy_migration.csv")
    migration_levels = [1]
    main_migration(migration_out_file, migration_levels)
    main_pop(out_file)
