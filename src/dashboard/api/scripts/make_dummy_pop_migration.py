from pathlib import Path
import pandas as pd
import geopandas as gpd
import numpy as np
import random

BASE = Path(__file__).resolve().parent


def divide_population(population, num_groups):
    division = np.random.multinomial(population, np.ones(num_groups)/num_groups)
    return division


def make_dummy_pop_data(admin_units, total_population=50000000):
    data = []
    level1_pop = divide_population(total_population, admin_units['ADM1_PCODE'].nunique())
    days = pd.date_range(start="2024-01-01", end="2024-02-29")
    age_groups = np.arange(0, 85, 5)
    
    for day in days:
        for i, (adm1, pop1) in enumerate(zip(admin_units['ADM1_PCODE'].unique(), level1_pop)):
            age_group_pop = divide_population(pop1, len(age_groups))
            for j, (age_min, pop2) in enumerate(zip(age_groups, age_group_pop)):
                sex_pop = divide_population(pop2, 2)
                for k, (sex, pop3) in enumerate(zip(['male', 'female'], sex_pop)):
                    pop_upper = pop3 * (1 + np.random.randint(0, 20)/100)
                    pop_lower = pop3 * (1 - np.random.randint(0, 20)/100)
                    pop_quantiles = np.random.randint(pop_lower, pop_upper + 1, 100).tolist()
                    data.append({
                        "admin_level": 1,
                        "pcode": adm1,
                        "day": day,
                        "age_min": age_min,
                        "age_max": age_min + 5,
                        "sex": sex,
                        "pop": pop3,
                        "pop_upper": pop_upper,
                        "pop_lower": pop_lower,
                        "pop_quantiles": pop_quantiles
                    })
    df = pd.DataFrame(data)
    return df



def main(adm_units: str | Path, out_csv: str | Path, pop: int = 50000000) -> None:
    df = pd.read_csv(adm_units)
    df_out = make_dummy_pop_data(df, pop)
    df_out.to_csv(out_csv, index=False)
    


if __name__ == "__main__":
    adm_units = BASE.joinpath("ADMIN_UNITS.csv")
    out_csv = BASE.joinpath("dummy_pop.csv")
    main(adm_units, out_csv)