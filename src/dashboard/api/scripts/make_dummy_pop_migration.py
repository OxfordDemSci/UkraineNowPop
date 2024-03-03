from pathlib import Path
import pandas as pd
import geopandas as gpd
import numpy as np
import random

BASE = Path(__file__).resolve().parent


# make an empty dataframe mirroring this sqlalchemy model:
def make_empty_df() -> pd.DataFrame:
    return pd.DataFrame(
        columns=[
            "country",
            "admin_level",
            "pcode",
            "day",
            "age_min",
            "age_max",
            "sex",
            "pop",
            "pop_upper",
            "pop_lower",
            "pop_quartiles",
        ]
    )


def divide_population(population, num_groups):
    # Divide a population randomly into groups
    division = np.random.multinomial(population, np.ones(num_groups)/num_groups)
    return division

# def make_dummy_pop_data(admin_units, total_population=50000000):
#     # Create an empty DataFrame to store the results
#     df = pd.DataFrame(columns=["admin_level", "pcode", "day", "age_min", "age_max", "sex", "pop", "pop_upper", "pop_lower", "pop_quartiles"])
#     data_frames = []
#     # Divide the total population into level 1 admin units
#     level1_pop = divide_population(total_population, admin_units['ADM1_PCODE'].nunique())
#     days = pd.date_range(start="2024-01-01", end="2024-01-31")
    
#     for day in days:
#         print(day)
#         for i, (adm1, pop1) in enumerate(zip(admin_units['ADM1_PCODE'].unique(), level1_pop)):
#             # Divide the level 1 population into age groups
#             age_groups = np.arange(0, 85, 5)
#             age_group_pop = divide_population(pop1, len(age_groups))
            
#             for j, (age_min, pop2) in enumerate(zip(age_groups, age_group_pop)):
#                 # Divide the age group population into male and female
#                 sex_pop = divide_population(pop2, 2)
                
#                 for k, (sex, pop3) in enumerate(zip(['male', 'female'], sex_pop)):
#                     pop_upper = pop3 * (1 + random.randint(0, 20)/100)
#                     pop_lower = pop3 * (1 - random.randint(0, 20)/100)
#                     pop_quartiles = np.random.randint(pop_lower, pop_upper + 1, 100).tolist()
#                     df = pd.DataFrame({
#                         "admin_level": [1],
#                         "pcode": [adm1],
#                         "day": [day],
#                         "age_min": [age_min],
#                         "age_max": [age_min + 5],
#                         "sex": [sex],
#                         "pop": [pop3],
#                         "pop_upper": [pop_upper],
#                         "pop_lower": [pop_lower],
#                         "pop_quartiles": [pop_quartiles]
#                     })
#                     data_frames.append(df)
#                     # Repeat the process for level 2 and level 3 admin units
#                     level2_units = admin_units[admin_units['ADM1_PCODE'] == adm1]['ADM2_PCODE'].unique()
#                     level2_pop = divide_population(pop3, len(level2_units))
                    
#                     for l2, pop4 in zip(level2_units, level2_pop):
#                         pop_upper = pop4 * (1 + random.randint(0, 20)/100)
#                         pop_lower = pop4 * (1 - random.randint(0, 20)/100)
#                         pop_quartiles = np.random.randint(pop_lower, pop_upper + 1, 100).tolist()
#                         df = pd.DataFrame({
#                             "admin_level": [2],
#                             "pcode": [l2],
#                             "day": [day],
#                             "age_min": [age_min],
#                             "age_max": [age_min + 5],
#                             "sex": [sex],
#                             "pop": [pop4],
#                             "pop_upper": [pop_upper],
#                             "pop_lower": [pop_lower],
#                             "pop_quartiles": [pop_quartiles]
#                         })
#                         data_frames.append(df)
#                         level3_units = admin_units[admin_units['ADM2_PCODE'] == l2]['ADM3_PCODE'].unique()
#                         level3_pop = divide_population(pop4, len(level3_units))
                        
#                         for l3, pop5 in zip(level3_units, level3_pop):
#                             pop_upper = pop5 * (1 + random.randint(0, 20)/100)
#                             pop_lower = pop5 * (1 - random.randint(0, 20)/100)
#                             pop_quartiles = np.random.randint(pop_lower, pop_upper + 1, 100).tolist()
#                             df = pd.DataFrame({
#                                 "admin_level": [3],
#                                 "pcode": [l3],
#                                 "day": [day],
#                                 "age_min": [age_min],
#                                 "age_max": [age_min + 5],
#                                 "sex": [sex],
#                                 "pop": [pop5],
#                                 "pop_upper": [pop_upper],
#                                 "pop_lower": [pop_lower],
#                                 "pop_quartiles": [pop_quartiles]
#                             })
#                             data_frames.append(df)
#     df_final = pd.concat(data_frames)
#     return df_final


def make_dummy_pop_data(admin_units, total_population=50000000):
    data = []
    level1_pop = divide_population(total_population, admin_units['ADM1_PCODE'].nunique())
    days = pd.date_range(start="2024-01-01", end="2024-01-31")
    age_groups = np.arange(0, 85, 5)
    
    for day in days:
        for i, (adm1, pop1) in enumerate(zip(admin_units['ADM1_PCODE'].unique(), level1_pop)):
            age_group_pop = divide_population(pop1, len(age_groups))
            for j, (age_min, pop2) in enumerate(zip(age_groups, age_group_pop)):
                sex_pop = divide_population(pop2, 2)
                for k, (sex, pop3) in enumerate(zip(['male', 'female'], sex_pop)):
                    pop_upper = pop3 * (1 + np.random.randint(0, 20)/100)
                    pop_lower = pop3 * (1 - np.random.randint(0, 20)/100)
                    pop_quartiles = np.random.randint(pop_lower, pop_upper + 1, 100).tolist()
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
                        "pop_quartiles": pop_quartiles
                    })
    df = pd.DataFrame(data)
    return df



def main(adm_units: str | Path, out_csv: str | Path, pop: int = 50000000) -> None:
    df = pd.read_csv(adm_units)
    #df_out = make_empty_df()
    df_out = make_dummy_pop_data(df, pop)
    df_out.to_csv(out_csv, index=False)
    


if __name__ == "__main__":
    adm_units = BASE.joinpath("ADMIN_UNITS.csv")
    out_csv = BASE.joinpath("dummy_pop.csv")
    main(adm_units, out_csv)