from pathlib import Path
import geopandas as gpd
import pandas as pd

OUT_GPKG = Path(__file__).resolve().parent.parent.joinpath("app", "data", "db-data", "GEODATA.gpkg")


def make_subset_columns(languages: list[str], levels: list[int]) -> list[str]:
    cols = []
    for level in levels:
        cols.append(f"ADM{level}_PCODE")
        for language in languages:
            cols.append(f"ADM{level}_{language}")
    cols.append("geometry")
    return cols


def subset_and_dissolve(gdf: gpd.GeoDataFrame, level: int, languages: list[str]) -> gpd.GeoDataFrame:
    col_map = {
        "pcode": f"adm{level}_pcode",
        "country": "country",
        "country_lan2": f"adm0_{languages[1].lower()}",
        "country_lan3": f"adm0_{languages[2].lower()}",
        "name_en": f"adm{level}_en",
        "name_lan2": f"adm{level}_{languages[1].lower()}",
        "name_lan3": f"adm{level}_{languages[2].lower()}",
        "geometry": "geometry"
    }
    subset_cols = list(col_map.values())
    gdf = gdf[subset_cols]
    gdf.rename(columns={value: key for key, value in col_map.items()}, inplace=True)
    gdf = gdf.dissolve(by=[x for x in gdf.columns if x != "geometry"]).reset_index()
    gdf["admin_level"] = level
    cols = list(col_map.keys())
    cols.insert(1, "admin_level")
    gdf = gdf[cols]
    return gdf


def prep_shp(
    shp: Path | str,
    iso3: str,
    languages: list[str],
    levels: list[int],
    tolerance: float = 0.001,
) -> None:
    gdf_list = []
    gdf = gpd.read_file(shp)
    cols = make_subset_columns(languages=languages, levels=levels)
    gdf_subset = gdf[cols]
    gdf_subset.loc[:, 'geometry'] = gdf_subset.simplify(tolerance)
    gdf_simplify = gdf_subset.copy()
    gdf_simplify["country"] = iso3
    gdf_simplify.rename(columns={x: x.lower() for x in gdf_simplify.columns}, inplace=True)
    gdf_simplify = gdf_simplify[[x for x in gdf_simplify.columns if x != "adm0_pcode"]]
    for level in levels[1:]:
        gdf_sub = subset_and_dissolve(gdf_simplify, level, languages=languages)
        gdf_list.append(gdf_sub)
    gdf_final = gpd.GeoDataFrame(pd.concat(gdf_list, ignore_index=True))
    print(gdf_final.head())
    print(gdf_final.columns)
    gdf_final.to_file(OUT_GPKG, layer=iso3, driver="GPKG")


if __name__ == "__main__":
    shp = r"C:\Users\dkerr\Documents\GISRede\OXFORD_UNI_WORK\UKR_data_playground\ukr_admbnda_sspe_20230201_shp\ukr_admbnda_sspe_20230201_SHP\ukr_admbnda_adm3_sspe_20230201.shp"
    languages = ["EN", "UA", "RU"]
    levels = [0, 1, 2, 3]
    prep_shp(shp, "UKR", languages=languages, levels=levels)
