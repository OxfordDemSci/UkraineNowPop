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


def subset_gdf_for_dissolve_and_rename_cols(gdf: gpd.GeoDataFrame, level: int, languages: list ) -> gpd.GeoDataFrame:
    cols = [x for x in gdf.columns if str(level) in x]
    cols += ["country", "geometry"]
    gdf_sub = gdf[cols]
    gdf_sub = gdf_sub.set_index([x for x in gdf_sub.columns if x != "geometry"])
    gdf_sub = gdf_sub.dissolve(by=[x for x in gdf_sub.columns if x != "geometry"])
    gdf_sub.reset_index(inplace=True)
    rename_cols = {f"ADM{level}_PCODE": "PCODE", f"ADM{level}_GEOMETRY": "geometry"}
    gdf_sub.rename(columns=rename_cols, inplace=True)
    return gdf_sub


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
        cols = ["country", "geometry"]
        cols += [x for x in gdf_simplify.columns if str(level) in x]
        breakpoint()
        gdf_sub = gdf_simplify[cols]
        # Set index and dissolve
        gdf_sub = gdf_sub.set_index([x for x in gdf_sub.columns if x not in ["geometry"]])
        gdf_sub = gdf_sub.dissolve(by=[x for x in gdf_sub.columns if x not in ["geometry"]])
        gdf_sub.reset_index(inplace=True)
        rename_cols = {"adm{level}_pcode": "pcode", "adm{level}_geometry": "geometry"}
        gdf_list.append(gdf_sub)
    gdf_final = gpd.GeoDataFrame(pd.concat(gdf_list, ignore_index=True))
    gdf_final.to_file(OUT_GPKG, layer=iso3, driver="GPKG")


if __name__ == "__main__":
    shp = r"C:\Users\dkerr\Documents\GISRede\OXFORD_UNI_WORK\UKR_data_playground\ukr_admbnda_sspe_20230201_shp\ukr_admbnda_sspe_20230201_SHP\ukr_admbnda_adm3_sspe_20230201.shp"
    languages = ["EN", "UA", "RU"]
    levels = [0, 1, 2, 3]
    prep_shp(shp, "UKR", languages=languages, levels=levels)
