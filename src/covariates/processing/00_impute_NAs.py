from py_helpers.utils import *

# parameters
country = "ua"
folder = os.getenv("ggfolder") + "_" + country
output_name = country + "_pwtt_oblast.csv"


# impute pwtt for missing dates by propogating the last valid observation forward to the next valid

pwtt = pd.read_csv(out_dir / "covariates" / "raw" / folder / output_name)

pwtt["value"] = pwtt.sort_values(["i", "t"]).groupby("i")["value"].ffill()

# save
pwtt.to_csv(out_dir / "covariates" / "interim" / output_name, index=False)
