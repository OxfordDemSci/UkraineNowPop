This working directory will contain inputs and outputs for most scripts in the `./src/` directory. The directory is included in our `.gitignore` so these files will not sync with GitHub; They only appear locally.  

Most scripts using input data that are publicly available include code to download those data dynamically. Some external data that are publicly-available are included in the repositories data directory `./data/`. External source files that are restricted access need to be manually added to `./wd/in/`. 

All outputs from scripts will be written into a sub-directory of `/wd/out`, usually with a name corresponding to the script that created the file. 