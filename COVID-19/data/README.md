This directory is used to store data on disk for the application:

The following files will be pulled from the source data github repo the first time the application is run. Any subsequent running of the application will make a call to the repo
and will check if the data has changed. If it's changed then it will be redownloaded, otherwise the data already on disk will be used. 

* confirmed.csv
* recovered.csv
* deaths.csv

The following files are config files for the application.
* github_details.csv - this file contains hashes of the above 3 files and is used as the basis for the check if the files have changed.
* access_tokens.csv - this contains the news api authentication details. This will need to be added by the user with following structure

| service  | token     |
|----------|-----------|
| news_api | "api_key" |


