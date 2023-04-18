# External Validation

Generates the boostrap CI's for the external datasets and prints the LaTex table. CI code adapted from https://github.com/nyukat/mammography_metarepository.

Requires csv files with one row per unit (breast or episode) with at least following columns (can have others):
```
episode_id,outcome,prediction,label
id-0001,0,0.02681,0
id-0002,2,0.12621,1
id-0003,0,0.45762,0
id-0004,1,0.6678,1
id-0005,0,0.0387234,0
```
`episode_id`: identifier for each episode

`outcome`: 0 for normal, 1 for screen-detected cancer, 2 for interval cancer, 3 for benign

`prediction`: model score

`label`: binary value for cancer/non-cancer (depending on whether intervals are used)
