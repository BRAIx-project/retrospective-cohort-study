# External Validation

Generates the boostrap CI's for the external datasets and prints the LaTex table. CI code adapted from https://github.com/nyukat/mammography_metarepository.

## Input data

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

## Installation
Using conda

```
conda install --file environment.yml
conda activate external-validation
```

## Usage

Example table generated through sample data files in `data/`:
```
python external_validation.py --base_path "." -n=100 --seed=123
``````

Generates an output table:

```
                                                       AI reader
Dataset                        Level   AUC                      
CSAW-CC (screen-detected only) Breast  ROC  0.375 (0.000, 0.688)
                                       PR   0.148 (0.056, 0.417)
                               Episode ROC  0.560 (0.188, 1.000)
                                       PR   0.692 (0.119, 1.000)
CSAW-CC                        Breast  ROC  0.800 (0.333, 1.000)
                                       PR   0.894 (0.636, 1.000)
                               Episode ROC  0.500 (0.143, 0.750)
                                       PR   0.183 (0.062, 0.367)
CMMD                           Breast  ROC  0.381 (0.000, 0.750)
                                       PR   0.228 (0.056, 0.539)
                               Episode ROC  0.762 (0.476, 1.000)
                                       PR   0.622 (0.100, 1.000)
BREAST                         Breast  ROC  0.417 (0.000, 1.000)
                                       PR   0.502 (0.118, 1.000)
                               Episode ROC  0.680 (0.238, 1.000)
                                       PR   0.708 (0.197, 1.000)
ADMANI Prospective             Breast  ROC  0.625 (0.000, 1.000)
                                       PR   0.238 (0.056, 1.000)
                               Episode ROC  0.500 (0.000, 1.000)
                                       PR   0.212 (0.050, 1.000)
```
