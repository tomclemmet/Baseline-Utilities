# Baseline Utilities

Code repository for my dissertation exploring methods for the estimation of baseline utilities.
 
## Folder Structure (including .gitignore files):

```
baseline-utilities
│   .gitattributes
│   .gitignore
│   baseline-utilities.Rproj
│   README.md
│   run-everything.R
│   session-info.txt
│
├───data
│       hse.csv
│       lifetables-1820.csv
│       npara-baselines.csv
│       para-baselines.csv
│
├───output
│   ├───1-literature
│   │       fig-01--baselines.png
│   │       fig-02--baselines.png
│   │
│   ├───2-data
│   │       app-a1--regression.html
│   │       fig-03--means.png
│   │       fig-04--distributions.png
│   │       fig-05--means.png
│   │       fig-06--variation.png
│   │       tab-01--summary.csv
│   │       tab-02--errors.csv
│   │
│   ├───3-results
│   │       app-b1--errors.csv
│   │       app-b2--errors.csv
│   │       app-c1--qales.csv
│   │       fig-07--kfold.png
│   │       fig-08--rmse.png
│   │       fig-09--rmse.png
│   │       fig-10--errors.png
│   │       fig-11--me.png
│   │       fig-12--me.png
│   │       fig-13--qale.png
│   │       kfold-results.csv
│   │       me-sex.png
│   │       models.txt
│   │       rmse-sex.png
│   │
│   └───4-discussion
│           app-d1--rcs7.csv
│           app-d2--rcs7.png
│
├───raw-data
│   │   hse03ai.tab
│   │   hse04ai.tab
│   │   hse05ai.tab
│   │   hse06ai.tab
│   │   hse08ah.tab
│   │   hse08ai.tab
│   │   hse10ai.tab
│   │   hse11ai.tab
│   │   hse12ai.tab
│   │   hse14ai.tab
│   │
│   └───data-dictionaries
│           hse03ai_UKDA_Data_Dictionary.rtf
│           hse04ai_UKDA_Data_Dictionary.rtf
│           hse05ai_UKDA_Data_Dictionary.rtf
│           hse06ai_UKDA_Data_Dictionary.rtf
│           hse08ai_ukda_data_dictionary.rtf
│           hse10ai_ukda_data_dictionary.rtf
│           hse11ai_ukda_data_dictionary.rtf
│           hse12ai_ukda_data_dictionary.rtf
│           hse14ai_ukda_data_dictionary.rtf
│
└───script
    │   kfold.R
    │   load-hse.R
    │
    └───tables-and-figures
            1-literature.R
            2-data.R
            3-results.R
            4-discussion.R
```
