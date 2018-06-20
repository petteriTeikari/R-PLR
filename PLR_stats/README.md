# PLR_stats

![Init Traces](https://github.com/petteriTeikari/PLR_stats/blob/master/images/init_PLR_plot.png "Init Traces")

_TODO!_ Check the error propagation

![Init Traces](https://github.com/petteriTeikari/PLR_stats/blob/master/images/init_PLR_plot_w323subjects.png "Init Traces")

**And the same with updated 323 subjects** (9th May 2018)

![Init Boxplot](https://github.com/petteriTeikari/PLR_stats/blob/master/images/init_box_plot.png "Init Boxplot")

_TODO!_ obviously, add the titles etc. Now 1st column max constriction, 2nd PIPR5Last5secAUC and Quick Phasic (which for some reason have a different sign)

![Init Boxplot](https://github.com/petteriTeikari/PLR_stats/blob/master/images/init_box_plot_w323subjects.png "Init Boxplot")

**And the same with updated 323 subjects** (9th May 2018)



## To use

`git clone https://github.com/petteriTeikari/PLR_stats.git`

`git submodule update --recursive --remote`

and run `RUN_stats.R` (Ctrl+Alt+R, Run All) from your RStudio IDE

## Some issues

### Inconsistent naming
![Inconsisent naming](https://github.com/petteriTeikari/PLR_stats/blob/master/images/consistency_in_master_sheet.png "Logo Title Text 1")

### Same column names for different sheets?
![Variable names](https://github.com/petteriTeikari/PLR_stats/blob/master/images/sheet_differences.png "Logo Title Text 1")

### Checks whether any subjects are missing from the Excel Data sheet
Which was the case for example for subject **4139* 
![Variable names](https://github.com/petteriTeikari/PLR_stats/blob/master/images/subject_missing.png "Logo Title Text 1")

But actually there was an error for that subject on the Excel Sheet
![Variable names](https://github.com/petteriTeikari/PLR_stats/blob/master/images/error_on_Excel.png "Logo Title Text 1")

### Duplicate rows in Excel datasheet
If you want to increase the manual work, or do the error handling for code, sure keep these, but now only the first kept.

![Variable names](https://github.com/petteriTeikari/PLR_stats/blob/master/images/duplicate_rows.png "Logo Title Text 1")
