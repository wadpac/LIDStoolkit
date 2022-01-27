# LIDStoolkit
R package to aid with LIDS analysis

## Installation

```
install.packages("remotes")
remotes::install_github("wadpac/LIDStoolkit")
library("LIDStoolkit")
```


## Adding sleep bout indicators to the GGIR time series

### 1. Load time series output

After loading the file it should reflect a data.frame hold a time series spanning the entire recording of a single participant. Each row represents one epoch in time. We expect various columns, but for now we rely on the columns:

- window: A number reflecting the 'day' of measure. Here, a day should have been defined as WW in GGIR, which reflects waking up to waking up.
- class_id: A number, where value = 0 reflects detected sleep

### 2. Use the LIDStoolkit funcion add_sleepbout_colum to add a column with detected sleep bouts.

```
ts_new = add_sleepbout_column(ts = ts, epochSize = epochSize))
```

Where ts is the data.frame from step 1, and epochSize is the epoch size in seconds the time series is stored in.

The added `sleepbout` column in ts_new will reflect a number:

- 0: Not a sleep bout, in other words the person was awake.
- 1: First sleep bout of the corresponding (waking-up to waking-up) window.
- 2: Second sleep bout of the corresponding  (waking-up to waking-up) window.
- 3: ... etc. for all subsequent sleep bouts for the corresponding (waking-up to waking-up) window.

Note that all arguments used for the `detect_sleepbout` function can also be passed on to the `add_sleepbout_column` function:

- sleepBinary: Numeric vector with for each epoch in time a 1 for sleep or a 0 for wakefulness
- wakeBoutThreshold: Number between 0 and 1 being the maximum ratio of wakefulness allowed per 30 minutes
- wakeBoutMin: Maximum duration of of a wakefulness bout
- sleepBoutMin: Minimum duration of a sleep bout
- epochSize: Epoch size in seconds