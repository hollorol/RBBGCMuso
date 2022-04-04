This is a si

## Preparations

### Loading the RBBGCMuso package and the necessary functions

```{r}
library(RBBGCMuso)
source("make_individual_trees.R") # The DT creation and update algorithms
source("glue.R") # GLUE optimizer algorithms
```

The file containing the path to the observation files (Martonvasar_maize.obs), and the parameter intervals (Martonj) 

### Reading the observations

The mean yield had to be adjust. see in art.

```{r}
measureFile <- "Martonvasar_maize.obs"
measurements <- read.csv2(measureFile, stringsAsFactors=FALSE)
measurements$mean <- measurements$mean / 10000 * 0.85
measurements$sd <- measurements$sd / 10000 * 0.85
```

### Define conditioning functions

constraints.json

```{json}
{
    "constraints": [

        {
            "Expression": "SELECT(harvest_index, max)|median",
            "Min": 0.45,
            "Max": 0.55
        },
        {
            "Expression": "SELECT(proj_lai, max)|quantile(.,0.5)",
            "Min": 2.7,
            "Max": 5
        },
        {
            "Expression": "SELECT(rootdepth5, max)|quantile(.,0.5)",
            "Min": 1.40,
            "Max": 1.80
        },			
        {
            "Expression": "SELECT(flower_date, max)|quantile(.,0.5)",
            "Min": 180,
            "Max": 190 
        }
    ],

    "treshold": 80
}
```

```{r}
constraints <- jsonlite::read_json("constraints.json",simplifyVector=TRUE)
```

### Cal file:

```{verbatim}

Martonvasar_maize.obs
Martonvasar_maize.set
site
Martonvasar_maize;211
```
