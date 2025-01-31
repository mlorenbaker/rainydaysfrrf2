# rainydaysfrrf2
## A Package for quick analysis of Act2Run and RunSTAF FRRf files
For RunSTAF files- loading function for all still incoming. Please contact mb203@rice.edu with issues

Act2Run file:
load.frrf() function is from Tom Bryce Kelly's TheSource package
Kelly, T.B., _TheSource: It will hold your beer. Zenodo (2019). DOI: 10.5281/zenodo.3468524 DOI 

# Installing
```{r}
devtools::install_github('mlorenbaker/rainydaysfrrf2')

library(rainydaysfrrf2)
```

### You also need a separate package
```{r}
library(dplyr)
```

# Loading Files
```{r}
setwd()
frrf.files = list.files('folder')

frrf = load.frrf(input.dir = 'folder/', file.names = frrf.files)
```

# Act2Run Files ONLY
Must remove a "stamp file" from loading function

```{r}
length(frrf) #check length of loaded files

frrf<-frrf[1:(length(frrf)-1)] 

length(frrf)
```
# Extracting Photophysiological Data 
Act2Run files:
Assuming you want values from LED A
TBK's loading function loads LED A-D. If you want functions for extracting B-D, please contact mb203@rice.edu

The below example functions extract 'dark acclimated values', where E = 0
```{r}
# Act2Run and LabSTAF files
get.fv.fm(frrf)
get.Ek(frrf)
get.alpha(frrf)
get.JVPIIm(frrf) # maximum JVPII value
get.pm(frrf)
get.Fm(frrf)
get.Fo(frrf)

# LabSTAF
get.sigmapii(frrf) # Sigma PII

# Act2Run
get.sigma(frrf) # Sigma

```




