# rainydaysfrrf2
## A Package for quick analysis of Act2Run and RunSTAF FRRf files
Welcome! This is a package designed to quickly extract data from Fast Repetition Rate fluorometry (FRRf) files in a user- friendly manner. Included are functions for both FastOcean/ Act2Run files and LabSTAF/ RunSTAF files. This package is still in development, however the base functions (photophys) are available. To install and use this package, please read the below information carefully!

Functions are contiunously being written and added- if there is something that would be useful for your data analysis that is not already included, please reach out at mb203@rice.edu to get it added. 

### NOTE: Due to data structure, there is differing information for Act2Run (FastOcean) and RunSTAF (LabSTAF) files. Please read carefully to find the information relevant to you. 
### LabSTAF information:
LabSTAF has several options for data export. Currently, functions are written to accomodate r-PE.csv files only. Should need arise, other file export types can be incorporated. 

Act2Run file:
load.frrf() function & modeling functions are from Tom Bryce Kelly's TheSource package
Kelly, T.B., _TheSource: It will hold your beer. Zenodo (2019). DOI: 10.5281/zenodo.3468524 DOI 

Contributors:
Tom Bryce Kelly (tbrycekell@gmail.com) (Act2Run file loading & modeling functions)

Jared M Rose (JR19@mailbox.sc.edu) (og code)

HoWan Chan (howan@rice.edu) (RunSTAF file loading)

#### Last update: 3-15-25 : 
Added Match.sheet functions- match metadata directly to FRRf files 

### Coming soon:
Calculate JVPII (sigma & absorption method)
Extract data from .saq LabSTAF files
Calculate paramters at acclimation light intensity
Match metadata from closest time

# Installing
To install rainydaysfrrf2, please run the following code: 
```{r}
devtools::install_github('mlorenbaker/rainydaysfrrf2')

library(rainydaysfrrf2)
```

### You also need a separate package
Act2Run loading and LabSTAF loading require different packages to load in. 
I didn't add dependencies sorry- you have to load them separately. One day, perhaps they will be added, but they are low priority. 
```{r}
library(dplyr) #Act2Run
library(readr) #LabSTAF
```

# Loading Files
## Act2Run
To load in Act2Run files, you need to set your working directory (wd) to the _folder outside of the folder your frrf files are in_. 
Ex: My frrf files are in the directory "C:/Users/mlorenbaker/Documents/FRRf/Data"
I would set my wd to "C:/Users/mlorenbaker/Documents/FRRf"
In the code, replace 'folder' with the specific folder holding your frrf files. In my case, it would be 'Data'
Ex: frrf.files = list.files('Data') ; frrf = load.frrf(input.dir = 'Data/', file.names = frrf.files)
```{r}
setwd()
frrf.files = list.files('folder')

frrf = load.frrf(input.dir = 'folder/', file.names = frrf.files)
```
## LabSTAF
To load in LabSTAF files, you need to set your working directory (wd) to the _folder your frrf files are in_. 
Ex: My frrf files are in the directory "C:/Users/mlorenbaker/Documents/FRRf/Data"
I would set my wd to "C:/Users/mlorenbaker/Documents/FRRf/Data"
In the code, replace "input directory" with this working directory path
Ex: frrf = load.labSTAF("C:/Users/mlorenbaker/Documents/FRRf/Data")

### Update as of 3-15-25: 
The rP-E files are structured differently based on your specific settings. The loading function for these files is now custom to the person- so please contact mb203@rice.edu to have a function written for your files. Hopefully this is not a permanent need- MLB is working on analysis of Saq files, which seem more standard. 


```{r}
frrf = load.LabSTAF_DJB("input directory")

frrf = load.LabSTAF_ASLO25("input directory")

# The 'DJB' here is the initials of the person this specific function is written for
# The 'ASLO25' is an example of the specific function written for a project of mine

```

# Act2Run Files ONLY
Tom Bryce Kelly wrote the load.frrf() function for the Act2Run files. In his code, there is an included "stamp" file with metadata associated with the original package TBK wrote (TheSource; no longer being maintained). **We must remove this file**. Please run the following code **before running any further code**. 
Must remove a "stamp file" from loading function

```{r}
length(frrf) #check length of loaded files

frrf<-frrf[1:(length(frrf)-1)] 

length(frrf)
```
# Extracting Photophysiological Data 
## Act2Run files:
At the current time, the base functions for extracting photophysiological data _are only for LED A_. 
TBK's loading function loads LED A-D. If you want functions for extracting B-D, please contact mb203@rice.edu
Additionally, TBK's loading function loads additional data, such as Ek, Ka, Alpha, etc. from other sections of the data file. 

The written functions extract "dark acclimated" data, where E = 0. As some labs have different settings on where the dark step is in the FLC set up, the code automatically searches for where E = 0 and extracts the specified value. 

This is not an exhaustive list of written functions- just commonly needed examples. Some functions are applicable to both Act2Run and LabSTAF files, however due to the nature of LabSTAF files, not all are (aka there are some LabSTAF and Act2Run specific functions). The Act2Run and LabSTAF specific functions are denoted below. 

The below example functions extract 'dark acclimated values', where E = 0
```{r}
# Act2Run and LabSTAF files
get.fvfm(frrf)
get.Ek(frrf)
get.alpha(frrf)
get.JVPIIm(frrf) # maximum JVPII value
get.pm(frrf)
get.Fm(frrf)
get.Fo(frrf)

# LabSTAF
get.sigmapii(frrf) # Sigma PII
get.F(frrf) # Fo
### NPQ and NSV calculations for LabSTAF are coming soon!

# Act2Run
get.sigma(frrf) # Sigma
get.NSV(frrf)
get.NPQ(frrf)
get.fvfm.LS(frrf)
calc.NPQ(frrf)

```
## Using the functions
To properly extract data, you must use the functions to save data to a vector. Then, bind vectors together to export. 

Examples:
```{r}
# Extract Data
fvfm <- get.fvfm(frrf) # Act2Run
fvfm <- get.fvfm.LS(frrf) #LabSTAF
NSV <- get.NSV(frrf)
file <- get.File(frrf) # Name of file!

## Werid thing for calculating NPQ with LabSTAF files:
frrf <- calc.NPQ(frrf)
## Will append the calculated NPQ into 'frrf[[i]]$A$NPQ.calc'

# Bind Vectors
final_data <- as.data.frame(cbind(file, fvfm, NSV))

# Export Data
write.csv(final_data, "export_dir")

```

# Using a Match sheet

A Match.sheet is a .csv file of metadata. It's linked to the FRRf data through the file name. This metadata can include anything you want- treatment, incubator, notes, etc. It's a way of helping organize your FRRf files in your experiment. To use it currently, you need to have the exact file name associated with the specific metadata. Usually when we take measurements, we write down the time of measurement and the culture it came from. The files save automatically, all saved in the same naming pattern (Act2Run: 'YYYYMMDD-hhmmss.csv', LabSTAF: 'YYMMDD-hhmm rP-E.csv').These functions should work for Act2Run and LabSTAF files. Here is a step by step protocol for proper organization: 
1. Copy desired FRRf files into a new folder
2. Sort by file name
3. Highlight all .csv files
4. Copy file path
5. Paste into .csv file (ensure that metadata is sorted by Datetime)
6. Sort the new file path column
7. Highlight file path column --> Text-to-columns --> Deliminated --> '/' --> Finish
8. Delete unnecessary columns from the file path
9. Check that the file name matches the Datetime correctly

Examples of columns that could be included in a Match.sheet, and how they would need to be named in the .csv: 
1. File.name
2. Datetime
3. Treatment
4. Incubator
5. Iron
6. Condition

## Using the Match.sheet
```{r}
Match.sheet <- read.csv(file = "Match.sheet.csv", header = T, sep = ",")

# File matching

frrf <- match.files(frrf, Match.sheet)

# Data extraction

get.Treatment(frrf)

```

# Example workflow for LabSTAF data 
```{r}

devtools::install_github('mlorenbaker/rainydaysfrrf2')

# Load Packages #####

library(rainydaysfrrf2)
library(readr)

# Load Files #####

frrf = load.LabSTAF("C:/Users/mlorenbaker/Documents/LabSTAF_Test_Files")

# Extract Basic Data #####

File <- get.File(frrf)

Fo <- get.F(frrf)

Fm <- get.Fm(frrf)

FvFm <- get.fvfm.LS(frrf)

Sigma <- get.sigmapii(frrf)

frrf <- calc.NPQ(frrf)

NPQ <- get.NPQ.LS(frrf)

# Extract file notes #####

blank <- get.Blank(frrf)

proj <- get.Project(frrf)

ref <- get.Reference(frrf)

notes <- get.Notes(frrf)

# Bind #####

data <- as.data.frame(cbind(File = File,
                            Fo = Fo,
                            Fm = Fm,
                            Fv.Fm = FvFm,
                            Sigma = Sigma,
                            NPQ = NPQ,
                            Blank = blank,
                            Project = proj,
                            Reference = ref,
                            Notes = notes))

# Export #####

write.csv(data, "C:/Users/mlorenbaker/Documents/LabSTAF_Test_Files/Test.data.csv")

```

