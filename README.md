# Childlessness and Development in sub-Saharan Africa
This repository contains the code used to generate the results published in the paper 

**Verkroost, F. C. J. & Monden, C. W. S. (forthcoming). Childlessness and Development in sub-Saharan Africa: Is There Evidence for a U-shaped Pattern? _European Journal of Population_.** 

Please cite this paper whenever referring to (parts of) this code.

## Explanation

To keep the code structured and clear, the code in this repository is spread across a number of different files:

| Script file                        | Functions file                     | Description                                          |
| ---------------------------------- | ---------------------------------- | ---------------------------------------------------- |
| ```script-initialization.R```      |                                    | Load packages, set seed                              |
| ```script-main.R```                | ```functions-main.R```             | Call other scripts for main and sensitivity analyses |
| ```script-data-loading.R```        |                                    | Load DHS data into R                                 |
| ```script-data-prep.R```           | ```functions-data-prep.R```        | Clean and wrangle all necessary data                 |
| ```script-data-plot.R```           | ```functions-data-plot.R```        | Make plots of data                                   |
| ```script-additional-analyses.R``` |                                    | Perform descriptive analyses                         |
| ```script-multilevel-model.R```    | ```functions-multilevel-model.R``` | Perform multilevel modelling                         |

The only file that the user needs is ```script-main.R```. As explained later, some DHS configurations need to be set by the user in this script; and the user can decide which analyses (e.g. only the main analyses or also some sensitivity analyses) to run.

## Preparation

To reproduce the results presented in the paper, the code published in this repository can be used. Some online, publicly available data should be downloaded by the person aiming to replicate these results. Note that these third-party data may have been updated since the code was last run and the results for the paper were obtained, which may affect how the code runs and what results are produced. In this case, the exact data used may be available from the author upon request.

It is highly recommended to create an RStudio project in the same folder in which the code lives. The folder in which the code exists should have the following structure before running any code:
```bash
├── main
│   ├── data_files
│   │   ├── GDL_shapes
│   │   ├── HIV_prevalence
│   │   │   ├── regional
│   │   │   ├── national
```

Some data will have to be downloaded by the user. Other data, such as the HIHD or the UN population data are downloaded automatically from the relevant webpages. The following steps should be taken before running any code, or else the code will exit with an error saying the necessary data are not available.

#### Step 1: Create a DHS profile to be able to download the necessary surveys from the DHS repository. 

1. To make a profile, follow the steps described on [this DHS webpage](https://dhsprogram.com/data/Access-Instructions.cfm). 
2. Make sure to fill in your email address and DHS project name in lines 10 respectively 11 of ```script-main.R```. 

#### Step 2: Download the SHDI data from GlobalDataLabs.

For each of the four relevant indicators (Sub-national HDI, Educational index, Health index, Income index), follow these steps:
1. Go to [this GlobalDataLabs webpage](https://globaldatalab.org/shdi/shdi/).
2. Choose the relevant indicator according to 'Index name' in the table below.
3. Click 'Download this' and choose the .csv file format.
4. Save the downloaded file to folder 'data_files' in the RStudio project directory under the name corresponding to the relevant index (see 'File name' in table below).

| Index name        | File name                            |
| ----------------- | ------------------------------------ |
| Sub-national HDI  | ```GDL-Sub-national-HDI-data.csv```  |
| Educational index | ```GDL-Educational-index-data.csv``` |
| Health index      | ```GDL-Health-index-data.csv```      |
| Income index      | ```GDL-Income-index-data.csv```      |

#### Step 3: Download the GlobalDataLabs shape file data for mapping.

1. Go to [this GlobalDataLabs webpage](https://globaldatalab.org/shdi/shapefiles/).
2. Download the GDL codes (```GDL Codes VX```) and shape files (```GDL Shapefiles VX```).
3. Put these all as separate files (without sub-folders) in folder ```GDL_shapes``` (under ```data_files```).

#### Step 4: Download the HIV prevalence data.

1. For the national data, go to the [UNAIDS webpage](http://aidsinfo.unaids.org/). Download the data separately for men and women, making sure to select young men and women. Save this in files under the ```National``` folder (in ```HIVDATA``` in ```data_files```) under names ```nationalYoungMen.csv``` and ```nationalYoungWomen.csv```.
2. For the regional data (part 1), visit [Statcompiler](https://www.statcompiler.com/c831c3ee-3d12-493f-840a-d4730d8e2a31). Save the data under name  ```STATcompilerHIVdata.xlsx``` in the ```Regional``` folder (in ```HIVDATA``` in ```data_files```).
3. For the regional data (part 2), visit [UNAIDS](http://aidsinfo.unaids.org/). Download the data by country and gender separately. Save in format ```countryRegionYoungGender.csv``` (e.g. ```beninRegionYoungMen.csv``` or ```coteIvoireRegionYoungWomen.csv```) in the ```Regional``` folder (in ```HIVDATA``` in ```data_files```). The countries for which these data were obtained originally are (in the way the file names are formatted): benin, coteIvoire, ethiopia, kenya, mozambique, nigeria, togo, zambia and zimbabwe.

## Results

The code will automatically output the results to a new folder ```results``` under ```main```. The main (i.e. not sensitivity) results are saved within folder ```ageCut40_lagW19_lagM24_HIV_sexRatios_sampleCut0_CLNeverBorn``` under ```results```. The results presented in the published paper are the following:

| Paper result | Description                                    | Sub-folder              | File name                                  |
| ------------ | ---------------------------------------------- | ----------------------- | ------------------------------------------ |
| Table 2      | Descriptive statistics                         |                         | descriptiveStatistics.txt                  |
| Figure 3     | Childlessness trends                           |                         | childlessness_trend_ssa_plot_sorted_na.png |
| Figure 4     | Childlessness maps                             |                         | mapChildlessness.png                       |
| Figure 5a    | National-level bivariate scatter plot          | scatter_plots/national/ | childlessSHIHD_bestfit.png                 |
| Figure 5b    | Regional-level bivariate scatter plot          | scatter_plots/regional/ | childlessSHIHD_bestfit.png                 |
| Figure 6     | Regional-level multivariate scatter plot       | scatter_plots/regional/ | finalFit_bestfit.png                       |
| Table 3      | Multilevel regression coefficients             | multilevel_results/     | linMenquadWomenTimeGrouped.txt             |
| Figure 7     | Childlessness types and development components | scatter_plots/regional/ | typeChildlessSHIHDcomp_lin_Genders.png     |

