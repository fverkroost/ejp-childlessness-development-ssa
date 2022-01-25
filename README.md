# ejp-childlessness-development-ssa
This repository contains the code used to generate the results published in the paper 

**Verkroost, F. C. J. & Monden, C. W. S. (forthcoming). Childlessness and Development in sub-Saharan Africa: Is There Evidence for a U-shaped Pattern? _European Journal of Population_.** 

Please cite this paper whenever referring to (parts of) this code.

To reproduce the results presented in the paper, the code published in this repository can be used. Some online, publicly available data should be downloaded by the person aiming to replicate these results. Note that these third-party data may have been updated since the code was last run and the results for the paper were obtained, which may affect how the code runs and what results are produced.

It is highly recommended to create an RStudio project in the same folder in which the code lives. The folder in which the code exists should have the following structure:
|___ Main folder (where the RStudio project is located, name of the folder does not matter)
|	|___ data_files
|	|	|___ GDL_shapes
|	|	|___ HIVDATA
|	|	|	|___ Regional
|	|	|	|___ National

The following steps should be taken before running any code, or else the code will exit with an error saying the necessary data are not available.

## Step 1: Create a DHS profile to be able to download the necessary surveys from the DHS repository. 

1. To make a profile, follow the steps described on [this DHS webpage](https://dhsprogram.com/data/Access-Instructions.cfm). 
2. Make sure to fill in your email address and DHS project name in lines 10 respectively 11 of ```script-main.R```. 

## Step 2: Download the SHDI data from GlobalDataLabs.

For each of the four relevant indicators (Sub-national HDI, Educational index, Health index, Income index), follow these steps:
1. Go to [the GlobalDataLabs webpage](https://globaldatalab.org/shdi/shdi/).
2. Choose the relevant indicator according to 'Index name' in the table below.
3. Click 'Download this' and choose the .csv file format.
4. Save the downloaded file to folder 'data_files' in the RStudio project directory under the name corresponding to the relevant index (see 'File name' in table below).

| Index name        | File name                      |
| ----------------- | ------------------------------ |
| Sub-national HDI  | GDL-Sub-national-HDI-data.csv  |
| Educational index | GDL-Educational-index-data.csv |
| Health index      | GDL-Health-index-data.csv      |
| Income index      | GDL-Income-index-data.csv      |


