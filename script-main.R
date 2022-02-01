# Load initialization and main functions scripts
source('script-initialization.R')
source('functions-main.R')

# Configure session (select Yes and fill in DHS password when prompted)
# This is needed to download the necessary data from the DHS repositories
# See https://dhsprogram.com/data/Access-Instructions.cfm for information 
# about getting access to the data
set_rdhs_config(
  email = "johndoe@gmail.com", # Fill in your email address
  project = "My project name", # Fill in the name of your project at DHS
  config_path = "~/.rdhs.json",
  global = TRUE
)

# ---------------------------------------------------------------------------------
# Perform main analysis (results in paper)
# ---------------------------------------------------------------------------------

# Prepare working directory for main analyses 
prepareFunction(
  ageCutoff = 40, 
  lagWomen = 19, 
  lagMen = 24, 
  HIV = TRUE, 
  sexRatios = TRUE, 
  sampleSizes = 0, 
  childlessnessDefinition = 'everBorn', 
  imputeIndividual = FALSE
)

# Execute main analyses
childlessnessHumanDevelopment(
  ageCutoff = 40, 
  lagWomen = 19, 
  lagMen = 24, 
  HIV = TRUE, 
  sexRatios = TRUE, 
  sampleSizes = 0, 
  childlessnessDefinition = 'everBorn', 
  path, 
  imputeIndividual = FALSE
)

# ---------------------------------------------------------------------------------
# Perform sensitivity analysis (1): higher age cut-off for permanent childlessness
# Do not run as standard; not necessary for main results of paper
# ---------------------------------------------------------------------------------

if (FALSE){
  
  # Prepare working directory for sensitivity analyses 
  prepareFunction(
    ageCutoff = 45, 
    lagWomen = 19, 
    lagMen = 24, 
    HIV = TRUE, 
    sexRatios = TRUE, 
    sampleSizes = 0, 
    childlessnessDefinition = 'everBorn', 
    imputeIndividual = FALSE
  )
  
  # Execute sensitivity analyses
  childlessnessHumanDevelopment(
    ageCutoff = 45, 
    lagWomen = 19, 
    lagMen = 24, 
    HIV = TRUE, 
    sexRatios = TRUE, 
    sampleSizes = 0, 
    childlessnessDefinition = 'everBorn', 
    path, 
    imputeIndividual = FALSE
  )
  
  # ---------------------------------------------------------------------------------
  # Perform sensitivity analysis (2): larger restricted sample sizes 
  # ---------------------------------------------------------------------------------
  
  # Prepare working directory for sensitivity analyses 
  prepareFunction(
    ageCutoff = 40, 
    lagWomen = 19, 
    lagMen = 24, 
    HIV = TRUE, 
    sexRatios = TRUE, 
    sampleSizes = 50, 
    childlessnessDefinition = 'everBorn', 
    imputeIndividual = FALSE
  )
  
  # Execute sensitivity analyses
  childlessnessHumanDevelopment(
    ageCutoff = 40, 
    lagWomen = 19, 
    lagMen = 24, 
    HIV = TRUE, 
    sexRatios = TRUE, 
    sampleSizes = 50, 
    childlessnessDefinition = 'everBorn', 
    path, 
    imputeIndividual = FALSE
  )
  
  # ---------------------------------------------------------------------------------
  # Perform sensitivity analysis (3): higher age cut-off for permanent childlessness
  # ---------------------------------------------------------------------------------
  
  # Prepare working directory for sensitivity analyses 
  prepareFunction(
    ageCutoff = 40, 
    lagWomen = 19, 
    lagMen = 24, 
    HIV = TRUE, 
    sexRatios = TRUE, 
    sampleSizes = 0, 
    childlessnessDefinition = 'childDied', 
    imputeIndividual = FALSE
  )
  
  # Execute sensitivity analyses
  childlessnessHumanDevelopment(
    ageCutoff = 40, 
    lagWomen = 19, 
    lagMen = 24, 
    HIV = TRUE, 
    sexRatios = TRUE, 
    sampleSizes = 0, 
    childlessnessDefinition = 'childDied', 
    path, 
    imputeIndividual = FALSE
  )
  
}
