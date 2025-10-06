
# This script contains code for calculating the Mortgage Disinvestment Index (MDI).
# Author: Yuhong Zhou, Kirsten Beyer
# Organization: Medical College of Wisconsin
# Date: 09/30/2024

###################################################  Functions ###################################

library(data.table)
library(foreign)
library(sf)

### define a few functions
factor2num <- function(x) {
  return(as.numeric(as.character(x)))
}

factor2chr <- function(x) {
  return(as.character(x))
}

getParameters <- function(model) {
  #Need to check that a model was able to be formed for this instance
  tryCatch({
    return(c(summary(model)$coefficients[indexCoefName, ], confint.default(model)[indexCoefName, ]))
  }, ## add CI to parameter outputs
  error = function(err) {
    print(err)
    return(c(NA, NA, NA, NA, NA, NA))
  })
}

getParameters2 <- function(model) {
  tryCatch({
    return(c(summary(model)$coefficients[indexCoefName2, ]))
  }, #,exp(cbind(OR=coef(d),confint.default(d)))[var1_cat3,]))}
  error = function(err) {
    print(err)
    return(c(NA, NA, NA, NA))
  })
}

getParameters3 <- function(model) {
  tryCatch({
    return(c(summary(model)$coefficients[indexCoefName3, ]))
  }, #,exp(cbind(OR=coef(d),confint.default(d)))[var1_cat3,]))}
  error = function(err) {
    print(err)
    return(c(NA, NA, NA, NA))
  })
}

para_to_dtable <- function(list_param, new_var) {
  list_param_T <- t(list_param)
  dt_param <- as.data.table(list_param_T)
  eval(parse(
    text = paste("dt_param$OR_", new_var, "<- exp(dt_param$Estimate)", sep =
                   "")
  ))
  if (new_var %in% c("Black", "Hisp", "Inside")) {
    new_var <- ""
    setnames(dt_param, "2.5 %", paste("l95_exp", new_var, sep = ""))
    setnames(dt_param, "97.5 %", paste("u95_exp", new_var, sep = ""))
  } else {
    new_var <- paste("_", new_var, sep = "")
  }
  setnames(dt_param, "Estimate", paste("Est", new_var, sep = ""))
  setnames(dt_param, "Std. Error", paste("SE", new_var, sep = ""))
  setnames(dt_param, "z value", paste("zval", new_var, sep = ""))
  setnames(dt_param, "Pr(>|z|)", paste("z_pval", new_var, sep = ""))
  dt_param$grid_id <- rownames(list_param_T)
  setcolorder(dt_param, c(length(dt_param), 1:(length(dt_param) - 1)))   ## move the last column grid_id to the first column
  if ((sapply(dt_param, class)["grid_id"]) == "integer") {
    dt_param[, grid_id := as.character(grid_id)]
  }
  return(dt_param)
}

#################################################################
########## Get the path and the name of an input file ###########
############# Input (filepath): full path of a data file ########
############# Output: path and name of the input file ###########
#################################################################

filedir <- function(filepath) {
  ind1 = regexpr("\\/[^\\/]*$", filepath)[1]
  path = substr(filepath, 1, ind1 - 1)
  fn = substr(filepath, ind1 + 1, nchar(filepath) - 4)
  return(c(path, fn))
}

################################################################################################################
########################      Join Distance Matrix Table With Data Variables                          ##########
############# Input (distmatrix) : data table object or full path for the distance matrix table (path+name+ext-string)
############# Input (id_name)    : field name for matching data Variables, string                     ##########
############# Input (datafiles)   : full path for the data with variables (path+name+ext-string)       ##########
#############                      variables are used for crude rate calculation                      ##########
############# Input (uid_name)   : field name for the unique identifier of points shapefile, string   ##########
############# Input (flagexport) : logical value (TRUE or FALSE)                                      ##########
#############       optional     if true, export the joined distance matrix as a csv file             ##########
############# Input (outdir)     : path for storing the distance matrix, string (No "/" at the end)   ##########
#############       optional                                                                          ##########
############# Input (tablename)  : csv file name for the exported distance matrix, string, no extension ########
#############       optional     Note: if the table exists, it will be overwritten!                   ##########
############# Output             : distance matrix table with X,Y coordinates and data Variables      ##########
################################################################################################################

dmjoindata = function (distmatrix,
                       id_name,
                       datafiles,
                       uid_name,
                       flagexport = FALSE,
                       outdir = "",
                       tablename = "") {
  ## check data type of parameters
  if (!is(distmatrix, "data.table") &
      !is(distmatrix, "character")) {
    stop(
      "The parameter for the distance matrix input is not correctly specified. Please refer to a data table object in memory or a valid text file!"
    )
  }
  if (!is(id_name, "character")) {
    stop(
      "The parameter for the identifier of distance matrix is not a valid string. Please provide a string representing a valid field!"
    )
  }
  if (!is(datafiles, "character")) {
    stop(
      "The parameter for the data input is not correctly specified. Please proide a string indicating the path to a valid shapefile or text file!"
    )
  }
  if (!is(uid_name, "character")) {
    stop(
      "The parameter for the identifier of data table is not a valid string. Please provide a string representing a valid field!"
    )
  }
  if (!is(flagexport, "logical")) {
    stop(
      "The parameter for writing distance matrix to a table is not a valid logical value. Please provide a TRUE or FALSE value!"
    )
  }
  if (!is(outdir, "character")) {
    stop(
      "The parameter for the output path is not correctly specified. Please provide a string indicating a valid path!"
    )
  }
  if (!is(tablename, "character")) {
    stop(
      "The parameter for the output table name is not correctly specified. Please provide a string indicating a valid table name!"
    )
  }
  
  ## check validity of parameters
  ## check whether the input distance matrix is the data table object or refers to a text file on disk;
  if (is(distmatrix, "data.table")) {
    dfdist_table <- distmatrix
  } else if (is(distmatrix, "character")) {
    if (!file.exists(distmatrix)) {
      stop(
        "The file for distance matrix does not exist. Please double check the input directory (the first parameter) for the data matrix!"
      )
    } else {
      ## if the extension of the input data file is a text file, read the table and check the validity of the input field names
      if ((file_ext(distmatrix) == "csv") |
          (file_ext(distmatrix) == "txt")) {
        dfdist_table <- data.table(
          read.table(
            distmatrix,
            header = TRUE,
            sep = ',',
            stringsAsFactors = FALSE,
            comment.char = ""
          )
        )
      }
    }
  }
  
  if (!id_name %in% names(dfdist_table)) {
    stop(
      "The field '",
      id_name,
      "' does not exist in the data matrix table. Please provide only valid column names."
    )
  } else {
    ## set the key for the data table
    setkeyv(dfdist_table, id_name)
  }
  
  if (!file.exists(datafiles)) {
    stop(
      "The data file does not exist. Please double check the input directory (the third parameter) for the data!"
    )
  } else {
    ## if the extension of the input data file is a text file, read the table and check the validity of the input field names
    if ((file_ext(datafiles) == "csv") |
        (file_ext(datafiles) == "txt")) {
      datatable <- data.table(
        read.table(
          datafiles,
          header = TRUE,
          sep = ',',
          stringsAsFactors = FALSE,
          comment.char = ""
        )
      )
    } else if (file_ext(datafiles) == "shp") {
      ## read the shapefile into a SpatialPointsDataFrame object
      pointshp <- read_sf(dsn = filedir(datafiles)[1], filedir(datafiles)[2])
      ## convert point data frame to data table for efficient merge
      datatable <- st_drop_geometry(pointshp)
      datatable <- data.table(datatable)
      #texttable = FALSE
    }
  }
  
  ## check whether the column for unique ids of the data is valid and truly unique
  if (!uid_name %in% names(datatable)) {
    stop(
      "The field '",
      uid_name,
      "' does not exist in the data file. Please provide only valid column names."
    )
  } else {
    id_val1 <-  eval(parse(text = paste("datatable$", uid_name)))
    if (any(duplicated(id_val1))) {
      stop(
        "The column '",
        pntid_name,
        "' have duplicate values in the spatial point data. Please provide only an valid unique column name."
      )
    } else {
      ## set the key for the data table
      setkeyv(datatable, uid_name)
    }
  }
  
  ## check parameters
  if (flagexport) {
    if (!file.exists(outdir)) {
      stop(
        "The directory for the output file does not exist. Please double check the output directory!"
      )
    } else {
      #if (file.exists(paste(outdir,"/", tablename, ".csv", sep=""))) {warning("The output file exists. Please specify a different file name!")}
      if (file.exists(paste(outdir, "/", tablename, ".csv", sep = ""))) {
        warning("The output file exists. Be aware it will be overwritten!")
      }
    }
  }
  
  ###### core code
  ### change the name and type of the unique identifier of census table to the same name as distance matrix table
  setnames(datatable, uid_name, "GeoID")
  if ((sapply(datatable, class)["GeoID"]) == "integer") {
    datatable[, GeoID := as.character(GeoID)]
  }   ## the varialbe name is pre-defined, not a variable
  
  setnames(dfdist_table, id_name, "GeoID")
  if ((sapply(dfdist_table, class)["GeoID"]) == "integer") {
    dfdist_table[, GeoID := as.character(GeoID)]
  }
  ### merge the data matrix table with the data file
  dfdist_merge_table <- merge(dfdist_table, datatable, by = "GeoID", all.x =
                                TRUE)
  
  ## export the distance matrix tabel as an csv file, if the user wants to do so
  if (flagexport) {
    write.csv(
      dfdist_merge_table,
      file = paste(outdir, "/", tablename, ".csv", sep = ""),
      row.names = FALSE
    )
  }
  
  return(dfdist_merge_table) ## ## return the data table for distance matrix
}

############################################################################################################




#############################################   Parameter Setting   ########################################

#### time period for estimation
y1 = 2008
y2 = 2012
print(paste0(y1, "-", y2))

#### directory for input data
rootdir = paste0("./", "demo")

### type of metrics - redlining here
REDLINING_WITHOUT_RACE <- 4
indexQuery <- REDLINING_WITHOUT_RACE

### census year for census boundary
censusyear <- "2010"
### input HMDA dataset
hmdayear = "0413"
msaName = "PIT"

outMatrix <- data.frame(
  MSA = character(),
  NumBGs = integer(),
  Num_largeSE5 = integer(),
  Perc_racesig = double(),
  Perc_lirsig = double(),
  Perc_sexsig = double(),
  Filter_Max = double(),
  Filter_Mean  = double(),
  Filter_Median = double(),
  stringsAsFactors = FALSE
)   ## the last parameter is used for adding character columns without the warning (invalid factor level, NA generated)

##############################  Read input data  ##############################
print(msaName)

if (censusyear == "2010") {
  outputFileGrid <- paste("Gridpoly2010_", msaName, "nb1", sep = "")
  outputFileGrid.centroid <- paste("Grid2010_", msaName, "nb1", sep = "")
  outputFileCases <- paste("Casespoly2010_", msaName, "nb1", sep = "")
  outputFileCases.centroid <- paste("Cases0010_", msaName, "nb1", sep =
                                      "")
}

outputDSN <- paste0(rootdir, "/", msaName)
combinedShapeGrid <- read_sf(outputDSN, outputFileGrid)
combinedShapeGrid.centroid <-  read_sf(outputDSN, outputFileGrid.centroid)
combinedShapeCases <- read_sf(outputDSN, outputFileCases)
combinedShapeCases.centroid <- read_sf(outputDSN, outputFileCases.centroid)

combinedShapeGrid.centroid$FIPS = as.character(combinedShapeGrid.centroid$FIPS)
combinedShapeCases.centroid$GEOID = as.character(combinedShapeCases.centroid$GEOID)

print("Reading input shapes is completed!")
##########################################################################################################

########################### Make Distance Matrix to be Joined to the Shapefile for Cases #################
print("Making Distance Matrix")

## check the projection compatibility of the two SpatialPointDataFrame objects
## get the projection of the object
sp_grd_point <- combinedShapeGrid.centroid
pointshp <- combinedShapeCases.centroid
pntid_name = "GEOID"
grid_ids = "FIPS"

proj_grid <- st_crs(sp_grd_point)
print(proj_grid)
proj_point <- st_crs(pointshp)
print(proj_point)

## here we assume the projection of the point shapefile is the target/benchmark one;
## the grid data is transformed to the benchmark one;
## however, we did not check and handle non-projected point shapefile)
if (proj_grid != proj_point) {
  warning("The projection of the grid is transformed to the same one as the point shapefile.")
  sp_grd_point.1 <- spTransform(sp_grd_point, CRS(proj_point))
  sp_grd_point <- sp_grd_point.1
  rm(sp_grd_point.1)
}

## create new columns for getting/updating XY coordinates of grids
sp_grd_point$Xnew <- st_coordinates(sp_grd_point)[, 1]
sp_grd_point$Ynew <- st_coordinates(sp_grd_point)[, 2]

##get the distance matrix of two spatial features
mdist <- st_distance(pointshp, sp_grd_point)

## get the ID column of the point data and assign it to the row names of the distance matrix
rownames(mdist) <- eval(parse(text = paste("pointshp$", pntid_name)))
colnames(mdist) <- eval(parse(text = paste("sp_grd_point$", grid_ids)))

### construct data frame/table from the matrix; extract the information about grid id, tractid and distance
dfdist_table <- data.table(
  col = rep(colnames(mdist), each = nrow(mdist)),
  row = rep(rownames(mdist), ncol(mdist)),
  value = as.vector(mdist)
)
setnames(dfdist_table, "col", "grid_id")
setnames(dfdist_table, "row", "GeoID")
setnames(dfdist_table, "value", "near_dist")
setkeyv(dfdist_table, c("grid_id", "GeoID"))

sp_grd_point_table <- st_drop_geometry(sp_grd_point)
setnames(sp_grd_point_table, grid_ids, "grid_id")
if ((sapply(sp_grd_point_table, class)["grid_id"]) == "integer") {
  sp_grd_point_table[, grid_id := as.character(grid_id)]
}
dfdist_merge_table <- merge(dfdist_table,
                            sp_grd_point_table,
                            by = "grid_id",
                            all.x = TRUE)
msaDistMatrix <- dfdist_merge_table
print(paste("Completed", Sys.time()))

print("Joining Distance Matrix")
msaDistmatrix_join <- dmjoindata(
  msaDistMatrix,
  "GeoID",
  paste(outputDSN, "/", outputFileCases.centroid, ".shp", sep = ""),
  "GEOID"
)
print(paste("Completed", Sys.time()))
###########################################################################################################

##############################################   Define Filters  ##########################################
gridid_name <- "grid_id"  ## grid id
dist_name <-  "near_dist" ## distance

if (indexQuery == REDLINING_WITHOUT_RACE) {
  keyvar1 <- "denyTotal"  ## threshold variable 2
  keyvar2 <- "apprTotal"  ## threshold variable 4
}

X_name <- "Xnew"      ## variable for X coordniate
Y_name <- "Ynew"      ## variable for Y coordniate

#Creating a summary table for the HMDA data
print("Creating Summary Table for HMDA")
hmdaFile <- paste("hmda", hmdayear, "_", msaName, "_convent_red.csv", sep =
                    "")
hmda_table <- data.table(
  read.table(
    paste(outputDSN, "/", hmdaFile, sep = ""),
    header = TRUE,
    sep = ',',
    quote = "\"",
    stringsAsFactors = TRUE,
    comment.char = ""
  )
)
hmda_table <- subset(hmda_table, hmda_table$year >= y1)
hmda_table <- subset(hmda_table, hmda_table$year <= y2) #for cohort y1-y2

## rename the levels of a factor variable - give simplified names for the levels, have to be in the same order as the current levels (alphabetical order)
### "" is for missing records;
levels(hmda_table$race) = gsub("American Indian or Alaska Native",
                               "Indian",
                               levels(hmda_table$race))
levels(hmda_table$race) = gsub("Black or African American", "Black", levels(hmda_table$race))
levels(hmda_table$race) = gsub(
  "Native Hawaiian or Other Pacific Islander",
  "Pacific Islander",
  levels(hmda_table$race)
)

levels(hmda_table$ethnicity) <- c("", "Hispanic", "Non Hispanic")
hmda_table$combRace <- factor(hmda_table$race, levels = c(levels(hmda_table$race), "Hispanic"))

#### temporarily set, and this variable may not be included in the model
if (indexQuery == REDLINING_WITHOUT_RACE) {
  hmda_table$combRace[which(hmda_table$ethnicity1 == "Hispanic")] <- "Hispanic"
}
## relevel - set the White group as reference group; the group without observation ("") is eliminated 'automatically'?
hmda_table$combRace <- factor(
  hmda_table$combRace,
  levels = c(
    "White",
    "Black",
    "Hispanic",
    "Asian",
    "Indian",
    "Pacific Islander"
  )
)

if (indexQuery == REDLINING_WITHOUT_RACE) {
  hmda_table.apps <- table(hmda_table$GeoID, hmda_table$combRace, useNA = "always")
  hmda_table.apps <- cbind(rownames(hmda_table.apps), hmda_table.apps)
  colnames(hmda_table.apps) <- c(
    "GeoID",
    "numWhite",
    "numBlack",
    "numHisp",
    "numAsian",
    "numIndian",
    "numPI",
    "numNA"
  )
  
  hmda_table.denies <- table(hmda_table$GeoID[hmda_table$denied == 'Denied'], hmda_table$combRace[hmda_table$denied == 'Denied'], useNA = "always")
  hmda_table.denies <- cbind(rownames(hmda_table.denies), hmda_table.denies)
  colnames(hmda_table.denies) <- c(
    "GeoID",
    "denyWhite",
    "denyBlack",
    "denyHisp",
    "denyAsian",
    "denyIndian",
    "denyPI",
    "denyNA"
  )    #colnames(hmda_table.denies) <- c("GeoID","denyWhite", "denyBlack", "denyHisp", "denyAsian", "denyIndian", "denyPI")
  hmda_table.sum <- merge(hmda_table.apps,
                          hmda_table.denies,
                          by = "GeoID",
                          all.x = TRUE)
  
  hmda_table.approves <- table(hmda_table$GeoID[hmda_table$denied == 'Approved'], hmda_table$combRace[hmda_table$denied == 'Approved'], useNA = "always")
  hmda_table.approves <- cbind(rownames(hmda_table.approves), hmda_table.approves)
  colnames(hmda_table.approves) <- c(
    "GeoID",
    "apprWhite",
    "apprBlack",
    "apprHisp",
    "apprAsian",
    "apprIndian",
    "apprPI",
    "apprNA"
  )    #colnames(hmda_table.approves) <- c("GeoID","apprWhite", "apprBlack", "apprHisp", "apprAsian", "apprIndian", "apprPI")
  hmda_table.sum <- merge(hmda_table.sum,
                          hmda_table.approves,
                          by = "GeoID",
                          all.x = TRUE)
  
  hmda_table.sum$GeoID <- as.character(hmda_table.sum$GeoID)
  # change all factor columns to numeric
  hmda_table.sum[, -1] <- apply(hmda_table.sum[, -1], 2, as.numeric)
  
  hmda_table.sum$numTotal <- apply(hmda_table.sum[, c(2:7)], 1, sum)
  hmda_table.sum$denyTotal <- apply(hmda_table.sum[, c(8:13)], 1, sum)
  hmda_table.sum$apprTotal <- apply(hmda_table.sum[, c(14:19)], 1, sum)
}

print(paste("Completed", Sys.time()))

dist_threshold1 <- 4  ## first threshold value
dist_threshold2 <- 4  ## second threshold value

dfdist_table <- merge(msaDistmatrix_join,
                      hmda_table.sum,
                      by = "GeoID",
                      all.x = TRUE)
dfdist_table[, eval(parse(
  text = paste(gridid_name, ":=as.character(", gridid_name, ")", sep = "")
))]
print(paste("Completed", Sys.time()))

# Sort the data table by the grid id column, then distance
print("Creating Grid and Setting Filters")
grid_point_case <- dfdist_table[eval(parse(text = paste(
  "order(", gridid_name, ",", dist_name, ")" , sep = ""
))), ]
print(paste("Distance Matrix Completed", Sys.time()))

## make sure na value is set to 0
if (indexQuery == REDLINING_WITHOUT_RACE) {
  for (col in colnames(hmda_table.sum[, -1])) {
    grid_point_case[is.na(get(col)), (col) := 0]
  }
}

# calculate the cumulative value of the base attribute keyvar (e.g. population or cancer cases) for each group
grid_point_case[, eval(parse(text = paste("cum_cases1:=cumsum(", keyvar1, ")", sep =
                                            ""))), by = eval(parse(text = paste("list(", gridid_name, ")", sep = "")))]
grid_point_case[, eval(parse(text = paste("cum_cases2:=cumsum(", keyvar2, ")", sep =
                                            ""))), by = eval(parse(text = paste("list(", gridid_name, ")", sep = "")))]

## Flag the first row for each grid id group
grid_point_case[, eval(parse(
  text = paste(
    "first := !duplicated(grid_point_case[,",
    gridid_name,
    "])",
    sep = ""
  )
))]

## create the filters
grid_point_case[, cum_cases_p1 := c(0, head(cum_cases1, -1))]
grid_point_case[, cum_cases_p2 := c(0, head(cum_cases2, -1))]
grid_point_case$threshold <- 0
grid_point_case[(first == TRUE), threshold := 1]
grid_point_case[((threshold == 0) &
                   (cum_cases_p1 < dist_threshold1) & !(first)), threshold := 2]
grid_point_case[((threshold == 0) &
                   (cum_cases_p2 < dist_threshold2) &
                   (cum_cases_p1 >= dist_threshold1) & !(first)
), threshold := 3]

## set the flag for inside/outside filter;
grid_point_case[(threshold > 0), member := 1]
grid_point_case[(threshold <= 0), member := 0]

grid_point_case1 <- grid_point_case[, list(GeoID, grid_id, member)]

## change the attribute types
if ((sapply(hmda_table, class)["GeoID"]) != "character") {
  hmda_table[, GeoID := as.character(GeoID)]
}
if ((sapply(grid_point_case1, class)["GeoID"]) != "character") {
  grid_point_case1[, GeoID := as.character(GeoID)]
}
if ((sapply(grid_point_case1, class)["grid_id"]) != "character") {
  grid_point_case1[, grid_id := as.character(grid_id)]
}

### set up for paramter extraction
options(warn = 1) #Stop making warnings into errors
grid_point_case1_plus <- merge(grid_point_case1,
                               hmda_table,
                               by = "GeoID",
                               allow.cartesian = TRUE)

grid_point_case1_plus$member <- factor(
  grid_point_case1_plus$member,
  levels = c(0, 1),
  labels = c("outside", "inside")
)
print(paste("Completed", Sys.time()))
############################################################################################################


############################  Get Estimates for Logistic Regression Model for Each Filter ##################
print("Making Sample Instance's Model")
if (indexQuery == REDLINING_WITHOUT_RACE) {
  sampleTime <- system.time(modelvector <- lapply(split(grid_point_case1_plus, grid_point_case1_plus$grid_id), function(d) {
    logitmodel <- try(glm(
      factor(denied) ~ member + loan_income_ratio + factor(app_sex),
      data = d,
      family = "binomial"
    ))
    if (isTRUE(class(logitmodel) == "try-error")) {
      return(NULL)
    } else {
      return(logitmodel)
    } #Check for model convergence
  }))
}
print(paste("Completed", Sys.time()))
options(warn = 1) #Stop making warnings into errors

## a list of variable names used for models; modify accordingly to fit your input table (variable names and types)
Yvar1 <- "denied"  ## Y
Xvar0 <- "member"  ## Xs
Xvar1 <- "combRace"
Xvar2 <- "loan_income_ratio"
Xvar3 <- "loan_amt"
Xvar5 <- "app_sex"
Xvar7 <- "ethnicity1"

print("Extracting Sample's Estimates")
#Temporary storage of estimate along with Std. Err, z Val, p Val, OR, confidence interval
if (indexQuery == REDLINING_WITHOUT_RACE) {
  indexCoefName <- "memberinside"
}

### extract the attributes from grid data and merge with the model results
if (indexQuery == REDLINING_WITHOUT_RACE) {
  newvar1 <- "Inside"
}

paramlist <- sapply(modelvector, getParameters)  ## no walds test
paramdt <- para_to_dtable(paramlist, newvar1)

## add coefficient estaimtes for sex variable
indexCoefName2 <- "factor(app_sex)Male"
paramlist2 <- sapply(modelvector, getParameters2)
paramdt2 <- para_to_dtable(paramlist2, "sex")

indexCoefName3 <- "loan_income_ratio"
paramlist3 <- sapply(modelvector, getParameters3)  ## no walds test
paramdt3 <- para_to_dtable(paramlist3, "lir")

aggdata <- grid_point_case[(threshold > 0), eval(parse(
  text = paste(
    "list(nearDmax = max(near_dist), X = mean(",
    X_name,
    "), Y = mean(",
    Y_name,
    "), numDenied = sum(",
    "denyTotal",
    "),  numTotal = sum(",
    "numTotal",
    "), numBlack = sum(",
    "numBlack",
    "), numWhite = sum(",
    "numWhite",
    "), denyBlack = sum(",
    "denyBlack",
    "), denyWhite = sum(",
    "denyWhite",
    "), apprBlack = sum(",
    "apprBlack",
    "), apprWhite = sum(",
    "apprWhite",
    "), numObs = sum(threshold))",
    sep = ""
  )
)), by = grid_id]
aggdata$allobs <- dim(hmda_table)[1]
aggdata$allwhite <- as.vector(table(hmda_table$combRace))[1]

aggdata_plus <- merge(paramdt, aggdata, by = "grid_id", all.x = TRUE)
aggdata_plus <- merge(aggdata_plus, paramdt2, by = "grid_id", all.x = TRUE)
aggdata_plus <- merge(aggdata_plus, paramdt3, by = "grid_id", all.x = TRUE)
aggdata_plus$MSA_name <- msaName

sp_grid_point_chr = st_as_sf(aggdata_plus, coords = c("X", "Y"), remove = FALSE)
st_crs(sp_grid_point_chr) = st_crs(combinedShapeGrid.centroid)
################################################################################################################

###############################################  Output Results  ##############################################
if (indexQuery == REDLINING_WITHOUT_RACE) {
  indexname <- paste(dist_threshold1, "da", sep = '')
}

outputDSNsub <- paste0(outputDSN, "/", y1, "_", y2)
if (dir.exists(outputDSNsub) == FALSE)
  dir.create(outputDSNsub)
if (censusyear == "2010") {
  shpname <- paste("Grid2010_", msaName, "nb1_", indexname, "_red_convent", sep =
                     "")
}
st_write(sp_grid_point_chr,
         paste0(outputDSNsub, "/", shpname, ".shp"),
         delete_dsn = TRUE)
write.dta(
  aggdata_plus,
  file = paste(
    outputDSNsub,
    "/redlining_",
    msaName,
    "_T",
    dist_threshold1,
    "_convent.dta",
    sep = ""
  )
)

#################################################  THE END  ################################################
