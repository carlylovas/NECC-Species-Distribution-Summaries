#####
## Center of Gravity from SDMTools: https://rdrr.io/cran/SDMTools/src/R/COGravity.R
wt.mean <- function(x, wt) {
    s = which(is.finite(x * wt))
    wt = wt[s]
    x = x[s] # remove NA info
    return(sum(wt * x) / sum(wt)) # return the mean
}

wt.var <- function(x, wt) {
    s = which(is.finite(x + wt))
    wt = wt[s]
    x = x[s] # remove NA info
    xbar = wt.mean(x, wt) # get the weighted mean
    return(sum(wt * (x - xbar)^2) * (sum(wt) / (sum(wt)^2 - sum(wt^2)))) # return the variance
}


wt.sd <- function(x, wt) {
    return(sqrt(wt.var(x, wt))) # return the standard deviation
}


COGravity <- function(x,y=NULL,z=NULL,wt=NULL) {
    # check if raster from sp or raster package and convert if necessary
    if (any(class(x) %in% "RasterLayer")) x = asc.from.raster(x)
    if (any(class(x) == "SpatialGridDataFrame")) x = asc.from.sp(x)
    #check if x is vector or matrix
    if (is.vector(x)) { #if the data is a vector...do calculations
    if (is.null(wt)) { # if no weighting supplied, calculate means & standard deviations
        out = c(COGx = mean(x, na.rm = TRUE), COGx.sd = sd(x, na.rm = TRUE))
        if (!is.null(y)) out = c(out, COGy = mean(y, na.rm = TRUE), COGy.sd = sd(y, na.rm = TRUE))
        if (!is.null(z)) out = c(out, COGz = mean(z, na.rm = TRUE), COGz.sd = sd(z, na.rm = TRUE))
    } else { # if weighting supplied, calculate weighted means and variances to get COG
        out = c(COGx = wt.mean(x, wt), COGx.sd = wt.sd(x, wt))
        if (!is.null(y)) out = c(out, COGy = wt.mean(y, wt), COGy.sd = wt.sd(y, wt))
        if (!is.null(z)) out = c(out, COGz = wt.mean(z, wt), COGz.sd = wt.sd(z, wt))
    }
    } else if (any(class(x) == 'asc')) { #if x is of class 'asc'
    if (is.null(wt)) { # if wt is null then assume that values in x are the weights
        pos = as.data.frame(which(is.finite(x), arr.ind = TRUE))
        pos$x = getXYcoords(x)$x[pos$row]
        pos$y = getXYcoords(x)$y[pos$col]
        pos$wt = x[cbind(pos$row, pos$col)]
        out = c(COGx = wt.mean(pos$x, pos$wt), COGx.sd = wt.sd(pos$x, pos$wt), COGy = wt.mean(pos$y, pos$wt), COGy.sd = wt.sd(pos$y, pos$wt))
    } else { # if wt is supplied, it must be of the same dim as x and then the values of x are assumed to be your z
        if (!all(dim(x) == dim(wt))) stop("the grids for x & weights must be of the same dimensions")
        pos = as.data.frame(which(is.finite(x), arr.ind = TRUE))
        pos$x = getXYcoords(x)$x[pos$row]
        pos$y = getXYcoords(x)$y[pos$col]
        pos$z = x[cbind(pos$row, pos$col)]
        pos$wt = wt[cbind(pos$row, pos$col)]
        out = c(COGx = wt.mean(pos$x, pos$wt), COGx.sd = wt.sd(pos$x, pos$wt), COGy = wt.mean(pos$y, pos$wt), COGy.sd = wt.sd(pos$y, pos$wt), COGz = wt.mean(pos$z, pos$wt), COGz.sd = wt.sd(pos$z, pos$wt))
    }
    }
    # return the output
    return(out)
}

"getXYcoords" <- function(w) {
    # check if raster from sp or raster package and convert if necessary
    if (any(class(w) %in% "RasterLayer")) w = asc.from.raster(w)
    if (any(class(w) == "SpatialGridDataFrame")) w = asc.from.sp(w)
    if (!inherits(w, "asc")) stop("must be of class asc")

    # Gets the attributes
    cs <- attr(w, "cellsize")
    xll <- attr(w, "xll")
    yll <- attr(w, "yll")

    ## Computation of the number of rows and columns of the matrix
    nr <- nrow(w)
    nc <- ncol(w)

    ## The results
    x <- xll + c(0:(nr - 1)) * cs
    y <- yll + c(0:(nc - 1)) * cs
    return(list(x = x, y = y))
}


make_survdat_occu <- function(survdat_clean, species_keep){
  #### 1. Filter SURVDAT to species of interest based on character or numeric vector `species_keep` and aggregate abundance and biomass data for these species at each tow location   ####
  if (all(is.character(species_keep))) {
    presence_data <- survdat_clean %>%
      dplyr::filter(., comname %in% species_keep)
  } else {
    presence_data <- survdat_clean %>%
      dplyr::filter(., comname %in% species_keep)
  }
  presence_data<- presence_data %>%
    dplyr::group_by(., id, svspp, comname) %>%
    dplyr::summarise(
      sum_abundance = sum(abundance),
      sum_biomass_kg = sum(unique(biomass_kg))
    ) %>%
    dplyr::mutate(presence = ifelse(sum_abundance > 0, 1, 0)) %>% # should all be 1s, presence = 1 if abundance >=1, presence = 0 if abundance = 0
    dplyr::select(id, svspp, comname, presence, sum_abundance, sum_biomass_kg) %>%
    dplyr::ungroup()
  #### 2. Create dataframe with all possible tow/species combinations   ####
  # Create a dataframe of all possible survey ID/species combinations
  all_spp <- survdat_clean %>% distinct(comname, svspp)
  all_id_spec_possibilites <- survdat_clean %>%
    tidyr::expand(id, comname) %>%
    left_join(all_spp, by = "comname") %>%
    dplyr::filter(., comname %in% presence_data$comname)
  #### 3. Join all possible tow/species dataframe with presence data and impute absences   ####
  survdat_occu<- all_id_spec_possibilites %>%
    dplyr::left_join(presence_data, by = c("id", "svspp", "comname")) %>%
    #populate "possibilities" dataset with presence data
    dplyr::mutate(presence = ifelse(is.na(presence) == T, 0, presence)) %>%
    dplyr::mutate(sum_biomass_kg = ifelse(is.na(sum_biomass_kg) == T, 0.000, sum_biomass_kg)) %>%
    dplyr::mutate(sum_abundance = ifelse(is.na(sum_abundance) == T, 0, sum_abundance)) %>%
    dplyr::select(id, svspp, comname, presence, sum_abundance, sum_biomass_kg)
  # Return it
  return(survdat_occu)
}


get_survdat_tows <- function(survdat_clean) {
  #### 1. Filter SURVDAT to unique tows and keep columns of interest   ####
  # Get unique tows
  survdat_tows <- survdat_clean %>%
    dplyr::distinct(id, est_towdate, est_year, est_month, est_day, season, svvessel, decdeg_beglat, decdeg_beglon, survey_area, avgdepth, surftemp, surfsalin, bottemp, botsalin) %>%
    dplyr::filter(!is.na(decdeg_beglat) & !is.na(decdeg_beglon))
  # Return it
  return(survdat_tows)
}
