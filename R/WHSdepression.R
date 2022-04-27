################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in the World Health Survey (WHS) method
# DA1-DA23 

whs_depression <- function(.data) {
  
  whs_depression_names <- c("da1","da5","da6","da7","da8","da9",
                            "da10","da11","da12","da13","da14","da15","da16",
                            "da17","da18","da19","da20","da21","da22","da23")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, whs_depression_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(whs_depression_names[!i], collapse=", ")))
  } else {
    .data %>% 
      # WHS CODE (1)
      mutate(across(c(da1, da5, da6, da7, da8, da9, da10, da11, da12, da13, da14, da15, da16, da17,
                      da18, da19, da20, da21, da22, da23), ~replace(., . >2, NA))) %>%
      # mutate(across(c(da1, da2, da3, da4, da5, da6, da7, da8, da9, da10, da11, da12, da13, da14, 
      #                 da15, da16, da17, da18, da19, da20, da21, da22, da23), ~replace(., . >2, NA))) %>%
      # WHS CODE (2)
      mutate(across(c(da9, da10, da11, da12, da13, da14, da15, da16, da17,
                      da18, da19, da20, da21, da22, da23), ~replace(., da6==2 & da7==2 & da8==2, 2))) %>%
      # WHS CODE (3)
      mutate(bb1 = if_else(da6==1 & da9==1 & da10==1, 1, 0, missing = 0)) %>% 
      mutate(bb2 = if_else(da7==1 | da21==1, 1, 0, missing = 0)) %>% 
      mutate(bb3 = if_else(da8==1, 1, 0, missing = 0)) %>% 
      mutate(cc1 = if_else(da19==1 | da20==1, 1, 0, missing = 0)) %>% 
      mutate(cc2 = if_else(da17==1 | da18==1, 1, 0, missing = 0)) %>% 
      mutate(cc3 = if_else(da22==1 | da23==1, 1, 0, missing = 0)) %>% 
      mutate(cc4 = if_else(da12==1 | da15==1, 1, 0, missing = 0)) %>% 
      mutate(cc5 = if_else(da16==1, 1, 0, missing = 0)) %>% 
      mutate(cc6 = if_else(da13==1 | da14==1, 1, 0, missing = 0)) %>% 
      mutate(cc7 = if_else(da11==1, 1, 0, missing = 0)) %>% 
      mutate(bb = bb1+bb2+bb3) %>% 
      mutate(bbc = bb1+bb2+bb3+cc1+cc2+cc3+cc4+cc5+cc6+cc7) %>% 
      mutate(depression = 0) %>% 
      mutate(depression = replace(depression, is.na(da6) | is.na(da7) | is.na(da8), NA)) %>% 
      mutate(depression = replace(depression, bb>=2 & bbc>=4, 1)) %>% 
      mutate(depression = replace(depression, da1==1 & da5==1, 1)) %>%
      # NOTE: filtered gives results as in STATA but unfiltered gives Epi Info results
      # filter(!is.na(depression)) %>% # NEEDS TO BE VARIFIED
      ## Label values depression yes/no for categorization 
      mutate(c = ifelse(depression==1, "Yes", NA)) %>%
      mutate(c = replace(c, depression==0, "No")) %>%
      mutate(c = factor(c))
  }
  
}

