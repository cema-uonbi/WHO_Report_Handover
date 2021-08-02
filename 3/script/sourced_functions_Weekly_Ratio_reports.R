# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#        WHO COVID19 REPORT       #
#         SOURCED SCRIPT          #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# FUNCTIONS USED IN Dt CALCULATIONS & CI  


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate the current weekly ratio with confidence intervals
weekly_ratios_ci <- function(A, B, altered){
  if(altered > 0){
    tab <- tibble(type = c("ratio_m","lci", "uci"),
                  est = c(-1, NA, NA))
  }
  else {
    e <- sqrt(1/A + 1/B)
    
    est <- c(exp(log(A/B) - e * 1.96),
             exp(log(A/B) + e * 1.96))
    
    tab <- tibble(type = c("ratio_m","lci", "uci"),
                  est = c(A/B, est[[1]], est[[2]]))
  }
  return(tab)
  
}

# loop weekly ratio test over all data 
weekly_ratios_2 <- function(df, outcome, smooth_by = 7){
  
  df <- df %>% 
    subset(date >= lubridate::ymd("2020-02-20")) %>%
    filter(!is.na(.data[[outcome]])) %>% 
    group_by(country) %>%
    mutate(altered = .data[[outcome]] < 0, 
           altered_window = roll_sumr(altered, n = smooth_by * 2),
           change = roll_sumr(.data[[outcome]], n = smooth_by),
           change_prev = lag(change, n = smooth_by),
           ratio = change/change_prev) %>%
    ungroup()
  
  df_long <- df %>%
    filter(!is.na(ratio) & !is.infinite(ratio)) %>%
    group_by(country,date) %>%
    nest() %>%
    mutate(tab = map(data, ~ weekly_ratios_ci(.x$change, .x$change_prev, .x$altered_window))) %>%
    mutate(ratio = map_dbl(tab, ~parse_number(as.character((.x[1,2]))))) %>%
    mutate(lci = map_dbl(tab, ~parse_number(as.character((.x[2,2]))))) %>% 
    mutate(uci = map_dbl(tab, ~parse_number(as.character((.x[3,2]))))) %>% 
    ungroup() %>%
    select(date, country, ratio, lci, uci) 
  
  df_long
  
}

# function to check that no cases/deaths have been redacted over the last 7 day window
# as this would make any weekly ratio calculations impossible
eight_day_pos_window <- function(positive){
  postive_window <- vector(length = length(positive))
  i <- 1
  j <- 1
  
  while(j < length(positive) + 1){
    postive_window[j] <- (sum(positive[i:j]) == length(positive[i:j]))
    j = j + 1
    if(j > 8) i = i + 1
  }
  
  return(postive_window)
}

# function to calculate the poisson error in a count/weekly ratio if value can be calulated
rpois_error <- function(iterations, observation, positive){
  if(positive) return(rpois(iterations,observation))
  else return(NA)
}

# custom function to interchange lists (for storage) to matrices (for calculations)
vector_of_lists_to_matrix <- function(vector_of_lists){
  sim_matrix <- matrix(nrow=length(vector_of_lists),ncol = length(vector_of_lists[[1]]))
  for(i in 1:length(vector_of_lists)){
    sim_matrix[i,] = vector_of_lists[[i]]
  }
  sim_matrix
}
# complement function to above
matrix_to_vector_of_lists <- function(matrix){
  vector_of_lists <- vector(mode="list",length = nrow(matrix))
  for(i in 1: nrow(matrix)){
    vector_of_lists[[i]] <- matrix[i,]
  }
  vector_of_lists
}

# function to sum possible cumulative totals given bootstrapped errors
sim_cum_calc_per_pop <- function(sim_obs_vector_of_lists,population,positive){
  if(positive){
    sim_obs_matrix <- vector_of_lists_to_matrix(sim_obs_vector_of_lists)
    sim_cum_obs_matrix <- apply(sim_obs_matrix,2,cumsum)
    sim_cum_obs_matrix_per_10k = sim_cum_obs_matrix / population[1]
    return(matrix_to_vector_of_lists(sim_cum_obs_matrix_per_10k))
  }
  else return(NA)
}

# function to calculate weekly ratios for a given window
compute.td<- function(seven_day_separation){
  Td<- round(7/(log2(seven_day_separation[2]/seven_day_separation[1])), 1)
  ifelse(seven_day_separation[2]==0|seven_day_separation[1]==0, NA, Td)
}

# repeat dt calculation for entire list of countries
Td.lapply<- function(cum_num_obs,date, t1, t2, positive,iteration){
  has_changed_observations <- tibble(date=date,positive=positive) %>%
    filter(date >=t1, date <=t2) %>%
    pull(positive)
  
  if(sum(has_changed_observations) == length(has_changed_observations)){
    cum_num_obs_matrix <- vector_of_lists_to_matrix(cum_num_obs[date == t2 | date == t1])
    obs_doubling_time <- apply(cum_num_obs_matrix,2,compute.td)
    return(list(obs_doubling_time))
  }
  else return(list(rep(NA,iteration)))
}

# function to construct summary sentences for the text below the country specific graphs
country_summary_text_function <- function(i, WHO_latest_day_cases_and_deaths_simulated, WHO_cases_and_deaths_weekly_ratio) {
  currrent_country_latest_cases <- WHO_latest_day_cases_and_deaths_simulated %>%
    filter(country == i) %>%
    pull(last_day_case_obs)
  
  currrent_country_latest_cases_rank <- WHO_latest_day_cases_and_deaths_simulated %>%
    arrange(desc(last_day_case_obs)) %>%
    mutate(rank = 1:length(WHO_latest_day_cases_and_deaths_simulated$country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  currrent_country_latest_deaths <- WHO_latest_day_cases_and_deaths_simulated %>%
    filter(country == i) %>%
    pull(last_day_deaths_obs)
  
  currrent_country_latest_deaths_rank <- WHO_latest_day_cases_and_deaths_simulated %>%
    arrange(desc(last_day_deaths_obs)) %>%
    mutate(rank = 1:length(WHO_latest_day_cases_and_deaths_simulated$country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  currrent_country_latest_cases_per_pop <- WHO_latest_day_cases_and_deaths_simulated %>%
    filter(country == i) %>%
    pull(last_day_case_obs_per_10k)
  
  currrent_country_latest_cases_per_pop_rank <- WHO_latest_day_cases_and_deaths_simulated %>%
    arrange(desc(last_day_case_obs_per_10k)) %>%
    mutate(rank = 1:length(WHO_latest_day_cases_and_deaths_simulated$country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  currrent_country_latest_deaths_per_pop <- WHO_latest_day_cases_and_deaths_simulated %>%
    filter(country == i) %>%
    pull(last_day_deaths_obs_per_10k)
  
  currrent_country_latest_deaths_per_pop_rank <- WHO_latest_day_cases_and_deaths_simulated %>%
    arrange(desc(last_day_deaths_obs_per_10k)) %>%
    mutate(rank = 1:length(WHO_latest_day_cases_and_deaths_simulated$country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  current_country_cases_WR <- WHO_cases_and_deaths_weekly_ratio  %>%
    filter(country == i) %>%
    pull(ratio_c)
  
  current_country_cases_WR_rank <- WHO_cases_and_deaths_weekly_ratio %>%
    filter(ratio_c > -1) %>%
    ungroup() %>%
    arrange(desc(ratio_c)) %>%
    mutate(rank = 1:length(country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  current_country_deaths_WR <- WHO_cases_and_deaths_weekly_ratio %>%
    filter(country == i) %>%
    pull(ratio_d)
  
  current_country_deaths_WR_rank <- WHO_cases_and_deaths_weekly_ratio %>%
    filter(ratio_d > -1) %>%
    ungroup() %>%
    arrange(desc(ratio_d)) %>%
    mutate(rank = 1:length(country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  if(current_country_cases_WR %in% c(Inf,-1,NA) | current_country_deaths_WR %in% c(Inf,-1,NA)){
    
    if(current_country_cases_WR %in% c(Inf,-1,NA) & current_country_deaths_WR %in% c(Inf,-1,NA)){
      if(current_country_cases_WR %in% c(Inf,NA)) sentence_3 = " The weekly ratio of reported cases cannot be calculated as no new cases have been reported in 1 of the last 2 weeks."
      else sentence_3 = " The weekly ratio of reported cases cannot be calculated as some cases have been redacted in last 14 days."
      if(current_country_deaths_WR %in% c(Inf,NA)) sentence_4 = " The weekly ratio of reported deaths cannot be calculated as no new deaths have been reported in 1 of the last 2 weeks."
      else sentence_4 = " The weekly ratio of reported deaths cannot be calculated as some deaths have been redacted in last 14 days."
    }
    else if(current_country_cases_WR %in% c(Inf,-1,NA)){
      if(current_country_cases_WR %in% c(Inf,NA)) sentence_3 = " The weekly ratio of reported cases cannot be calculated as no new cases have been reported in 1 of the last 2 weeks."
      else sentence_3 = " The weekly ratio of reported cases cannot be calculated as some cases have been redacted in one of the last 2 weeks."
      sentence_4 = paste0(" The weekly ratio of reported deaths over the last 2 weeks is ", signif(current_country_deaths_WR,2), " (", toOrdinal(current_country_deaths_WR_rank),").")
    }
    else{
      if(current_country_deaths_WR %in% c(Inf,NA)) sentence_4 = " The weekly ratio of reported deaths cannot be calculated as no new deaths have been reported in one of the last 2 weeks."
      else sentence_4 = " The weekly ratio of reported deaths cannot be calculated as some deaths have been redacted in one of the last 2 weeks."
      sentence_3 = paste0(" The weekly ratio of reported cases over the last 2 weeks  is ", signif(current_country_cases_WR,2), " (", toOrdinal(current_country_cases_WR_rank), ").")
    }
    
  }
  else {
    sentence_3 = paste0(" The weekly ratio of reported cases over the last 2 weeks is ", signif(current_country_cases_WR,2), " (", toOrdinal(current_country_cases_WR_rank), ").", " The weekly ratio of reported deaths over the last 2 weeks is " , signif(current_country_deaths_WR,2), " (", toOrdinal(current_country_deaths_WR_rank),").")
    sentence_4 = NULL
  }
  tibble(
    sentence_1 = paste0(i, " has ", currrent_country_latest_cases, " reported case(s) (", toOrdinal(currrent_country_latest_cases_rank), " in the region) and ", currrent_country_latest_deaths, " reported death(s) (", toOrdinal(currrent_country_latest_deaths_rank), ")."),
    sentence_2 = paste0(" Cumulative counts per 10,000 population of reported cases and deaths are ", signif(currrent_country_latest_cases_per_pop,2), " (", toOrdinal(currrent_country_latest_cases_per_pop_rank), ") and ", signif(currrent_country_latest_deaths_per_pop,2), " (", toOrdinal(currrent_country_latest_deaths_per_pop_rank), ") respectively."),
    sentence_3 = sentence_3,
    sentence_4 = sentence_4
    )
}






