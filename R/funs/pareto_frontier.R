
#' Determine Pareto Frontier Status
#'
#' @param data data.frame with one row per strategy, one column for a cost variable and one cost for 
#' @param cost_var cost variable name
#' @param benefit_var benefit variable name
#' @param epsilon numeric(2) with tolerance values for benefit and cost variable (in this order)
#' @param remove_extended_dominated defaults to T, and 
#'
#' @return same data.frame with additional colums determining pareto.order and is.dominated
pareto_frontier = function(data, cost_var, benefit_var, epsilon, remove_extended_dominated = T) {
  
  # Order by benefit variable.
  data <- data %>% 
    arrange(!!as.name(benefit_var)) %>% 
    mutate(pareto_order = row_number(),
           pareto_frontier = "Unknown")
  
  # This is executed while ICER is not monotonic (i.e., there are pareto)
  frontier = compute_frontier(data = data, cost_var = cost_var, benefit_var = benefit_var, epsilon = epsilon)
  
  filtered_frontier <- frontier %>%
    mutate(extended_dominated = F)
  
  # Start by assuming there are extended-dominated strategies:
  has_extended_dominated <- T
  
  while(remove_extended_dominated & has_extended_dominated) {
    
    filtered_frontier <- filtered_frontier %>% 
      filter(pareto_frontier == "Efficient") %>%
      mutate(delta.cost = !!sym(cost_var) - lag(!!sym(cost_var), default = 0),
             delta.benefit = !!sym(benefit_var) - lag(!!sym(benefit_var), 1, default = 0),
             icer = delta.cost / delta.benefit) %>%
      mutate(delta_icer = icer - lag(icer, 1, default = 0)) %>%
      mutate(extended_dominated =  lead(delta_icer < 0, default = FALSE))
    
    # Do not consider first strategy as extended-dominated
    filtered_frontier$extended_dominated[1] = F
    
    plot(filtered_frontier[,cost_var], filtered_frontier[,benefit_var])
    
    has_extended_dominated <- any(filtered_frontier$extended_dominated)
    
    if(has_extended_dominated) {
      extended_dominated_strategies <- filtered_frontier$Strategy[filtered_frontier$extended_dominated]
      
      print(paste0("Found ", length(extended_dominated_strategies), " extended dominated strategies: ", extended_dominated_strategies))
      
      frontier <- frontier %>%
        mutate(pareto_frontier = ifelse(Strategy %in% extended_dominated_strategies, "Weakly dominated", pareto_frontier))
      
      filtered_frontier <- filtered_frontier %>%
        filter(!extended_dominated)
    }
    
  }
  
  frontier
  
}


compute_frontier <- function(data, cost_var, benefit_var, epsilon) {
  
  # This is executed for every strategy
  for(i in 1:nrow(data)){
    
    m_data = epsilon_data = data %>% 
      select(all_of(c(cost_var, benefit_var))) %>%
      as.matrix()
    
    # This "adds" a penalty to the policy.
    epsilon_data[i,benefit_var] = epsilon_data[i,benefit_var] - epsilon[1]
    epsilon_data[i,cost_var] = epsilon_data[i,cost_var] + epsilon[2]
    
    # determine dominance
    is.dominated <- any(
      epsilon_data[i,benefit_var] < epsilon_data[,benefit_var]  &
        epsilon_data[i,cost_var] > epsilon_data[,cost_var]
    )
    
    if(!is.dominated) {
      is.weakly.dominated  <- any(
        m_data[i,benefit_var] < m_data[,benefit_var]  &
          m_data[i,cost_var] > m_data[,cost_var]
      )
      
    }
    
    data$pareto_frontier[i] <- ifelse(is.dominated, "Dominated", ifelse(is.weakly.dominated, "Nearly Efficient", "Efficient"))
    
  }
  
  data
  
}

# Calls the pareto_frontier function for each scenario.
pareto_frontier_by_scenario <- function(data, cost_var, benefit_var, epsilon, scenario_var) {
  
  scenarios <- as.numeric(t(unique(data[,scenario_var]))) 
  
  pf <- list()
  
  for(s in scenarios) {
    
    d_scenario <- data[data[,scenario_var]==s,]
    
    pf[[s]] <- pareto_frontier(data = d_scenario, cost_var = cost_var, benefit_var = benefit_var, epsilon = epsilon)
    
  }
  
  do.call(rbind, pf)
  
}


# Example:
# Determines strategies that belong to the pareto frontier
#data = data.frame(strategy = c(1,2,3), lyg = c(30,10,20), cost = c(3,1,10))
#data %>% pareto_frontier(cost_var = "cost", benefit_var = "lyg", epsilon = c(0,0))
