vars_vec <- names(my_data) %in% my_vars$variable
factor_vars <- c()
for(i in 1:length(vars_vec)){
  if(vars_vec[i] == T && as.logical(my_vars[my_vars$variable == names(my_data)[i],]$factor)){
    factor_vars[i] = T
  }else{
    factor_vars[i] = F
  }
}
my_data[, factor_vars] <- lapply(my_data[, factor_vars], factor)
factor_label <- my_vars[my_vars$variable %in% names(my_data[, factor_vars]), ]$label
factor_var_name <- names(my_data)[factor_vars]
levels(my_data[[factor_var_name[1]]]) <- c("rural", "urban")
levels(my_data[[factor_var_name[2]]]) <- c("female", "male")
levels(my_data[[factor_var_name[3]]]) <- c("no", "yes")
