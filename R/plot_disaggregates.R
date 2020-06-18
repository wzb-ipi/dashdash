#' plot disaggregates over time
#'
#'
#' @param df dataframe
#' @num number of ids
#' @param my_vars df with info about variables
#' @param pd ggplot dodge parameter
#' @export

row_function <- function(num) {
  num <- as.integer(num)
  if (num > 8){
    row <- ceiling(num/8)
  }else {
    row <- 1
  }
  return(row)
}

# convert some vars to factor vars
factorize_vars <- function(df, my_vars){
  vars_vec <- names(df) %in% my_vars$variable
  factor_vars <- c()
  for(i in 1:length(vars_vec)){
    if(vars_vec[i] == T && as.logical(my_vars[my_vars$variable == names(df)[i],]$factor)){
      factor_vars[i] = T
    }else{
      factor_vars[i] = F
    }
  }
  # convert to factor vars
  df[, factor_vars] <- lapply(df[, factor_vars], factor)
  # extract labels of all factor vars
  factor_label <- my_vars[my_vars$variable %in% names(df[, factor_vars]), ]$label
  # extract factor vars name
  factor_var_name <- names(df)[factor_vars]
  # change levels of each factor var
  for(i in 1:length(factor_var_name)){
    levels(df[[factor_var_name[i]]]) <- unlist(strsplit(factor_label[i], ","))
  }

  return(df)
}



plot_disaggregates_row <- function(df, my_vars, pd = ggplot2::position_dodge(.1), switch = "y", nrow = NULL){

  fac_var <- as.logical(my_vars$factor)
  vars <- pull(my_vars, variable)
  var_labs <- my_vars  %>% pull(short_label)
  names(var_labs) <- vars

  # Gather mins amd maxs
  ranges <- my_vars %>% select(one_of("variable", "min", "max"))
  if(!("min" %in% names(ranges))) ranges$min <- NA
  if(!("max" %in% names(ranges))) ranges$max <- NA

  # Reshape and bring in mins and maxs
  df2 <- df %>%
    select(all_of(c("id", "date", vars))) %>%
    reshape2::melt(id.vars= c("id", "date"))  %>%
    group_by(id, date, variable) %>%
    summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE), n = ~gdata::nobs(.))) %>%
    mutate(se = sd / sqrt(n), ymin=mean-1.96*se, ymax=mean+1.96*se) %>%
    left_join(ranges)


  # Row num for layout
  nrow <- row_function(length(unique(df2$id)))
  row_id <- split(unique(df2$id), rep(seq(nrow), each = length(unique(df2$id)) /nrow))
  # subplot to 4 parts
  subplots <- list()
  for(i in 1:nrow){
     subplots[[i]]<-
      ggplot(data = df2[df2$id %in% row_id[[i]],], aes(x = date, y = mean)) +
      geom_point(position=pd) +
      geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.1, position=pd) +
      geom_line(position=pd) +
      facet_grid(variable ~ id, scales = "free_y",
                 labeller = labeller(variable = var_labs),
                 switch = switch) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            strip.text.y.left = element_text(angle = 0))
  }

  # pass every subplot to grid.arrange, followed by nrow
  g <-  do.call(grid.arrange, c(subplots, list(nrow=nrow)))

  g
}


plot_disaggregates <- function(df, my_vars, pd = ggplot2::position_dodge(.1), switch = "y"){

  vars <- pull(my_vars, variable)
  var_labs <- my_vars  %>% pull(short_label)
  names(var_labs) <- vars

  # Gather mins amd maxs
  ranges <- my_vars %>% select(one_of("variable", "min", "max"))
   if(!("min" %in% names(ranges))) ranges$min <- NA
   if(!("max" %in% names(ranges))) ranges$max <- NA

  # Reshape and bring in mins and maxs
  df2 <- df %>%
    select(all_of(c("id", "date", vars))) %>%
    reshape2::melt(id.vars= c("id", "date"))  %>%
    group_by(id, date, variable) %>%
    summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE), n = ~gdata::nobs(.))) %>%
    mutate(se = sd / sqrt(n), ymin=mean-1.96*se, ymax=mean+1.96*se) %>%
    left_join(ranges)

  # Prep plot
  g <- df2 %>%
    ggplot(aes(x=date, y=mean)) +
      geom_point(position=pd) +
      geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.1, position=pd) +
      geom_line(position=pd) +
      facet_grid(variable ~ id, scales = "free_y",
                 labeller = labeller(variable = var_labs),
                 switch = switch) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

  # Add floor and ceiling as ghost layer
  if(!(all(is.na(df2$max)))) g <- g + geom_blank(aes(x=df2$date, y=df2$max))
  if(!(all(is.na(df2$min)))) g <- g + geom_blank(aes(x=df2$date, y=df2$min))

  g

}
