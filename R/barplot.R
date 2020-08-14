#' row fun
#'
#' @param num number of rows to truncate
#' @param my_vars df with info about variables
row_function <- function(num) {
  num <- as.integer(num)
  if (num > 8){
    row <- ceiling(num/8)
  }else {
    row <- 1
  }
  return(row)
}

#' all_bar_plot function to plot all vars with bar
#'
#' @param df dataset
#' @param my_vars df with info about variables
#' @param nrow numer of rows
#' @export
all_bar_plot <- function(df, my_vars, nrow = NULL){

  # extract vars' info from my_var
  vars <- pull(my_vars, variable)
  var_labs <- my_vars  %>% pull(short_label)
  names(var_labs) <- vars

  # No. of vars
  length_var <- length(vars)

  # select all vars from my_vars
  df2 <- df %>% select(all_of(vars))

  # check if vars with attributed labels
  types_df2 <- sapply(1:length(df2), function(x){class(df2[[x]])})
  if ("haven_labelled" %in% types_df2){
    # convert all vars with attributed labels to factor
    for(i in(1:length(df2))){
      if(class(df2[[i]]) == "haven_labelled"){
        df2[[i]] <- haven::as_factor(df2[[i]],levels = "labels")
      }
    }
  }else{
    # set all vars as factor vars
    df2[, vars] <- lapply(df[, vars], factor)
  }



  if(is.null(nrow)){nrow <- row_function(length_var)}

  # generate bar plot for each var, and store them into subplots list
  subplots <- lapply(1:length(vars), function(x){
    ggplot(data = df2 %>% filter(!is.na(!!sym(vars[x]))), aes(x=.data[[vars[x]]]))+
      geom_bar(aes(y = (..count..)/sum(..count..)), na.rm = TRUE) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1))+
      labs(title = var_labs[names(var_labs) == vars[x]], x= NULL, y= "percent") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            axis.title=element_text(size=8,face="bold"),
            plot.title = element_text(hjust = 0.5),
            strip.text.y.left = element_text(angle = 0))})

  # place all subplots in nrow
  g <-  do.call(grid.arrange, c(subplots, list(nrow=nrow)))
  g

}


