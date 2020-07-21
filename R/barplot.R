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

  # set all vars as factor vars
  df2[, vars] <- lapply(df[, vars], factor)

  if(is.null(nrow)){nrow <- row_function(length_var)}

  # generate bar plot for each var, and store them into subplots list
  subplots <- lapply(1:length(vars), function(x){
    ggplot(data = df2, aes(x=.data[[vars[x]]]))+
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      xlab(var_labs[names(var_labs) == vars[x]]) +
      ylab("percent") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.title=element_text(size=8,face="bold"),
            strip.text.y.left = element_text(angle = 0))})
  # place all subplots in nrow
  g <-  do.call(grid.arrange, c(subplots, list(nrow=nrow)))
  g

}
