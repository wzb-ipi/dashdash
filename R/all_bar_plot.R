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


#' together_plot one plot with relative freq
#'
#' @param df dataset
#' @param my_vars df with info about variables
#' @export
#'
#'
together_plot <- function(df, my_vars){
  together_vars <- filter(my_vars, !is.na(together))
  prefix <- together_vars$together[[1]]
  common_question <-  together_vars$title[[1]]

  together <- df %>% dplyr::select(starts_with(paste0(prefix)))  %>% names

  df_together <- data.frame(
    question = get_label(df[,together]),
    foreach(i=together,.combine="rbind") %do%{
      eachqs = df[,i]
      obs = nrow(eachqs %>% drop_na())
      yes = sum(eachqs=="yes", na.rm = TRUE)
      no = sum(eachqs=="no", na.rm = TRUE)
      perc = round(yes/obs,3)*100
      c("observations" = obs,"Yes-1" = yes,"No-0"=no,"Perc."=perc)
    },
    stringsAsFactors = FALSE)

  rownames(df_together) <- NULL

  df_together$question=factor(df_together$question, levels = df_together$question[order(df_together$Perc.)])

  p_together <- ggplot(data = df_together,aes(x=question,y=Perc.)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    geom_text(aes(label=Perc.),size=3, hjust = -0.2) +
    ylab("% of Respondents") + xlab("Option") + ylim(0,100)+
    theme(axis.text.x = element_text(colour="black"),
          axis.text.y = element_text(colour="black"),
          plot.title = element_text(size = 10, face = "bold")) +
    ggtitle(paste0(common_question))

}



