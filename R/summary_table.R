#' summary table
#'
#'
#' @param df dataframe
#' @param my_vars df with info about variables
#' @param date date variable
#' @export



summary_table <- function(df, my_vars){

  # count the frequences by districts
  counts <- df %>% group_by(id) %>% count() %>% rename(N = n)
  # merge counts to df
  counts_df <- merge(counts, df)
  # check if var is date variable
  is.date <- function(date) inherits(date, 'Date')
  # remove date variable
  counts_df <- counts_df[,!sapply(counts_df, is.date)]
  # calculate average in all and responding frequence
  all_mean <- apply(df[, sapply(df, is.numeric)], 2, mean)
  all_count <- nrow(df)
  names(all_count) <- "N"
  all_summary <- round(c(all_count, all_mean),4)
  # transpose the datframe
  melt_data <- dcast(melt(counts_df), variable ~ id, mean)
  # select numeric var
  is.num <- sapply(melt_data, is.numeric)
  melt_data[is.num] <- lapply(melt_data[is.num], round, 4)

  # add overall summary to melt_data
  melt_data[, "Overall"] <- all_summary
  # rename var by my_var
  melt_data$variable <- as.character(melt_data$variable)
  if(sum(melt_data$variable[-1] == my_vars$variable) >0){
    melt_data$variable[-1] <- sapply(1:length(melt_data$variable[-1]),function(x){
                                    if (melt_data$variable[-1][x] == my_vars$variable[x]){
                                          melt_data$variable[-1][x]  <-  my_vars$short_label[x]}
                                    })
  }
  DT::datatable(melt_data, rownames = FALSE,
                extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = I('colvis')))

}
