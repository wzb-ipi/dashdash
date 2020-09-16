bars_together <- function(df, my_vars, nrow=NULL){

  if("together" %in% colnames(my_vars))
  {
    #choose that don't go together
    separate_vars <- filter(my_vars, is.na(together))

    if(nrow(separate_vars)!=0){
      # extract vars' info from my_var
      vars <- pull(separate_vars, variable)
      var_labs <- separate_vars  %>% pull(short_label)
      names(var_labs) <- vars

      # No. of vars
      length_var <- length(vars)

      # select all vars from separate_vars
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

        question_label <- split_every(var_labs[names(var_labs) == vars[x]], 6, pattern = " ")
        question_label <- paste(question_label,collapse="\n")

        ggplot(data = df2 %>% filter(!is.na(!!sym(vars[x]))), aes(x=.data[[vars[x]]]))+
          geom_bar(aes(y = (..count..)/sum(..count..)), na.rm = TRUE) +
          geom_text(aes(label = scales::percent(signif((..count..)/sum(..count..), 3)), y= (..count..)/sum(..count..)), stat= "count",size=3, hjust = -0.2) +
          coord_flip() +
          scale_y_continuous(labels = scales::percent_format(accuracy = .1), limits=c(0,1))+
          labs(title = question_label, x= NULL, y= "percent") +
          theme(axis.text.x = element_text(angle = 0, hjust = 1),
                axis.title=element_text(face="bold"),
                plot.title = element_text(face = "bold", size=1),
                strip.text.y.left = element_text(angle = 0),
                text = element_text(size=20))+
          theme_minimal() })

    }


    #do together plot
    together_vars <- filter(my_vars, !is.na(together))
    together_list <- list()
    for (common in unique(together_vars$together)) {
      n_common <- filter(together_vars, together==common)
      t <- dashdash:::together_plot(my_data, n_common)
      together_list[[common]] <- t
    }

    if(exists("subplots")){
      subplots <- c(together_list, subplots)
      # place all subplots in nrow
      nrow <- nrow
      if(is.null(nrow)) nrow <- round(length(subplots)/2)
      g <-  do.call(grid.arrange, c(subplots, list(nrow=nrow)))
      return(g)
    }

    if(!exists("subplots")){
      nrow <- nrow
      if(is.null(nrow)) nrow <- round(length(subplots)/2)
      g <-  do.call(grid.arrange, c(together_list, list(nrow=nrow)))
      return(g)
    }
  }

  if(!isTRUE("together" %in% colnames(my_vars)))
  {
    all_bar_plot(my_data, my_vars, nrow=4)
  }
}
