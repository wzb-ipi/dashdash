#' het_bar plot function to plot all vars with bar divided by id
#'
#' @param df dataset
#' @param my_vars df with info about variables
#' @export
#'
#'
het_bar_all <- function(df, my_vars, nrow=NULL){


  if("together" %in% colnames(my_vars))
  {
    #choose that don't go together
    separate_vars <- filter(my_vars, is.na(together))

    if(nrow(separate_vars)!=0){
      subplots <- het_bar_plot(df, separate_vars)
    }


    #do together plot
    together_vars <- filter(my_vars, !is.na(together))
    together_list <- list()
    for (common in unique(together_vars$together)) {
      n_common <- filter(together_vars, together==common)
      t <- dashdash:::het_together(my_data, n_common)
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
    het_bar_plot(my_data, my_vars, nrow=4)
  }
}

#' het_bar plot function to plot all vars with bar divided by id
#'
#' @param df dataset
#' @param my_vars df with info about variables
#' @export
#'
#'
#'
het_bar_plot <- function(df, my_vars){
  # extract vars' info from my_var
  vars <- pull(my_vars, variable)
  var_labs <- my_vars  %>% pull(short_label)
  names(var_labs) <- vars

  # No. of vars
  length_var <- length(vars)

  # select all vars from separate_vars
  df2 <- df %>% select(id, all_of(vars))

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

  # generate bar plot for each var, and store them into subplots list
  subplots <- lapply(1:length(vars), function(x){
    ggplot(data = df2 %>% filter(!is.na(!!sym(vars[x]))), aes(x=.data[[vars[x]]], fill=id))+
      geom_bar(aes(y = (..count..)/sum(..count..)), na.rm = TRUE, position = position_dodge()) +
      geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), stat= "count", hjust = -0.2,
                position = position_dodge(.9)) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = .01), limits=c(0,1))+
      scale_fill_manual(name = "State", labels = c("Delta", "Edo"), values=c("#999999", "black")) +
      labs(title = var_labs[names(var_labs) == vars[x]], x= NULL, y= "percent") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            axis.title=element_text(face="bold"),
            plot.title = element_text(hjust = 0.5),
            strip.text.y.left = element_text(angle = 0))})
}


#' het_together function to plot together plot by id
#'
#' @param df dataset
#' @param my_vars df with info about variables
#' @export
#'
#'
het_together <- function(df, my_vars){
  together_vars <- my_vars
  prefix <- together_vars$together[[1]]
  common_question <-  together_vars$title[[1]]

  together <- df %>% dplyr::select(starts_with(paste0(prefix)))  %>% names
  ids <- unique(df$id)

  by_id_df <- list()

  for (state in ids) {

    df_id <- filter(df, id==state)

    df_together <- data.frame(
      question = get_label(df_id[,together]),
      foreach(i=together,.combine="rbind") %do%{
        eachqs = df_id[,i]
        obs = nrow(eachqs %>% drop_na())
        yes = sum(eachqs=="yes"|eachqs==1, na.rm = TRUE)
        no = sum(eachqs=="no"|eachqs==0, na.rm = TRUE)
        perc = round(yes/obs,3)*100
        c("observations" = obs,"Yes-1" = yes,"No-0"=no,"Perc."=perc)
      },
      stringsAsFactors = FALSE)

    if(""%in%df_together$question){
      df_together <- rownames_to_column(df_together, var = "rowname")

      together_vars_ <- select(together_vars, rowname=variable, short_label)
      df_together <- left_join(df_together, together_vars_, by="rowname") %>%
        select(-question) %>%
        rename(question=short_label)
    }

    rownames(df_together) <- NULL

    df_together$question=factor(df_together$question, levels = df_together$question[order(df_together$Perc.)])

    df_together <- mutate(df_together, id=state)

    by_id_df[[state]] <- df_together

  }


  df_together = do.call(rbind, by_id_df)

  p_together <- ggplot(data = df_together,aes(x=question,y=Perc., fill=id)) +
    geom_bar(stat = 'identity', position = position_dodge()) +
    coord_flip() +
    geom_text(aes(label=Perc.), hjust = -0.2,
              position = position_dodge(.9)) +
    ylab("% of Respondents") + xlab("Option") + ylim(0,100)+
    theme(axis.text.x = element_text(colour="black"),
          axis.text.y = element_text(colour="black"),
          plot.title = element_text(face = "bold")) +
    ggtitle(paste0(common_question)) +
    scale_fill_manual(name = "State", labels = c("Delta", "Edo"), values=c("#999999", "black"))
}
