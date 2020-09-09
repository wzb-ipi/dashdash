together_plot <- function(df, my_vars){
  together_vars <- my_vars
  prefix <- together_vars$together[[1]]
  common_question <-  together_vars$title[[1]]

  together <- df %>% dplyr::select(starts_with(paste0(prefix)))  %>% names

  df_together <- data.frame(
    question = get_label(df[,together]),
    foreach(i=together,.combine="rbind") %do%{
      eachqs = df[,i]
      obs = nrow(eachqs %>% drop_na())
      yes = sum(eachqs=="yes"|eachqs==1, na.rm = TRUE)
      no = sum(eachqs=="no"|eachqs==0, na.rm = TRUE)
      perc = round(yes/obs,3)*100
      c("observations" = obs,"Yes-1" = yes,"No-0"=no,"Perc."=perc)
    },
    stringsAsFactors = FALSE)

  if(""%in%df_together$question){
    df_together <- rownames_to_column(df_together, var = "rowname")

    together_vars <- select(together_vars, rowname=variable, short_label)
    df_together <- left_join(df_together, together_vars, by="rowname") %>%
      select(-question) %>%
      rename(question=short_label)
  }

  rownames(df_together) <- NULL

  df_together$question=factor(df_together$question, levels = df_together$question[order(df_together$Perc.)])

  p_together <- ggplot(data = df_together,aes(x=question,y=Perc.)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    geom_text(aes(label=Perc.), hjust = -0.2) +
    ylab("% of Respondents") + xlab("Option") + ylim(0,100)+
    theme(axis.text.x = element_text(colour="black"),
          axis.text.y = element_text(colour="black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          text = element_text(size=20)) +
    ggtitle(paste0(common_question))+
    theme_minimal()

}
