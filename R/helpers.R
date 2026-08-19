# ---------------------------------------------------------
# file: helpers.R
# author: Jason Grafmiller
# date: 2026-04-20
# description:
# 
# ---------------------------------------------------------
suppressMessages(library(dplyr)) # for dplyr, ggplot, etc.

get_items_reported <- function(data){
    data |> 
      count(N_items_reported) |> 
      filter(N_items_reported != "") |> 
      mutate(perc = round(100*n/sum(n),1)) |> 
      pull(perc) |> 
      first()
}

get_conds_reported <- function(data){
    data |> 
      count(N_conditions_reported) |> 
      filter(N_conditions_reported != "") |> 
      mutate(perc = round(100*n/sum(n),1)) |> 
      pull(perc) |> 
      first()
}

get_data_inclusion_perc <- function(data){
  round(100*(1 - nrow(data[data$data_avail == "Not Reported",])/nrow(data)), 1)
}

get_items_inclusion_perc <- function(data){
    round(100*(1 - nrow(data[data$item_avail == "Not Reported",])/nrow(data)), 1)
}