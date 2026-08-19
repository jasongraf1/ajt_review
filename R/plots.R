# ---------------------------------------------------------
# file: plots.R
# author: Jason Grafmiller
# date: 2026-04-16
# description:
# Functions for making reactive plots in Shiny dashboard
# ---------------------------------------------------------

suppressMessages(library(here)) # for pathing to/from project directory
suppressMessages(library(tidyverse)) # for dplyr, ggplot, etc.
suppressMessages(library(janitor)) # for cleaning data
suppressMessages(library(plotly))
suppressMessages(library(showtext))

# Global variables and helper functions ---------------

# set fonts
font <- list(
  family = "Roboto"
)

# for ggplot
font_add_google("Roboto", family = "Roboto")
showtext_auto()

# Plot functions --------------------------------------

make_barplot <- function(data, y, xmax = NULL, font = NULL,
                         height = 400, colors = c("steelblue4", "grey")){
  d <- data |> 
    count(.data[[y]]) |> 
    mutate(
      # perc = round(100 * n / sum(n), 1),
      "{y}" := fct_reorder(.data[[y]], n),
      color = ifelse(.data[[y]] == "Not Reported", "b", "a") |> 
        as.factor(),
      labels = n
    )
  
  if(is.null(xmax)) {xmax <- max(d$n) + 10}
  y_vals <- d[[y]]
  
  plot_ly(
    data = d,
    x = ~n,
    y = y_vals,
    color = ~color,
    text = ~labels,
    customdata = ~n,
    type = "bar",
    orientation = "h",
    textposition = 'outside',
    hovertemplate = "N = %{customdata}<extra></extra>",
    colors = setNames(colors, c("a", "b")),
    height = height
  ) |>
    layout(
      font = font,
      yaxis = list(title = "", tickfont = list(size = 16), ticksuffix = " "),
      xaxis = list(
        title = "% of studies",
        showgrid = TRUE,
        showline = TRUE,
        range = c(0, xmax),
        zeroline = FALSE,
        ticks = "outside",
        tickfont = list(size = 14),
        ticksuffix = "%"
      ),
      uniformtext = list(minsize = 16, mode = "show"),
      showlegend = FALSE
    ) |>
    config(displayModeBar = FALSE)
}

make_gg_barplot <- function(data, y, xmax = NULL, base_size = 14,
                         colors = c("steelblue4", "grey")){
  d <- data |> 
    count(.data[[y]]) |> 
    mutate(
      "{y}" := fct_reorder(.data[[y]], n),
      color = ifelse(.data[[y]] == "Not Reported", "b", "a") |> 
        as.factor(),
      labels = n
    )
  
  if (is.null(xmax)) {xmax <- max(d$n) + 10}
  
  ggplot(d, aes(x = .data[[y]], y = n, fill = color)) +
    geom_col(width = 0.7) +
    geom_text(
      aes(label = labels),
      hjust = -0.1,
      size = base_size / .pt   # match textfont size ~16
    ) +
    scale_fill_manual(values = setNames(colors, c("a", "b")), guide = "none") +
    scale_y_continuous(
      name = "Number of studies",
      limits = c(0, xmax),
      expand = expansion(mult = c(0, 0.02)),
    ) +
    coord_flip(clip = "off") +
    labs(x = NULL) +
    theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "Roboto"),
      axis.text.y = element_text(size = base_size),
      axis.text.x = element_text(size = base_size),
      axis.title.x = element_text(size = base_size),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(color = "black")
    )
}

make_split_gg_barplot <- function(data, y, xmax = NULL, base_size = 14,
                                    colors = c("steelblue4", "grey")){
  
  d <- data |> 
    rowid_to_column("row_id") |> 
    separate_rows(.data[[y]], sep = ";") |>
    mutate("{y}" := str_trim(.data[[y]])) |>
    distinct(row_id, .data[[y]]) |>
    count(.data[[y]]) |>
    mutate(
      "{y}" := fct_reorder(.data[[y]], n),
      color = ifelse(.data[[y]] == "Not Reported", "b", "a") |> 
        as.factor(),
      labels = n
    )
  
  if (is.null(xmax)) {xmax <- max(d$n) + 10}
  
  ggplot(d, aes(x = .data[[y]], y = n, fill = color)) +
    geom_col(width = 0.7) +
    geom_text(
      aes(label = labels),
      hjust = -0.2,
      size = base_size / .pt   # match textfont size ~16
    ) +
    scale_fill_manual(values = setNames(colors, c("a", "b")), guide = "none") +
    scale_y_continuous(
      name = "Number of studies",
      limits = c(0, xmax),
      expand = expansion(mult = c(0, 0.02)),
    ) +
    coord_flip(clip = "off") +
    labs(x = NULL) +
    theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "Roboto"),
      axis.text.y = element_text(size = base_size),
      axis.text.x = element_text(size = base_size),
      axis.title.x = element_text(size = base_size),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(color = "black")
    )
}

# Plot for making the horizontal bar plots of percentages
make_perc_barplot <- function(data, y, xmax = NULL, font = NULL,
                               colors = c("steelblue4", "grey")) {
  
  d <- data |> 
    count(.data[[y]]) |> 
    mutate(
      perc = round(100 * n / sum(n), 1),
      "{y}" := fct_reorder(.data[[y]], perc),
      color = ifelse(.data[[y]] == "Not Reported", "b", "a") |> 
        as.factor(),
      labels = paste0(perc, "%")
    )
  
  if(is.null(xmax)) {xmax <- max(d$perc) + 10}
  y_vals <- d[[y]]
  
  plot_ly(
    data = d,
    x = ~perc,
    y = y_vals,
    color = ~color,
    text = ~labels,
    customdata = ~n,
    type = "bar",
    orientation = "h",
    textposition = 'outside',
    hovertemplate = "N = %{customdata}<extra></extra>",
    colors = setNames(colors, c("a", "b")),
    height = 400
  ) |>
    layout(
      font = font,
      yaxis = list(
        font = font,
        title = "", 
        tickfont = list(size = 16, family = font), 
        ticksuffix = " "),
      xaxis = list(
        title = "% of studies",
        showgrid = TRUE,
        showline = TRUE,
        range = c(0, xmax),
        zeroline = FALSE,
        ticks = "outside",
        tickfont = list(size = 14, family = font),
        ticksuffix = "%"
      ),
      uniformtext = list(minsize = 16, mode = "show"),
      margin = list(b = 100),
      showlegend = FALSE
    ) |>
    config(displayModeBar = FALSE)
}

make_perc_gg_barplot <- function(data, y, xmax = NULL, base_size = 14,
                              colors = c("steelblue4", "grey")) {
  
  d <- data |> 
    count(.data[[y]]) |> 
    mutate(
      perc = round(100 * n / sum(n), 1),
      "{y}" := fct_reorder(.data[[y]], perc),
      color = ifelse(.data[[y]] == "Not Reported", "b", "a") |> 
        as.factor(),
      labels = paste0(n, " (",perc, "%)")
    )
  
  if (is.null(xmax)) {xmax <- max(d$perc) + 12}
  
  ggplot(d, aes(x = .data[[y]], y = perc, fill = color)) +
    geom_col(width = 0.7) +
    geom_text(
      aes(label = labels, color = color),
      hjust = -0.2,
      size = base_size / .pt
    ) +
    scale_fill_manual(values = setNames(colors, c("a", "b")), guide = "none") +
    scale_color_manual(values = setNames(colors, c("a", "b")), guide = "none") +
    scale_y_continuous(
      name = "% of studies",
      limits = c(0, xmax),
      expand = expansion(mult = c(0, 0.02)),
      labels = function(x) paste0(x, "%")
    ) +
    coord_flip(clip = "off") +
    labs(x = NULL) +
    theme_minimal(base_size = base_size) +
    theme(
      axis.text.y = element_text(size = base_size),
      axis.text.x = element_text(size = base_size),
      axis.title.x = element_text(size = base_size),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      plot.margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)
    )
}



make_split_perc_barplot <- function(data, y, xmax = NULL, font = NULL,
                                    colors = c("steelblue4", "grey")){
  
  d <- data |> 
    mutate(row_id = row_number()) |>
    separate_rows(.data[[y]], sep = ";") |>
    mutate("{y}" := str_trim(.data[[y]])) |>
    distinct(row_id, .data[[y]]) |>
    count(.data[[y]]) |>
    mutate(
      perc = round(100 * n / nrow(data), 1),
      labels = paste0(perc, "%"),
      "{y}" := fct_reorder(.data[[y]], perc),
      color = ifelse(.data[[y]] == "Not Reported", "b", "a") |> 
        as.factor()
    )
  d
  
  if(is.null(xmax)) {xmax <- max(d$perc) + 10}
  y_vals <- d[[y]]
  
  plot_ly(
    data = d,
    x = ~perc,
    y = y_vals,
    color = ~color,
    text = ~labels,
    customdata = ~n,
    type = "bar",
    orientation = "h",
    textposition = 'outside',
    hovertemplate = "N = %{customdata}<extra></extra>",
    colors = setNames(colors, c("a", "b")),
    height = 400
  ) |>
    layout(
      font = font,
      yaxis = list(title = "", tickfont = list(size = 16), ticksuffix = " "),
      xaxis = list(
        title = "% of studies",
        showgrid = TRUE,
        showline = TRUE,
        range = c(0, xmax),
        zeroline = FALSE,
        ticks = "outside",
        tickfont = list(size = 14),
        ticksuffix = "%"
      ),
      uniformtext = list(minsize = 16, mode = "show"),
      showlegend = FALSE
    ) |>
    config(displayModeBar = FALSE)
}

make_split_perc_gg_barplot <- function(data, y, xmax = NULL, base_size = 14,
                                  colors = c("steelblue4", "grey")){
  
  d <- data |> 
    rowid_to_column("row_id") |> 
    separate_rows(.data[[y]], sep = ";") |>
    mutate("{y}" := str_trim(.data[[y]])) |>
    distinct(row_id, .data[[y]]) |>
    count(.data[[y]]) |>
    mutate(
      perc = round(100 * n / sum(n), 1),
      "{y}" := fct_reorder(.data[[y]], perc),
      color = ifelse(.data[[y]] == "Not Reported", "b", "a") |> 
        as.factor(),
      labels = paste0(n, " (",perc, "%)")
    )
  
  if (is.null(xmax)) {xmax <- max(d$perc) + 10}
  
  ggplot(d, aes(x = .data[[y]], y = perc, fill = color)) +
    geom_col(width = 0.7) +
    geom_text(
      aes(label = labels, color = color),
      hjust = -0.1,
      size = base_size / .pt   # match textfont size ~16
    ) +
    scale_fill_manual(values = setNames(colors, c("a", "b")), guide = "none") +
    scale_color_manual(values = setNames(colors, c("a", "b")), guide = "none") +
    scale_y_continuous(
      name = "% of studies",
      limits = c(0, xmax),
      expand = expansion(mult = c(0, 0.02)),
      labels = function(x) paste0(x, "%")
    ) +
    coord_flip(clip = "off") +
    labs(x = NULL) +
    theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "Roboto"),
      axis.text.y = element_text(size = base_size),
      axis.text.x = element_text(size = base_size),
      axis.title.x = element_text(size = base_size),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(color = "black")
    )
}

make_boxplot <- function(data, x, xlab, xrange = NULL,
                         colors = c("steelblue4", "lightblue")){
  
  q1  <- quantile(data[[x]], 0.25, na.rm = TRUE)
  q3  <- quantile(data[[x]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  med <- median(data[[x]], na.rm = TRUE)
  lower <- max(min(data[[x]], na.rm = TRUE), q1 - 1.5 * iqr)
  upper <- min(max(data[[x]], na.rm = TRUE), q3 + 1.5 * iqr)
  
  set.seed(123)
  
  if(is.null(xrange)){
    xrange <- c(0, max(data[[x]]) + 10)
  }
  
  data |> 
    mutate(y_jit = rnorm(n(), mean = 0, sd = 0.05)) |>
    plot_ly() |>
    add_trace(
      type = "box",
      orientation = "h",
      q1 = list(q1),
      median = list(med),
      q3 = list(q3),
      lowerfence = list(lower),
      upperfence = list(upper),
      hoverinfo = "skip",
      boxpoints = FALSE,
      fillcolor = colors[2],
      line = list(color = colors[1])
    ) |>
    add_trace(
      x = ~.data[[x]],
      y = ~y_jit,
      type = "scatter",
      mode = "markers",
      hovertext = ~paste0(
        "<b>", xlab, ":</b> ", .data[[x]],
        "<br>", citation
      ),
      hoverinfo = "text",
      color = I(colors[1]),
      marker = list(size = 6, opacity = 0.6)
    ) |>
    layout(
      yaxis = list(
        title = '',
        showticklabels = FALSE,
        zeroline = FALSE
      ),
      xaxis = list(
        title = paste('Number of', xlab),
        showgrid = TRUE,
        showline = TRUE,
        zeroline = FALSE,
        range = xrange,
        ticks = "outside"
      ),
      uniformtext = list(minsize = 16, mode = 'show'),
      font = font,
      margin = list(t = 10),
      showlegend = FALSE,          
      autosize = TRUE
    ) |>
    plotly::config(displayModeBar = F, responsive = T)
}

make_stacked_perc_barplot <- function(
    data, y, xmax = NULL, font = NULL, orientation = "h", 
    sort = TRUE,
    colors = c("#198754", "grey")){
  d <- data |> 
    mutate(
      "{y}" := str_replace_all(.data[[y]], "Jo", "J of "),
      "{y}" := str_replace_all(.data[[y]], "\\s+", " ")
    ) |> 
    count(.data[[y]], coded) |> 
    pivot_wider(names_from = coded, values_from = n) |> 
    clean_names() |>
    mutate(
      across(2:3, ~ifelse(is.na(.x), 0, .x)),
      total = coded + not_coded
    )
  
  if(sort){
    d <- d |> 
      mutate(
      "{y}" := paste(.data[[y]], " "),
      "{y}" := fct_reorder(.data[[y]], total)
      )
  }
  
  if(is.null(xmax)) {xmax <- max(d$total) + 10}
  y_vals <- d[[y]]
  
  if(orientation == "h"){
    plot_ly(
        data = d, 
        x = ~coded, y = y_vals,
        type = "bar",
        orientation = "h",
        hovertemplate = "%{hovertext}<extra></extra>",
        hovertext = ~paste(coded, "coded"),
        name = "Coded",
        color = I("#198754")
      ) |> 
      add_trace(
        x = ~not_coded, 
        y = y_vals,
        hovertext = ~paste(not_coded, "remaining"),
        orientation = "h",
        name = "Not coded",
        color = I("grey80")
      ) |> 
      layout(
        barmode = 'stack', font = font,
        yaxis = list(title = '', tickfont = list(size = 16)),
        xaxis = list(title = '', showgrid = T, showline = T, 
                     zeroline = FALSE, ticks="outside",
                     tickfont = list(size = 14)),
        legend = list(x = 0.7, y = 0.1, font = list(size = 16)),
        autosize = TRUE
      ) |>
      plotly::config(displayModeBar = F, responsive = T)
    } else {
      plot_ly(
        data = d, 
        y = ~coded, 
        x = y_vals,
        type = "bar",
        # orientation = "h",
        hovertemplate = "%{hovertext}<extra></extra>",
        hovertext = ~paste(coded, "coded"),
        name = "Coded",
        color = I("#198754")
      ) |> 
        add_trace(
          y = ~not_coded, 
          x = y_vals,
          hovertext = ~paste(not_coded, "remaining"),
          name = "Not coded",
          color = I("grey80")
        ) |> 
        layout(
          barmode = 'stack', font = font,
          xaxis = list(title = '', tickfont = list(size = 16)),
          yaxis = list(title = '', showgrid = T, showline = T, 
                       zeroline = FALSE, ticks="outside",
                       tickfont = list(size = 14)),
          legend = list(x = 0.1, y = 0.9, font = list(size = 16)),
          autosize = TRUE
        ) |>
        plotly::config(displayModeBar = F, responsive = T)
    }
  
}

make_perc_gg_barplot <- function(data, y, xmax = NULL, base_size = 14,
                                 colors = c("steelblue4", "grey")) {
  
  d <- data |> 
    count(.data[[y]]) |> 
    mutate(
      perc = round(100 * n / sum(n), 1),
      "{y}" := fct_reorder(.data[[y]], perc),
      color = ifelse(.data[[y]] == "Not Reported", "b", "a") |> 
        as.factor(),
      labels = paste0(n, " (",perc, "%)")
    )
  
  if (is.null(xmax)) {xmax <- max(d$perc) + 12}
  
  ggplot(d, aes(x = .data[[y]], y = perc, fill = color)) +
    geom_col(width = 0.7) +
    geom_text(
      aes(label = labels, color = color),
      hjust = -0.2,
      size = base_size / .pt
    ) +
    scale_fill_manual(values = setNames(colors, c("a", "b")), guide = "none") +
    scale_color_manual(values = setNames(colors, c("a", "b")), guide = "none") +
    scale_y_continuous(
      name = "% of studies",
      limits = c(0, xmax),
      expand = expansion(mult = c(0, 0.02)),
      labels = function(x) paste0(x, "%")
    ) +
    coord_flip(clip = "off") +
    labs(x = NULL) +
    theme_minimal(base_size = base_size) +
    theme(
      axis.text.y = element_text(size = base_size),
      axis.text.x = element_text(size = base_size),
      axis.title.x = element_text(size = base_size),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      plot.margin = margin(t = 5.5, r = 20, b = 20, l = 5.5)
    )
}


make_histogram <- function(data, x, xlab, xrange = c(0, 400),
                           colors = c("steelblue4", "lightblue")){
  
  data |> 
    plot_ly(
      x = data[[x]],
      type = "histogram"
    ) |> 
    layout(
      font = font,
      yaxis = list(title = "", tickfont = list(size = 16), ticksuffix = " "),
      xaxis = list(
        title = "Items per condition",
        showgrid = TRUE,
        showline = TRUE,
        # range = c(0, xmax),
        zeroline = FALSE,
        ticks = "outside",
        tickfont = list(size = 14),
        ticksuffix = "%"
      ),
      uniformtext = list(minsize = 16, mode = "show"),
      margin = list(b = 100),
      showlegend = FALSE
    ) |>
    config(displayModeBar = FALSE)
}




# Overview --------------------------------------------


# Participants ----------------------------------------

plot_recruitment_trends <- function(data){
  d <- data |> 
    count(part_recruitment, year) |> 
    group_by(year) |> 
    mutate(
      perc = round(100*n/sum(n),1),
      part_recruitment = fct_reorder(part_recruitment, perc),
      color = ifelse(part_recruitment == "Not Reported", "b", "a") |> 
        as.factor(),
      labels = paste0(perc,"%")
    )
  
  p <- d |> 
    filter(part_recruitment %in% c("University affiliation", "Online platform", "Not Reported")) |>
    mutate(
      part_recruitment = as.factor(part_recruitment),
      part_recruitment = relevel(part_recruitment, ref = "Not Reported")
    ) |> 
    ggplot(aes(year, perc, color = part_recruitment)) +
    geom_smooth(se = F, method = "gam", formula = y ~ s(x, k = 4, bs = "cs")) +
    labs(x = "", y = "", color = "") +
    scale_y_continuous(
      limits = c(0,100),
      breaks = seq(0, 100, 20),
      labels = paste0(seq(0, 100, 20), "%")
    ) +
    scale_color_manual(values = c("#868686FF", "#0073C2FF", "#CD534CFF")) +
    theme(
      axis.line.x = element_blank()
    ) +
    coord_capped_cart(left = "both")
  
  ggplotly(p) |> 
    layout(
      font = font,
      yaxis = list(
        title = "% of studies", 
        range = c(0, 100),
        tickfont = list(size = 16)
      ),
      xaxis = list(
        title = "",
        showgrid = FALSE,
        showline = FALSE,
        zeroline = FALSE,
        ticks = "outside",
        tickfont = list(size = 14)
      ),
      uniformtext = list(minsize = 16, mode = "show"),
      legend = list(x = 0.1, y = 0.95, orientation = 'h',
                    font = list(size = 16))
    ) |> 
    config(displayModeBar = FALSE)
}


# Design feature plots --------------------------------

plot_percent_reporting <- function(data){
  d <- data |> 
    pivot_longer(cols = c(N_items_reported, N_conditions_reported), 
                 names_to = "variable", values_to = "value") |>
    count(variable, value) |> 
    filter(value != "") |> 
    group_by(variable) |> 
    mutate(perc = round(100*n/sum(n),1)) |> 
    ungroup() |> 
    filter(value == "Yes") |> 
    mutate(
      color = "a",
      variable = str_replace(variable, "N_items_reported", "Items"),
      variable = str_replace(variable, "N_conditions_reported", "Conditions"),
      labels = paste0(perc,"%")
    )

  make_perc_bar_plot(d, y = "variable", xmax = 100)
}


## Items ----

plot_rating_levels <- function(data){
  d <- data |> 
    filter(response_scale %in% c("Ordinal (Likert)")) |> 
    mutate(response_levels = as.numeric(response_levels)) |> 
    count(response_levels)

  y_vals <- d$n
  
  ymax <- max(y_vals) + 10
  
  plot_ly(
    data = d,
    x = ~response_levels,
    y = y_vals,
    # color = ~color,
    text = ~n,
    customdata = ~n,
    type = "bar",
    orientation = "v",
    textposition = 'outside',
    hovertemplate = "N = %{customdata}<extra></extra>",
    color = I("steelblue4"),
    showlegend = FALSE
  ) |>
    # add_text(
    #   x = rep(xmax, nrow(data)),
    #   y = y_vals,
    #   text = ~labels,
    #   textposition = "middle left",
    #   showlegend = FALSE,
    #   inherit = FALSE,
    #   textfont = list(color = "white")
    # ) |>
    layout(
      font = font,
      xaxis = list(
        title = "Number of values on response scale", 
        tickfont = list(size = 14), 
        ticksuffix = "",
        tickvals = list(2,3,4,5,6,7,8,9,10,11)
        ),
      yaxis = list(
        title = "# of studies",
        showgrid = TRUE,
        showline = TRUE,
        range = c(0, ymax),
        zeroline = FALSE,
        ticks = "outside",
        tickfont = list(size = 14),
        ticksuffix = ""
      ),
      uniformtext = list(minsize = 16, mode = "show"),
      showlegend = FALSE
    ) |>
    config(displayModeBar = FALSE)
  
}

plot_rating_levels_gg <- function(data, base_size = 14){
  d <- data |> 
    filter(response_scale %in% c("Ordinal (Likert)")) |> 
    mutate(response_levels = as.numeric(response_levels)) |> 
    count(response_levels)
  
  y_vals <- d$n
  
  ymax <- max(y_vals) + 10
  
  ggplot(d, aes(x = response_levels, y = n)) +
  geom_col(width = 0.7, fill = "steelblue") +
  geom_text(
    aes(label = n),
    hjust = 0.5,
    color = "steelblue",
    nudge_y = 4,
    size = base_size / .pt   # match textfont size ~16
  ) +
  scale_y_continuous(
    name = "Number of studies",
    limits = c(0, ymax),
    expand = expansion(mult = c(0, 0.02)),
  ) +
  scale_x_continuous(
    name = "Number of values on response scale",
    breaks = seq(min(d$response_levels), max(d$response_levels)),
    expand = expansion(mult = c(0.02, 0.02)),
  ) +
  theme_minimal(base_size = base_size) +
  theme(
    text = element_text(family = "Roboto"),
    axis.text.y = element_text(size = base_size),
    axis.text.x = element_text(size = base_size),
    axis.title.x = element_text(size = base_size),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.y = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black")
  )
}

# Analysis --------------------------------------------


# Reproducibility -------------------------------------

plot_item_avail_by_year <- function(data){
  data |> 
    mutate(
      year = as.numeric(year),
      item_avail = ifelse(item_avail == "Not Reported", "No", "Yes")
      ) |> 
    count(year, item_avail) |> 
    pivot_wider(names_from = item_avail, values_from = n) |> 
    clean_names() |>
    mutate(
      across(2:3, ~ifelse(is.na(.x), 0, .x))
    ) |>
    plot_ly(
      x = ~year, y = ~yes,
      type = "bar",
      # orientation = "h",
      hovertemplate = "%{hovertext}<extra></extra>",
      hovertext = ~paste(yes, "studies"),
      name = "Items Provided",
      color = I("steelblue4")
    ) |> 
    add_trace(
      x = ~year, y = ~no,
      hovertext = ~paste(no, "studies"),
      # orientation = "h",
      name = "Items Not Provided",
      color = I("grey80")
    ) |> 
    layout(
      barmode = 'stack', 
      font = font,
      xaxis = list(
        title = '# of total studies', 
        range = list(1993, 2024), 
        tickfont = list(size = 16)
      ),
      yaxis = list(
        title = '', 
        showgrid = F, 
        showline = T, 
        tickfont = list(size = 14),
        zeroline = FALSE, 
        ticks="outside"
      ),
      legend = list(x = 0.1, y = 0.9, font = list(size = 16),
                    title = list(text= "Full item list"))
    ) |>
    plotly::config(displayModeBar = F)
}

plot_data_avail_by_year <- function(data){
  data |> 
    mutate(
      year = as.numeric(year),
      data_avail = ifelse(data_avail == "Not Reported", "No", "Yes")
    ) |> 
    count(year, data_avail) |> 
    pivot_wider(names_from = data_avail, values_from = n) |> 
    clean_names() |>
    mutate(
      across(2:3, ~ifelse(is.na(.x), 0, .x))
    ) |>
    plot_ly(
      x = ~year, y = ~yes,
      type = "bar",
      # orientation = "h",
      hovertemplate = "%{hovertext}<extra></extra>",
      hovertext = ~paste(yes, "studies"),
      name = "Provided",
      color = I("orchid4")
    ) |> 
    add_trace(
      x = ~year, y = ~no,
      hovertext = ~paste(no, "studies"),
      # orientation = "h",
      name = "Not Provided",
      color = I("grey80")
    ) |> 
    layout(
      barmode = 'stack', 
      font = font,
      xaxis = list(
        title = '# of total studies', 
        range = list(1993, 2024), 
        tickfont = list(size = 16)
      ),
      yaxis = list(
        title = '', 
        showgrid = F, 
        showline = T, 
        tickfont = list(size = 14),
        zeroline = FALSE, 
        ticks="outside"
      ),
      legend = list(x = 0.1, y = 0.9, font = list(size = 16),
                    title = list(text= "Full data and/or code"))
    ) |>
    plotly::config(displayModeBar = F)
}
