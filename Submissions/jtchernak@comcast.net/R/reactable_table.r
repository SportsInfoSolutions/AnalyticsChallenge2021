library(crosstalk)
library(readr)
library(dplyr)
library(tidyverse)
library(janitor)
library(purrr)
library(ggplot2)
library(scales)
library(reactable)
library(tidyr)
library(reactable)
library(shiny)
library(crosstalk)
library(shinythemes)
library(bslib)
library(gt)
library(rsconnect)

BuYlRd <- function(x) rgb(colorRamp(c("#7fb7d7", "#ffffbf", "#fc8d59"))(x), maxColorValue = 255)

bscols(
  widths = c(3, 9),
  list(
    filter_checkbox("Coverage Scheme", "Coverage Scheme", data, ~`Coverage Scheme`),
    filter_slider("Occurances", "Min. Attempts", data, ~`Occurances`, width = "100%")
  ),
  reactable(data,
            resizable = TRUE, wrap = TRUE, showSortIcon = FALSE, highlight = TRUE,
            # ALL one page option (no scrolling or page swapping)
            pagination = TRUE,
            striped = TRUE, # add stripes
            # compact for an overall smaller table width wise
            compact = TRUE,
            # borderless - TRUE or FALSE
            borderless = FALSE,
            # fullWidth - either fit to width or not
            fullWidth = FALSE,
            filterable = TRUE,
            columns = list(
              `Route Combo`     = colDef(align = "left", minWidth = 230),
              `Coverage Scheme` = colDef(name = "Coverage", align = "left", minWidth = 150),
              `Completion Percentage Estimate` =   colDef(name = "Comp %", minWidth = 100, align = "center" ,  format = colFormat(percent = T , digits = 1),
                                                          style = function(value) {
                                                            value
                                                            normalized <- (value - min(Table_Data$`Completion Percentage Estimate`)) / (max(Table_Data$`Completion Percentage Estimate`) - min(Table_Data$`Completion Percentage Estimate`))
                                                            color <- BuYlRd(normalized)
                                                            list(background = color)
                                                          }),
              `EPA Estimate` = colDef(name = "EPA" , align = "center",  minWidth = 100, format = colFormat(digits = 4),
                                      style = function(value) {
                                        value
                                        normalized <- (value - min(Table_Data$`EPA Estimate`)) / (max(Table_Data$`EPA Estimate`) - min(Table_Data$`EPA Estimate`))
                                        color <- BuYlRd(normalized)
                                        list(background = color)
                                      },
                                      class = "border-left"),
              `EPA` = colDef(align = "center",  minWidth = 100, 
                             class = "cell number border-left",
                             cell = function(value) {
                               if (value == "+")
                                 tagAppendAttributes(shiny::icon("plus"))
                               else
                                 tagAppendAttributes(shiny::icon("minus"))
                             },
                             style = function(value) {
                               if (value == "+") {
                                 color <- "green"
                               } else {
                                 color <- "red"
                               }
                               list(color = color)
                             }),
              `Completion Percentage` = colDef(align = "center", minWidth = 100, name = "Comp %",
                                               cell = function(value) {
                                                 if (value == "+")
                                                   tagAppendAttributes(shiny::icon("plus"))
                                                 else
                                                   tagAppendAttributes(shiny::icon("minus"))
                                               },
                                               style = function(value) {
                                                 if (value == "+") {
                                                   color <- "green"
                                                 } else {
                                                   color <- "red"
                                                 }
                                                 list(color = color)
                                               }),
              `Ranking` = colDef(align = "left", minWidth = 50, name = "Rank" ),
              `Occurances` = colDef(align = "center", minWidth = 150, name = "Attempts" )),
            columnGroups = list(
              colGroup(name = "Estimates", columns = c("EPA Estimate", "Completion Percentage Estimate")),
              colGroup(name = "Overall Effectiveness", columns = c("EPA", "Completion Percentage"))
            ))
)
