#' Water balance plot
#' @description
#' # HBV Results
#' Water balance of HBV in time series with a) Flow and b) water state.
#' @param df_Model_Results (tibble / data.frame) water balance variable from HBV models
#' @importFrom reshape2 melt
#' @import patchwork tidyverse
#' @export
plot_water_balance.HBV <- function(df_Model_Results, if_Perc = FALSE) {
  if (!is.Date(df_Model_Results$Date)) df_Model_Results$Date <- as_date(df_Model_Results$Date |> as.character(), format = "%Y%m%d")

  melt_flow <- melt(df_Model_Results[,c("AET", "Q0", "Q1", "Q2", "Date")], id.vars = "Date")
  melt_water <- melt(df_Model_Results[,c("SUZ", "SLZ", "Date")], id.vars = "Date")
  sum_erb <- colSums(df_Model_Results[,c("AET", "Q0", "Q1", "Q2")])
  mean_sg <- colMeans(df_Model_Results[,c("SUZ", "SLZ")])
  mean_ps <- colMeans(df_Model_Results[,c("Precipitation", "Qsim")])
  max_ps <- apply(df_Model_Results[,c("Precipitation", "Qsim")], 2, max)
  # plot flow ---------
  if (if_Perc) {
    gp_10 <- ggplot() +
      geom_col(aes(Date, Precipitation, color = "Precipitation"), data = df_Model_Results, alpha = .5, fill = "cyan")
  } else {
    gp_10 <- ggplot() +
      geom_col(aes(Date, 0, color = "Precipitation"), data = df_Model_Results, alpha = .5, fill = "cyan")

  }
  gp_1 <- gp_10 +
    geom_area(aes(Date, value, fill = variable), data = melt_flow) +
    geom_line(aes(Date, Qsim, color = "Qsim"), data = df_Model_Results, size = 1.5) +
    scale_color_manual("", labels = c(paste0("Precipatation: ", round(mean_ps["Precipitation"], 2), " (mean) ", round(max_ps["Precipitation"], 2), " (max)", " mm/TS"),
                                      paste0("Streamflow: ", round(mean_ps["Qsim"], 2), " (mean) ", round(max_ps["Qsim"], 2), " (max)", " mm/TS")), values = c("cyan", "#00305d")) +
    scale_fill_manual("", labels = c(paste0("Actual ET: ", round(sum_erb["AET"] / sum(sum_erb) * 100, 1), "%"),
                                     paste0("Runoff: ", round(sum_erb["Q0"] / sum(sum_erb) * 100, 1), "%"),
                                     paste0("Subflow: ", round(sum_erb["Q1"] / sum(sum_erb) * 100, 1), "%"),
                                     paste0("Baseflow: ", round(sum_erb["Q2"] / sum(sum_erb) * 100, 1), "%")), values = c("palegreen3", "lightskyblue", "#006ab2", "#00a1d9")) +
    labs(y = "Flow (mm / TS)") +
    scale_x_date(expand = c(0,0))+
    scale_y_continuous(expand = expansion(mult = c(0, .1)))+
    # coord_cartesian(expand = F) +
    theme(legend.position = "top",
          legend.margin = margin(0, 0, 0, 0),
          legend.box="vertical") +
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 2))

  # plot water ------
  gp_2 <- ggplot(melt_water) +
    geom_area(aes(Date, value, fill = variable, group = variable), color = "#00305d", size = 1, alpha = .8) +
    facet_grid(vars(variable), scales = "free") +
    scale_fill_manual("", labels = c(paste0("Soilwater: ", mean_sg["SUZ"] |> round(2), " mm"),
                                     paste0("Groundwater: ", mean_sg["SLZ"] |> round(2), " mm")),
                      values = c("tan1", "tan4")) +
    labs(y = "Water (mm)") +
    scale_x_date(expand = c(0,0))+
    # scale_y_continuous()+
    # coord_cartesian(expand = F)+
    theme(legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_blank(),
          legend.margin = margin(0, 0, 0, 0))

  layout <- "
AA
BC
"
  guide_area() +  gp_1 + gp_2 +
    plot_layout(ncol = 2, guides = "collect", design = layout, heights = c(1,4))
}



