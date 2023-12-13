#' Hydrological models
#' @name model
#' @description
#' \loadmathjax
#' # Linear Reservoir
#' Linear Reservoir is a method and just assuming that the watershed behaves like a **linear reservoir**, where the outflow is **proportional** to the water storage within the reservoir.
#'
#' \mjsdeqn{Q_{out} = \frac{1}{K}S(t)}
#'
#'
#' In addition to their relationship with output and storage, linear reservoir models also adhere to the **continuity equation**, often referred to as the **water balance equation**.
#'
#'  \mjsdeqn{\frac{\mathrm{d}S(t)}{\mathrm{d}t} = Q_{in} - Q_{out}}
#'
#' By combining both equations, we obtain a differential equation (DGL).
#'
#'  \mjsdeqn{Q_{in} = Q_{out} + K\frac{\mathrm{d}Q_{out}(t)}{\mathrm{d}t}}
#'
#'
#'  \mjsdeqn{Q_{out}(t)=\int_{\tau=t0}^{t}Q_{in}(\tau)\frac{1}{K}e^{-\frac{t-\tau}{K}}\mathrm{d}\tau + Q_{out}(t_0)\frac{1}{K}e^{-\frac{t-t0}{K}}}
#'
#' Where:
#'
#' - \mjseqn{Q_{in}} is the inflow of the reservoir
#' - \mjseqn{Q_{out}} is the outflow of the reservoir
#' - \mjseqn{S} is the storage of the reservoir
#' - \mjseqn{K} is the parameter that defines the relationship between $Q_{out}$ and $S$
#'
#' @param Q_In (vector) Inflow Boundary condition (Forcing) data
#' @param Q_Out0 (num) Innitial condition, Outflow in the first timepoint, DEFAULT in 0
#' @param param_K (num) Parameter K of the linear reservoir method
#' @return Outflow (vector)
#' @export
model_linearReservoir <- function(Q_In, Q_Out0 = 0, param_K = 1) {
  n_Step <- length(Q_In)
  Q_Out <- c(Q_Out0, rep(0, n_Step - 1))

  for (i in 2:n_Step) {
    Q_Out[i] <- Q_Out[i-1] + (Q_In[i-1] - Q_Out[i-1]) / (param_K + 0.5) + (Q_In[i] - Q_In[i-1]) * .5 / (param_K + 0.5)
  }

  Q_Out

}
