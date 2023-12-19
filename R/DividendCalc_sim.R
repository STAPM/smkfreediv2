#' Simulate Smoke-free Dividend Calculation
#'
#' Calculate the smoke-free dividend for each local authority, using simulation
#' methods to incorporate parameter uncertainty in weekly expenditure and smoking prevalence
#'
#' @param data_spend Data table. Weekly spending by local authority from the cleaned toolkit data.
#' @param data_prevalence Data table. Smoking prevalence by local authority from OHID tobacco profiles.
#' @param upshift Numeric. Parameter to upshift expenditures calculated from the toolkit data.
#' @param div Numeric. Proportion of licit tobacco expenditure which is smoke-free dividend.
#' @param n_sim Numeric. Number of simulations.
#' @param seed Numeric. Random number seed.
#' @param illicit_prop Numeric. The proportion of total tobacco expenditure which is illicit.
#'
#' @return Smoke-free dividend calculations with simulated uncertainty
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
DividendCalc_sim <- function(data_spend,
                             data_prevalence,
                             upshift = 1.417,
                             div = 0.93,
                             illicit_prop = 1298 / (14307 + 1298),
                             n_sim = 100,
                             seed = 2021) {

  set.seed(seed)

  ######## MERGE PREVALENCE AND SPENDING DATA

  merge <- merge(data_prevalence, data_spend, by = "UTLA22CD", all.x = TRUE)

  ######## PROBABILISTIC CALCULATION

  ## Initialise matrices for probabilistic variables

  m_n_smokers           <- matrix(rep(NA,n_sim*152), ncol = n_sim)
  m_smk_prev            <- matrix(rep(NA,n_sim*152), ncol = n_sim)
  m_mean_week_spend     <- matrix(rep(NA,n_sim*152), ncol = n_sim)
  m_total_annual_exp    <- matrix(rep(NA,n_sim*152), ncol = n_sim)
  m_dividend            <- matrix(rep(NA,n_sim*152), ncol = n_sim)
  m_dividend_pp         <- matrix(rep(NA,n_sim*152), ncol = n_sim)
  m_dividend_ps         <- matrix(rep(NA,n_sim*152), ncol = n_sim)

  for (i in 1:n_sim) {

    cat("\t\tMonte-Carlo simulation progress...", round(100*i/n_sim,2),"%", "               \r")
    utils::flush.console()
    if(i == n_sim) { cat("\n") }

    ######## Calculate the smoke-free dividend #############

    ### generate probabilistic variables

    suppressWarnings(
      sim_data <- merge %>%
        arrange(UTLA22CD,UTLA22NM) %>%
        group_by(UTLA22CD,UTLA22NM) %>%
        mutate(smk_prev_se           = (smk_prev_uci - smk_prev)/1.96,
               prob_smk_prev         = rnorm(1, mean = smk_prev, sd = smk_prev_se),
               prob_n_smokers        = prob_smk_prev * pop,
               prob_mean_week_spend  = rnorm(1, mean = mean_week_spend, sd = se_week_spend),
               prob_total_annual_exp = (prob_n_smokers * prob_mean_week_spend * 52)/1000000,
               prob_dividend         = (prob_total_annual_exp * illicit_prop) + (prob_total_annual_exp * (1 - illicit_prop))*div,
               prob_dividend_pp      = prob_dividend / pop,
               prob_dividend_ps      = prob_dividend / n_smokers) %>%
        ungroup()
    )
    ######## save out probabilistic results ################

    m_n_smokers[,i]           <- as.vector(as.matrix(sim_data[,"prob_n_smokers"]))
    m_smk_prev[,i]            <- as.vector(as.matrix(sim_data[,"prob_smk_prev"]))
    m_mean_week_spend[,i]     <- as.vector(as.matrix(sim_data[,"prob_mean_week_spend"]))
    m_total_annual_exp[,i]    <- as.vector(as.matrix(sim_data[,"prob_total_annual_exp"]))
    m_dividend[,i]            <- as.vector(as.matrix(sim_data[,"prob_dividend"]))
    m_dividend_pp[,i]         <- as.vector(as.matrix(sim_data[,"prob_dividend_pp"]))
    m_dividend_ps[,i]         <- as.vector(as.matrix(sim_data[,"prob_dividend_ps"]))

  } ## end loop over simulation iterations

  ##################################################################
  #### Get means and standard deviations of simulated variables ####

  ### ------------- (1) Number of Smokers -----------------------------###
  #cat(crayon::cyan("\t\tNumber of Smokers"))

  m <- data.table(m_n_smokers)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_n_smokers   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_n_smokers, c("M","SD"), c("n_smokers_m","n_smokers_sd"))

  #cat("\t\tdone\n")
  ### -----------------------------------------------------------------###
  ### -------------- (2) Smoking Prevalence ---------------------------###
  #cat(crayon::cyan("\t\tSmoking Prevalence"))

  m <- data.table(m_smk_prev)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_smk_prev   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_smk_prev, c("M","SD"), c("smk_prev_m","smk_prev_sd"))

  #cat("\t\tdone\n")
  ### -----------------------------------------------------------------###
  ### ----------- (3) Mean Weekly Spending ----------------------------###
  #cat(crayon::cyan("\t\tMean Weekly Expenditure"))

  m <- data.table(m_mean_week_spend)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_mean_week_spend   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_mean_week_spend, c("M","SD"), c("mean_week_spend_m","mean_week_spend_sd"))

  #cat("\t\tdone\n")
  ### -----------------------------------------------------------------###
  ### ----------- (4) Total annual spending ---------------------------###
 # cat(crayon::cyan("\t\tTotal Annual Expenditure"))

  m <- data.table(m_total_annual_exp)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_total_annual_exp   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_total_annual_exp, c("M","SD"), c("total_annual_exp_m","total_annual_exp_sd"))

 # cat("\t\tdone\n")
  ### -----------------------------------------------------------------###
  ### ----------- (5) Smoke-free dividend -----------------------------###
  #cat(crayon::cyan("\t\tSmokefree Dividend"))

  m <- data.table(m_dividend)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_dividend   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_dividend, c("M","SD"), c("dividend_m","dividend_sd"))

  #cat("\t\tdone\n")
  ### -----------------------------------------------------------------###
  ### ----------- (6) Smoke-free dividend per person ------------------###
  #cat(crayon::cyan("\t\tSmokefree Dividend"))

  m <- data.table(m_dividend_pp)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_dividend_pp   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_dividend, c("M","SD"), c("dividend_pp_m","dividend_pp_sd"))

  #cat("\t\tdone\n")
  ### -----------------------------------------------------------------###
  ### ----------- (7) Smoke-free dividend per smoker ------------------###
  #cat(crayon::cyan("\t\tSmokefree Dividend"))

  m <- data.table(m_dividend_ps)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_dividend_ps   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_dividend, c("M","SD"), c("dividend_ps_m","dividend_ps_sd"))

  #cat("\t\tdone\n")
  ### -----------------------------------------------------------------###

  utla <- merge %>%
    arrange(UTLA22CD,UTLA22NM) %>%
    select(UTLA22CD,UTLA22NM)

  data_out <- cbind(utla, m_n_smokers, m_smk_prev, m_mean_week_spend, m_total_annual_exp,
                    m_dividend, m_dividend_pp, m_dividend_ps)

  return(data_out)
}
