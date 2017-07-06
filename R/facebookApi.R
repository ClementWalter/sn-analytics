#' Get Facebook Metricx of a page
#'
#' @param token a token for the Facebook API
#' @param pagename a page name
#'
#' @return a ggplot2 object with data about comments, likes and shares
#'
#' @export
#'
#' @import Rfacebook
#' @import ggplot2
#' @import scales
#' @import magrittr
#' @import dplyr

getFacebookMetrics <- function(token, pagename){
  if(missing(token)){
    token = "EAACEdEose0cBAEysQ5vZCeLX87JERjSIpBxZBUMZCrxFo9sdCeEqPIEtkwyNJc7TlELZA4C4VxYvPZByGsCbCyjLWCSOF06Hk52DVXkfrMNKFujScZAITonmisLWvcs5vZBIwau1yenSzDK83UnjIyV7eBcDf1jtBmZCBL1UMu63xK3tCgYz8FZBSJp57mvM5aZCwZD"
  }
  if(missing(pagename)){
    pagename <- "croixrouge"
  }

  page <- getPage(pagename, token, n = 5000)

  ## convert Facebook date format to R date format
  format.facebook.date <- function(datestring) {
    date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
  }
  ## aggregate metric counts over month
  aggregate.metric <- function(metric) {
    m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month),
                   mean)
    m$month <- as.Date(paste0(m$month, "-15"))
    m$metric <- metric
    return(m)
  }

  # create data frame with average metric counts per month
  page$datetime <- format.facebook.date(page$created_time)
  page$month <- format(page$datetime, "%Y-%m")
  page_metrics <- page %>% select(datetime, likes_count, comments_count, shares_count) %>% gather(metric, value, -datetime) %>%
    mutate(month = as.Date(paste0(format(datetime, "%Y-%m"), "-15"))) %>%
    group_by(metric, month) %>% summarise(Mean = mean(value)) %>%
    ungroup() %>%
    mutate(metric = recode(metric, comments_count = "Comments", likes_count = "Likes", shares_count = "Shares", .default = levels(metric)))

  metric_plot <- page_metrics %>% ggplot(aes(month, Mean)) + geom_smooth(aes(color = metric)) +
    scale_x_date(date_breaks = "years", labels = date_format("%Y")) + scale_y_log10("Average count per post") + xlab("")

  metric_plot$labels$colour <- "Metric"
  metric_plot
}
