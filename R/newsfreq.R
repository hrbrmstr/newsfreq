#' @title Search newsfreq.com API
#' @description \code{news_search} provides an interface to the \code{newsfreq.com} news keyword search API
#' @details \code{newsfreq.com}'s interface shows you the frequency that given keywords appear in
#'     American News Media and can be used to look at trends in news reporting.
#'     \code{news_search} provides programmatic access to the search function, returning results in an R data frame where
#'     additional analyses can be performed or data can be visualized.\cr
#'     \cr
#'     You can use boolean operators \code{AND} and \code{OR} with parentheses to specify multiple terms in \code{keywords}.\cr
#'     \cr
#'     The \code{target} parameter controls which "field" in news stories are queried. Valid values are:
#'     \itemize{
#'       \item \code{""} Search all article fields (the default)
#'       \item \code{lead} Search in article lead paragraph
#'       \item \code{title} Search in article headlines
#'       \item \code{topics} Search in terms identified as index terms by the article provider
#'       \item \code{author} Search for articles by a particular author
#'       \item \code{section} Search for articles appearing in a particular news section
#'       \item \code{source} Search for articles from a particular news source
#'     }
#'     Search dates must not be in the future and must also not be on the current day (they won't be in the API database).
#'     They can be an atomic character vector in a format \code{as.Date} will recognize or
#'     anything that can be coerced to a \code{Date} class.
#' @param keywords search term(s) to query (see Details for specifics)
#' @param target news article component to search in (see Details for valid values and their meaning)
#' @param date_from start date for the search (<= \code{date_to} and not current day or future date).
#'     Defaults to yesterday. See Details for information on date formatting.
#' @param date_to end date for search (>= \code{date_from} and not current day). Defaults to yesterday.
#'     See Details for information on date formatting.
#' @param source filter search by news source. You can filter your search by news organization.
#'     Entering 'Fox News' or 'The Boston Globe' will search only articles from sources matching that name.
#'     Full list available on \href{http://bit.ly/newsbanksources}{NewsBank}
#' @param summarize Either "\code{monthly}" or "\code{annual}"
#' @return \code{data.frame} (with additional class of \code{newsfreq}) of search
#'     results. If \code{summarize} is "\code{monthly}",
#'     then the results will also include \code{year}, \code{month} and
#'     \code{month_abb}. For "\code{annual}", only \code{year} will be added to
#'     the results.
#' @note The "\code{percent}" value is a per-annum calculation and will only be calculated
#'     and added to the results of searches that are summarized \code{monthly} and span full years.
#'     It is at best a crude way to normalize the results since the number of news sources
#'     (and, hence, articles) changes over time.
#' @export
#' @examples \dontrun{
#' news_search("data breach", date_from="2014-01-01", date_to="2014-12-31")
#' ##     date_from    date_to year month_abb month count search_date search_terms        pct
#' ## 1  2014-01-01 2014-01-31 2014       Jan    01  3963  2015-02-02  data breach 0.12539155
#' ## 2  2014-02-01 2014-02-28 2014       Feb    02  2856  2015-02-02  data breach 0.09036545
#' ## 3  2014-03-01 2014-03-31 2014       Mar    03  2589  2015-02-02  data breach 0.08191742
#' ## 4  2014-04-01 2014-04-30 2014       Apr    04  2170  2015-02-02  data breach 0.06866002
#' ## 5  2014-05-01 2014-05-31 2014       May    05  2680  2015-02-02  data breach 0.08479671
#' ## 6  2014-06-01 2014-06-30 2014       Jun    06  1973  2015-02-02  data breach 0.06242683
#' ## 7  2014-07-01 2014-07-31 2014       Jul    07  1962  2015-02-02  data breach 0.06207879
#' ## 8  2014-08-01 2014-08-31 2014       Aug    08  2585  2015-02-02  data breach 0.08179086
#' ## 9  2014-09-01 2014-09-30 2014       Sep    09  3326  2015-02-02  data breach 0.10523651
#' ## 10 2014-10-01 2014-10-31 2014       Oct    10  2862  2015-02-02  data breach 0.09055529
#' ## 11 2014-11-01 2014-11-30 2014       Nov    11  2473  2015-02-02  data breach 0.07824711
#' ## 12 2014-12-01 2014-12-31 2014       Dec    12  2166  2015-02-02  data breach 0.06853346
#' }
news_search <- newsfreq <- function(keywords=NA, target="",
                        date_from=(Sys.Date()-1), date_to=(Sys.Date()-1),
                        source="",
                        summarize="monthly") {

  if (is.na(keywords) | length(keywords)==0) stop("Must specify keywords to search for")

  date_from <- try(as.Date(date_from), silent=TRUE)
  date_to <- try(as.Date(date_to), silent=TRUE)

  if (class(date_from) == "try-error" | class(date_to) == "try-error") stop("Must use valid dates")
  if (date_to < date_from) stop("date_to must be >= date_from")
  if (date_to >= Sys.Date()) stop("Cannot search in the future or current day")

  target <- tolower(target)
  if (!target %in% c("", "lead", "topics", "author", "section", "source")) stop("invalid search target specified")

  summarize <- tolower(summarize)
  if (!summarize %in% c("monthly", "annual")) stop("summarize must be one of 'monthly' or 'annual'")

  DateFrom <- format(date_from, "%m/%d/%Y")
  DateTo <- format(date_to, "%m/%d/%Y")
  DateString <- sprintf("%s to %s", date_from, date_to)
  SearchString <- keywords
  SearchTarget  <- ucfirst(target)
  SearchSource <- source
  SearchType <- ucfirst(summarize)

  url <- "http://www.newsfreq.com/DataNervesApi/api/NewsLibrary"
  ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.17 Safari/537.36 R/3.0.0"

  resp <- POST(url,
               encode="form",
               user_agent(ua),
               add_headers(Referer="http://www.newsfreq.com/",
                           `X-Requested-With`="XMLHttpRequest",
                           Accept="application/json, text/javascript, */*; q=0.01"),
               body=list(`query[DateFrom]`=DateFrom,
                         `query[DateTo]`=DateTo,
                         `query[DateString]`=DateString,
                         `query[SearchString]`=SearchString,
                         `query[SearchTarget]`=SearchTarget,
                         `query[SearchSource]`=SearchSource,
                         searchType=SearchType))

  ct <- new_context()
  dat <- fromJSON(ct$call("JSON.parse", content(resp, as="text")))

  dat %>%
    mutate(DateFrom=js_date_parse(DateFrom),
           DateTo=js_date_parse(DateTo),
           CreatedDate=js_date_parse(CreatedDate),
           year=format(DateFrom, "%Y"),
           month=format(DateFrom, "%m"),
           month_abb=format(DateFrom, "%b")) %>%
    select(date_from=DateFrom,
           date_to=DateTo,
           year,
           month_abb,
           month, count=Count,
           search_date=CreatedDate,
           search_terms=SearchString) -> dat

  if (summarize == "annual") {

    dat %>% select(-month_abb, -month) -> dat

  } else {

    mods <- dat %>% group_by(year) %>% summarize(tot=n() %% 12)

    if (all(mods$tot == 0)) {
      dat %>%
        group_by(year) %>%
        mutate(tot=sum(count),
               pct=count/tot) %>%
        ungroup %>%
        select(-tot) -> dat
    }

  }

  class(dat) <- c("newsfreq", "data.frame")

  dat

}

#' @rdname news_search
#' @export
is.newsfreq <- function(x) inherits(x, "newsfreq")

#' @title Autoplot for \code{newsfreq} objects
#' @description Quick method with sane defaults for generating a \code{ggplot}
#'        plot for \code{news_search} results.
#' @param data a \code{newsfreq} object
#' @param breaks A character string, containing one of "\code{day}", "\code{week}",
#'        "\code{month}", "\code{quarter}" or "\code{year}". This can optionally
#'        be preceded by a (positive or negative) integer and a space, or followed
#'        by "\code{s}". Passed to \code{scale_x_date}. Defaults to "\code{year}".
#' @param label_format passed to \code{scale_x_date}. Defaults to "\code{\%Y}"
#' @param value either "\code{count}" for raw article counts or "\code{percent}"
#'        Defaults to "\code{count}".
#' @param add_point add points to the lines? Default "\code{FALSE}" (do not plot points)
#' @return \code{ggplot} object
#' @export
autoplot.newsfreq <- function(data, breaks="year", label_format="%Y",
                              value="count", add_point=FALSE) {

  if (!inherits(dat, "newsfreq")) {
    stop("'data' must be a 'newsfreq' object")
  }

  if (!value %in% c("count", "percent")) {
    stop("'value' must be either 'count' or 'percent'")
  }

  if (value == "percent" & !"pct" %in% names(data)) {
    value <- "count"
    message("'pct' column not found, using 'count'")
  }

  y_lab <- "# Articles"

  if (value == "count") {
    gg <- ggplot(data, aes(x=date_from, y=count))
  } else {
    gg <- ggplot(data, aes(x=date_from, y=pct))
    y_lab <- "% Articles"
  }

  gg <- gg + geom_line()

  if (add_point) gg <- gg + geom_point(size=1.5)

  gg <- gg + scale_x_date(breaks=date_breaks(breaks),
                          labels=date_format(label_format), expand=c(0,0))

  if (value == "count") {
    gg <- gg + scale_y_continuous(label=comma)
    gg <- gg + labs(x=NULL, y="# Articles")
  } else {
    gg <- gg + scale_y_continuous(label=percent)
    gg <- gg + labs(x=NULL, y="% Articles")
  }

  gg <- gg + ggtitle(sprintf(unique(data$search_terms)))

  gg <- gg + theme_bw()
  gg <- gg + theme(panel.grid=element_blank())
  gg <- gg + theme(panel.border=element_blank())
  gg

}

.onAttach <- function(...) {
  if (!interactive()) return()
  packageStartupMessage("Data provided by DataNerves; Quantitative data sourced from NewsLibrary.com for use for educational, scholarly and news reporting purposes only")
}

# from Hmisc
ucfirst <- function (string) {
  capped <- grep("^[^A-Z]*$", string, perl = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
  return(string)
}

# helper for fugly "Date(#)" components
js_date_parse <- function(x) {
  str_extract(x, "[[:digit:]]+") %>%
    as.numeric %>%
    divide_by(1000) %>%
    as.POSIXct(origin="1970-01-01 00:00:00") %>%
    as.Date()
}