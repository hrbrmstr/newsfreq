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
#'     Search dates must not be in the future and must also not be on the current day (they won't be in the API database). \cr
#'     They can be an atomic character vector in \code{mm/dd/YYYY} format or anything that can be coerced to a \code{Date} class.
#' @return \code{data.frame} of search results. If \code{summarize} is "\code{monthly}",
#'     then the \code{data.frame} will also include \code{year}, \code{month} and
#'     \code{month_abb}. For "\code{annual}", only \code{year} will be added to the results.
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
#' @export
#' @examples \dontrun{
#' news_search("data breach", date_from="2014-01-01", date_to="2014-12-31")
#' ##     date_from    date_to count search_date search_terms
#' ## 1  2014-01-01 2014-01-31  3963  2015-01-31  data breach
#' ## 2  2014-02-01 2014-02-28  2856  2015-01-31  data breach
#' ## 3  2014-03-01 2014-03-31  2589  2015-01-31  data breach
#' ## 4  2014-04-01 2014-04-30  2170  2015-01-31  data breach
#' ## 5  2014-05-01 2014-05-31  2680  2015-01-31  data breach
#' ## 6  2014-06-01 2014-06-30  1973  2015-01-31  data breach
#' ## 7  2014-07-01 2014-07-31  1962  2015-01-31  data breach
#' ## 8  2014-08-01 2014-08-31  2585  2015-01-31  data breach
#' ## 9  2014-09-01 2014-09-30  3326  2015-01-31  data breach
#' ## 10 2014-10-01 2014-10-31  2862  2015-01-31  data breach
#' ## 11 2014-11-01 2014-11-30  2473  2015-01-31  data breach
#' ## 12 2014-12-01 2014-12-31  2166  2015-01-31  data breach
#' }
news_search <- function(keywords=NA, target="",
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
    dat %>% select(-month_abb, -month)
  } else {
    dat
  }

}

# from Hmisc
ucfirst <- function (string) {
  capped <- grep("^[^A-Z]*$", string, perl = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
  return(string)
}

js_date_parse <- function(x) {
  str_extract(x, "[[:digit:]]+") %>%
    as.numeric %>%
    divide_by(1000) %>%
    as.POSIXct(origin="1970-01-01 00:00:00") %>%
    as.Date()
}