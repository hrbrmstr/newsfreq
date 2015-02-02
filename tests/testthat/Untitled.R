library(newsfreq)
breach <- news_search(keywords="(data breach) OR hacking OR (denial of service)",
                      date_from="2010-01-01", date_to="2014-01-31")


breach
