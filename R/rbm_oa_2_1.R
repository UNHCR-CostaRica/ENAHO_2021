library(tidyverse)
library(lubridate)
library(jsonlite)

coa <- "COS" # iso3 code

apps <- fromJSON(glue::glue("https://api.unhcr.org/population/v1/asylum-applications/?limit=100&dataset=asylum-applications&displayType=totals&yearFrom=1951&yearTo=2022&coa={coa}&columns%5B%5D=procedure_type&columns%5B%5D=app_type&columns%5B%5D=app_pc&columns%5B%5D=app_size&columns%5B%5D=dec_level&columns%5B%5D=applied"))$items |> as_tibble()
decs <- fromJSON(glue::glue("https://api.unhcr.org/population/v1/asylum-decisions/?limit=100&dataset=asylum-decisions&displayType=totals&yearFrom=1951&yearTo=2022&coa={coa}&columns%5B%5D=procedure_type&columns%5B%5D=dec_level&columns%5B%5D=dec_pc&columns%5B%5D=dec_recognized&columns%5B%5D=dec_other&columns%5B%5D=dec_rejected&columns%5B%5D=dec_closed&columns%5B%5D=dec_total"))$items |> as_tibble()

data <- 
  left_join(apps |> filter(dec_level == "FI"),
            decs |> filter(dec_level == "FI")) |> 
  arrange(year) |> 
  mutate(across(c(applied, dec_total), replace_na, 0)) |> 
  transmute(year = if_else(year == last(year), year+.5, year+1), # MYSR correction
            apps = cumsum(applied), 
            decs = cumsum(dec_total))

idx <- detect_index(data$apps, ~.<last(data$decs), .dir = "backward")
t <- date_decimal(data$year[idx]+(last(data$decs)-data$apps[idx])/(data$apps[idx+1]-data$apps[idx]))
stat <- interval(t, make_date(2022, 6, 30))/days()

data |> 
  pivot_longer(-year, names_to = "flow", values_to = "n") |> 
  ggplot(aes(year, n, fill = flow)) + 
  geom_area(position = "identity") +
  annotate("segment", x = decimal_date(t), y = last(data$decs), xend = last(data$year), yend = last(data$decs)) +
  annotate("text", x = 2020, y = last(data$decs), vjust = -.5, label = glue::glue("{round(stat)} days")) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_fill_manual(labels = c(decs = "Decisions", apps = "Applications"),
                    values = c(decs = "#0072BC", apps = "#FAEB00"),
                    name = NULL) +
  labs(x = NULL, y = "cumulative total",
       title = "Average processing time (in days) from registration to first instance asylum decision",
       subtitle = glue::glue("{first(apps$coa_name)}, MYSR 2022")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(legend.position = "right")
