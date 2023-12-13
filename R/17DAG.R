library(dagitty)
library(ggdag)
library(ggplot2)
library(grid)

dag_string <- "
dag {
  \"14-day mortality\" [pos=\"1.260,1.262\"]
  \"7-day pre-AKI SCr measurements\" [pos=\"-1.692,0.065\"]
  \"AKI progression\" [outcome,pos=\"0.921,1.307\"]
  \"AKI to Randomization Time\" [adjusted,pos=\"-0.240,0.607\"]
  \"Absolute SCrΔ\" [exposure,pos=\"-1.654,1.295\"]
  \"Baseline Creatinine\" [adjusted,pos=\"-1.277,0.665\"]
  \"Elixhauser Comorbidity Index\" [adjusted,pos=\"0.883,0.061\"]
  Age [adjusted,pos=\"-0.283,0.053\"]
  \"7-day pre-AKI SCr measurements\" -> \"Absolute SCrΔ\"
  \"7-day pre-AKI SCr measurements\" -> \"Baseline Creatinine\"
  \"AKI progression\" -> \"14-day mortality\"
  \"AKI to Randomization Time\" -> \"AKI progression\"
  \"AKI to Randomization Time\" -> \"Absolute SCrΔ\"
  \"Absolute SCrΔ\" -> \"AKI progression\"
  \"Baseline Creatinine\" -> \"14-day mortality\"
  \"Baseline Creatinine\" -> \"Absolute SCrΔ\"
  \"Elixhauser Comorbidity Index\" -> \"14-day mortality\"
  \"Elixhauser Comorbidity Index\" -> \"AKI to Randomization Time\"
  Age -> \"14-day mortality\"
  Age -> \"7-day pre-AKI SCr measurements\"
  Age -> \"AKI progression\"
  Age -> \"Baseline Creatinine\"
  Age -> \"Elixhauser Comorbidity Index\"
}
"

dag_model <- dagitty(dag_string)

plot(dag_model)
