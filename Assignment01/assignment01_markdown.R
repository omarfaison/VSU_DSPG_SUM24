

```markdown
---
title: "Cancer Mortality Analysis"
author: "Your Name"
date: "2024-06-01"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r load-packages}
# Load necessary libraries
library(tidyverse)
```

```{r read-data}
# Set working directory if necessary
# setwd("D:/Users/mfaison/code/VSU_DSPG_SUM24/Assignment01")

# Read in the data
data <- read.csv("us_cancer_mortality_county_05-06-2024.csv", header = TRUE)
```

```{r data-wrangling}
# Wrangle the data
proj_data <- data %>%
  filter(State == "Virginia", RE == "All", Sex == "All") %>%
  select(FIPS:County, All.Site, Pancreas) %>%
  mutate(pct_pancreas = Pancreas / All.Site)
  
# Save the processed data if needed
# saveRDS(proj_data, "assignment01_finaldata.RDS")
# proj_data <- readRDS("assignment01_finaldata.RDS")
```

```{r plot-all-site}
# Version 1 of graphing: All.Site
ggplot(proj_data, aes(x = reorder(County, All.Site), y = All.Site)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("County") +
  ylab("All Site Count")
```

```{r plot-all-site-alt}
# Alternative plot for All.Site
ggplot(proj_data, aes(x = reorder(County, All.Site), y = All.Site)) +
  geom_col() +
  coord_flip() +
  xlab("County") +
  ylab("All Site Count")
```

```{r plot-pct-pancreas}
# Plot by pct_pancreas
ggplot(proj_data, aes(x = reorder(County, pct_pancreas), y = pct_pancreas)) +
  geom_col() +
  coord_flip() +
  xlab("County") +
  ylab("% Cancer cases that are pancreatic")
```

```{r plot-pct-pancreas-naomit}
# Plot by pct_pancreas with NA values removed
ggplot(na.omit(proj_data), aes(x = reorder(County, pct_pancreas), y = pct_pancreas)) +
  geom_col() +
  coord_flip() +
  xlab("County") +
  ylab("% Cancer cases that are pancreatic")
```

```{r plot-pct-pancreas-dropna}
# Plot by pct_pancreas using drop_na
ggplot(drop_na(proj_data, pct_pancreas), aes(x = reorder(County, pct_pancreas), y = pct_pancreas)) +
  geom_col() +
  coord_flip() +
  xlab("County") +
  ylab("% Cancer cases that are pancreatic")
```

```{r save-pdf-all-cases, eval=FALSE}
# Save All Site plot to PDF
pdf("all.cases.pdf", width = 8.5, height = 11)
ggplot(proj_data, aes(x = reorder(County, All.Site), y = All.Site)) +
  geom_col() +
  coord_flip() +
  xlab("County") +
  ylab("All Site Count")
dev.off()
```

```{r save-pdf-both-graphs, eval=FALSE}
# Save both graphs to PDF
pdf("both_graphs.pdf", width = 8.5, height = 11)
ggplot(proj_data, aes(x = reorder(County, All.Site), y = All.Site)) +
  geom_col() +
  coord_flip() +
  xlab("County") +
  ylab("All Site Count")

ggplot(drop_na(proj_data, pct_pancreas), aes(x = reorder(County, pct_pancreas), y = pct_pancreas)) +
  geom_col() +
  coord_flip() +
  xlab("County") +
  ylab("% Cancer cases that are pancreatic")
dev.off()
```

```{r save-pdf-pancreas-graph, eval=FALSE}
# Save pancreas graph to PDF
pancreas_graph <- ggplot(drop_na(proj_data, pct_pancreas), aes(x = reorder(County, pct_pancreas), y = pct_pancreas)) +
  geom_col() +
  coord_flip() +
  xlab("County") +
  ylab("% Cancer cases that are pancreatic")

pdf("pancreas_graph.pdf", width = 8.5, height = 11)
pancreas_graph
dev.off()
```

```{r plot-pct-pancreas-point}
# Plot pct_pancreas as points
ggplot(drop_na(proj_data, pct_pancreas), aes(x = reorder(County, pct_pancreas), y = pct_pancreas)) +
  geom_point() +
  coord_flip() +
  xlab("County") +
  ylab("% Cancer cases that are pancreatic")
```
```
