library(tidyverse)
library(ggplot2)

csvs <- system("ls Results/csv/*_edges.csv", intern = TRUE)

cityNames <- sapply(csvs, function(x) {
  gsub("_edges.csv", "",  gsub("Results/csv/", "", x))
}  )
names(cityNames) <- NULL

Cities <- lapply(csvs, read_csv)
names(Cities) <- cityNames

##### Prepping for regression

for(i in 1:length(Cities)){
  Fhat <- ecdf(Cities[[i]]$`Cost of return`)
  Cities[[i]]$city <- names(Cities)[i]
  Cities[[i]]$p_CoR <- Fhat(Cities[[i]]$`Cost of return`)
}

commonColumns <- Reduce(intersect, lapply(Cities, names))
forPlotting <- do.call(rbind, lapply(Cities, function(city){
  city[, commonColumns]
})
)

forPlotting <- forPlotting %>% mutate(CoR = `Cost of return` )

head(forPlotting)
names(forPlotting)


#' Duplicates data to create additional facet
#' @param df a dataframe
#' @param col the name of facet column
#'  
CreateAllFacet <- function(df, col){
  ## Taken (and modified) from https://stackoverflow.com/questions/18933575/easily-add-an-all-facet-to-facet-wrap-in-ggplot2
  df$facet <- df[[col]]
  temp <- df
  temp$facet <- "All"
  merged <- rbind(temp, df)
  
  # ensure the facet value is a factor
  merged[[col]] <- as.factor(merged[[col]])
  
  ## Putting 'All' last
  rawNames <- sort(unique(merged$facet))
  pos <- which(rawNames == "All")
  merged$facet <- factor(merged$facet,
                         levels = c(rawNames[-pos], rawNames[pos]))
  
  return(merged)
}

ddf <- CreateAllFacet(forPlotting, "city")


CoR.plot <- ggplot(data = ddf,
       aes(x = Groups,
           y = `Cost of return`,
           colour = Groups,
           fill = Groups)) +
  geom_boxplot(alpha = 0.5) +
  scale_x_discrete("") +
  facet_wrap(facet~., scales = "free_y") + 
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

ggsave(CoR.plot,
       file = "CoR_cities.pdf",
       scale = 1,
       width = 297,
       height = 210,
       units = "mm",
       dpi = 300)

CoR_prob.plot <- ggplot(data = ddf,
       aes(x = Groups,
           y = p_CoR,
           colour = Groups,
           fill = Groups)) +
  geom_boxplot(alpha = 0.5) +
  scale_x_discrete("") +
  facet_wrap(facet~.) + 
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

ggsave(CoR_prob.plot,
       file = "CoR_probabilities_cities.pdf",
       scale = 1,
       width = 297,
       height = 210,
       units = "mm",
       dpi = 300)
