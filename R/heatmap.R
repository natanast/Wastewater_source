


rm(list = ls())
gc()


# libaries ------

library(data.table)
library(stringr)

library(ggplot2)
library(paletteer)
library(shadowtext)
library(colorspace)

# load data ------

d0 <- "data/Wastewater Sources Summary.xlsx" |>
    readxl::read_xlsx(sheet = 2) |>
    setDT()


d0$URL <- NULL

# plot -----------------------------------

library(ggside)
library(tidyr)

d2 <- d0$Diseases |> 
    str_split("\\,") |> 
    lapply(str_squish) |> 
    lapply(function(x) data.table("Disease" = x)) |>
    rbindlist(idcol = "id")

d3 <- d0[, by = Country, .(`No. locations` = `No. locations` |> sum(na.rm = TRUE))]


d2$Country <- d0[d2$id]$Country
d2$`No. locations` <- d0[d2$id]$`No. locations`


d2$color  <- ifelse( is.na(d2$`No. locations`), "empty", "value" )



# fills missing ones with 0
df_heatmap <- d2 |>
    complete(Country, Disease, fill = list(`No. locations` = 0))

df_heatmap$location_present  <- ifelse( is.na(df_heatmap$color), "empty2", df_heatmap$color )



gr2 = df_heatmap |>
    ggplot() +
    
    geom_tile(aes(Country, Disease, fill = factor(location_present)), color = "grey20", linewidth = .2) +
    
    geom_xsidecol(data = d3, aes(Country, `No. locations`), fill = "#2c5769") +
    
    scale_fill_manual(
        values = c("empty2" = NA,  "empty" = "#6F99AD", value = "#6F99AD"),
        name = "No. of locations",
        na.value = "transparent"
    ) +


    # scale_fill_stepsn(
    # colors =  c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#a33a3a'),
    #   # breaks = c(1, 10, 100, 1000, 3000),
    #     # transform = "log10",
    #     # labels = scales::comma,
    #     name = "No. of locations",
    #     na.value = "grey96",
    #     guide = guide_colorsteps(
    #         barheight = unit(8, "lines"),
    #         barwidth = unit(0.55, "lines")
    #     )
    # ) +
    
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    
    labs(
      y = "Disease                                            No. of Locations"  
    ) +
    
    theme_minimal() +
    
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        
        axis.title.y = element_text(size = 10, hjust = 1),
        
        legend.position = "none",
        
        # legend.title.position = "left",
        # 
        # legend.title = element_text(size = 8, angle = 90, hjust = .5, face = "bold", family = "Candara", color = "grey30"),
        # legend.text = element_text(size = 8, family = "Candara", color = "grey30"),
        # 
        panel.grid = element_blank(),
        panel.background = element_rect(color = "grey98", fill = NA),
        

        ggside.panel.scale = .5,
        ggside.panel.border = element_blank(),
        ggside.panel.grid = element_line(linewidth = .3, linetype = "dashed", color = "grey85"),
        ggside.panel.background = element_blank(),
        
        plot.margin = margin(20, 20, 20, 20)
    ) 

gr2

ggsave(
    plot = gr2, filename = "02_plot.png",
    width = 10, height = 5, units = "in", dpi = 600
)
