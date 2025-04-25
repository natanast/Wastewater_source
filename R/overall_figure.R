




# libaries ------

library(data.table)
library(stringr)


# load data ------

d0 <- "data/Wastewater Sources Summary.xlsx" |>
    readxl::read_xlsx(sheet = 2) |>
    setDT()


d0$URL <- NULL

info <- countries::country_info(d0$Country) |> setDT()

d0$region <- info$region

# Plot 1: Packcircles ----------------

library(packcircles)
library(ggplot2)
library(paletteer)
library(shadowtext)
library(colorspace)

d1 <- d0[which(!is.na(`No. locations`)), by = .(region, Country, Status), .(N = `No. locations` |> sum())]


gr1 <- d1 |>
    ggplot(aes(Status, Country)) +
    geom_tile(aes(fill = N), color = "white") +
    scale_fill_stepsn(transform = "log10", colors = c('#00429d', '#73a2c6', '#f4777f', '#93003a')) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    
    facet_grid(rows = vars(region), scales = "free_y", space = "free_y") +
    
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )


d2 <- d0[, by = .(region, Country), .(`Start Date` = min(`Start Date`, na.rm = TRUE), `End Date` = max(`End Date`, na.rm = TRUE))]

d3 <- d0[which(Status == "Active"), by = .(region, Country), .(`Start Date` = min(`Start Date`, na.rm = TRUE))]

gr2 <- d0 |>
    ggplot(aes(y = Country)) +
    geom_segment(aes(x = `Start Date`, y = Country, xend = `End Date`, yend = Country)) +
    geom_segment(data = d3, aes(x = `Start Date`, y = Country, xend = max(d0$`End Date`, na.rm = TRUE), yend = Country), linetype = "dashed") +
    
    geom_point(aes(x = `Start Date`), shape = 21, size = 2, stroke = .25, fill = "white", color = "#00429d") +
    geom_point(aes(x = `End Date`), shape = 21, size = 2, stroke = .25, fill = "white", color = "#93003a") +
    
    geom_point(data = d2, aes(y = Country, x = `Start Date`), shape = 21, size = 2.5, stroke = .25, color = "white", fill = "#00429d") +
    geom_point(data = d2, aes(y = Country, x = `End Date`), shape = 21, size = 2.5, stroke = .25, color = "white", fill = "#93003a")





d4 <- d0$Diseases |> 
    str_split("\\,") |>
    lapply(str_squish) |>
    lapply(function(x) data.table("Disease" = x)) |>
    rbindlist(idcol = "id")


d4$region <- d0[d4$id]$region
d4$Country <- d0[d4$id]$Country
d4$Disease <- ifelse(is.na(d4$Disease), "Not available", d4$Disease)

gr3 <- d4 |>
    ggplot(aes(Disease, Country)) +
    geom_tile(color = "white") +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

library(patchwork)


(gr1 | gr2 | gr3) + plot_layout(widths = c(.5, 3.5, 2))




d1 <- d1[order(-N)] |> split(by = "Status")

p1 <- d1 |>
    lapply(function(x) {
        
        o <- log10(x$N) |>
            circleProgressiveLayout(sizetype = 'area') |>
            circleLayoutVertices(npoints = 100) |>
            setDT()
        
        o <- cbind(x[o$id], o)
        
    }) |>
    rbindlist()



p1t <- p1[, by = .(Status, Country, N), .(x = (min(x) + max(x)) / 2, y = (min(y) + max(y)) / 2)]

p1t$Country <- p1t$Country |> str_wrap(width = 5)

gr1 = p1 |>
    ggplot() + 
    geom_polygon(aes(x, y, group = id, fill = N), colour = "grey10", linewidth = .25) +
    geom_shadowtext(data = p1t, aes(x, y, label = Country, size = N), fontface = "bold", color = "grey10", bg.color = "white", bg.r = .05) +
    
    scale_fill_stepsn(colors = paletteer_c("ggthemes::Blue-Teal", 8), 
                      breaks = c(250, 500, 750, 1000, 1250, 1500, 1750),
                      guide = guide_colorsteps(barwidth = unit(16, "lines"),
                                               barheight = unit(.5, "lines"),
                                               title = "No. of locations")) +
    
    scale_size_continuous(range = c(2, 6), guide = "none") +
    
    facet_wrap(vars(Status), nrow = 1) +
    
    coord_equal() +
    theme_void() + 
    theme(
        legend.position = "bottom",
        legend.title.position = "top"
    )


gr1

ggsave(
    plot = gr1, filename = "01_plot.png",
    width = 6, height = 6, units = "in", dpi = 600
)