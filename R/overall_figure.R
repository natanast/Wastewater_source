


# libaries ------

library(data.table)
library(stringr)

library(ggplot2)
library(ggh4x)
library(shadowtext)

library(paletteer)
library(colorspace)

# load data ------

d0 <- "data/Wastewater Sources Summary.xlsx" |>
    readxl::read_xlsx(sheet = 2) |>
    setDT()


d0$URL <- NULL

info <- countries::country_info(d0$Country) |> setDT()

d0$region <- info$region

# Plot 1 ----------------


d1 <- d0[which(!is.na(`No. locations`)), by = .(region, Country, Status), .(N = `No. locations` |> sum())]


gr1 <- d1 |>
    ggplot(aes(Status, Country)) +
    geom_tile(aes(fill = N), color = "white") +
    scale_fill_stepsn(transform = "log10", colors = c('#00429d', '#73a2c6', '#f4777f', '#93003a')) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +

    facet_grid(rows = vars(region), scales = "free_y", space = "free_y") +

    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        
        strip.text.y = element_text(angle = 0)
    )

# Plot 2 ------------------------------------------

d2 <- d0[, by = .(region, Country), .(`Start Date` = min(`Start Date`, na.rm = TRUE), `End Date` = max(`End Date`, na.rm = TRUE))]

d3 <- d0[which(Status == "Active"), by = .(region, Country), .(`Start Date` = min(`Start Date`, na.rm = TRUE))]

gr2 <- d0 |>
    ggplot(aes(y = Country)) +
    geom_segment(aes(x = `Start Date`, y = Country, xend = `End Date`, yend = Country)) +
    geom_segment(data = d3, aes(x = `Start Date`, y = Country, xend = max(d0$`End Date`, na.rm = TRUE), yend = Country), linetype = "dashed") +

    geom_point(aes(x = `Start Date`), shape = 21, size = 2, stroke = .25, fill = "white", color = "#00429d") +
    geom_point(aes(x = `End Date`), shape = 21, size = 2, stroke = .25, fill = "white", color = "#93003a") +

    geom_point(data = d2, aes(y = Country, x = `Start Date`), shape = 21, size = 2.5, stroke = .25, color = "white", fill = "#00429d") +
    geom_point(data = d2, aes(y = Country, x = `End Date`), shape = 21, size = 2.5, stroke = .25, color = "white", fill = "#93003a") +
    facet_grid(rows = vars(region), scales = "free_y", space = "free_y")



# Plot n ------------------------------------------------

dd <- d0[which(!is.na(`No. locations`)), c("region", "Country", "Status", "Start Date", "End Date", "No. locations"), with = FALSE] |> unique()

dd$`Start Date` <- dd$`Start Date` |> lubridate::as_date()
dd$`End Date`   <- dd$`End Date` |> lubridate::as_date()
dd$`End Date`   <- ifelse(is.na(dd$`End Date`), Sys.Date() |> lubridate::as_date(), dd$`End Date`)
dd$`End Date`   <- dd$`End Date` |> lubridate::as_date()

a <- dd[, c("region", "Country", "Status", "Start Date", "No. locations"), with = FALSE]
b <- dd[, c("region", "Country", "Status", "End Date", "No. locations"), with = FALSE]

colnames(a)[4] <- "Date"
colnames(b)[4] <- "Date"

b$`No. locations` <- ifelse(b$Status == "Defunct", (-1) * b$`No. locations`, 0)


dd <- rbind(a, b) |> unique()
dd <- dd[order(region, Country, Date)]
dd[, by = .(region, Country), N := cumsum(`No. locations`)]


dd <- dd[order(Country, -Date)]

ty <- dd[, by = Country, head(.SD, 1)]
ty <- ty[which(N == 0)]

dd$activity <- ifelse(dd$Country %in% ty$Country, "Defunctional", "Active")

gr_n <- dd |>
    ggplot(aes(Date, N)) +
    geom_area(aes(group = Country, fill = activity), alpha = .25) +
    geom_line(aes(group = Country, color = activity)) +
    geom_point(aes(color = activity), shape = 21, fill = "white", stroke = .25) +
    facet_wrap2(vars(region, Country), nrow = 4) +
    
    scale_y_continuous(
        transform = scales::pseudo_log_trans(base = 10), 
        breaks = c(1, 10, 100, 1000),
        expand = c(0, 0), limits = c(0, 5000)
    ) +
    
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
    )


# Plot 3 ---------------------------------

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
    facet_grid(rows = vars(region), scales = "free_y", space = "free_y") +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y = element_text(angle = 0)
    )


# Patchwork --------------------------------------------

library(patchwork)

multi1 <- (gr_n | gr3) + plot_layout(widths = c(3, 1))

multi2 <- (gr1 | gr2 | gr3) + plot_layout(widths = c(.5, 3.5, 2))

# Save Plots -----------------------------------------


save_plot <- function(gr, w, h, file_name = "Rplot") {
    
    ggsave(
        plot = gr, filename = paste0(file_name, ".png"),
        width = w, height = h, units = "in", dpi = 600
    )
    
    ggsave(
        plot = gr, filename = paste0(file_name, ".pdf"),
        width = w, height = h, units = "in", device = cairo_pdf
    )
    
}


save_plot(gr1, file_name = "plots/01_plot", w = 4, h = 10)
save_plot(gr2, file_name = "plots/02_plot", w = 5, h = 10)
save_plot(gr3, file_name = "plots/03_plot", w = 5, h = 10)
save_plot(multi1, file_name = "plots/01_multi_plot", w = 12, h = 10)
save_plot(multi2, file_name = "plots/02_multi_plot", w = 12, h = 10)



