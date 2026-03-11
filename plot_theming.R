library(extrafont)

colour_palette <- c("#b07c9e","#8cb59f","#fabe8c","#a3a380","#6c7a96","#d4a259","#9c89b8","#c97c5d","#5a7d7c","#e7d7c1")
continuous_palette_discrete <- c("#FABE8C", "#D5BB92", "#B1B899", "#8CB59F")


theme_personal <- function(base_size = 10, base_family = "Garamond") {
    theme_classic(base_size = base_size, base_family = base_family) +
        theme(
            axis.line         = element_line(color = "white", linewidth = 0.8),
            axis.ticks        = element_line(color = "white", linewidth = 0.8),
            axis.ticks.length = unit(-0.2, "cm"),
            plot.title        = element_text(face = "bold", hjust = 0.5, color="white"),
            axis.text         = element_text(face = "plain", color="white"),
            legend.title      = element_text(face = "bold", color="white"),
            strip.background  = element_blank(),
            strip.text        = element_text(face = "bold", color="white"),
            panel.background = element_rect(fill="black"),
            plot.background = element_rect(fill="black"),
            panel.grid = element_line(color="white"),
            axis.title = element_text(face = "bold", color="white")
        )
}


theme_personal_online <- function(base_size = 10, base_family = "Garamond") {
    theme_classic(base_size = base_size, base_family = base_family) +
        theme(
            axis.line         = element_line(color = "black", linewidth = 0.8),
            axis.ticks        = element_line(color = "black", linewidth = 0.8),
            axis.ticks.length = unit(-0.2, "cm"),
            plot.title        = element_text(face = "bold", hjust = 0.5),
            axis.title.x      = element_text(face = "bold", hjust = 0.5),
            axis.title.y      = element_text(face = "bold", hjust = 0.5),
            axis.text         = element_text(face = "plain"),
            legend.title      = element_text(face = "bold"),
            strip.background  = element_blank(),
            strip.text        = element_text(face = "bold")
        )
}