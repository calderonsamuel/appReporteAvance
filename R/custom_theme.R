custom_theme <- function() {
    
    # This mimics what fresh::bs4dash_color() does internally
    theme_colors <- reportes_bs_colors()
    # class(theme_colors) <- c("fresh_sass_vars", "bs4dash_vars", class(theme_colors))
    class(theme_colors) <- c("fresh_sass_vars", "adminlte", class(theme_colors))
            
    fresh::create_theme(
        fresh::bs4dash_layout(
            control_sidebar_width = "350px"
        ),
        theme_colors
    )
}

reportes_bs_colors <- function() {
    allowed_bs4Dash_colors <- list(
        blue = "#007BFF",
        lightblue = "#B9E0F7",
        navy = "#8DA4BD",
        cyan = "#17A2B8",
        teal = "#80B1D3",
        olive = "#C9EB8F",
        green = "#28A745",
        lime = "#CCEBC5",
        orange = "#FFCF99",
        yellow = "#FFC107",
        fuchsia = "#FF9CE9",
        purple = "#D095D1",
        # pink = "#f568a9",
        maroon = "#FB8BB4",
        red = "#DC3545",
        black = "#111111",
        # gray_x_light = "#D2D6DE",
        # gray_600 = "#6C757D",
        # gray_800 = "#343A40",
        # gray_900 = "#212529",
        white = "#FFFFFF"
    )
    
    c(allowed_bs4Dash_colors, reportes_inner_colors()) # combining two lists
}

reportes_compute_colors <- function() {
    
    palettes <- RColorBrewer::brewer.pal.info |> 
        subset(category == "qual") 
    
    ncolors <- palettes$maxcolors
    names <- rownames(palettes)
    
    color_data <- purrr::map2(ncolors, names, ~{
        data.frame(
            color_name = paste0(tolower(.y), "-", seq_len(.x)), 
            color_value = RColorBrewer::brewer.pal(.x, .y)
        )
    }) |> 
        purrr::reduce(rbind)
    
    color_data$color_value |> 
        lapply(\(x) x) |> 
        setNames(color_data$color_name)
}

reportes_inner_colors <- function() {
    # This is the result of the following
    # reportes_compute_colors() |> dput()
    list(`accent-1` = "#7FC97F", `accent-2` = "#BEAED4", `accent-3` = "#FDC086", 
         `accent-4` = "#FFFF99", `accent-5` = "#386CB0", `accent-6` = "#F0027F", 
         `accent-7` = "#BF5B17", `accent-8` = "#666666", `dark2-1` = "#1B9E77", 
         `dark2-2` = "#D95F02", `dark2-3` = "#7570B3", `dark2-4` = "#E7298A", 
         `dark2-5` = "#66A61E", `dark2-6` = "#E6AB02", `dark2-7` = "#A6761D", 
         `dark2-8` = "#666666", `paired-1` = "#A6CEE3", `paired-2` = "#1F78B4", 
         `paired-3` = "#B2DF8A", `paired-4` = "#33A02C", `paired-5` = "#FB9A99", 
         `paired-6` = "#E31A1C", `paired-7` = "#FDBF6F", `paired-8` = "#FF7F00", 
         `paired-9` = "#CAB2D6", `paired-10` = "#6A3D9A", `paired-11` = "#FFFF99", 
         `paired-12` = "#B15928", `pastel1-1` = "#FBB4AE", `pastel1-2` = "#B3CDE3", 
         `pastel1-3` = "#CCEBC5", `pastel1-4` = "#DECBE4", `pastel1-5` = "#FED9A6", 
         `pastel1-6` = "#FFFFCC", `pastel1-7` = "#E5D8BD", `pastel1-8` = "#FDDAEC", 
         `pastel1-9` = "#F2F2F2", `pastel2-1` = "#B3E2CD", `pastel2-2` = "#FDCDAC", 
         `pastel2-3` = "#CBD5E8", `pastel2-4` = "#F4CAE4", `pastel2-5` = "#E6F5C9", 
         `pastel2-6` = "#FFF2AE", `pastel2-7` = "#F1E2CC", `pastel2-8` = "#CCCCCC", 
         `set1-1` = "#E41A1C", `set1-2` = "#377EB8", `set1-3` = "#4DAF4A", 
         `set1-4` = "#984EA3", `set1-5` = "#FF7F00", `set1-6` = "#FFFF33", 
         `set1-7` = "#A65628", `set1-8` = "#F781BF", `set1-9` = "#999999", 
         `set2-1` = "#66C2A5", `set2-2` = "#FC8D62", `set2-3` = "#8DA0CB", 
         `set2-4` = "#E78AC3", `set2-5` = "#A6D854", `set2-6` = "#FFD92F", 
         `set2-7` = "#E5C494", `set2-8` = "#B3B3B3", `set3-1` = "#8DD3C7", 
         `set3-2` = "#FFFFB3", `set3-3` = "#BEBADA", `set3-4` = "#FB8072", 
         `set3-5` = "#80B1D3", `set3-6` = "#FDB462", `set3-7` = "#B3DE69", 
         `set3-8` = "#FCCDE5", `set3-9` = "#D9D9D9", `set3-10` = "#BC80BD", 
         `set3-11` = "#CCEBC5", `set3-12` = "#FFED6F")
}

create_custom_css <- function() {
    all_colors <- reportes_bs_colors() 
    color_names <- names(all_colors)
    color_values <- all_colors |> purrr::map_chr(~.x) |> unname()
    
    color_list_collapsed <- glue::glue("'{color_names}': '{color_values}'") |> 
        glue::glue_collapse(sep = ", ")
    
    colors_for_sass <- glue::glue("({color_list_collapsed})")
    
    sass::sass(
        list(colors = colors_for_sass,
             "@each $color_name, $color_value in $colors {
                .bg-#{$color_name} {
                    background-color: #{$color_value} !important
                }
             }"
        ), 
        output = "inst/app/www/bg-colors.css"
    )
}


