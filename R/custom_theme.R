custom_theme <- function() {
    fresh::create_theme(
        do.call(fresh::bs4dash_color, reportes_bs_colors())
    )
}

reportes_bs_colors <- function() {
    list(
        blue = "#007bff",
        lightblue = "#b9e0f7",
        navy = "#8da4bd",
        cyan = "#17a2b8",
        teal = "#80B1D3",
        olive = "#c9eb8f",
        green = "#28a745",
        lime = "#CCEBC5",
        orange = "#ffcf99",
        yellow = "#ffc107",
        fuchsia = "#ff9ce9",
        purple = "#d095d1",
        # pink = "#f568a9",
        maroon = "#fb8bb4",
        red = "#dc3545",
        black = "#111",
        gray_x_light = "#d2d6de",
        gray_600 = "#6c757d",
        gray_800 = "#343a40",
        gray_900 = "#212529",
        white = "#FFFFFF"
    )
}

# fresh::bs4dash_color(
#     blue = "#007bff",
#     # indigo = "#b68cf9",
#     lightblue = "#b9e0f7",
#     navy = "#8da4bd",
#     cyan = "#17a2b8",
#     teal = "#80B1D3",
#     olive = "#c9eb8f",
#     green = "#28a745",
#     lime = "#CCEBC5",
#     orange = "#ffcf99",
#     yellow = "#ffc107",
#     fuchsia = "#ff9ce9",
#     purple = "#d095d1",
#     # pink = "#f568a9",
#     maroon = "#fb8bb4",
#     red = "#dc3545",
#     black = "#111",
#     gray_x_light = "#d2d6de",
#     gray_600 = "#6c757d",
#     gray_800 = "#343a40",
#     gray_900 = "#212529",
#     white = "#FFFFFF"
# )
