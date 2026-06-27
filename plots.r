library(ggplot2)
library(plotly)

my_theme <- theme_minimal(base_size = 14) +

  theme(

    plot.title = element_text(
      face = "bold",
      size = 18,
      colour = "#1E293B"
    ),

    axis.title = element_text(
      face = "bold",
      size = 13
    ),

    panel.grid.minor = element_blank(),

    panel.grid.major.x = element_blank(),

    legend.position = "top",

    plot.background = element_rect(
      fill = "white",
      colour = NA
    )

  )
  age_histogram <- function(df){

  p <- ggplot(df,
              aes(x = Age)) +

    geom_histogram(

      bins = 20,

      fill = "#2563EB",

      colour = "white",

      linewidth = 0.3

    ) +

    labs(

      title = "Patient Age Distribution",

      x = "Age (Years)",

      y = "Number of Patients"

    ) +

    my_theme

  ggplotly(p) %>%
    config(displayModeBar = FALSE)

}
kidney_bubble <- function(df){

  p <- ggplot(

    df,

    aes(

      x = Creatinine_Level,

      y = GFR,

      colour = factor(CKD_Status),

      size = Age

    )

  ) +

    geom_point(

      alpha = .55

    ) +

    scale_colour_manual(

      values = c(

        "#3B82F6",

        "#EF4444"

      ),

      labels = c(

        "Healthy",

        "CKD"

      )

    ) +

    scale_size(range = c(2,7)) +

    labs(

      title = "Creatinine vs GFR",

      x = "Creatinine (mg/dL)",

      y = "GFR",

      colour = "Status"

    ) +

    my_theme

  ggplotly(p) %>%
    config(displayModeBar = FALSE)

}
creatinine_box <- function(df){

  p <- ggplot(

    df,

    aes(

      factor(CKD_Status),

      Creatinine_Level,

      fill = factor(CKD_Status)

    )

  ) +

    geom_boxplot(

      width = .5,

      alpha = .8

    ) +

    scale_fill_manual(

      values = c(

        "#60A5FA",

        "#F87171"

      ),

      labels = c(

        "Healthy",

        "CKD"

      )

    ) +

    labs(

      title = "Creatinine Levels by CKD Status",

      x = "",

      y = "Creatinine (mg/dL)",

      fill = "Status"

    ) +

    my_theme

  ggplotly(p) %>%
    config(displayModeBar = FALSE)

}
ckd_age <- function(df){

  age_group <- cut(

    df$Age,

    breaks = c(20,30,40,50,60,70,80,90),

    include.lowest = TRUE

  )

  temp <- data.frame(

    AgeGroup = age_group,

    CKD = df$CKD_Status

  )

  summary <- aggregate(

    CKD ~ AgeGroup,

    temp,

    mean

  )

  p <- ggplot(

    summary,

    aes(

      AgeGroup,

      CKD

    )

  ) +

    geom_col(

      fill = "#2563EB"

    ) +

    labs(

      title = "CKD Rate by Age Group",

      x = "Age Group",

      y = "Average CKD Rate"

    ) +

    my_theme

  ggplotly(p) %>%
    config(displayModeBar = FALSE)

}