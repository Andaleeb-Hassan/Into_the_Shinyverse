# Into the Shinyverse
#
# Same chi-square lesson as lesson.R, with menus.
# Read lesson.R first if the steps are new.
#
# From this folder, after installing shiny, bslib, and ggplot2:
#   shiny::runApp()

library(shiny)
library(ggplot2)

# ---- practice cohort ----------------------------------------------------
# data/lung_cancer.csv is simulated. Smoking status depends on gender
# and race. Gender and race do not depend on each other.

raw <- read.csv(
  file.path("data", "lung_cancer.csv"),
  stringsAsFactors = FALSE
)
n_raw <- nrow(raw)

long_names <- c(
  "Black or African-American" = "Black",
  "Native Hawaiian or Other Pacific Islander" = "Hawaiian",
  "American Indian or Alaskan Native" = "Native",
  "Participant refused to answer" = "Other"
)
known <- !is.na(raw$race) & raw$race %in% names(long_names)
raw$race[known] <- unname(long_names[raw$race[known]])

ok <- !is.na(raw$gender) & nzchar(raw$gender) &
  !is.na(raw$race) & nzchar(raw$race) &
  !is.na(raw$smoker) & nzchar(raw$smoker) &
  !(raw$race %in% c("N/A", "More than one race"))
n_dropped <- n_raw - sum(ok)
cohort <- raw[ok, c("gender", "race", "smoker")]

# Keep a teaching order: Female before Male, Current before Former.
order_factor <- function(x, preferred) {
  present <- unique(as.character(x))
  factor(x, levels = c(intersect(preferred, present), sort(setdiff(present, preferred))))
}
cohort$gender <- order_factor(cohort$gender, c("Female", "Male"))
cohort$smoker <- order_factor(cohort$smoker, c("Current", "Former", "Never"))
cohort$race <- order_factor(
  cohort$race,
  c("Asian", "Black", "Hawaiian", "Native", "Other", "White")
)

pretty_name <- function(name) {
  known_labels <- c(
    gender = "Gender",
    race = "Race",
    smoker = "Smoking status",
    carat = "Carat",
    cut = "Cut",
    color = "Color",
    clarity = "Clarity",
    depth = "Depth (%)",
    table = "Table (%)",
    price = "Price (USD)",
    x = "Length (mm)",
    y = "Width (mm)",
    z = "Depth (mm)"
  )
  if (name %in% names(known_labels)) unname(known_labels[[name]]) else name
}

# Muted colors that stay distinct when a figure is printed.
group_colors <- c(
  "#44AA99", "#CC6677", "#88CCEE", "#DDCC77",
  "#117733", "#332288", "#AA4499", "#882255"
)

paper <- theme_bw(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#fcfbf8", color = NA),
    panel.background = element_rect(fill = "#fcfbf8", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#efeae3"),
    legend.position = "bottom"
  )

diamonds <- ggplot2::diamonds

# ---- page ---------------------------------------------------------------

theme <- bslib::bs_theme(
  version = 5,
  bg = "#f3f0ea",
  fg = "#2c3338",
  primary = "#3d5366",
  base_font = "sans-serif",
  heading_font = "serif"
)

ui <- bslib::page_navbar(
  title = "Into the Shinyverse",
  window_title = "Into the Shinyverse",
  theme = theme,
  fillable = FALSE,
  navbar_options = bslib::navbar_options(bg = "#3d5366", theme = "dark"),
  header = tags$p(
    style = "padding: 0.8rem 1rem 0; max-width: 42rem; color: #3a454b;",
    "Look at a picture on Diamonds. Then open Association and walk the seven steps in the server code: counts, expected counts, chi-square, degrees of freedom, critical value, decision, odds ratio."
  ),
  bslib::nav_panel(
    "Diamonds",
    bslib::layout_sidebar(
      fillable = FALSE,
      sidebar = bslib::sidebar(
        open = "always",
        width = 300,
        bg = "#f7f4ee",
        tags$h4("Pick two variables"),
        selectInput("x", "Horizontal", choices = names(diamonds), selected = "carat"),
        selectInput("y", "Vertical", choices = names(diamonds), selected = "price"),
        textOutput("diamond_note")
      ),
      bslib::card(
        bslib::card_header("What the picture is for"),
        plotOutput("diamond_plot", height = "520px")
      )
    )
  ),
  bslib::nav_panel(
    "Association",
    bslib::layout_sidebar(
      fillable = FALSE,
      sidebar = bslib::sidebar(
        open = "always",
        width = 320,
        bg = "#f7f4ee",
        tags$h4("The hypotheses"),
        tags$p("H0: the row grouping and the column grouping are independent."),
        tags$p("Ha: they are not independent. That is not a claim about cause."),
        selectInput("row_var", "Row variable", choices = names(cohort), selected = "gender"),
        selectInput("col_var", "Column variable", choices = names(cohort), selected = "smoker"),
        selectInput(
          "alpha",
          "Significance level (alpha)",
          choices = c("0.10" = 0.10, "0.05" = 0.05, "0.01" = 0.01),
          selected = 0.05
        ),
        tags$p(
          "Alpha is how often this rule rejects a true H0, when the chi-square curve is a fair approximation. That false rejection is a Type I error. A smaller alpha moves the slate line to the right."
        ),
        textOutput("cohort_note")
      ),
      bslib::layout_column_wrap(
        width = "320px",
        bslib::card(
          bslib::card_header("Chi-square curve if H0 is true"),
          plotOutput("curve", height = "420px")
        ),
        bslib::card(
          bslib::card_header("Share of the column inside each row"),
          plotOutput("bars", height = "420px")
        )
      ),
      bslib::layout_column_wrap(
        width = "320px",
        bslib::card(
          bslib::card_header("Observed counts"),
          tableOutput("observed_table")
        ),
        bslib::card(
          bslib::card_header("Expected counts if H0 is true"),
          tags$p("E = (row total × column total) / grand total."),
          tableOutput("expected_table")
        ),
        bslib::card(
          bslib::card_header("Decision"),
          uiOutput("decision")
        )
      )
    )
  ),
  bslib::nav_panel(
    "Formula sheet",
    bslib::card(
      bslib::card_header("The seven lines the app calculates"),
      tags$p("O is the observed count in a cell. E is the expected count under independence."),
      tags$p("E = (row total × column total) / grand total"),
      tags$p("chi-square = Σ (O − E)² / E"),
      tags$p("df = (rows − 1) × (columns − 1)"),
      tags$p("Critical value = the chi-square number with probability alpha to its right. Reject H0 when the statistic is larger than this value."),
      tags$p("p-value = probability of a statistic at least this large if H0 is true. It is not the probability that H0 is true."),
      tags$p("For a 2 by 2 table, odds ratio = (n11 × n22) / (n12 × n21). One means the two rows have the same odds. A larger table does not reduce to one odds ratio."),
      tags$p("The diamonds pictures describe a sample. A smooth is not a straight-line regression and not a cause. A box shows the median and the middle half. Bars show counts."),
      tags$p("The cohort file is simulated practice data. A result on that tab is not a health finding. A large sample can reject H0 for a modest gap, so read the bars and the odds ratio for the size.")
    )
  )
)

server <- function(input, output, session) {
  output$diamond_note <- renderText({
    req(input$x, input$y)
    if (input$x == input$y) {
      return("Choose two different variables. A variable compared with itself has nothing to show.")
    }
    x_num <- is.numeric(diamonds[[input$x]])
    y_num <- is.numeric(diamonds[[input$y]])
    if (x_num && y_num) {
      "Both variables are numbers. Each point is a diamond. The curve summarizes the cloud. It is not a cause."
    } else if (x_num || y_num) {
      "One variable is a group and one is a number. The line in a box is the median. The box is the middle half of the values."
    } else {
      "Both variables are groups. Bar height is how many diamonds fall in that pair of groups."
    }
  })

  output$diamond_plot <- renderPlot({
    req(input$x, input$y)
    validate(need(input$x != input$y, "Choose two different variables."))
    x <- input$x
    y <- input$y
    x_num <- is.numeric(diamonds[[x]])
    y_num <- is.numeric(diamonds[[y]])

    if (x_num && y_num) {
      # Enough points to see the cloud, not all 54,000.
      show <- diamonds[seq(1, nrow(diamonds), length.out = 4000), ]
      ggplot(show, aes(x = .data[[x]], y = .data[[y]])) +
        geom_point(alpha = 0.3, color = "#6d8b86", size = 1.1) +
        geom_smooth(
          method = "gam",
          formula = y ~ s(x, bs = "cs"),
          color = "#3d5366",
          fill = "#d5e0e6",
          linewidth = 1
        ) +
        labs(
          x = pretty_name(x),
          y = pretty_name(y),
          title = "Two numbers",
          subtitle = "The curve is a smooth summary, not a cause."
        ) +
        paper +
        theme(legend.position = "none")
    } else if (x_num || y_num) {
      group <- if (x_num) y else x
      number <- if (x_num) x else y
      ggplot(diamonds, aes(x = .data[[group]], y = .data[[number]])) +
        geom_boxplot(fill = "#e7eef0", color = "#3d5366", outlier.color = "#CC6677") +
        labs(
          x = pretty_name(group),
          y = pretty_name(number),
          title = "A number inside each group",
          subtitle = "Line = median. Box = middle half."
        ) +
        paper +
        theme(legend.position = "none")
    } else {
      ggplot(diamonds, aes(x = .data[[x]], fill = .data[[y]])) +
        geom_bar(position = "dodge") +
        scale_fill_manual(values = group_colors) +
        labs(
          x = pretty_name(x),
          y = "Diamonds",
          fill = pretty_name(y),
          title = "Counts for two groupings"
        ) +
        paper
    }
  })

  output$cohort_note <- renderText({
    sprintf(
      "%s rows are in the test. %s rows were set aside (missing gender, race, or smoking status, or race was N/A or More than one race).",
      format(nrow(cohort), big.mark = ","),
      format(n_dropped, big.mark = ",")
    )
  })

  # The whole test. Every table and sentence on the page reads this list.
  pair <- reactive({
    req(input$row_var, input$col_var, input$alpha)
    validate(need(
      input$row_var != input$col_var,
      "Choose two different variables."
    ))

    row_var <- input$row_var
    col_var <- input$col_var
    alpha <- as.numeric(input$alpha)

    # Step 1. Observed counts.
    observed <- table(cohort[[row_var]], cohort[[col_var]])
    observed <- observed[rowSums(observed) > 0, colSums(observed) > 0, drop = FALSE]
    validate(need(
      nrow(observed) >= 2 && ncol(observed) >= 2,
      "Each variable needs at least two groups that actually appear."
    ))

    # Step 2. Expected counts if the variables are independent.
    # E = (row total * column total) / grand total
    n <- sum(observed)
    expected <- outer(rowSums(observed), colSums(observed)) / n

    # Step 3. Chi-square statistic. Add (O - E)^2 / E over the cells.
    chi_sq <- sum((observed - expected)^2 / expected)

    # Step 4. Degrees of freedom.
    df <- (nrow(observed) - 1) * (ncol(observed) - 1)

    # Step 5. Critical value and p-value, from the chi-square curve with this df.
    critical <- qchisq(alpha, df, lower.tail = FALSE)
    p_value <- pchisq(chi_sq, df, lower.tail = FALSE)

    # Step 6. Decision. Reject H0 when the statistic is past the critical value.
    reject <- isTRUE(chi_sq > critical)
    p_text <- if (p_value < 0.001) "p < 0.001" else sprintf("p = %.3f", p_value)
    if (reject) {
      decision <- sprintf(
        "Reject H0 at alpha = %.2f. The statistic %.2f is larger than the critical value %.2f (df = %d, %s).",
        alpha, chi_sq, critical, df, p_text
      )
      meaning <- "This means the table is unlikely if the variables are independent. It does not say the gap is large, and it does not say one variable causes the other."
    } else {
      decision <- sprintf(
        "Do not reject H0 at alpha = %.2f. The statistic %.2f is not larger than the critical value %.2f (df = %d, %s).",
        alpha, chi_sq, critical, df, p_text
      )
      meaning <- "This table is still plausible under independence. That is not proof of independence. Missing a real association is a Type II error, and this page does not calculate that risk."
    }
    p_sentence <- "The p-value is the probability of a statistic at least this large if H0 is true. It is not the probability that H0 is true."

    # Step 7. Odds ratio, and only when the table is 2 by 2.
    if (nrow(observed) == 2 && ncol(observed) == 2) {
      counts <- observed
      zero_note <- ""
      if (any(counts == 0)) {
        # A zero cell makes the ratio 0 or infinite. Adding 0.5 to every
        # cell is the Haldane-Anscombe correction.
        counts <- counts + 0.5
        zero_note <- " A cell was 0, so 0.5 was added to every cell before the ratio."
      }
      n11 <- counts[1, 1]
      n12 <- counts[1, 2]
      n21 <- counts[2, 1]
      n22 <- counts[2, 2]
      odds_ratio <- (n11 * n22) / (n12 * n21)
      se <- sqrt(1 / n11 + 1 / n12 + 1 / n21 + 1 / n22)
      z <- qnorm(1 - alpha / 2)
      ci <- exp(log(odds_ratio) + c(-1, 1) * z * se)
      level <- round((1 - alpha) * 100)
      fisher_p <- fisher.test(observed)$p.value
      fisher_text <- if (fisher_p < 0.001) "p < 0.001" else sprintf("p = %.3f", fisher_p)
      odds_sentence <- sprintf(
        "Odds of %s rather than %s are %.2f times as large for %s as for %s (Wald %d%% interval %.2f to %.2f). Fisher's exact test %s.%s",
        colnames(observed)[1], colnames(observed)[2], odds_ratio,
        rownames(observed)[1], rownames(observed)[2],
        level, ci[1], ci[2], fisher_text, zero_note
      )
    } else {
      odds_sentence <- "An odds ratio is one number only for a 2 by 2 table. This table is larger, so compare the color mixes in the bars."
    }

    small_expected <- sum(expected < 5)
    if (small_expected > 0) {
      check <- sprintf(
        "%d expected cell count(s) are below 5, so the chi-square curve is only a rough approximation. For a 2 by 2 table, prefer Fisher's exact test.",
        small_expected
      )
    } else {
      check <- "Every expected count is at least 5, so the chi-square curve is a reasonable approximation."
    }

    list(
      observed = observed,
      expected = expected,
      chi_sq = unname(chi_sq),
      df = df,
      critical = critical,
      decision = decision,
      p_sentence = p_sentence,
      meaning = meaning,
      odds_sentence = odds_sentence,
      check = check
    )
  })

  output$observed_table <- renderTable({
    shown <- addmargins(pair()$observed)
    rownames(shown)[nrow(shown)] <- "Total"
    colnames(shown)[ncol(shown)] <- "Total"
    as.data.frame.matrix(shown)
  }, rownames = TRUE, digits = 0)

  output$expected_table <- renderTable({
    as.data.frame.matrix(pair()$expected)
  }, rownames = TRUE, digits = 1)

  output$decision <- renderUI({
    result <- pair()
    tagList(
      tags$p(tags$strong(result$decision)),
      tags$p(result$p_sentence),
      tags$p(result$meaning),
      tags$p(result$odds_sentence),
      tags$p(result$check)
    )
  })

  output$curve <- renderPlot({
    result <- pair()
    df <- result$df
    chi_sq <- result$chi_sq
    critical <- result$critical

    # Show the critical value and the body of the curve.
    # A huge statistic is named in the subtitle instead of stretching the axis.
    right <- max(qchisq(0.999, df) * 1.05, critical * 1.45)
    too_far <- chi_sq > right
    if (!too_far) right <- max(right, chi_sq * 1.08)

    plot <- ggplot(data.frame(x = c(0, right)), aes(x = x)) +
      stat_function(fun = dchisq, args = list(df = df), linewidth = 0.9, color = "#2c3338") +
      stat_function(
        fun = dchisq,
        args = list(df = df),
        xlim = c(critical, right),
        geom = "area",
        fill = "#8fb4c9",
        alpha = 0.55
      ) +
      geom_vline(xintercept = critical, color = "#3d5366", linewidth = 1)

    if (!too_far) {
      plot <- plot + geom_vline(
        xintercept = chi_sq,
        color = "#CC6677",
        linetype = "dashed",
        linewidth = 1
      )
    }

    subtitle <- if (too_far) {
      sprintf("Statistic = %.1f, past the right edge. Slate line = critical value. Shade = reject H0 here.", chi_sq)
    } else {
      "Dashed rose line = statistic. Slate line = critical value. Shade = reject H0 here."
    }

    plot <- plot +
      labs(
        title = paste("If H0 is true, df =", df),
        subtitle = subtitle,
        x = "Chi-square value",
        y = "Density"
      ) +
      paper +
      theme(legend.position = "none")

    # df = 1 goes to infinity at zero. Clip that spike so the tail stays visible.
    if (df < 2) {
      plot <- plot + coord_cartesian(
        xlim = c(0, right),
        ylim = c(0, max(dchisq(critical, df) * 6, 0.2))
      )
    }
    plot
  })

  output$bars <- renderPlot({
    req(input$row_var, input$col_var)
    validate(need(input$row_var != input$col_var, "Choose two different variables."))
    row_var <- input$row_var
    col_var <- input$col_var
    ggplot(cohort, aes(x = .data[[row_var]], fill = .data[[col_var]])) +
      geom_bar(position = "fill", width = 0.72) +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), expand = c(0, 0)) +
      scale_fill_manual(values = group_colors) +
      labs(
        x = pretty_name(row_var),
        y = "Share within the row",
        fill = pretty_name(col_var),
        subtitle = "Each bar is 100%. Matching color mixes are what independence looks like."
      ) +
      paper
  })
}

shinyApp(ui, server)

