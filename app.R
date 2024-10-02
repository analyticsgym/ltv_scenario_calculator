library(shiny)
library(tidyverse)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("5 Year LTV Calculator"),
  p("Example LTV scenario calculator for annual subscription business."),
  HTML("<p><a href='https://github.com/analyticsgym/ltv_scenario_calculator/blob/main/app.R'>Code on Github</a></p>"),
  # Explanatory text and formulas
  tags$div(
    HTML("
      <h2>Breaking down the per period LTV contribution formula</h2>
      <p><i>(avg_aov * (1 - avg_refund_rate)) * (1 - user_cogs_pct_net_sales) * period_x_segment_start_rate</i></p>
      
      <h4>Part 1: avg user net sales</h4>
      <ul>
        <li>average amount received post discounts and adjusted for refunds</li>
        <li>average order value * (1 - average refund rate)</li>
      </ul>

      <h4>Part 2: avg user gross profit</h4>
      <ul>
        <li>average user net sales adjusted for variables costs</li>
        <li>average user net sales * average user contribution margin</li>
        <li>average user contribution margin = (1 - average user cogs pct net sales)</li>
      </ul>

      <h4>Part 3: ltv contribution period X</h4>
      <ul>
        <li>weights avg user gross profit by the proportion of the initial segment that reaches period X (acts similar to a weighted average in practice)</li>
        <li>average user gross profit period X * proportion of initial segment to reach period X</li>
      </ul>
    "),
    p("Scenario math below assumes year 1 renewal rate is net of refunders (year 1 renewers / year 1 users who didn't refund)"),
  ),
  # Compact input styling
  tags$head(
    tags$style(HTML("
            .shiny-input-container { 
                font-size: 80%; 
                margin-bottom: 5px; 
            }
            .numeric-input-plus { 
                width: 80%; 
            }
        "))
  ),
  
  fluidRow(
    column(4, # Scenario 1
           h3("Scenario 1"),
           fluidRow(
             column(6,
                    numericInput("refundRate1", "Year 1 Refund Rate", 0.05),
                    numericInput("y1aov1", "Year 1 AOV", 100),
                    numericInput("y2aov1", "Year 2 AOV", 100),
                    numericInput("y3aov1", "Year 3 AOV", 100),
                    numericInput("y4aov1", "Year 4 AOV", 100),
                    numericInput("y5aov1", "Year 5 AOV", 100)
             ),
             column(6,
                    numericInput("renewalRate11", "Year 1 Renewal Rate", 0.55),
                    numericInput("renewalRate12", "Year 2 Renewal Rate", 0.60),
                    numericInput("renewalRate13", "Year 3 Renewal Rate", 0.65),
                    numericInput("renewalRate14", "Year 4 Renewal Rate", 0.70),
                    numericInput("cogsPct1", "COGS % Net Sales", 0.20)
             )
           ),
           br(),
           textOutput("totalLtv1"),
           br(),
           plotOutput("ltvPlot1")
    ),
    column(4, # Scenario 2
           h3("Scenario 2"),
           fluidRow(
             column(6,
                    numericInput("refundRate2", "Year 1 Refund Rate", 0.05),
                    numericInput("y1aov2", "Year 1 AOV", 100),
                    numericInput("y2aov2", "Year 2 AOV", 100),
                    numericInput("y3aov2", "Year 3 AOV", 100),
                    numericInput("y4aov2", "Year 4 AOV", 100),
                    numericInput("y5aov2", "Year 5 AOV", 100)
             ),
             column(6,
                    numericInput("renewalRate21", "Year 1 Renewal Rate", 0.55),
                    numericInput("renewalRate22", "Year 2 Renewal Rate", 0.60),
                    numericInput("renewalRate23", "Year 3 Renewal Rate", 0.65),
                    numericInput("renewalRate24", "Year 4 Renewal Rate", 0.70),
                    numericInput("cogsPct2", "COGS % Net Sales", 0.20)
             )
           ),
           br(),
           textOutput("totalLtv2"),
           br(),
           plotOutput("ltvPlot2")
    ),
    column(4, # Scenario 3
           h3("Scenario 3"),
           fluidRow(
             column(6,
                    numericInput("refundRate3", "Year 1 Refund Rate", 0.05),
                    numericInput("y1aov3", "Year 1 AOV", 100),
                    numericInput("y2aov3", "Year 2 AOV", 100),
                    numericInput("y3aov3", "Year 3 AOV", 100),
                    numericInput("y4aov3", "Year 4 AOV", 100),
                    numericInput("y5aov3", "Year 5 AOV", 100)
             ),
             column(6,
                    numericInput("renewalRate31", "Year 1 Renewal Rate", 0.55),
                    numericInput("renewalRate32", "Year 2 Renewal Rate", 0.60),
                    numericInput("renewalRate33", "Year 3 Renewal Rate", 0.65),
                    numericInput("renewalRate34", "Year 4 Renewal Rate", 0.70),
                    numericInput("cogsPct3", "COGS % Net Sales", 0.20)
             )
           ),
           br(),
           textOutput("totalLtv3"),
           br(),
           plotOutput("ltvPlot3")
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  calculate_ltv <- function(y1_to_y5_aovs, y1_refund_rate, y1_to_y4_renewal_rate, user_cogs_pct_net_sales) {
    setup <- tibble(
      sub_year = 1:5,
      user_cogs_pct_net_sales = rep(user_cogs_pct_net_sales, 5),
      sub_year_aov = y1_to_y5_aovs,
      sub_year_refund_rate = c(y1_refund_rate, rep(0, 4)),
      # assume year 1 renewal rate is net of refunds so we need to adjust for that
      sub_year_renewal_rate = c(y1_to_y4_renewal_rate, NA) * (1 - sub_year_refund_rate)
    )
    
    setup %>%
      mutate(
        cohort_start_rate = lag(cumprod(sub_year_renewal_rate), default = 1),
        sub_year_ltv_contribution = (sub_year_aov * (1 - sub_year_refund_rate)) * (1 - user_cogs_pct_net_sales) * cohort_start_rate,
        pct_total_5yr_ltv = sub_year_ltv_contribution / sum(sub_year_ltv_contribution)
      )
  }
  
  # Render Total LTV for each scenario
  output$totalLtv1 <- renderText({
    scenario_data <- calculate_ltv(c(input$y1aov1, input$y2aov1, input$y3aov1, input$y4aov1, input$y5aov1), 
                                   input$refundRate1, 
                                   c(input$renewalRate11, input$renewalRate12, input$renewalRate13, input$renewalRate14), 
                                   input$cogsPct1)
    paste("\nTotal 5 Year LTV: $", round(sum(scenario_data$sub_year_ltv_contribution),0), "\n")
  })
  output$totalLtv2 <- renderText({
    scenario_data <- calculate_ltv(c(input$y1aov2, input$y2aov2, input$y3aov2, input$y4aov2, input$y5aov2), 
                                   input$refundRate2, 
                                   c(input$renewalRate21, input$renewalRate22, input$renewalRate23, input$renewalRate24), 
                                   input$cogsPct2)
    paste("\nTotal 5 Year LTV: $", round(sum(scenario_data$sub_year_ltv_contribution),0), "\n")
  })
  output$totalLtv3 <- renderText({
    scenario_data <- calculate_ltv(c(input$y1aov3, input$y2aov3, input$y3aov3, input$y4aov3, input$y5aov3), 
                                   input$refundRate3, 
                                   c(input$renewalRate31, input$renewalRate32, input$renewalRate33, input$renewalRate34), 
                                   input$cogsPct3)
    paste("\nTotal 5 Year LTV: $", round(sum(scenario_data$sub_year_ltv_contribution),0), "\n")
  })
  
  output_plot <- function(output_id, y1aov_input, y2aov_input, y3aov_input, y4aov_input, y5aov_input, refundRate_input, renewalRate1_input, renewalRate2_input, renewalRate3_input, renewalRate4_input, cogsPct_input) {
    output[[output_id]] <- renderPlot({
      y1_to_y5_aovs <- c(input[[y1aov_input]], input[[y2aov_input]], input[[y3aov_input]], input[[y4aov_input]], input[[y5aov_input]])
      y1_refund_rate <- input[[refundRate_input]]
      y1_to_y4_renewal_rate <- c(input[[renewalRate1_input]], input[[renewalRate2_input]], input[[renewalRate3_input]], input[[renewalRate4_input]])
      user_cogs_pct_net_sales <- input[[cogsPct_input]]
      
      scenario_data <- calculate_ltv(y1_to_y5_aovs, y1_refund_rate, y1_to_y4_renewal_rate, user_cogs_pct_net_sales)
      
      ggplot(scenario_data, aes(x = sub_year, y = sub_year_ltv_contribution, fill = as.factor(sub_year))) +
        geom_col(alpha = 0.7, show.legend = F) +
        geom_text(aes(label = paste0("$", round(sub_year_ltv_contribution, 0))), vjust = -0.5) +
        geom_text(aes(label = paste0(round(pct_total_5yr_ltv * 100, 0), "%")), vjust = 1.5, color = "white") +
        labs(title = "LTV Contribution by Year\nXX% of Total 5yr LTV", x = "Year", y = "LTV Contribution") +
        theme_minimal() +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              plot.title = element_text(size = 14))
    })
  }
  
  output_plot("ltvPlot1", "y1aov1", "y2aov1", "y3aov1", "y4aov1", "y5aov1", "refundRate1", "renewalRate11", "renewalRate12", "renewalRate13", "renewalRate14", "cogsPct1")
  output_plot("ltvPlot2", "y1aov2", "y2aov2", "y3aov2", "y4aov2", "y5aov2", "refundRate2", "renewalRate21", "renewalRate22", "renewalRate23", "renewalRate24", "cogsPct2")
  output_plot("ltvPlot3", "y1aov3", "y2aov3", "y3aov3", "y4aov3", "y5aov3", "refundRate3", "renewalRate31", "renewalRate32", "renewalRate33", "renewalRate34", "cogsPct3")
}

# Run the application 
shinyApp(ui = ui, server = server)
