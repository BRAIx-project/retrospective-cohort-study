# Process the data for simulation ----------------------------------------------
setup <- function(input_dir, output_dir, is_test) {
  reader_file <- ifelse(is_test,
                        "client_test_reader.csv",
                        "client_dev_reader.csv")
  model_file <- ifelse(is_test,
                       "client_test_image_mean_ensemble.csv",
                       "client_dev_image_mean_ensemble.csv")
  prepare_data_for_simulation(
    file.path(input_dir, model_file),
    file.path(input_dir, reader_file),
    is_test,
    output_dir
  )
}


# Estimate a ROC curve for the human readers -----------------------------------
reader_roc <- function(reader_perf, model_pred) {
  require(dplyr)
  require(magrittr)
  require(tidyr)
  sroc_plot <- function(res_df) {
    sroc <- mada::reitsma(res_df)
    bound <- c(min(res_df$FPR), max(res_df$FPR))
    srocmat <- mada::sroc(sroc, type = "ruttergatsonis")

    plot(c(0, 1), c(0,1),
         main = glue::glue("Summary ROC of {nrow(res_df)} radiologists VS Empirical ROC of AI"),
         ylab = "Sensitivities",
         xlab = "1 - Specificities",
         type = 'n')
    abline(v = seq(0, 1, 0.05), h = seq(0, 1, 0.05), col = 'lightgray', lty = 3)
    points(res_df$FPR, res_df$TPR)

    srocmat <- rbind(c(0, 0), srocmat)
    lines(srocmat[,1], srocmat[,2], col = "lightgreen", lwd = 2)
    lines(srocmat[cut(srocmat[, 1], bound, "withinbound") == "withinbound", ], col = "darkgreen", lwd = 2)

    list(sroc = sroc, bound = bound, fitted_values = srocmat)
  }

  normalised_pAUC <- function(df0) {
    xs <- diff(df0$fpr)
    xs2 <- c(0, 0.5 * xs) + c(0.5 * xs, 0)
    ori_pAUC <- sum(xs2 * df0$tpr)

    m0 <- diff(range(df0$fpr)) * sum(range(df0$fpr)) / 2
    M0 <- diff(range(df0$fpr))

    0.5 * (1 + (ori_pAUC - m0) / (M0 - m0))
  }

  res <- reader_perf |>
    mutate(episode_prediction = individual_recall) |>
    filter(reader_number %in% c(1, 2)) |>
    group_by(reader_id) |>
    mutate(n_reads = n()) |>
    # filter(n_reads >= 2500) |>
    nest() |>
    extract2("data") |>
    lapply(confusion_matrix) |>
    lapply(data.frame)

  res_df <- do.call(rbind, res) |>
    mutate(n_reads = P + N) |>
    filter(n_reads > quantile(n_reads, 0.1))
  sroc_curve <- sroc_plot(res_df)

  write.csv(res_df, output("radiologists_performance.csv"), row.names = FALSE)
  write.csv(sroc_curve$fitted_values, output("radiologists_sroc.csv"))




  # Add model curve and legend
  eroc <- pROC::roc(model_pred$episode_outcome %in% c(1,2),
                    model_pred$episode_prediction)
  lines(1 - eroc$specificities, eroc$sensitivities, lty = 2, col = 'blue')
  sroc_summary <- summary(sroc_curve$sroc)$AUC

  spAUC <- data.frame(sroc_curve$fitted_values) |>
    filter(fpr >= min(res_df$FPR) & fpr <= max(res_df$FPR)) |>
    set_colnames(c("fpr", "tpr")) |>
    normalised_pAUC()

  epAUC <- pROC::auc(eroc,
                     partial.auc = 1 - c(min(res_df$FPR), max(res_df$FPR)),
                     partial.auc.correct = TRUE)

  legend("bottomright",
         c("Radiologists",
           glue::glue("Radiologists summary ROC (AUC: {round(sroc_summary$AUC, 3)}, pAUC: {round(spAUC, 3)})"),
           glue::glue("AI empirical ROC (AUC: {round(eroc$auc, 3)}, pAUC: {round(epAUC, 3)})")),
         pch = c(1, NA, NA),
         lty = c(NA, 1, 2),
         col = c("black", "black", "blue"))


  # Confidence Interval for AUC and pAUC =========================================
  # pROC::ci.auc(eroc, conf.level = 0.95, method = "delong")
  eroc_auc_ci <- pROC::ci.auc(eroc, conf.level = 0.95, method = "bootstrap", boot.n = 2000)
  eroc_pauc_ci <- pROC::ci.auc(epAUC, boot.n = 2000)  # partial AUC only uses 'bootstrap'


  # Need bootstrap for sroc over radiologists
  set.seed(1234)
  pb <- txtProgressBar(min = 1, max = 2000, initial = 1, style = 3)
  sroc_ci <- lapply(1:2000, function(i) {
    sample_res_df <- res_df |> slice_sample(replace = TRUE, n = nrow(res_df))
    setTxtProgressBar(pb, value = i)
    sroc_plot(sample_res_df)
  })
  sroc_ci_auc <- sroc_ci |> sapply(\(x) summary(x$sroc)$AUC$AUC)
  sroc_ci_pauc <- sroc_ci |>
    sapply(function(x) {
      data.frame(x$fitted_values) |>
        filter(fpr >= min(res_df$FPR) & fpr <= max(res_df$FPR)) |>
        set_colnames(c("fpr", "tpr")) |>
        normalised_pAUC()
    })


  # Save result to disk
  all_auc <- list(
    eroc_auc_ci = eroc_auc_ci,
    eroc_pauc_ci = eroc_pauc_ci,
    sroc_ci_auc = sroc_ci_auc,
    sroc_ci_pauc = sroc_ci_pauc
  )
  saveRDS(all_auc, output("eroc_sroc_with_ci.RDS"))


  # Results
  f <- \(x) round(x, 3)
  mean_pm_sd <- \(x) glue::glue("{f(mean(x))} ({f(mean(x) - 2 * sd(x))}, {f(mean(x) + 2 * sd(x))})")
  cpaste <- \(...) cat(paste(...), "\n")
  local({
    cpaste("AI AUC:", mean_pm_sd(eroc_auc_ci))
    cpaste("AI pAUC:", mean_pm_sd(eroc_pauc_ci))
    cpaste("Radiologists AUC:", mean_pm_sd(sroc_ci_auc))
    cpaste("Radiologists pAUC:", mean_pm_sd(sroc_ci_pauc))
  })


  all_auc_df <- data.frame(
    labels = c("AI AUC", "AI pAUC", "Radiologists AUC", "Radiologists pAUC"),
    mean = sapply(all_auc, mean),
    mean_minus_two_sd = sapply(all_auc, \(x) mean(x) - 2 * sd(x)),
    mean_plus_two_sd = sapply(all_auc, \(x) mean(x) + 2 * sd(x))
  )
  write.csv(all_auc_df, output("eroc_sroc_with_ci.csv"),
            row.names = FALSE)
}


# MASAI extension --------------------------------------------------------------
masai_correct <- function(res, prop) {
  single_read_df <- res |> dplyr::filter(by == "reader-1" | by == "reader-2")
  dbl_thd_read_df <- res |> dplyr::filter(by == "reader-1-and-2" | by == "reader-3")

  miss_ind <- which(single_read_df$episode_outcome != single_read_df$episode_prediction)
  corr_ind <- sample(miss_ind, floor(length(miss_ind) * prop))

  modified_single_read <- single_read_df
  modified_single_read$episode_prediction[corr_ind] <- 1 - modified_single_read$episode_prediction[corr_ind]

  res['episode_id'] |>
    dplyr::left_join(rbind(modified_single_read, dbl_thd_read_df),
                     by = "episode_id")
}

masai_correct_perf <- function(res_masai, p) {
  res_masai_correct <- masai_correct(res_masai, p)
  data.frame(scenario_summary(res_masai_correct)$performance)
}

masai_correct_econ <- function(res_masai, p) {
  res_masai_correct <- masai_correct(res_masai, p)
  data.frame(t(unlist(
    BRAIxMOP:::masai_economics(scenario_summary(res_masai_correct))
  )))
}

masai_correct_tbl <- function(res_masai, ps, f = masai_correct_perf, seed) {
  results <- lapply(ps, \(p, seed) {
    if (!missing(seed)) set.seed(seed)
    f(res_masai, p)
  }, seed = seed)
  cbind(corr_prop = ps, do.call(rbind, results))
}

solve_equivalent <- function(res_masai, res_baseline, seed) {
  target_tnr <- data.frame(scenario_summary(res_baseline)$performance)$TNR
  f <- function(p, seed) {
    if (!missing(seed)) set.seed(seed)
    tnr <- masai_correct_perf(res_masai, p)$TNR
    (tnr - target_tnr)
  }
  uniroot(f, c(0, 1), seed = seed)
}


# Table helper functions -------------------------------------------------------
table_2 <- function(m0) {
  stringr::str_interp(
    r"(
    \begin{table}[htbp]
    \centering
    \resizebox{\textwidth}{!}{
    \begin{tabular}{lrrrr}
        \hline
        Variable &
        \makecell{Current screening \\pathway} &
        \makecell{AI reader-replacement\\} &
        AI band-pass &
        MASAI\\
        \hline\\
        Sensitivity (\%) & ${m0[1,1]} & ${m0[1,2]} & ${m0[1,3]} & ${m0[1,4]}\\
        Specificity (\%) & ${m0[2,1]} & ${m0[2,2]} & ${m0[2,3]} & ${m0[2,4]} \\\\
        No. of episodes & ${m0[3,1]} & ${m0[3,2]} & ${m0[3,3]} & ${m0[3,4]}\\
        $\quad$ True positive & ${m0[4,1]} & ${m0[4,2]}  & ${m0[4,3]} & ${m0[4,4]}\\
        $\quad$ True negative & ${m0[5,1]} & ${m0[5,2]} & ${m0[5,3]} & ${m0[5,4]}\\
        $\quad$ False positive & ${m0[6,1]} & ${m0[6,2]} & ${m0[6,3]} & ${m0[6,4]}\\
        $\quad$ False negative & ${m0[7,1]} & ${m0[7,2]} & ${m0[7,3]} & ${m0[7,4]}\\\\
        Workload & \\
        $\quad$ Assessments & ${m0[8,1]} & ${m0[8,2]} & ${m0[8,3]} & ${m0[8,4]}\\
        $\quad$ Human reads & ${m0[9,1]} & ${m0[9,2]} & ${m0[9,3]} & ${m0[9,4]}\\
        $\quad$ *Third reads & ${m0[10,1]} & ${m0[10,2]} & ${m0[10,3]} & ${m0[10,4]} \\\\
        Reading and assessment cost & ${m0[11,1]} & ${m0[11,2]} & ${m0[11,3]} & ${m0[11,4]}\\
        \hline
        \multicolumn{4}{l}{\footnotesize{*The thirds reads are part of the human readings. The separate entry is presented to show the workload impact on the third reader.}}\\
    \end{tabular}}
    \caption{Comparison of the current screening pathway and the AI-integrated scenarios by the screening outcomes, workload and cost. For the AI reader replacement scenario, mean values based on 1000 simulations are presented (rounded to the nearest integer, and the standard deviations are presented for the sensitivity and specificity). The AI band-pass scenario does not require simulating the third reader, and the result is entirely based on the real data. Percentages in the brackets with the plus / minus sign indicate the percentages change compared with the current screening pathway.}
    \label{tab:pathway-comparison}
    \end{table}
    )")
}

baseline_summary_col <- function(res_econ) {
  dollar_format <- function(x) {
    paste0("\\$", formatC(as.numeric(x), format="f", digits=0, big.mark=","))
  }

  c(sensitivity = round(100 * res_econ$benefit_rate$TP, 1),
    specificity = round(100 * res_econ$benefit_rate$TN, 1),
    num_of_episode = round(res_econ$episodes),
    TP = res_econ$benefit$TP,
    TN = res_econ$benefit$TN,
    FP = res_econ$harms$FP,
    FN = res_econ$harms$FN,
    assessment = res_econ$cost_drivers$assessments$total,
    human_reads = res_econ$cost_drivers$human_readings$total_reads,
    third_reads = res_econ$cost_drivers$human_readings$reader_3_reads,
    total_cost = dollar_format(res_econ$cost$total_cost$total))
}

bp_summary_col <- function(res_econ_tbl) {
  res <- res_econ_tbl$`_data`

  dollar_format <- function(x) {
    paste0("\\$", formatC(as.numeric(x), format="f", digits=0, big.mark=","))
  }

  p <- function(x, dollar = FALSE) {
    base <- ifelse(dollar, dollar_format(x[2]), x[2])
    diff_percent <- round(x[4] * 100, 1)
    ifelse(diff_percent > 0,
           sprintf("%s (+%s\\%%)", base, diff_percent),
           sprintf("%s (%s\\%%)", base, diff_percent))
  }

  c(sensitivity = round(100 * res$benefit_rate.TP[2], 1),
    specificity = round(100 * res$benefit_rate.TN[2], 1),
    num_of_episode = round(res$episodes[2]),
    TP = p(res$benefit.TP),
    TN = p(res$benefit.TN),
    FP = p(res$harms.FP),
    FN = p(res$harms.FN),
    assessment = p(res$cost_drivers.assessments.total),
    human_reads = p(res$cost_drivers.human_readings.total_reads),
    third_reads = p(res$cost_drivers.human_readings.reader_3_reads),
    total_cost = p(res$cost.total_cost.total, dollar = TRUE))
}

masai_summary_col <- function(res_econ, baseline_econ) {
  dollar_format <- function(x) {
    paste0("\\$", formatC(as.numeric(x), format="f", digits=0, big.mark=","))
  }

  p <- function(x, dollar = FALSE) {
    base <- ifelse(dollar, dollar_format(x[2]), x[2])
    diff_percent <- round(x[4] * 100, 1)
    ifelse(diff_percent > 0,
           sprintf("%s (+%s\\%%)", base, diff_percent),
           sprintf("%s (%s\\%%)", base, diff_percent))
  }

  res_econ_table <- BRAIxMOP:::diff_baseline(baseline_econ, res_econ) |>
    BRAIxMOP:::diff_table("MASAI")
  print(res_econ_table)

  res <- res_econ_table$`_data`
  c(sensitivity = round(100 * res$benefit_rate.TP[2], 1),
    specificity = round(100 * res$benefit_rate.TN[2], 1),
    num_of_episode = round(res$episodes[2]),
    TP = p(res$benefit.TP),
    TN = p(res$benefit.TN),
    FP = p(res$harms.FP),
    FN = p(res$harms.FN),
    assessment = p(res$cost_drivers.assessments.total),
    human_reads = p(res$cost_drivers.human_readings.total_reads),
    third_reads = p(res$cost_drivers.human_readings.reader_3_reads),
    total_cost = p(res$cost.total_cost.total, dollar = TRUE))
}

repl_summary_col <- function(res_boot, baseline_econ) {
  dollar_format <- function(x) {
    paste0("\\$", formatC(as.numeric(x), format="f", digits=0, big.mark=","))
  }

  p <- function(x, dollar = FALSE) {
    # Second entry is baseline
    delta_x <- x[1] - x[2]
    delta_p <- delta_x / x[2]
    base <- ifelse(dollar, dollar_format(x[1]), round(x[1]))
    diff_percent <- round(delta_p * 100, 1)
    ifelse(diff_percent > 0,
           sprintf("%s (+%s\\%%)", base, diff_percent),
           sprintf("%s (%s\\%%)", base, diff_percent))
  }

  with_sd <- function(mean0, sd0) {
    base <- 100 * mean0  # convert to percentage
    std <- 100 * sd0   # convert to percentage
    r1 <- \(x) round(x, 1)
    sprintf("%s (%s, %s)", r1(base), r1(base - 2*std), r1(base + 2 * std))
  }

  res <- data.frame(t(colMeans(res_boot$econ)))
  res_sd <- data.frame(t(apply(res_boot$econ, 2, sd)))

  c(sensitivity = with_sd(res$benefit_rate.TP, res_sd$benefit_rate.TP),
    specificity = with_sd(res$benefit_rate.TN, res_sd$benefit_rate.TN),
    num_of_episode = round(res$episodes),
    TP = p(c(res$benefit.TP, baseline_econ$benefit$TP)),
    TN = p(c(res$benefit.TN, baseline_econ$benefit$TN)),
    FP = p(c(res$harms.FP, baseline_econ$harms$FP)),
    FN = p(c(res$harms.FN, baseline_econ$harms$FN)),
    assessment = p(c(res$cost_drivers.assessments.total,
                     baseline_econ$cost_drivers$assessments$total)),
    human_reads = p(c(res$cost_drivers.human_readings.total_reads,
                      baseline_econ$cost_drivers$human_readings$total_reads)),
    third_reads = p(c(res$cost_drivers.human_readings.reader_3_reads,
                      baseline_econ$cost_drivers$human_readings$reader_3_reads)),
    total_cost = p(c(res$cost.total_cost.total,
                     baseline_econ$cost$total_cost$total), dollar = TRUE))
}


# Flowchart helper functions ---------------------------------------------------
flowchart_A <- function(res_baseline) {
  extract <- `[`

  res_sum <- scenario_summary(res_baseline)$summary
  num_in <- sum(res_sum$count)
  num_agree_recall <- res_sum |>
    filter(episode_prediction == 1, by == "reader-1-and-2") |>
    extract("count")
  num_agree_no_recall <- res_sum |>
    filter(episode_prediction == 0, by == "reader-1-and-2") |>
    extract("count")
  num_arbitrate_recall <- res_sum |>
    filter(episode_prediction == 1, by == "reader-3") |>
    extract("count")
  num_arbitrate_no_recall <- res_sum |>
    filter(episode_prediction == 0, by == "reader-3") |>
    extract("count")
  num_disagree <- num_arbitrate_recall + num_arbitrate_no_recall

  stringr::str_interp(
    r"(
    \begin{tikzpicture}[
        auto,
        scale=0.4,
        every node/.style={
            fill=white,
            font=\sffamily,
            block_center_rounded
        },
        align=center,
        node distance=1cm and 0.5cm,
        block_center_rounded/.style ={
            rectangle, rounded corners, draw=black,
            thick, fill=white, text width=8em,
            text centered, minimum height=4em
        },
        reader_block/.style ={
            rectangle, rounded corners, draw=black,
            thick, fill=lightgray, text width=6em,
            text centered, minimum height=3em
        },
        text_center/.style ={
            rectangle, draw=black, rounded corners=0cm,
            thick, fill=white, text centered,
            text width=3.5em, minimum height=1.5em, minimum width=1.5em,
        },
        no_recall_color/.style = {fill={green!25}},
        recall_color/.style = {fill=pink},
        size_two/.style={text width=6.5em, minimum height=3em},
        decoration={markings, mark=at position 0.5 with {\arrow{>}}}
    ]
     % nodes
     \definecolor{almond}{rgb}{0.94, 0.87, 0.8}
     \definecolor{diamond}{rgb}{0.73, 0.90, 1.0}
     \node (start)      {Screening Episodes};
     \coordinate[below=2.98cm of start] (aux1);
     \node (reader_1)   [below left=0.75cm and 0.35cm of aux1, reader_block] {Reader 1};
     \node (reader_2)   [below right=0.75cm and 0.35cm of aux1, reader_block] {Reader 2};
     \coordinate[below right=0.75cm and 0.35cm of reader_1] (aux2);
     \coordinate[below=0.75cm of aux2] (aux2b);
     \node (reader_3)   [below=1.33cm of aux2b, reader_block] {Reader 3};
     \coordinate[below=0.67cm of reader_3] (aux3);
     \node (recall)     [below left=1cm and 0.45cm of aux3, recall_color, size_two] {Recall};
     \node (no_recall)  [below right=1cm and 0.45cm of aux3, no_recall_color, size_two] {No Recall};
     % edges
     \draw[->]  (start) -- (aux1) -| (reader_1);
     \draw[->]  (aux1) -| (reader_2);
     \draw[postaction={decorate}]  (reader_1) |- (aux2);
     \draw[postaction={decorate}]  (reader_2) |- (aux2);
     \draw[postaction={decorate}] (aux2) -- (aux2b);
     \draw[->] (aux2b) -- (reader_3);
     \draw[->] (aux2b) -| ([xshift=-5cm] recall);
     \draw[->] (aux2b) -| ([xshift=5cm] no_recall);
     \draw[->]  (reader_3) -- (aux3) -| ([xshift=3cm]recall);
     \draw[->]  (aux3) -| ([xshift=-3cm] no_recall);
     % floating textbox
     \node[draw, text_center] (num_in) at (0,\By) {${num_in}};
     \node[draw, text_center] (num_disagree) at (0,\Cy) {${num_disagree}};
     \node[draw, text_center, no_recall_color] (num_agree_no_recall) at (\Cgap,\Cy) {${num_agree_no_recall}};
     \node[draw, text_center, recall_color] (num_agree_recall) at (-\Cgap,\Cy) {${num_agree_recall}};
     \node[draw, text_center, no_recall_color] (num_arbitrate_no_recall) at (\Dgap,\Dy) {${num_arbitrate_no_recall}};
     \node[draw, text_center, recall_color] (num_arbitrate_recall) at (-\Dgap,\Dy) {${num_arbitrate_recall}};
     \node[draw=none, fill=white, text width=5.5cm] (labels) at (0, \headingy) {\LARGE\textbf{Current screening pathway}};
     \node[draw=none, fill=none, text width=1cm] (labels) at (-6.5, \labely) {\LARGE\textbf{A}};
    %  \node[minimum height=1em, draw=none] () at (0,-25.5) {};
    \end{tikzpicture}
    )"
  )
}

flowchart_B <- function(res_repl) {
  extract <- `[`

  res_sum <- res_repl$summary |> colMeans() |> data.frame()

  num_in <- sum(res_sum)
  num_agree_recall <- round(res_sum["1_reader-1-and-AI", ] + res_sum["1_reader-2-and-AI", ])
  num_agree_no_recall <- round(res_sum["0_reader-1-and-AI", ] + res_sum["0_reader-2-and-AI", ])
  num_arbitrate_recall <- round(res_sum["1_sim-reader-3", ])
  num_arbitrate_no_recall <- round(res_sum["0_sim-reader-3", ])
  num_disagree <- num_arbitrate_recall + num_arbitrate_no_recall

  if (num_agree_recall + num_agree_no_recall + num_disagree != num_in) {
    message(stringr::str_interp(
      "Total count is off by ${abs(num_agree_recall + num_agree_no_recall + num_disagree - num_in)} due to rounding. Please make manual adjustment.")
    )
  }

  stringr::str_interp(r"(
    \begin{tikzpicture}[
        auto,
        scale=0.4,
        every node/.style={
            fill=white,
            font=\sffamily,
            block_center_rounded
        },
        align=center,
        node distance=1cm and 0.5cm,
        block_center_rounded/.style ={
            rectangle, rounded corners, draw=black,
            thick, fill=white, text width=8em,
            text centered, minimum height=4em
        },
        reader_block/.style ={
            rectangle, rounded corners, draw=black,
            thick, fill=lightgray, text width=6em,
            text centered, minimum height=3em
        },
        text_center/.style ={
            rectangle, draw=black, rounded corners=0cm,
            thick, fill=white, text centered,
            text width=3.5em, minimum height=1.5em, minimum width=1.5em,
        },
        no_recall_color/.style = {fill={green!25}},
        recall_color/.style = {fill=pink},
        size_two/.style={text width=6.5em, minimum height=3em},
        decoration={markings, mark=at position 0.5 with {\arrow{>}}}
    ]
     % nodes
     \definecolor{almond}{rgb}{0.94, 0.87, 0.8}
     \definecolor{diamond}{rgb}{0.73, 0.90, 1.0}
     \node (start)      {Screening Episodes};
     \coordinate[below=2.98cm of start] (aux1);
     \node (reader_1)   [below left=0.75cm and 0.35cm of aux1, reader_block] {Reader 1};
     \node (reader_2)   [below right=0.75cm and 0.35cm of aux1, reader_block, fill=diamond] {AI Reader};
     \coordinate[below right=0.75cm and 0.35cm of reader_1] (aux2);
     \coordinate[below=0.75cm of aux2] (aux2b);
     \node (reader_3)   [below=1.33cm of aux2b, reader_block] {Reader 3};
     \coordinate[below=0.67cm of reader_3] (aux3);
     \node (recall)     [below left=1cm and 0.45cm of aux3, recall_color, size_two] {Recall};
     \node (no_recall)  [below right=1cm and 0.45cm of aux3, no_recall_color, size_two] {No Recall};
     % edges
     \draw[->]  (start) -- (aux1) -| (reader_1);
     \draw[->]  (aux1) -| (reader_2);
     \draw[postaction={decorate}]  (reader_1) |- (aux2);
     \draw[postaction={decorate}]  (reader_2) |- (aux2);
     \draw[postaction={decorate}] (aux2) -- (aux2b);
     \draw[->] (aux2b) -- (reader_3);
     \draw[->] (aux2b) -| ([xshift=-5cm] recall);
     \draw[->] (aux2b) -| ([xshift=5cm] no_recall);
     \draw[->]  (reader_3) -- (aux3) -| ([xshift=3cm]recall);
     \draw[->]  (aux3) -| ([xshift=-3cm] no_recall);
     % floating textbox
     \node[draw, text_center] (num_in) at (0,\By) {${num_in}};
     \node[draw, text_center] (num_disagree) at (0,\Cy) {${num_disagree}};
     \node[draw, text_center, no_recall_color] (num_agree_no_recall) at (\Cgap,\Cy) {${num_agree_no_recall}};
     \node[draw, text_center, recall_color] (num_agree_recall) at (-\Cgap,\Cy) {${num_agree_recall}};
     \node[draw, text_center, no_recall_color] (num_arbitrate_no_recall) at (\Dgap,\Dy) {${num_arbitrate_no_recall}};
     \node[draw, text_center, recall_color] (num_arbitrate_recall) at (-\Dgap,\Dy) {${num_arbitrate_recall}};
     \node[draw=none, fill=white, text width=5.5cm] (labels) at (0, \headingy) {\LARGE\textbf{AI reader-replacement}};
     \node[draw=none, fill=none, text width=1cm] (labels) at (-6.5, \labely) {\LARGE\textbf{B}};
    \end{tikzpicture}
  )")
}

flowchart_C <- function(res_bp) {
  extract <- `[`

  res_sum <- res_bp$system_summary$summary
  num_in <- sum(res_sum$count)

  num_direct_recall <- res_sum |>
    filter(episode_prediction == 1, by == "AI-reader") |>
    extract("count")

  num_direct_no_recall <- res_sum |>
    filter(episode_prediction == 0, by == "AI-reader") |>
    extract("count")

  num_agree_recall <- res_sum |>
    filter(episode_prediction == 1, by == "reader-1-and-2") |>
    extract("count")

  num_agree_no_recall <- res_sum |>
    filter(episode_prediction == 0, by == "reader-1-and-2") |>
    extract("count")

  num_arbitrate_recall <- res_sum |>
    filter(episode_prediction == 1, by == "reader-3") |>
    extract("count")

  num_arbitrate_no_recall <- res_sum |>
    filter(episode_prediction == 0, by == "reader-3") |>
    extract("count")

  num_disagree <- num_arbitrate_recall + num_arbitrate_no_recall
  num_reads <- num_disagree + num_agree_recall + num_agree_no_recall

  stringr::str_interp(r"(
    \begin{tikzpicture}[
        auto,
        scale=0.4,
        every node/.style={
            fill=white,
            font=\sffamily,
            block_center_rounded
        },
        align=center,
        node distance=0.8cm and 0.4cm,
        block_center_rounded/.style ={
            rectangle, rounded corners, draw=black,
            thick, fill=white, text width=8em,
            text centered, minimum height=4em
        },
        reader_block/.style ={
            rectangle, rounded corners, draw=black,
            thick, fill=lightgray, text width=6em,
            text centered, minimum height=3em
        },
        text_center/.style ={
            rectangle, draw=black, rounded corners=0cm,
            thick, fill=white, text centered,
            text width=3.5em, minimum height=1.5em, minimum width=1.5em,
        },
        no_recall_color/.style = {fill={green!25}},
        recall_color/.style = {fill=pink},
        size_two/.style={text width=6.5em, minimum height=3em},
        decoration={markings, mark=at position 0.5 with {\arrow{>}}}
    ]
     % nodes
     \definecolor{almond}{rgb}{0.94, 0.87, 0.8}
     \definecolor{diamond}{rgb}{0.73, 0.90, 1.0}
     \node (start)      {Screening Episodes};
     \node (screening) [below=1.33cm of start, reader_block, fill=diamond] {AI Reader};
     \coordinate[below=0.67cm of screening] (aux1);
     \node (reader_1)   [below left=0.75cm and 0.35cm of aux1, reader_block] {Reader 1};
     \node (reader_2)   [below right=0.75cm and 0.35cm of aux1, reader_block] {Reader 2};
     \coordinate[below right=0.75cm and 0.35cm of reader_1] (aux2);
     \coordinate[below=0.75cm of aux2] (aux2b);
     \node (reader_3)   [below=1.33cm of aux2b, reader_block] {Reader 3};
     \coordinate[below=0.67cm of reader_3] (aux3);
     \node (recall)     [below left=1cm and 0.45cm of aux3, recall_color, size_two] {Recall};
     \node (no_recall)  [below right=1cm and 0.45cm of aux3, no_recall_color, size_two] {No Recall};
     \coordinate[left=1.8cm of screening] (assist_L1);
     \coordinate[right=1.8cm of screening] (assist_R1);
     \coordinate[below=8cm of assist_L1] (assist_L2) ;
     \coordinate[below=8cm of assist_R1] (assist_R2) ;
     % edges
     \draw[->]  (start) -- (screening);
     \draw[->]  (screening) -- (assist_L1) -- (assist_L2) -| ([xshift=-3cm] recall);
     \draw[->]  (screening) -- (assist_R1) -- (assist_R2) -| ([xshift=3cm] no_recall);
     \draw[->]  (screening) -- (aux1) -| (reader_1);
     \draw[->]  (aux1) -| (reader_2);
     \draw[postaction={decorate}]  (reader_1) |- (aux2);
     \draw[postaction={decorate}]  (reader_2) |- (aux2);
     \draw[postaction={decorate}] (aux2) -- (aux2b);
     \draw[->] (aux2b) -- (reader_3);
     \draw[->] (aux2b) -| ([xshift=-4cm] recall);
     \draw[->] (aux2b) -| ([xshift=4cm] no_recall);
     \draw[->]  (reader_3) -- (aux3) -| ([xshift=3cm] recall);
     \draw[->]  (aux3) -| ([xshift=-3cm] no_recall);
     % floating textbox
     \node[draw, text_center] (num_in) at (0,\Ay) {${num_in}};
     \node[draw, text_center] (num_reads) at (0,\By) {${num_reads}};
     \node[draw, text_center, no_recall_color] (num_direct_no_recall) at (\Bgap,\By) {${num_direct_no_recall}};
     \node[draw, text_center, recall_color] (num_direct_recall) at (-\Bgap,\By) {${num_direct_recall}};

     \node[draw, text_center] (num_disagree) at (0,\Cy) {${num_disagree}};
     \node[draw, text_center, no_recall_color] (num_agree_no_recall) at (\Cgap,\Cy) {${num_agree_no_recall}};
     \node[draw, text_center, recall_color] (num_agree_recall) at (-\Cgap,\Cy) {${num_agree_recall}};

     \node[draw, text_center, no_recall_color] (num_arbitrate_no_recall) at (\Dgap,\Dy) {${num_arbitrate_no_recall}};
     \node[draw, text_center, recall_color] (num_arbitrate_recall) at (-\Dgap,\Dy) {${num_arbitrate_recall}};

     \node[draw=none, fill=white, text width=6cm] (labels) at (0,\headingy) {\LARGE\textbf{AI band-pass}};
     \node[draw=none, fill=none, text width=0cm] (labels) at (-7.5, \labely) {\LARGE\textbf{C}};
    \end{tikzpicture}
  )")
}

flowchart_D <- function(res_masai) {
  res_sum <- scenario_summary(res_masai)$summary
  num_episode <- sum(res_sum$count)
  get_count <- function(x, pred, reader) {
    filter(x, episode_prediction == pred, by == reader)$count
  }

  num_agree_recall <- get_count(res_sum, 1, "reader-1-and-2")
  num_agree_no_recall <- get_count(res_sum, 0, "reader-1-and-2")

  num_arbitrate_recall <- get_count(res_sum, 1, "reader-3")
  num_arbitrate_no_recall <- get_count(res_sum, 0, "reader-3")

  num_single_recall <- get_count(res_sum, 1, "reader-1") + get_count(res_sum, 1, "reader-2")
  num_single_no_recall <- get_count(res_sum, 0, "reader-1") + get_count(res_sum, 0, "reader-2")

  num_single <- num_single_recall + num_single_no_recall
  num_triple <- num_arbitrate_recall + num_arbitrate_no_recall
  num_double <- num_triple + num_agree_recall + num_agree_no_recall

  stopifnot(num_single + num_double == num_episode)

  stringr::str_interp(r"(
    \begin{tikzpicture}[
    auto,
    scale=1.0,
    align=center,
    font=\sffamily,
    no_recall_color/.style = {fill={green!25}},
    recall_color/.style = {fill=pink},
    normalnode/.style={
        rectangle,
        fill=white,
        draw=black,
        thick,
        rounded corners,
        minimum width=2cm,
        minimum height=0.9cm
    },
    squarenode/.style={
        thick,
        rectangle,
        fill=white,
        draw=black,
        minimum width=1.4cm,
        minimum height=0.5cm
    },
    decoration={markings, mark=at position 0.5 with {\arrow{>}}}
]
    \definecolor{diamond}{rgb}{0.73, 0.90, 1.0}
    % Nodes
    \node[normalnode] (A) at (0, 0) {Screening Episodes};
    \node[normalnode, fill=diamond] (B) at (0, -2) {AI Reader};
    \node[normalnode, fill=lightgray] (C_left) at (-2, -4) {Reader 1};
    \coordinate (C_right) at (2, -3); % C_right is a coordinate
    \coordinate (D) at (-2, -10); % D is a coordinate
    \node[normalnode, fill=lightgray] (E) at (0.75, -4) {Reader 1}; % Updated position of E
    \node[normalnode, fill=lightgray] (F) at (3.25, -4) {Reader 2}; % Updated position of F
    \coordinate (G) at (2, -5);   % G is a coordinate
    \coordinate (J-above) at (2, -5.5);
    \node[normalnode, fill=lightgray] (J) at (2, -7) {Reader 3};
    \coordinate (J-below) at (2, -8);
    \node[normalnode, recall_color] (Y) at (0, -10) {Recall}; % Moved Y down
    \node[normalnode, no_recall_color] (Z) at (4, -11) {No Recall};  % Moved Z down

    % Edges
    \draw[->] (A) -- (B); % Squared arrow
    \draw[->] (B) -| (C_left); % Squared arrow
    \draw[->] (B) -| (C_right) -| (E); % Squared arrow
    \draw[->] (B) -| (C_right) -| (F); % Squared arrow
    \draw[->] (C_left) -- (D) -- ([xshift=-1cm] Y); % Squared arrow
    \draw[->] (C_left) -- ([yshift=-1cm] D) -- (Z); % Squared arrow
    \draw[-, postaction={decorate}] (E) |- (G);
    \draw[-, postaction={decorate}] (F) |- (G);
    \draw[->] (G) -- (J);
    \draw[->] (G) -- (J-above);
    \draw[->] (J) -- (J-below) -| ([xshift=2cm] Y); % Squared arrow
    \draw[->] (J) -- (J-below) -| ([xshift=-2cm] Z); % Squared arrow
    \draw[->] (G) -- (J-above) -| (Y); % Squared arrow
    \draw[->] (G) -- (J-above) -| ([xshift=1.5cm]Z); % Squared arrow

    % New types of nodes with squared corners
    \node[squarenode] (AB-node) at (0, -1) {${num_episode}}; % Between A and B
    \node[squarenode] (C-left-above) at (-2, -3) {${num_single}}; % Above C-left
    \node[squarenode] (C-right-node) at (C_right) {${num_double}}; % At C-right
    \node[squarenode] (J-above-node) at (2, -6) {${num_triple}}; % Between J-above and J
    \node[squarenode, recall_color] (J-above-node-left) at (0, -6) {${num_agree_recall}};
    \node[squarenode, no_recall_color] (J-above-node-right) at (4, -6) {${num_agree_no_recall}};
    \node[squarenode, recall_color] (reader-3-recall) at (1, -8) {${num_arbitrate_recall}}; %
    \node[squarenode, no_recall_color] (reader-3-no-recall) at (3.2, -8) {${num_arbitrate_no_recall}}; % Below J-below
    \node[squarenode, recall_color] (reader-1-recall) at (Y) [left=1.5cm]{${num_single_recall}};
    \node[squarenode, no_recall_color] (reader-1-no-recall) at (reader-1-recall) [below=0.725cm]{${num_single_no_recall}};

    % Heading
    \node[draw=none, fill=white, text width=6cm] (labels) at (0, 1.5) {\LARGE\textbf{MASAI}};
    \node[draw=none, fill=none, text width=0cm] (labels) at (-2, 2.5) {\LARGE\textbf{D}};
\end{tikzpicture}
  )")
}


# Scenario ROC curves ----------------------------------------------------------
masai_curve <- function(qs = seq(0, 0.99, 0.01)) {
  pb <- txtProgressBar(0, length(qs), style = 3)
  env <- new.env()
  env$i <- 0
  res <- do.call(rbind, lapply(qs, function(s) {
    env$i <- env$i + 1
    masai_threshold <- quantile(model_df$episode_prediction, s)
    res_masai <- AI_masai(accession_df, reader_df, model_df, masai_threshold)
    res_masai_econ <- BRAIxMOP:::masai_economics(scenario_summary(res_masai))
    setTxtProgressBar(pb, env$i)
    unlist(res_masai_econ)
  }))
  data.frame(res)
}

eff <- function(df0) {
  df0 |>
    efficient_boundary_2() |>
    filter(is_boundary == TRUE) |>
    arrange(FPR)
}

efficient_boundary_2 <- function(df0) {
  if (nrow(df0) <= 200) {
    return(filter(efficient_boundary(df0), is_boundary == TRUE))
  }

  df0 <- df0[sample(nrow(df0)), ]
  size <- 50
  split_pt <- seq(1, nrow(df0), size)
  message(glue::glue(
    "Splitting {nrow(df0)} rows into {length(split_pt)} parts to compute the frontier"
  ))
  pb <- txtProgressBar(min = 0, max = length(split_pt), style = 3)
  i <- 0
  df1 <- do.call(rbind, lapply(split_pt, function(ind) {
    result <- efficient_boundary(df0[ind:min(ind + size - 1, nrow(df0)), ])
    i <<- i + 1
    setTxtProgressBar(pb, i)

    filter(result, is_boundary == TRUE)
  }))

  if (nrow(df1) == nrow(df0)) {
    return(filter(efficient_boundary(df0), is_boundary == TRUE))
  }

  efficient_boundary_2(df1)
}

efficient_boundary <- function(df0) {
  stopifnot(all(c("TPR", "FPR") %in% colnames(df0)))

  is_better_than <- function(pt, pt2) {
    (pt$TPR >= pt2$TPR) && (pt$FPR <= pt2$FPR)
  }

  some_point_is_better_than <- function(pt, ref_group) {
    1:nrow(ref_group) |>
      purrr::map_lgl(~is_better_than(ref_group[.x, ], pt)) |>
      any()
  }

  df1 <- df0 |> mutate(is_boundary = FALSE)
  for (i in 1:nrow(df1)) {
    # If there is a better point, then the current point is not at the efficient boundary
    df1$is_boundary[i] <- !some_point_is_better_than(df1[i, ], df1[-i, ])
  }
  df1
}


# Supplementary tables ---------------------------------------------------------
supp_table_repl <- function(res_econ, baseline_econ) {
  f <- function(x) data.frame(t(unlist(x)))

  dollar_format <- function(x) {
    y <- as.numeric(x)
    if (y > 0) {
      paste0("\\$", formatC(y, format="f", digits=0, big.mark=","))
    } else {
      paste0("-\\$", formatC(abs(y), format="f", digits=0, big.mark=","))
    }
  }

  del <- function(x) {
    ifelse(x > 0, sprintf("+%s", x), sprintf("%s", x))
  }

  p <- function(x) {
    diff_percent <- format(round(x * 100, 1), nsmall = 1)
    ifelse(diff_percent > 0,
           sprintf("+%s\\%%", diff_percent),
           sprintf("%s\\%%", diff_percent))
  }

  col_1 <- unlist(baseline_econ)
  col_2 <- unlist(res_econ)
  col_3 <- col_2 - col_1
  col_4 <- sapply(col_3 / col_1, p)

  col_2[1:16] <- round(col_2[1:16])
  col_3[1:16] <- round(col_3[1:16])
  m0 <- cbind(col_1, col_2, col_3, col_4)[-c(8:11, 21:23), ]
  m0[1:16, 3] <- m0[1:16, 3] |> sapply(del)
  m0[17:19, 1:3] <- m0[17:19, 1:3] |>
    sapply(dollar_format) |>
    matrix(nrow = 3, ncol = 3)

  stringr::str_interp(r"(
  \begin{table}[H]
  % \captionsetup[table]{labelformat=empty,skip=1pt}
  \begin{longtable}{rrrrr}
  \multicolumn{5}{c}{{\large AI reader replacement}}\\
  % \multicolumn{5}{c}{{\small AI improved individual reader}}\\
  \toprule
  \multicolumn{1}{l}{} & Baseline & Scenario & delta \# & delta \% \\
  \midrule
  \multicolumn{1}{l}{Episodes} \\
  \midrule
  episodes & ${m0[1,1]} & ${m0[1,2]} &  &  \\
  cancers & ${m0[2,1]} & ${m0[2,2]} &  &  \\
  normals & ${m0[3,1]} & ${m0[3,2]} &  &  \\
  \midrule
  \multicolumn{1}{l}{Benefit} \\
  \midrule
  TN & ${m0[4,1]} & ${m0[4,2]} & ${m0[4,3]} & ${m0[4,4]} \\
  TP & ${m0[5,1]} & ${m0[5,2]} & ${m0[5,3]} & ${m0[5,4]} \\
  \midrule
  \multicolumn{1}{l}{Harms} \\
  \midrule
  FN (Missed cancer) & ${m0[6,1]} & ${m0[6,2]} & ${m0[6,3]} & ${m0[6,4]} \\
  FP (Unnecessary recall) & ${m0[7,1]} & ${m0[7,2]} & ${m0[7,3]} & ${m0[7,4]} \\
  \midrule
  \multicolumn{1}{l}{Cost drivers - Human readings} \\
  \midrule
  None & ${m0[8,1]} & ${m0[8,2]} &  &  \\
  Single & ${m0[9,1]} & ${m0[9,2]} &  &  \\
  Double & ${m0[10,1]} & ${m0[10,2]} &  &  \\
  Triple & ${m0[11,1]} & ${m0[11,2]} &  &  \\
  Total reads & ${m0[12,1]} & ${m0[12,2]} & ${m0[12,3]} & ${m0[12,4]} \\
  Reader 3 reads & ${m0[13,1]} & ${m0[13,2]} & ${m0[13,3]} & ${m0[13,4]} \\
  \midrule
  \multicolumn{1}{l}{Cost drivers - Assessments} \\
  \midrule
  Necessary & ${m0[14,1]} & ${m0[14,2]} &  &  \\
  Unnecessary & ${m0[15,1]} & ${m0[15,2]} &  &  \\
  Total & ${m0[16,1]} & ${m0[16,2]} & ${m0[16,3]} & ${m0[16,4]} \\
  \midrule
  \multicolumn{1}{l}{Cost drivers - Total cost} \\
  \midrule
  Reading cost & ${m0[17,1]} & ${m0[17,2]} & ${m0[17,3]} & ${m0[17,4]} \\
  Assessment cost & ${m0[18,1]} & ${m0[18,2]} & ${m0[18,3]} & ${m0[18,4]} \\
  Total cost & ${m0[19,1]} & ${m0[19,2]} & ${m0[19,3]} & ${m0[19,4]} \\
  \bottomrule
  \caption{The economics analysis of the AI reader replacement. Numbers are rounded to the nearest integer. Percentages are rounded to 1 decimal place.}
  \label{tab:econ-ai-replacement-matched}
  \end{longtable}
  \end{table}
  )")
}

supp_table_bp <- function(res_econ, baseline_econ) {
  f <- function(x) data.frame(t(unlist(x)))

  dollar_format <- function(x) {
    y <- as.numeric(x)
    if (y > 0) {
      paste0("\\$", formatC(y, format="f", digits=0, big.mark=","))
    } else {
      paste0("-\\$", formatC(abs(y), format="f", digits=0, big.mark=","))
    }
  }

  del <- function(x) {
    ifelse(x > 0, sprintf("+%s", x), sprintf("%s", x))
  }

  p <- function(x) {
    diff_percent <- format(round(x * 100, 1), nsmall = 1)
    ifelse(diff_percent > 0,
           sprintf("+%s\\%%", diff_percent),
           sprintf("%s\\%%", diff_percent))
  }

  col_1 <- unlist(baseline_econ)
  col_2 <- unlist(res_econ)
  col_3 <- col_2 - col_1
  col_4 <- sapply(col_3 / col_1, p)
  m0 <- cbind(col_1, col_2, col_3, col_4)[-c(8:11, 21:23), ]
  m0[1:16, 3] <- m0[1:16, 3] |> sapply(del)
  m0[17:19, 1:3] <- m0[17:19, 1:3] |>
    sapply(dollar_format) |>
    matrix(nrow = 3, ncol = 3)

  stringr::str_interp(r"(
  \begin{table}[H]
  % \captionsetup[table]{labelformat=empty,skip=1pt}
  \begin{longtable}{rrrrr}
  \multicolumn{5}{c}{{\large AI band-pass}}\\
  % \multicolumn{5}{c}{{\small AI improved individual reader}}\\
  \toprule
  \multicolumn{1}{l}{} & Baseline & Scenario & delta \# & delta \% \\
  \midrule
  \multicolumn{1}{l}{Episodes} \\
  \midrule
  episodes & ${m0[1,1]} & ${m0[1,2]} &  &  \\
  cancers & ${m0[2,1]} & ${m0[2,2]} &  &  \\
  normals & ${m0[3,1]} & ${m0[3,2]} &  &  \\
  \midrule
  \multicolumn{1}{l}{Benefit} \\
  \midrule
  TN & ${m0[4,1]} & ${m0[4,2]} & ${m0[4,3]} & ${m0[4,4]} \\
  TP & ${m0[5,1]} & ${m0[5,2]} & ${m0[5,3]} & ${m0[5,4]} \\
  \midrule
  \multicolumn{1}{l}{Harms} \\
  \midrule
  FN (Missed cancer) & ${m0[6,1]} & ${m0[6,2]} & ${m0[6,3]} & ${m0[6,4]} \\
  FP (Unnecessary recall) & ${m0[7,1]} & ${m0[7,2]} & ${m0[7,3]} & ${m0[7,4]} \\
  \midrule
  \multicolumn{1}{l}{Cost drivers - Human readings} \\
  \midrule
  None & ${m0[8,1]} & ${m0[8,2]} &  &  \\
  Single & ${m0[9,1]} & ${m0[9,2]} &  &  \\
  Double & ${m0[10,1]} & ${m0[10,2]} &  &  \\
  Triple & ${m0[11,1]} & ${m0[11,2]} &  &  \\
  Total reads & ${m0[12,1]} & ${m0[12,2]} & ${m0[12,3]} & ${m0[12,4]} \\
  Reader 3 reads & ${m0[13,1]} & ${m0[13,2]} & ${m0[13,3]} & ${m0[13,4]} \\
  \midrule
  \multicolumn{1}{l}{Cost drivers - Assessments} \\
  \midrule
  Necessary & ${m0[14,1]} & ${m0[14,2]} &  &  \\
  Unnecessary & ${m0[15,1]} & ${m0[15,2]} &  &  \\
  Total & ${m0[16,1]} & ${m0[16,2]} & ${m0[16,3]} & ${m0[16,4]} \\
  \midrule
  \multicolumn{1}{l}{Cost drivers - Total cost} \\
  \midrule
  Reading cost & ${m0[17,1]} & ${m0[17,2]} & ${m0[17,3]} & ${m0[17,4]} \\
  Assessment cost & ${m0[18,1]} & ${m0[18,2]} & ${m0[18,3]} & ${m0[18,4]} \\
  Total cost & ${m0[19,1]} & ${m0[19,2]} & ${m0[19,3]} & ${m0[19,4]} \\
  \bottomrule
  \caption{The economics analysis of the AI band-pass screening scenario. Numbers are rounded to the nearest integer. Percentages are rounded to 1 decimal place.}
  \label{tab:econ-ai-band-pass}
  \end{longtable}
  \end{table}
  )")
}

supp_table_masai <- function(res_econ, baseline_econ) {
  f <- function(x) data.frame(t(unlist(x)))

  dollar_format <- function(x) {
    y <- as.numeric(x)
    if (y > 0) {
      paste0("\\$", formatC(y, format="f", digits=0, big.mark=","))
    } else {
      paste0("-\\$", formatC(abs(y), format="f", digits=0, big.mark=","))
    }
  }

  del <- function(x) {
    ifelse(x > 0, sprintf("+%s", x), sprintf("%s", x))
  }

  p <- function(x) {
    diff_percent <- format(round(x * 100, 1), nsmall = 1)
    ifelse(diff_percent > 0,
           sprintf("+%s\\%%", diff_percent),
           sprintf("%s\\%%", diff_percent))
  }

  col_1 <- unlist(baseline_econ)
  col_2 <- unlist(res_econ)
  col_3 <- col_2 - col_1
  col_4 <- sapply(col_3 / col_1, p)
  m0 <- cbind(col_1, col_2, col_3, col_4)[-c(8:11, 21:23), ]
  m0[1:16, 3] <- m0[1:16, 3] |> sapply(del)
  m0[17:19, 1:3] <- m0[17:19, 1:3] |>
    sapply(dollar_format) |>
    matrix(nrow = 3, ncol = 3)

  stringr::str_interp(r"(
  \begin{table}[H]
  % \captionsetup[table]{labelformat=empty,skip=1pt}
  \begin{longtable}{rrrrr}
  \multicolumn{5}{c}{{\large MASAI}}\\
  % \multicolumn{5}{c}{{\small AI improved individual reader}}\\
  \toprule
  \multicolumn{1}{l}{} & Baseline & Scenario & delta \# & delta \% \\
  \midrule
  \multicolumn{1}{l}{Episodes} \\
  \midrule
  episodes & ${m0[1,1]} & ${m0[1,2]} &  &  \\
  cancers & ${m0[2,1]} & ${m0[2,2]} &  &  \\
  normals & ${m0[3,1]} & ${m0[3,2]} &  &  \\
  \midrule
  \multicolumn{1}{l}{Benefit} \\
  \midrule
  TN & ${m0[4,1]} & ${m0[4,2]} & ${m0[4,3]} & ${m0[4,4]} \\
  TP & ${m0[5,1]} & ${m0[5,2]} & ${m0[5,3]} & ${m0[5,4]} \\
  \midrule
  \multicolumn{1}{l}{Harms} \\
  \midrule
  FN (Missed cancer) & ${m0[6,1]} & ${m0[6,2]} & ${m0[6,3]} & ${m0[6,4]} \\
  FP (Unnecessary recall) & ${m0[7,1]} & ${m0[7,2]} & ${m0[7,3]} & ${m0[7,4]} \\
  \midrule
  \multicolumn{1}{l}{Cost drivers - Human readings} \\
  \midrule
  None & ${m0[8,1]} & ${m0[8,2]} &  &  \\
  Single & ${m0[9,1]} & ${m0[9,2]} &  &  \\
  Double & ${m0[10,1]} & ${m0[10,2]} &  &  \\
  Triple & ${m0[11,1]} & ${m0[11,2]} &  &  \\
  Total reads & ${m0[12,1]} & ${m0[12,2]} & ${m0[12,3]} & ${m0[12,4]} \\
  Reader 3 reads & ${m0[13,1]} & ${m0[13,2]} & ${m0[13,3]} & ${m0[13,4]} \\
  \midrule
  \multicolumn{1}{l}{Cost drivers - Assessments} \\
  \midrule
  Necessary & ${m0[14,1]} & ${m0[14,2]} &  &  \\
  Unnecessary & ${m0[15,1]} & ${m0[15,2]} &  &  \\
  Total & ${m0[16,1]} & ${m0[16,2]} & ${m0[16,3]} & ${m0[16,4]} \\
  \midrule
  \multicolumn{1}{l}{Cost drivers - Total cost} \\
  \midrule
  Reading cost & ${m0[17,1]} & ${m0[17,2]} & ${m0[17,3]} & ${m0[17,4]} \\
  Assessment cost & ${m0[18,1]} & ${m0[18,2]} & ${m0[18,3]} & ${m0[18,4]} \\
  Total cost & ${m0[19,1]} & ${m0[19,2]} & ${m0[19,3]} & ${m0[19,4]} \\
  \bottomrule
  \caption{The economics analysis of the MASAI scenario. Numbers are rounded to the nearest integer. Percentages are rounded to 1 decimal place.}
  \label{tab:econ-ai-masai}
  \end{longtable}
  \end{table}
  )")
}

supp_table_sensitivity_test <- function(baseline_econ) {
  original_sims <- readRDS(output("simulation_replacement_original.RDS"))
  second_sims <- readRDS(output("simulation_replacement_second_reader_arbiter.RDS"))
  mixed_sims <- readRDS(output("simulation_replacement_mixed_third_reader.RDS"))


  f1 <- \(x) format(round(100 * x, 1), nsmall = 1L)
  f2 <- \(x) format(round(x), big.mark = ",")
  f3 <- \(x) paste0("\\$", formatC(as.numeric(x), format="f", digits=0, big.mark=","))
  df_to_list <- function(sim_df0) {
    df0 <- data.frame(t(colMeans(sim_df0)))

    list(
      sens = f1(df0$benefit_rate.TP),
      spec = f1(df0$benefit_rate.TN),
      third_read = f2(df0$cost_drivers.human_readings.reader_3_reads),
      total_read = f2(df0$cost_drivers.human_readings.total_reads),
      total_cost = f3(df0$cost.total_cost.total),
      sens_ci = sprintf(
        "(%s, %s)",
        f1(mean(sim_df0$benefit_rate.TP) - 2 * sd(sim_df0$benefit_rate.TP)),
        f1(mean(sim_df0$benefit_rate.TP) + 2 * sd(sim_df0$benefit_rate.TP))
      ),
      spec_ci = sprintf(
        "(%s, %s)",
        f1(mean(sim_df0$benefit_rate.TN) - 2 * sd(sim_df0$benefit_rate.TN)),
        f1(mean(sim_df0$benefit_rate.TN) + 2 * sd(sim_df0$benefit_rate.TN))
      )
    )
  }


  baseline <- df_to_list(data.frame(t(unlist(baseline_econ))))
  original <- df_to_list(data.frame(original_sims$econ))
  second <- df_to_list(data.frame(second_sims$econ))
  mixed <- df_to_list(data.frame(mixed_sims$econ))


  stringr::str_interp(r"(
    \begin{tabular}{cccccc}
Reader 3 &  Sensitivity (\%) & Specificity (\%) & Reader 3 / Total reads & Total cost\\
\hline
% Baseline  & ${baseline$sens} & ${baseline$spec} & ${baseline$third_read} & ${baseline$total_read} & ${baseline$total_cost}\\
Fully simulated
& ${original$sens}
& ${original$spec}
& \multirow{2}{*}{${original$third_read} / ${original$total_read}}
& \multirow{2}{*}{${original$total_cost}}\\
(original)
& ${original$sens_ci}
& ${original$spec_ci}
&&&\\
Second reader as
& ${second$sens}
& ${second$spec}
& \multirow{2}{*}{${second$third_read} / ${second$total_read}}
& \multirow{2}{*}{${second$total_cost}}\\
third reader
& ${second$sens_ci}
& ${second$spec_ci}
&&&\\
Mixed simulated
& ${mixed$sens}
& ${mixed$spec}
& \multirow{2}{*}{${mixed$third_read} / ${mixed$total_read}}
& \multirow{2}{*}{${mixed$total_cost}}\\
& ${mixed$sens_ci}
& ${mixed$spec_ci}
&&&\\
    \end{tabular}
  )")
}


# Hypothesis testing -----------------------------------------------------------
# Pair samples by 'episode_id'
pair_scenarios <- function(scenario_1_df, scenario_2_df) {
  paired_samples <- left_join(
    scenario_1_df |>
      select(episode_id, episode_outcome, episode_prediction) |>
      rename(scenario_1_prediction = episode_prediction) |>
      mutate(episode_is_cancer = (episode_outcome %in% c(1, 2)) * 1.0,
             scenario_1_correct = scenario_1_prediction == episode_is_cancer),
    scenario_2_df |>
      select(episode_id, episode_outcome, episode_prediction) |>
      rename(scenario_2_prediction = episode_prediction) |>
      mutate(episode_is_cancer = (episode_outcome %in% c(1, 2)) * 1.0,
             scenario_2_correct = scenario_2_prediction == episode_is_cancer)
  )
  # unique(paired_samples$episode_outcome)
  # unique(paired_samples$episode_is_cancer)
  # head(paired_samples)
  sens <- paired_samples |>
    filter(episode_is_cancer == TRUE) |>
    select(scenario_1_correct, scenario_2_correct) |>
    table() |>
    as.matrix()

  spec <- paired_samples |>
    filter(episode_is_cancer == FALSE) |>
    select(scenario_1_correct, scenario_2_correct) |>
    table() |>
    as.matrix()

  list(data = paired_samples, sens_tbl = sens, spec_tbl = spec)
}

# McNemar test
run_test <- function(paired) {
  sens <- paired$sens_tbl
  spec <- paired$spec_tbl

  message("Testing sensitivity")

  if (max(sens[1,2], sens[2,1]) == 0) {
    message("Nothing to test - the two scenarios have identical performance.")
    sens_mt <- sens_bt <- list(p.value = NA)
  } else {
    message(sprintf("Testing scenario %s is superior",
                    ifelse(sens[2,1] >= sens[1,2], 1, 2)))
    print(sens)
    print(sens_mt <- mcnemar.test(sens, correct = TRUE))
    sens_bt <- binom.test(max(sens[1,2], sens[2,1]),
                          n = sens[1,2] + sens[2,1],
                          p = 0.5,
                          alternative = "greater") |> print()
  }

  message("Testing specificity")
  if (max(spec[1,2], spec[2,1]) == 0) {
    message("Nothing to test - the two scenarios have identical performance.")
    spec_mt <- spec_bt <- list(p.value = NA)
  } else {
    message(sprintf("Testing scenario %s is superior",
                    ifelse(spec[2,1] >= spec[1,2], 1, 2)))
    print(spec)
    print(spec_mt <- mcnemar.test(spec, correct = TRUE))
    spec_bt <- binom.test(max(spec[1,2], spec[2,1]),
                          n = spec[1,2] + spec[2,1],
                          p = 0.5,
                          alternative = "greater") |> print()
  }

  cat(paste(as.character(c(
    "Sensitivity",
    as.vector(t(sens)),
    ifelse(sens_mt$p.value <= 0.05, "Significant difference", "No difference"),
    sens_mt$p.value,
    ifelse(sens_bt$p.value <= 0.05, "Superior", "Null"),
    sens_bt$p.value
  ))), sep = " & ")
  cat("\n")

  cat(paste(as.character(c(
    "Specificity",
    as.vector(t(spec)),
    ifelse(spec_mt$p.value <= 0.05, "Significant difference", "No difference"),
    spec_mt$p.value,
    ifelse(spec_bt$p.value <= 0.05, "Superior", "Null"),
    spec_bt$p.value
  ))), sep = " & ")
}


# Testing consistency between client split and prospective splits (both w.r.t. radiologists) ----
# Comparison against radiologists performance
mutate_performance <- function(df0) {
  df0 |>
    mutate(episode_is_cancer = episode_outcome %in% c(1, 2),
           ai_correct = episode_prediction == episode_is_cancer,
           reader_correct = individual_recall == episode_is_cancer)
}

sens_matrix <- function(x) {
  x |>
    mutate_performance() |>
    filter(episode_is_cancer == TRUE) |>
    select(ai_correct, reader_correct) |>
    table() |>
    as.matrix()

}

spec_matrix <- function(x) {
  x |>
    mutate_performance() |>
    filter(episode_is_cancer == FALSE) |>
    select(ai_correct, reader_correct) |>
    table() |>
    as.matrix()
}

test_table <- function(x, split, metrics) {
  print(sprintf("Data: %s; Metrics: %s", split, metrics))

  versus <- rev(as.vector(t(x)))

  rename_df <- function(df0) {
    colnames(df0) <- c("Correct-Correct", "Correct-Incorrect",
                       "Incorrect-Correct", "Incorrect-Incorrect",
                       "Result", "p-value")
    cbind(data.frame(data = split, metrics = metrics), df0)
  }

  test_stat <- mcnemar.test(x, correct = TRUE)  # Reject

  if (test_stat$p.value >= 0.05) {
    df0 <- data.frame(t(versus), "AI non-inferior", test_stat$p.value)
    return(rename_df(df0))
  }

  # Has effect (/ performance difference)
  test_stat_2 <- binom.test(x[2,1],
                            n = x[1,2] + x[2,1],
                            p = 0.5,
                            alternative = "greater")
  if (test_stat_2$p.value < 0.05) {
    return(rename_df(data.frame(t(versus), "AI superior", test_stat_2$p.value)))
  }

  test_stat_3 <- binom.test(x[2,1],
                            n = x[1,2] + x[2,1],
                            p = 0.5,
                            alternative = "less")
  if (test_stat_3$p.value < 0.05) {
    return(rename_df(data.frame(t(versus), "AI inferior", test_stat_3$p.value)))
  }

  stop("Please check your results.")
}

`%+%` <- paste0

test_client_split_against_radiologists_SDC <- function(model_df, reader_df, seed = 1234) {
  if (!is.null(seed)) set.seed(seed)

  thresholded_model_df <- model_df |>
    apply_manufacturer_threshold(readRDS(output("AI_performance.RDS"))$threshold) |>
    select(episode_id, episode_prediction, episode_outcome)

  reader_episode_df <- reader_df |>
    filter(reader_number %in% c(1,2)) |>
    group_by(episode_id) |>
    slice_sample(n = 1) |>
    select(episode_id, reader_number, individual_recall)

  paired_episode_df <- dplyr::left_join(thresholded_model_df, reader_episode_df, by = "episode_id")
  paired_episode_df <- paired_episode_df |> dplyr::filter(episode_outcome != 2)

  thresholded_model_df |> confusion_matrix() |> as.data.frame() |> print()
  sens <- paired_episode_df |> sens_matrix()
  spec <- paired_episode_df |> spec_matrix()

  list(data = paired_episode_df,
       sens_tbl = sens,
       spec_tbl = spec)
}

test_prospective_split_against_radiologists <- function(model_df, reader_df, seed = 1234) {
  if (!is.null(seed)) set.seed(seed)

  mask_code <- function(s, key) {
    set.seed(key)
    dict <- sample(c(LETTERS, letters, 0:9, strsplit("-!@#$%^&*()_+{}:<>?,./ ", split="")[[1]]))
    dict_map <- setNames(seq_along(dict), dict)
    sprintf(
      paste0("%0", 2, "d"),
      dict_map[strsplit(s, "")[[1]]]
    ) |>
      paste0(collapse = "")
  }

  unmask_code <- function(n, key) {
    set.seed(key)
    dict <- sample(c(LETTERS, letters, 0:9, strsplit("-!@#$%^&*()_+{}:<>?,./ ", split="")[[1]]))
    dict_rev_map <- setNames(dict, seq_along(dict))
    cph <- unlist(strsplit(n, "(?<=\\G..)", perl = TRUE)) |> as.numeric()
    paste0(dict_rev_map[cph], collapse = "")
  }

  retro_threshold <- readRDS(output("AI_performance.RDS"))$threshold
  thresholded_model_df <- model_df |>
    # Need to make sure the manufacturer exists and is named correctly
    mutate(manufacturer = sapply(image_manufacturer, function(x) {
      if (x == unmask_code("33387138670427667904164148", 1234)) {
        return(unmask_code("33387138670427", 1234))
      }
      if (x == unmask_code("60040557051060", 1234)) {
        return(unmask_code("600405570510607951794031", 1234))
      }
      stop("Unmapped manufacturer: ", x)
    })) |>
    apply_manufacturer_threshold(retro_threshold) |>
    select(episode_id, episode_prediction, episode_outcome)

  reader_episode_df <- reader_df |>
    filter(reader_number %in% c(1,2)) |>
    group_by(episode_id) |>
    slice_sample(n = 1) |>
    select(episode_id, reader_number, individual_recall)

  paired_episode_df <- dplyr::left_join(thresholded_model_df, reader_episode_df, by = "episode_id")

  thresholded_model_df |> confusion_matrix() |> as.data.frame() |> print()
  sens <- paired_episode_df |> sens_matrix()
  spec <- paired_episode_df |> spec_matrix()

  list(data = paired_episode_df, sens_tbl = sens, spec_tbl = spec)
}
