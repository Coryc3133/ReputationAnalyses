rep_consensus_group_mod_builder <- function(p1_reports, p2_reports, groups, n_triads = length(p1_reports),
                                            n_p1s_per_p2s = 1, n_p2s_per_p1s = 1){

  if(n_triads > 0 &
     n_p1s_per_p2s == 1 &
     n_p2s_per_p1s == 1){
    rep_consensus_model <- rep_consensus_builder(p1_reports, p2_reports, n_triads = length(p1_reports),
                                               n_p1s_per_p2s = 1, n_p2s_per_p1s = 1)
    #group1 <- lavaanify(rep_consensus_model)
    # get key linking relation to label
    #relation_label <- lavaanify(rep_consensus_model$model) %>%
     # dplyr::select(lhs, op, rhs, label)

    #lavaanify(rep_consensus_model$model) %>%
     # dplyr::select(-label) %>%
      #dplyr::left_join(relation_label_key)

    #orig_param_table <- lavaanify(rep_consensus_model$model, ngroups = length(groups))

    #single_group_labs <- orig_param_table %>%
    #  distinct(label)

    #equal_param_table <- orig_param_label %>%
    #  mutate(label = single_group_labs)

    n_groups <- length(groups)

    param_labs <- rep_consensus_model$model %>%
      lavaanify() %>%
      dplyr::select(label) %>%
      dplyr::filter(str_detect(label, "")) %>%
      tidyr::crossing(groups) %>%
      dplyr::mutate(groups = str_extract(groups, "....")) %>%
      tidyr::unite(grp_labs, groups, label, remove = FALSE) %>%
      dplyr::select(-groups) %>%
      dplyr::distinct() %>%
      split(.$label) %>%
      purrr::map(~dplyr::select(., -label)) %>%
      purrr::map(~str_flatten(.)) %>%
      unlist() %>%
      sort(decreasing = TRUE)

    # Relative Elevation are a little different
    # since they are defined parameters. We'll
    # extract those labels and deal with them separately.
    rel_el_labs <- rep_consensus_model$model %>%
      lavaanify() %>%
      dplyr::select(label) %>%
      dplyr::filter(str_detect(label, "")) %>%
      tidyr::crossing(groups) %>%
      dplyr::mutate(groups = str_extract(groups, "....")) %>%
      tidyr::unite(grp_labs, groups, label, remove = FALSE) %>%
      dplyr::select(-groups) %>%
      dplyr::distinct() %>%
      split(.$label) %>%
      purrr::map(~dplyr::select(., -label)) %>%
      tibble::as_tibble() %>%
      dplyr::select(dplyr::contains("rel_el"))

    # Relative Elevation requires subtracting
    # the pooled intercepts, so we need to separate
    # out a df of those too.
    int_labs <- rep_consensus_model$model %>%
      lavaanify() %>%
      dplyr::select(label) %>%
      dplyr::filter(str_detect(label, "")) %>%
      tidyr::crossing(groups) %>%
      dplyr::mutate(groups = str_extract(groups, "....")) %>%
      tidyr::unite(grp_labs, groups, label, remove = FALSE) %>%
      dplyr::select(-groups) %>%
      dplyr::distinct() %>%
      split(.$label) %>%
      purrr::map(~dplyr::select(., -label)) %>%
      tibble::as_tibble() %>%
      dplyr::select(dplyr::contains("int_"))

    # code for 1 triad - much simpler
    if(n_triads == 1){

      model <-
        # hearsay (P1-P2) consensus
        paste(paste(p1_reports, param_labs["hc"], "*", p2_reports),
              # intercepts
              paste(p1_reports, "~", param_labs["int_p1"], "*1"),
              paste(p2_reports, "~", param_labs["int_p2"], "*1"),

              # variances
              paste(p1_reports, "~~", param_labs["v_p1"], "*", p1_reports),

              paste(p2_reports, "~~", param_labs["v_p2"], "*", p2_reports, "\n"), sep = "\n") %>%
        stringr::str_flatten(collaps = "\n")
    }
    # code for 2 triads
    if(n_triads > 1 &
       n_p1s_per_p2s == 1 &
       n_p2s_per_p1s == 1){
      # create empty model
      model <- ""
      for(i in 1:n_triads){
        # cross-target correlations
        if(i < n_triads){
          prev_i <- i:1
          m   <- paste(p1_reports[i], param_labs["m"], "*", p2_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
          rec <- paste(p1_reports[i], param_labs["rec"], "*", p1_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
          h   <-  paste(p2_reports[i],param_labs["h"], "*", p2_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          xtrs <- paste(m, rec, h, sep = "\n")
          model <- paste(model, xtrs)}}
      hc <- paste(p1_reports, param_labs["hc"], "*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      int_p1 <- paste(p1_reports, "~", param_labs["int_p1"], "*1") %>% stringr::str_flatten(collapse = "\n")
      int_p2 <- paste(p2_reports, "~", param_labs["int_p2"], "*1") %>% stringr::str_flatten(collapse = "\n")
      v_p1 <- paste(p1_reports, "~~", param_labs["v_p1"], "*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
      v_p2 <- paste(p2_reports, "~~", param_labs["v_p2"], "*", p2_reports) %>% stringr::str_flatten(collapse = "\n")

      model <- paste(hc, model,  int_p1, int_p2, v_p1, v_p2, sep = "\n")
    }
    # define relative elevation for each group
    # and paste that into the model.
    for(i in 1:n_groups){
      rel_el <- paste(rel_el_labs[[1]][i,], ":=", "1*", int_labs[[1]][i,], "- (1*", int_labs[[2]][i,], ")")
      model <- paste(model, rel_el, sep = "\n")
    }
    # Put the model info together.
    rep_model_info <- tibble::as_tibble(list(model_type = "Simple Hearsay Consensus (P1-P2) with Group Moderator",
                                             ex_triads = n_triads,
                                             n_groups = n_groups,
                                             p1s_per_p2s = n_p1s_per_p2s,
                                             p2s_per_p1s = n_p2s_per_p1s))
    return(list(model = model,
                rep_model_info = rep_model_info))}
  if(n_p1s_per_p2s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
  if(n_p2s_per_p1s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
}
