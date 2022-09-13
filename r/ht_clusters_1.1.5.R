ht_clusters_1.1.5 <- function (mat, cl, dend = NULL, col = c("white", "red"), draw_word_cloud = is_GO_id(rownames(mat)[1]) || 
            !is.null(term), term = NULL, min_term = round(nrow(mat) * 
                                                            0.01), order_by_size = FALSE, exclude_words = character(0), 
          max_words = 10, word_cloud_grob_param = list(), fontsize_range = c(4, 
                                                                             16), bg_gp = gpar(fill = "#DDDDDD", col = "#AAAAAA"), 
          column_title = NULL, ht_list = NULL, use_raster = TRUE, run_draw = TRUE, 
          ...) 
{
  if (length(col) == 1) 
    col = c("white", rgb(t(col2rgb(col)), maxColorValue = 255))
  col_fun = circlize::colorRamp2(seq(0, quantile(mat, 0.95), length = length(col)), 
                       col)
  if (!is.null(dend)) {
    ht = Heatmap(mat, col = col_fun, name = "Similarity", 
                 column_title = column_title, show_row_names = FALSE, 
                 show_column_names = FALSE, cluster_rows = dend, cluster_columns = dend, 
                 show_row_dend = TRUE, show_column_dend = FALSE, row_dend_width = unit(4, 
                                                                                       "cm"), border = "#404040", row_title = NULL, 
                 use_raster = use_raster)
    draw(ht)
    return(invisible(NULL))
  }
  else {
    if (inherits(cl, "try-error")) {
      grid.newpage()
      pushViewport(viewport())
      grid.text("Clustering has an error.")
      popViewport()
      return(invisible(NULL))
    }
    cl = as.vector(cl)
    cl_tb = table(cl)
    cl[as.character(cl) %in% names(cl_tb[cl_tb < min_term])] = 0
    cl = factor(cl, levels = c(setdiff(sort(cl), 0), 0))
    if (order_by_size) {
      cl = factor(cl, levels = c(setdiff(names(sort(table(cl), 
                                                    decreasing = TRUE)), 0), 0))
    }
    od2 = unlist(lapply(levels(cl), function(le) {
      l = cl == le
      if (sum(l) <= 1) {
        return(which(l))
      }
      else {
        mm = mat[l, l, drop = FALSE]
        which(l)[hclust(stats::dist(mm))$order]
      }
    }))
    ht = Heatmap(mat, col = col_fun, name = "Similarity", 
                 column_title = column_title, show_row_names = FALSE, 
                 show_column_names = FALSE, show_row_dend = FALSE, 
                 show_column_dend = FALSE, row_order = od2, column_order = od2, 
                 border = "#404040", row_title = NULL, use_raster = use_raster) + 
      NULL
    if (is.null(term)) {
      if (is.null(rownames(mat))) {
        draw_word_cloud = FALSE
      }
      else if (!grepl("^GO:[0-9]+$", rownames(mat)[1])) {
        draw_word_cloud = FALSE
      }
    }
    if (draw_word_cloud) {
      go_id = rownames(mat)
      if (!is.null(term)) {
        if (length(term) != length(go_id)) {
          stop_wrap("Length of `term` should be the same as the nrow of `mat`.")
        }
      }
      align_to = split(seq_along(cl), cl)
      go_id = split(go_id, cl)
      if (!is.null(term)) 
        term = split(term, cl)
      align_to = align_to[names(align_to) != "0"]
      go_id = go_id[names(go_id) != "0"]
      if (!is.null(term)) 
        term = term[names(term) != 0]
      if (length(align_to)) {
        ht = ht + rowAnnotation(keywords = anno_word_cloud_from_GO(align_to, 
                                                                   go_id, term=term, exclude_words = exclude_words, 
                                                                   max_words = max_words, word_cloud_grob_param = word_cloud_grob_param, 
                                                                   fontsize_range = fontsize_range, bg_gp = bg_gp))
      }
      else {
        ht = ht + Heatmap(ifelse(cl == "0", "< 5", ">= 5"), 
                          col = c(`< 5` = "darkgreen", `>= 5` = "white"), 
                          width = unit(1, "mm"), heatmap_legend_param = list(title = "", 
                                                                             at = "< 5", labels = "Small clusters"), show_column_names = FALSE)
      }
    }
    else {
      if (any(cl == "0")) {
        ht = ht + Heatmap(ifelse(cl == "0", "< 5", ">= 5"), 
                          col = c(`< 5` = "darkgreen", `>= 5` = "white"), 
                          width = unit(1, "mm"), heatmap_legend_param = list(title = "", 
                                                                             at = "< 5", labels = "Small clusters"), show_column_names = FALSE)
      }
    }
  }
  gap = unit(2, "pt")
  if (!is.null(ht_list)) {
    n = length(ht_list)
    ht = ht_list + ht
    gap = unit.c(unit(rep(2, n), "mm"), gap)
  }
  ht@ht_list[[1]]@heatmap_param$post_fun = function(ht) {
    decorate_heatmap_body("Similarity", {
      grid.rect(gp = gpar(fill = NA, col = "#404040"))
      cl = factor(cl, levels = unique(cl[od2]))
      tbcl = table(cl)
      ncl = length(cl)
      x = cumsum(c(0, tbcl))/ncl
      grid.segments(x, 0, x, 1, default.units = "npc", 
                    gp = gpar(col = "#404040"))
      grid.segments(0, 1 - x, 1, 1 - x, default.units = "npc", 
                    gp = gpar(col = "#404040"))
    })
  }
  if (run_draw) {
    ht = draw(ht, main_heatmap = "Similarity", gap = gap, 
              ...)
  }
  return(invisible(ht))
}