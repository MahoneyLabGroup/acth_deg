#get model coefficients and p-values for volcano plot

get_model_values <- function(gee_results, term_col, direction = c("up", "down", "all"), p_cutoff = FALSE) {
  
  if(p_cutoff) {
    gee_results <- gee_results %>% 
      filter(fdr_adj_p < 0.1)
  }
  
  gene_values <- gee_results %>%
    pivot_wider(names_from = term,
                values_from = model_coef) %>% 
    dplyr::select(gene_name, fdr_adj_p, {{ term_col }}) %>% 
    drop_na() %>% 
    mutate(log_fdr_adj_p = -log10(fdr_adj_p),
           log2_mod_coefs = -log2((abs({{ term_col }}))^(sign({{ term_col }})))) 
  
  if(direction == "all") {
    
    all_gene_values <- gene_values %>% 
      filter(log2_mod_coefs < -1 | log2_mod_coefs > 1) %>% 
      filter(!duplicated(gene_name)) %>% 
      dplyr::select(gene_name, log2_mod_coefs, log_fdr_adj_p)
    
    return(all_gene_values)
    
  }else if(direction == "up") {
    
    up_gene_values <- gene_values %>% 
      filter(log2_mod_coefs > 1) %>% 
      filter(!duplicated(gene_name)) %>% 
      dplyr::select(gene_name, log2_mod_coefs, log_fdr_adj_p)
    
    return(up_gene_values)
    
  }else if(direction == "down") {
    
    down_gene_values <- gene_values %>% 
      filter(log2_mod_coefs < -1) %>% 
      filter(!duplicated(gene_name)) %>% 
      dplyr::select(gene_name, log2_mod_coefs, log_fdr_adj_p)
    
    return(down_gene_values)
    
  }
  
}