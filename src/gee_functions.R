# Functions to run and post-process GEE models

run_gees <- function(model_df) {
  
  gee_df <- model_df %>% mutate(model = map(data, ~ gee(expr_val ~ treat_group + sex + region + treat_group:region, 
                                                        id = animal,
                                                        corstr = "exchangeable",
                                                        data = .)))
  
} 

get_gee_p <- function(mod_obj) {
  
  p_list <- vector("list", nrow(mod_obj))
  
  for(i in 1:nrow(mod_obj)) {
    
    p_list[[i]] <- 2 * pnorm(abs(coef(summary(mod_obj$model[[i]]))[,5]), lower.tail = FALSE)
    
  }
  
  adjusted_p <- vector("list", length(p_list))
  
  for(i in 1:length(p_list)) {
    
    adjusted_p[[i]] <- p.adjust(p_list[[i]], method = "fdr")
    
  }
  
  names(adjusted_p) <- mod_obj$gene_id
  
  return(adjusted_p)
  
}


add_coefs <- function(pval_obj, mod_obj) {
  
  pvals_w_coefs <- vector("list", length(pval_obj))
  
  for(i in 1:length(pval_obj)) {
    
    pvals_w_coefs[[i]] <- t(rbind(pval_obj[[i]], coef(summary(mod_obj$model[[i]]))[,1]))
    colnames(pvals_w_coefs[[i]]) <- c("fdr_adj_p", "model_coef")
    
  }
  
  names(pvals_w_coefs) <- names(pval_obj)
  
  return(pvals_w_coefs)
  
}