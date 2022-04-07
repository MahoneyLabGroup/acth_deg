#get gene names from the Rat ensembl build 102
get_gene_names <- function(sig_gene_tib, anno_tib) {
  
  sig_gene_tib_anno <- sig_gene_tib %>% 
    inner_join(anno_genes, by = c("gene_id" = "gene_stable_id")) %>% 
    dplyr::select(gene_id, gene_name, everything())
  
  return(sig_gene_tib_anno)
  
}