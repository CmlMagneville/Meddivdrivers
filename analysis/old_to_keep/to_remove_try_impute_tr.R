

sp_tr_TREES <- readRDS(here::here("transformed_data",
                                  "raw_traits_TREES.rds"))
# Species as rownames:
sp_tr_TREES <- tibble::column_to_rownames(sp_tr_TREES,
                                          "Species")
# Load trees phylogeny:
trees_phylogeny <- ape::read.tree(file = here::here("transformed_data",
                                                    "phylogeny_TREES.tree"))
trees_phylo <- ape::as.phylo(trees_phylogeny)
trees_phylo$tip.label <- stringr::str_replace(trees_phylo$tip.label,
                                              "_",
                                              " ")

# FUNSPACE:
try <- funspace::impute(sp_tr_TREES,
                        phylo = trees_phylo)

# MISS FOREST DOES ALLOW TO INCLUDE PHYLO DATA (CAN DO IT BUT AS TO COMPUTE
# PHYLO DISTANCES AND PCOA AND ...)


#

