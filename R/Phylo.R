read_phylo_tree  <- function(sps){
 require(ape)
 temp <- tempfile()
download.file("http://datadryad.org/bitstream/handle/10255/dryad.59003/PhylogeneticResources.zip?sequence=1",temp)
 Tree <- read.tree(unz(temp,
                       "PhylogeneticResources/Vascular_Plants_rooted.dated.tre"))

 TreeInv <- gsub(" ", "_", sps$sp)
 TreeInv[TreeInv == "Pinus_uncinata"] <- "Pinus_mugo"
 TreeInv[TreeInv == "Betula"] <- "Betula_pubescens"
 PHYLOname  <- c('Pinus_mugo', 'Betula_pubescens')
 FUNDIVname <-  c('Pinus uncinata', 'Betula')
 names(FUNDIVname)  <- PHYLOname
 # check which species are not in Phylogeny
 if(length(Tree$tip.label[c(which (! TreeInv %in% Tree$tip.label))]) >0)
     stop('missing species in phylogeny')

 #  prune the tree to the good species list
 Drop <- Tree$tip.label[-c(which(Tree$tip.label %in% TreeInv))]
 TreeFunDiv<-drop.tip(Tree,Drop)
 # Change trip name to FUNDIV
 TreeFunDiv$tip.label[TreeFunDiv$tip.label %in% PHYLOname] <-
     FUNDIVname[TreeFunDiv$tip.label[TreeFunDiv$tip.label %in% PHYLOname]]
 TreeFunDiv$tip.label  <- gsub("_", " ",  TreeFunDiv$tip.label)
 return(TreeFunDiv)
}
