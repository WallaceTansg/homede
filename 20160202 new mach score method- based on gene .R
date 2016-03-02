source("http://bioconductor.org/biocLite.R")
biocLite("Biostrings")
library(Biostrings)
pairwiseAlignment(pattern=c("succeed", "precede"), subject="supersede")
?pairwiseAlignment
subject <- AAString("SMDDTEKMSMKL")

pattern <- AAStringSet(c("HLDNLKGTF", "HVDDMPNAL"))
a<-pairwiseAlignment( pattern,subject, type="local")

slotNames(a)
a@score



pairwiseAlignment(pattern=c("succeed", "precede"), subject="supersede",type = "local")


## Nucleotide global, local, and overlap alignments
s1 <- 
  DNAString("ACTTCACCAGCTCCCTGGCGGTAAGTTGATCAAAGGAAACGCAAAGTTTTCAAG")
s2 <-
  DNAString("GTTTCACTACTTCCTTTCGGGTAAGTAAATATATAAATATATAAAAATATAATTTTCATC")

# First use a constant substitution matrix
mat <- matrix(-3, nrow = 4, ncol = 4)
diag(mat) <- 1
rownames(mat) <- colnames(mat) <- DNA_ALPHABET[1:4]
globalAlign <-
  pairwiseAlignment(s1, s2, substitutionMatrix = mat, gapOpening = -5, gapExtension = -2)
localAlign <-
  pairwiseAlignment(s1, s2, type = "local", substitutionMatrix = mat, gapOpening = -5, gapExtension = -2)
overlapAlign <-
  pairwiseAlignment(s1, s2, type = "overlap", substitutionMatrix = mat, gapOpening = -5, gapExtension = -2)

# Then use quality-based method for generating a substitution matrix
pairwiseAlignment(s1, s2,
                  patternQuality = rep(c(22L, 12L), times = c(36, 18)),
                  subjectQuality = rep(c(22L, 12L), times = c(40, 20)),
                  scoreOnly = TRUE)

