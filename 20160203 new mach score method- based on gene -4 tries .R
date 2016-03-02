source("http://bioconductor.org/biocLite.R")
biocLite("Biostrings")
library(Biostrings)

options(width=72)

library(Biostrings)

train$product_title[1],train$search_term[1]


#try1#
pp=train$product_title[1]
tt=train$search_term[1]

as.character(trt_title[1])
ain$producwhile(j<=proNum2)

proNum2=9
i=1

while(i<=proNum2){
cc<-pairwiseAlignment(pattern = c(as.character(train$product_title[i])), subject =as.character(train$search_term[j]) ,
                      type = "global", gapOpening = 0, gapExtension = 1)

}
slotNames(cc)
cc@pattern

score(cc)

#try2#
ee<-pairwiseAlignment(pattern = c("BEHR Premium Textured"), subject = "l bracket",
                      type = "global", gapOpening = 1, gapExtension = 0)

slotNames(ee)
ee@pattern
ee@score


#try3#

dd<-pairwiseAlignment(pattern = c("BEHR Premium Textured"), subject = "l bracket",
                      type = "global", gapOpening = 0, gapExtension = 0)
slotNames(dd)
dd@pattern
dd@score


#try4#
bb<-pairwiseAlignment(pattern = c("BEHR Premium Textured"), subject = "l bracket",
                      type = "global", gapOpening = 1, gapExtension = 1)

slotNames(bb)
bb@pattern
bb@score


