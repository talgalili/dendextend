

# I must add this for roxygen2 to work...
khan <- NULL

#' @title Microarray gene expression dataset from Khan et al., 2001. Subset of 306 genes.
#'
#' @description
#'
#' Khan contains gene expression profiles of four types of small round
#' blue cell tumours of childhood (SRBCT) published by Khan et al. (2001).
#' It also contains further gene annotation retrieved from SOURCE at \url{http://source.stanford.edu/}.
#'
#' @format
#'
#' Khan is dataset containing the following:
#'    \itemize{
#'       \item{\$train:}{\code{\link{data.frame}} of 306 rows and 64 columns.
#'          The training dataset of 64 arrays and 306 gene expression values}
#'       \item{\$test:}{\code{\link{data.frame}}, of 306 rows and 25 columns.
#'          The test dataset of 25 arrays and 306 genes expression values}
#'       \item{\$gene.labels.imagesID:}{\code{vector} of 306 Image clone identifiers
#'          corresponding to the rownames of \$train and \$test.}
#'       \item{\$train.classes:}{\code{\link{factor}} with 4 levels "EWS",
#'          "BL-NHL", "NB" and "RMS", which correspond to the four groups in
#'          the \$train dataset}
#'       \item{\$test.classes:}{\code{\link{factor}} with 5 levels "EWS",
#'          "BL-NHL", "NB", "RMS" and "Norm" which correspond to the five
#'          groups in the \$test dataset}
#'       \item{\$annotation:}{\code{\link{data.frame}} of 306 rows and 8 columns.
#'          This table contains further gene annotation retrieved from SOURCE
#'          \url{http://SOURCE.stanford.edu} in May 2004.  For each of the 306 genes,
#'          it contains: \itemize{
#'             \item{\$CloneID}{Image Clone ID}
#'             \item{\$UGCluster}{The Unigene cluster to which the gene is assigned}
#'             \item{\$Symbol}{The HUGO gene symbol}
#'             \item{\$LLID}{The locus ID}
#'             \item{\$UGRepAcc}{Nucleotide sequence accession number}
#'             \item{\$LLRepProtAcc}{Protein sequence accession number}
#'             \item{\$Chromosome}{chromosome location}
#'             \item{\$Cytoband}{cytoband location}
#'          }
#'       }
#'    }
#'
#'
#'
#' @details
#'
#' Khan et al., 2001 used cDNA microarrays containing 6567 clones of which
#' 3789 were known genes and 2778 were ESTs to study the expression of
#' genes in of four types of small round blue cell tumours of childhood (SRBCT).
#' These were neuroblastoma (NB), rhabdomyosarcoma (RMS), Burkitt lymphoma, a
#' subset of non-Hodgkin lymphoma (BL), and the Ewing family of tumours
#' (EWS). Gene expression profiles from both tumour biopsy and cell line
#' samples were obtained and are contained in this dataset. The dataset downloaded
#' from the website contained the filtered dataset of 2308 gene expression profiles as described
#' by Khan et al., 2001.  This dataset is available from the \url{http://bioinf.ucd.ie/people/aedin/R/}.
#'
#' In order to reduce the size of the MADE4 package, and produce small example datasets, the top 50 genes from the
#' ends of 3 axes following \code{bga} were selected. This produced a reduced datasets of 306 genes.
#'
#' @source
#'
#' \code{khan} contains a filtered data of 2308 gene expression profiles
#' as published and provided by Khan et al. (2001) on the supplementary
#' web site to their publication
#' \url{http://research.nhgri.nih.gov/microarray/Supplement/}.
#'
#' The data was copied from the made4 package (\url{http://www.bioconductor.org/packages/release/bioc/html/made4.html})
#'
#'
#' @references
#'    Culhane AC, et al., 2002 Between-group analysis of microarray
#'    data. Bioinformatics. 18(12):1600-8.
#'
#'    Khan,J., Wei,J.S., Ringner,M., Saal,L.H., Ladanyi,M., Westermann,F.,
#'    Berthold,F., Schwab,M., Antonescu,C.R., Peterson,C. et al. (2001) Classification and diagnostic
#'    prediction of cancers using gene expression profiling and artificial neural networks.
#'    Nat. Med., 7, 673-679.
#'
#' @examples
#'
#' data(khan)
#' summary(khan)
"khan"




# on how to do it: http://r-pkgs.had.co.nz/data.html
# library(made4)
# data(khan)
# khan2 <- khan
# cars <- khan
# names(khan)
# devtools::use_data(khan)
# devtools::use_data(khan2, overwrite = TRUE)
# devtools::use_data(cars, overwrite = TRUE)
#
