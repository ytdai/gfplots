#R code from vignette source 'microarrayAnalysis.Rnw'

###################################################
#code chunk number 1: load ALL data (eval = FALSE)
###################################################
library(ReportingTools)
library(ALL)
library(hgu95av2.db)
library(genefilter)

data(ALL)


###################################################
#code chunk number 2: filter ALL data (eval = FALSE)
###################################################
ALL <- ALL[, ALL$mol.biol %in% c('NEG','BCR/ABL') &
           !is.na(ALL$sex)]
ALL$mol.biol <- factor(ALL$mol.biol, 
                       levels = c('NEG', 'BCR/ABL'))
ALL <- featureFilter(ALL)


###################################################
#code chunk number 3: limma linear model (eval = FALSE)
###################################################
library(limma)
model <- model.matrix(~mol.biol+sex, ALL)
fit <- eBayes(lmFit(ALL, model))


###################################################
#code chunk number 4: making the DE report (eval = FALSE)
###################################################
library(lattice)
rep.theme <- reporting.theme()
lattice.options(default.theme = rep.theme)

deReport <- HTMLReport(shortName = 'de_analysis',
                       title = 'Analysis of BCR/ABL translocation differential expression',
                       reportDirectory = "./reports")
publish(fit, deReport, eSet=ALL, factor=ALL$mol.biol, coef=2, n=100)
finish(deReport)


###################################################
#code chunk number 5: making the DE report with new images (eval = FALSE)
###################################################
library(hwriter)
makeNewImages <- function(df,...){
	imagename <- c()
	for (i in 1:nrow(df)){
		probeId <- df$ProbeId[i]
		y_at <- pretty(exprs(ALL)[probeId,])
		y_labels <- formatC(y_at, digits = 1, format = 'f')
		imagename[i] <- paste0("plot", probeId, ".png")
		png(filename = paste0("./reports/figuresde_analysis/", 
                      imagename[i]))
		print(stripplot(exprs(ALL)[probeId,]~ALL$mol.biol|ALL$sex))
		dev.off()
	}
	df$Image <- hwriteImage(paste0("figuresde_analysis/", imagename), 
                                link=paste0("figuresde_analysis/", imagename), 
                                table=FALSE, width=100)
	return(df)
}
deReport2 <- HTMLReport(shortName='de_analysis2',
                        title = 'Analysis of BCR/ABL translocation differential expression with new plots',
                        reportDirectory = "./reports")
publish(fit, deReport2, eSet = ALL, factor = ALL$mol.biol, coef=2, 
        n=100, 
       ##.modifyDF=list(modifyReportDF, makeNewImages) ) ##to add new images to default RT output
       .modifyDF=makeNewImages)
finish(deReport2)


###################################################
#code chunk number 6: Do GO analysis (eval = FALSE)
###################################################
library(GOstats)
tt <- topTable(fit, coef = 2, n = 100)
selectedIDs <- unlist(mget(rownames(tt), hgu95av2ENTREZID))
universeIDs <- unlist(mget(featureNames(ALL), hgu95av2ENTREZID))
goParams <- new("GOHyperGParams", 
                geneIds = selectedIDs, 
                universeGeneIds = universeIDs, 
                annotation = annotation(ALL), 
                ontology = "BP", 
                pvalueCutoff = 0.01,
                conditional = TRUE, 
                testDirection = "over")
goResults <- hyperGTest(goParams)


###################################################
#code chunk number 7: make the GO report (eval = FALSE)
###################################################
goReport <- HTMLReport(shortName = 'go_analysis',
                       title = 'GO analysis of BCR/ABL translocation',
                       reportDirectory = "./reports")
publish(goResults, goReport)
finish(goReport)


###################################################
#code chunk number 8: Do PFAM analysis (eval = FALSE)
###################################################
library(Category)
pfamParams <- new("PFAMHyperGParams", 
                  geneIds = selectedIDs, 
                  universeGeneIds = universeIDs, 
                  annotation = annotation(ALL),  
                  pvalueCutoff = 0.01,
                  testDirection = "over")
PFAMResults <- hyperGTest(pfamParams)


###################################################
#code chunk number 9: make the PFAM report (eval = FALSE)
###################################################
PFAMReport <- HTMLReport(shortName = 'pfam_analysis',
                         title = 'PFAM analysis of BCR/ABL translocation',
                         reportDirectory = "./reports")
publish(PFAMResults, PFAMReport, categorySize = 3)
finish(PFAMReport)


###################################################
#code chunk number 10: Make Gene Sets (eval = FALSE)
###################################################
library(GSEAlm)
library(GSEABase)
mapped_genes <- mappedkeys(org.Hs.egSYMBOL)
eidsAndSymbols <- as.list(org.Hs.egSYMBOL[mapped_genes])
geneEids <- names(eidsAndSymbols)
set.seed(123)
set1 <- GeneSet(geneIds=sample(geneEids, 100, replace=FALSE), setName="set1", 
                shortDescription = "This is set1")
set2 <- GeneSet(geneIds=sample(geneEids, 10, replace=FALSE), setName="set2",  
                shortDescription = "This is set2")
set3 <- GeneSet(geneIds=sample(geneEids, 37, replace=FALSE), setName="set3",  
                shortDescription = "This is set3") 
set4 <- GeneSet(geneIds=sample(geneEids, 300, replace=FALSE), setName="set4", 
                shortDescription = "This is set4")
geneSets <- GeneSetCollection(c(set1, set2, set3, set4))


###################################################
#code chunk number 11: Make Simple Gene Set Table (eval = FALSE)
###################################################
geneSetsReport <- HTMLReport(shortName = "gene_sets",
                             title = "Gene Sets", 
                             reportDirectory = "./reports")
publish(geneSets, geneSetsReport, annotation.db = "org.Hs.eg")
finish(geneSetsReport)


###################################################
#code chunk number 12: Get incidence matrix (eval = FALSE)
###################################################
mat <- matrix(data=0, ncol=length(universeIDs),nrow=length(geneSets))
for(i in 1:length(geneSets)){
  geneIdEntrez <- unlist(geneIds(geneSets[[i]]))
  mat[i,match(geneIdEntrez, universeIDs)] <- 1
}
colnames(mat) <- universeIDs
rownames(mat) <- sapply(geneSets, function(x) x@setName)


###################################################
#code chunk number 13: Run GSEA (eval = FALSE)
###################################################
lm <- lmPerGene(ALL, ~mol.biol+sex, na.rm=TRUE)
GSNorm <- GSNormalize(lm$tstat[2,], mat)
#one-sided p-values
pVals <- gsealmPerm(ALL,~mol.biol+sex, mat, nperm=100)  
bestPval <- apply(pVals, 1, min)


###################################################
#code chunk number 14: make the GSEA report (eval = FALSE)
###################################################
gseaReport <- HTMLReport(shortName = "gsea_analysis",
                         title = "GSEA analysis", 
                         reportDirectory = "./reports")
publish(geneSets, gseaReport, annotation.db = "org.Hs.eg", 
        setStats = GSNorm, setPValues = 2*bestPval)
finish(gseaReport)


###################################################
#code chunk number 15: make the GSEA report with new columns (eval = FALSE)
###################################################
runGSEA <- function(df,...){
  mat <- matrix(data = 0, ncol = length(universeIDs), nrow = length(geneSets))
  for(i in 1:length(geneSets)){
    geneIdEntrez <- unlist(geneIds(geneSets[[i]]))
    mat[i,match(geneIdEntrez, universeIDs)] <- 1
  }
  colnames(mat) <- universeIDs
  rownames(mat) <- sapply(geneSets, function(x) x@setName)	
  lm <- lmPerGene(ALL, ~mol.biol+sex, na.rm=TRUE)
  GSNorm <- GSNormalize(lm$tstat[2,], mat)
  pVals <- gsealmPerm(ALL,~mol.biol+sex, mat, nperm = 100)  
  bestPval <- apply(pVals,1, min)
  df <- cbind(df, GSNorm, bestPval)
  return(df)
}

gseaReport2 <- HTMLReport(shortName = "gsea_analysis2",
                          title = "GSEA analysis", 
                          reportDirectory = "./reports")
publish(geneSets, gseaReport2, annotation.db = "org.Hs.eg", 
        .modifyDF = runGSEA)
finish(gseaReport2)


###################################################
#code chunk number 16: make the index page (eval = FALSE)
###################################################
indexPage <- HTMLReport(shortName = "index",
                        title = "Analysis of ALL Gene Expression",
                        reportDirectory = "./reports")
publish(Link(list(deReport, goReport), report = indexPage), indexPage)
publish(Link(PFAMReport, report = indexPage), indexPage)
publish(Link("GSEA report has a new title", "gsea_analysis.html"), indexPage)
finish(indexPage)

