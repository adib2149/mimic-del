# To clear/remove all variables:
# remove(list = ls())


#===============================================================================
# CSV output format: 
# amat format from pcalg (see amatType in doc)
# GENERAL CODE:
# 0: No edge or tail
# 1: Arrowhead
# CPDAG:
# amat[a,b] = 0 and amat[b,a] = 1 implies a --> b.
# amat[a,b] = 1 and amat[b,a] = 0 implies a <-- b.
# amat[a,b] = 0 and amat[b,a] = 0 implies a b.
# amat[a,b] = 1 and amat[b,a] = 1 implies a --- b.
# PAG:
# amat[a,b] = 2 and amat[b,a] = 3 implies a --> b.
# amat[a,b] = 3 and amat[b,a] = 2 implies a <-- b.
# amat[a,b] = 2 and amat[b,a] = 2 implies a <-> b.
# amat[a,b] = 1 and amat[b,a] = 3 implies a --o b.
# amat[a,b] = 0 and amat[b,a] = 0 implies a b.


#===============================================================================
# for pcalg, install these from BiocManager first: graph, RBGL, Rgraphviz
library(pcalg)
library(Rfast)
library(pchc)

#===============================================================================
# read data and preprocess
base_directory = "/Users/adib/Documents/GitHub/RCHE-Data-Science/adib2149/delirium_study/00_final_works_for_paper_and_dissertation/"
graph_dir = "graph_output/"

df <- read.csv(paste(base_directory, "delirium_data_cleaned.csv", sep=""))
df <- subset(df, select = -c(subject_id, hadm_id, icustay_id, icd9_codes))

# convert all columns to appropriate format
# as factor
# df$race<-as.factor(df$race)
# df$surgery<-as.factor(df$surgery)
# df$pneumonia<-as.factor(df$pneumonia)
# df$sepsis<-as.factor(df$sepsis)
# df$dementia<-as.factor(df$dementia)
# df$alzheimers<-as.factor(df$alzheimers)
# df$depression<-as.factor(df$depression)
# df$anxiety<-as.factor(df$anxiety)
# df$met_acidosis<-as.factor(df$met_acidosis)
# df$airway_obs<-as.factor(df$airway_obs)
# df$copd<-as.factor(df$copd)
# df$liver_disease<-as.factor(df$liver_disease)
# df$heart_disease<-as.factor(df$heart_disease)
# df$mechvent<-as.factor(df$mechvent)
# df$death_hosp<-as.factor(df$death_hosp)
# df$death_timeline<-as.factor(df$death_timeline)
# # as numeric
# df$sofa<-as.numeric(df$sofa)
# df$apsiii<-as.numeric(df$apsiii)
# df$mechvent_count<-as.numeric(df$mechvent_count)
# df$drug_categories_distinct_count<-as.numeric(df$drug_categories_distinct_count)
# df$drug_timelength<-as.numeric(df$drug_timelength)

df$sex[df$sex == 'M'] <- 0
df$sex[df$sex == 'F'] <- 1
df$sex <- as.numeric(df$sex)

df$drug_group[df$drug_group == "ND"] <- 0
df$drug_group[df$drug_group == "OD"] <- 1
df$drug_group[df$drug_group == "HL"] <- 2
df$drug_group <- as.numeric(df$drug_group)

# convert all columns to numeric format
df[] <- lapply(df, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(df, class)

# If column names are too long, then the node in the plot becomes unreadable
# saving old names for later use
write.csv(file=paste(base_directory, graph_dir, "colnames.csv", sep=""), 
          x=colnames(df), row.names = FALSE, quote=FALSE) 
colnames(df) <- c(1:ncol(df))
colnumber <- ncol(df)


#===============================================================================
# 1. PC algo
n <- nrow(df)
V <- colnames(df)
pc.fit <- pc(suffStat = list(C = cor(df), n = n), indepTest = gaussCItest, 
             alpha=0.01, labels = V, verbose = TRUE)

# 1.1 save pdf image
pdf(file=paste(base_directory, graph_dir, "1_pc.pdf", sep=""))
plot(pc.fit, main = "PC output")
dev.off()

# 1.2 save csv edges
write.csv(file=paste(base_directory, graph_dir, "1_pc.csv", sep=""), 
          x=as(pc.fit, "amat"), row.names = FALSE, quote=FALSE)


#===============================================================================
# 2. FCI algo
fci.fit <- fci(suffStat = list(C = cor(df), n = n), indepTest = gaussCItest, 
             alpha=0.01, labels = V, verbose = TRUE)

# 2.1 save pdf image
pdf(file=paste(base_directory, graph_dir, "2_fci.pdf", sep=""))
plot(fci.fit, main = "FCI Output")
dev.off()

# 2.2 save csv edges
write.csv(file=paste(base_directory, graph_dir, "2_fci.csv", sep=""), 
          x=as(fci.fit, "amat"), row.names = FALSE, quote=FALSE)


#===============================================================================
# 3. GES algo
score <- new("GaussL0penObsScore", df)
ges.fit <- ges(score, labels = V, verbose = TRUE)

# 3.1 save pdf image
pdf(file=paste(base_directory, graph_dir, "3_ges.pdf", sep=""))
plot(ges.fit$essgraph, main = "GES Output")
dev.off()

# 3.2 save csv edges
ges.mat <- matrix(0, nrow = colnumber, ncol = colnumber)
edges <- ges.fit$essgraph$.in.edges
idx = 1
for(edge.out.list in edges){
  for (edge.in in edge.out.list){
    ges.mat[idx, edge.in] <- 1
  }
  idx <- idx + 1
}

write.csv(file=paste(base_directory, graph_dir, "3_ges.csv", sep=""), 
          x=ges.mat, row.names = FALSE, quote=FALSE)


#===============================================================================
# 4. GIES algo
gies.fit <- gies(score, labels = V, verbose = TRUE)

# 4.1 save pdf image
pdf(file=paste(base_directory, graph_dir, "4_gies.pdf", sep=""))
plot(gies.fit$essgraph, main = "GIES Output")
dev.off()

# 4.2 save csv edges
gies.mat <- matrix(0, nrow = colnumber, ncol = colnumber)
edges <- gies.fit$essgraph$.in.edges
idx = 1
for(edge.out.list in edges){
  for (edge.in in edge.out.list){
    gies.mat[idx, edge.in] <- 1
  }
  idx <- idx + 1
}
write.csv(file=paste(base_directory, graph_dir, "4_gies.csv", sep=""), 
          x=gies.mat, row.names = FALSE, quote=FALSE)


#===============================================================================
# 5. GDS algo
gds.fit <- gds(score, labels = V, verbose = TRUE)

# 5.1 save pdf image
pdf(file=paste(base_directory, graph_dir, "5_gds.pdf", sep=""))
plot(gds.fit$essgraph, main = "GDS Output")
dev.off()

# 5.2 save csv edges
gds.mat <- matrix(0, nrow = colnumber, ncol = colnumber)
edges <- gds.fit$essgraph$.in.edges
idx = 1
for(edge.out.list in edges){
  for (edge.in in edge.out.list){
    gds.mat[idx, edge.in] <- 1
  }
  idx <- idx + 1
}
write.csv(file=paste(base_directory, graph_dir, "5_gds.csv", sep=""), 
          x=gds.mat, row.names = FALSE, quote=FALSE)


#===============================================================================
# 6. LINGAM algo: DOES NOT WORK
lingam.fit <- lingam(df, verbose = TRUE)

# 6.1 save pdf image
# NO IDEA HOW
# pdf(file=paste(base_directory, graph_dir, "6_lingam.pdf", sep=""))
# plot(lingam.fit, main = "LINGAM Output")
# dev.off()

# 6.2 save csv edges
lingam.mat <- as(lingam.fit, "amat")
lingam.mat[lingam.mat == 'TRUE'] <- 1
lingam.mat[lingam.mat == 'FALSE'] <- 0
write.csv(file=paste(base_directory, graph_dir, "6_lingam.csv", sep=""), 
          x=lingam.mat, row.names = FALSE, quote=FALSE)


#===============================================================================
# 7. MMHC algo
df.mat <- data.frame.to_matrix(df)
mmhc.fit <- mmhc(df.mat)

# 7.1 save pdf image
pdf(file=paste(base_directory, graph_dir, "7_mmhc.pdf", sep=""))
plot(mmhc.fit$dag, main = "MMHC Output")
dev.off()

# 7.2 save csv edges
write.csv(file=paste(base_directory, graph_dir, "7_mmhc.csv", sep=""), 
          x=mmhc.fit$dag$arcs, row.names = FALSE, quote=FALSE)


#===============================================================================
# 8. TABU algo
mmtabu.fit <- mmtabu(df.mat)

# 8.1 save pdf image
pdf(file=paste(base_directory, graph_dir, "8_mmtabu.pdf", sep=""))
plot(mmtabu.fit$dag, main = "TABU Output")
dev.off()

# 8.2 save csv edges
write.csv(file=paste(base_directory, graph_dir, "8_mmtabu.csv", sep=""), 
          x=mmtabu.fit$dag$arcs, row.names = FALSE, quote=FALSE)