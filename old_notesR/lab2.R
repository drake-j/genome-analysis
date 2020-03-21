SNPs <- c("AA", "AA", "GG", "AG", "AG", "AA","AG", "AA", "AA", "AA", "AG")
SNPs

SNPs_cat <- factor(SNPs)
SNPs_cat

table(SNPs_cat)

as.numeric(SNPs_cat)

Day1 <- c(2,4,6,8)
Day2 <- c(3,6,9,12)
Day3 <- c(1,4,9,16)
A <- cbind(Day1,Day2,Day3)
A

Day1 <- c(2,4,6,8)
Day2 <- c(3,6,9,12)
Day3 <- c(1,4,9,16)
B <- rbind(Day1,Day2,Day3)
B

Day4 <- c(5,10,11,20)
C <- rbind(B,Day4)
C

A * 10
A[1]
A[12]

A[ ,c(1,3)]

A[c(2,4), ]

t(A)

# data frames

Gene1 <- c(2,4,6,8)
Gene2 <- c(3,6,9,12)
Gene3 <- c(1,4,9,16)
Gene <- c("Day 1", "Day 2","Day 3", "Day 4")
RNAseq <- data.frame(Gene1, Gene2, Gene3, row.names = Gene)
RNAseq

RNAseq$Gene3

plot(RNAseq$Gene1,RNAseq$Gene3)

RNAseq$Gene4 <- c(5, 10, 15, 20)
RNAseq

RNAseq[,"Gene5"] <- c(1, 2, 3, 3)
RNAseq

RNAseq["Day 4",] <- rbind(10, 14, 20, 22, 3)

# objectifying R

x<-1
str(x)

a = "ATGCCCTGA"
str(a)

str(SNPs)
str(SNPs_cat)

Day1 <- c(2,4,6,8)
Day2 <- c(3,6,9,12)
Day3 <- c(1,4,9,16)
B <- rbind(Day1,Day2,Day3)
str (B)

Gene1 <- c(2,4,6,8)
Gene2 <- c(3,6,9,12)
Gene3 <- c(1,4,9,16)
Gene <- c("Day 1", "Day 2","Day 3", "Day 4")
RNAseq <- data.frame(Gene1, Gene2, Gene3, row.names = Gene)
str(RNAseq)

#important data import

read.table("file.csv", header = TRUE, sep = ",")
read.table("file.txt", header = TRUE, sep = "\t")
read.table("file.dat", header = TRUE, sep = "\t", row.names = 1)

#don't use xlsx bc JAVA is the debil baby

#load truncated snp
#rsid = reference SNP cluster ID
SNP_table <- read.table("23andMe_example_cat25.txt", header = TRUE, sep = "\t")
SNP_table

names(SNP_table)
str(SNP_table)
levels(SNP_table$genotype)
dim(SNP_table)
class(SNP_table)
SNP_table
head(SNP_table, n=10)
tail(SNP_table, n=5)
help(read.table)

SNP_table$chromosome <- as.factor(SNP_table$chromosome)
str(SNP_table) 

SNP_table$chromosome <- as.integer(SNP_table$chromosome)
str(SNP_table) 

SNP_table_AG <- subset(SNP_table, genotype == 'AG') 
SNP_table_AG
table(SNP_table_AG$chromosome)

subset(SNP_table, position > 700000 & position < 800000)

# ex 1
x <- c(1,3,6,9,12)
y<- c(1,0,1,0,1)
x+y;x-y;x*y;x/y
#ex 2
x <- c(0,1,2,3); str(x)
y <- c("aa","bb","cc","dd") ; str(y)
z <- c("aa", 1, "bb",2) ; str(z)
#ex 3
gene1 <- c("AA", "AA", "AG", "GG", "GG" )
gene2 <- c("AA", "AA", "GG", "GG", "GG")
m <- cbind(gene1, gene2)
table(m)
#ex 4
treat1 <- c(0,1,2,3,4)
treat2 <- c(0,2,4,6,8)
treat3 <- c(0,3,6,9,12)
time <- c(0,2,4,6,8)
df<-data.frame(treat1,treat2, treat3, time)

#ex 5
SNP_table <- read.table("23andMe_example_cat.txt", header = TRUE, sep = "\t")
str(SNP_table$chromosome)

#ex 6
table(SNP_table$genotype)

#ex 7 
subset1 <- subset(SNP_table, genotype=="A")
tablel(subset)