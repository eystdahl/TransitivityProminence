# Make a matrix of transitivity prominence values based on Haspelmath's (2015) verb list

TransProm <- matrix(c(40, 40, 52, 49, 9, 12, 8, 3, 17, 17, 6, 10), ncol = 4, byrow = TRUE)

# Add colnames and rownames

colnames(TransProm) <- c("Vedic", "Greek", "Latin", "Hittite")
rownames(TransProm) <- c("Canonical verbs", "Middle voice", "Non-canonical object")

# Check matrix

TransProm

#Make barplot

barplot(TransProm, ylim = range(0:100), main = "Transitivity prominence in Indo-European", ylab = "Absolute frequency", col = c("black", "grey60", "grey40"), beside = TRUE)
legend("topleft", fill = c("black","grey60", "grey40"), legend=c("Canonically transitive morphosyntax", "Middle voice marking", "Non-canonical object marking"))

#Make barplot of relative proportions

TransPromProportion <- prop.table(TransProm, 2)*100
TransPromProportion

barplot(TransPromProportion, ylim = range(0:100), main = "Transitivity prominence in Indo-European", ylab = "Relative frequency (%)", col = c("black", "grey60", "grey40"), beside = TRUE)
legend("topleft", fill = c("black","grey60", "grey40"), legend=c("Canonically transitive morphosyntax", "Middle voice marking", "Non-canonical object marking"))

# chisquare test of TransProm

chisq.test(TransProm)

# Get expected values

chisq.test(TransProm)$expected

#Fisher test of observed values
#Canonical verb frames

VedicCanVerbs <- matrix(c(40, 141, 26, 56), ncol = 2, byrow = TRUE)
fisher.test(VedicCanVerbs, alternative = "less")

GreekCanVerbs <- matrix(c(40, 141, 29, 53), ncol = 2, byrow = TRUE)
fisher.test(GreekCanVerbs, alternative = "less")

LatinCanVerbs<- matrix(c(52, 129, 14, 68), ncol = 2, byrow = TRUE)
fisher.test(LatinCanVerbs, alternative = "greater")

HittiteCanVerbs <- matrix(c(49, 132, 13, 69), ncol = 2, byrow = TRUE)
fisher.test(HittiteCanVerbs, alternative = "greater")

#Middle verb forms

VedicMiddleVerbs <- matrix(c(9, 23, 57, 174), ncol = 2, byrow = TRUE)
fisher.test(VedicMiddleVerbs, alternative = "greater")

GreekMiddleVerbs <- matrix(c(12, 20, 57, 174), ncol = 2, byrow = TRUE)
fisher.test(GreekMiddleVerbs, alternative = "greater")

LatinMiddleVerbs <- matrix(c(8, 24, 58, 173), ncol = 2, byrow = TRUE)
fisher.test(LatinMiddleVerbs, alternative = "less")

HittiteMiddleVerbs <- matrix(c(3, 29, 59, 179), ncol = 2, byrow = TRUE)
fisher.test(HittiteMiddleVerbs, alternative = "less")

#Non-canonical objects

VedicNonCanObj <- matrix(c(17, 33, 49, 164), ncol = 2, byrow = TRUE)
fisher.test(VedicNonCanObj, alternative = "greater")

GreekNonCanObj <- matrix(c(17, 33, 52, 161), ncol = 2, byrow = TRUE)
fisher.test(GreekNonCanObj, alternative = "greater")

LatinNonCanObj <- matrix(c(6, 44, 60, 153), ncol = 2, byrow = TRUE)
fisher.test(LatinNonCanObj, alternative = "less")

HittiteNonCanObj <- matrix(c(10, 40, 52, 161), ncol = 2, byrow = TRUE)
fisher.test(HittiteNonCanObj, alternative = "less")

TransPromProportion <- prop.table(TransProm, 2)*100

barplot(TransPromProportion, ylim = range(0:100), sub = "Relative proportion of ")

#Make a matrix of transitivity prominence values based on Creissels' (2018b) verb list

TransProm2 <- matrix(c(14, 14, 15, 19, 7, 7.5, 6, 3.5, 9, 4.5, 12, 5.5), ncol = 4, byrow = TRUE)

# Add colnames and rownames

colnames(TransProm2) <- c("Hittite", "Vedic", "Greek", "Latin")
rownames(TransProm2) <- c("Canonical verbs", "Middle voice", "Non-canonical object")
# Perform chi-squared test

chisq.test(TransProm2)
