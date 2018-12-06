use "C:\Users\minjing\Downloads\bfi.dta"
#run factor analysis, principle components analysis
factor A1-A5 C1-C5 E1-E5 N1-N5 O1-O5,pcf

#screeplot
scree
#parallel analysis for pca, install fapara package first
search fapara
fapara,pca reps(100)
#See that only 5 of the 6 factors have observed eigenvalues greater than simulated eigenvalues
#Try to use 5 factors instead
factor A1-A5 C1-C5 E1-E5 N1-N5 O1-O5,pcf factor(5)
estat kmo
#rotate
rotate, orthogonal varimax blanks(.45)
