

# cmd <- paste0(
#   '"C:/Program Files/sirius/sirius.exe" ',
#   '-i=',
#   'C:/Temp/20241216/test',
#   ' -o=',
#   'C:/Temp/20241216/test/out',
#   ' --recompute ',
#   '--naming-convention=%compoundname ',
#   '--cores 8 ',
#   'formula ',
#   '--ppm-max 5 --ppm-max-ms2 5 ',
#   '-d ',
#   '--database "pubchem" ',
#   '-p ',
#   'qtof ',
#   'structure ',
#   'canopus'
# )
# 
# system(
#   cmd
# )

# config spectra-search --FormulaSearchSettings.applyFormulaConstraintsToBottomUp=false --IsotopeSettings.filter=true --UseHeuristic.useOnlyHeuristicAboveMz=650 --FormulaSearchDB=, --Timeout.secondsPerTree=0 --FormulaSettings.enforced=H,C,N,O,P --Timeout.secondsPerInstance=0 --AlgorithmProfile=qtof --SpectralMatchingMassDeviation.allowedPeakDeviation=10.0ppm --AdductSettings.enforced=, --AdductSettings.prioritizeInputFileAdducts=true --UseHeuristic.useHeuristicAboveMz=300 --IsotopeMs2Settings=IGNORE --MS2MassDeviation.allowedMassDeviation=10.0ppm --SpectralMatchingMassDeviation.allowedPrecursorDeviation=10.0ppm --FormulaSearchSettings.performDeNovoBelowMz=400.0 --FormulaSearchSettings.applyFormulaConstraintsToDatabaseCandidates=false --EnforceElGordoFormula=true --NumberOfCandidatesPerIonization=1 --FormulaSettings.detectable=B,S,Cl,Se,Br --NumberOfCandidates=10 --AdductSettings.fallback=[[M+H]+,[M+K]+,[M+Na]+] --FormulaSearchSettings.performBottomUpAboveMz=0 --FormulaResultThreshold=true --ExpansiveSearchConfidenceMode.confidenceScoreSimilarityMode=APPROXIMATE --StructureSearchDB=METACYC,BloodExposome,CHEBI,COCONUT,FooDB,GNPS,HMDB,HSDB,KEGG,KNAPSACK,LOTUS,LIPIDMAPS,MACONDA,MESH,MiMeDB,NORMAN,PLANTCYC,TeroMol,YMDB --RecomputeResults=false formulas fingerprints classes structures

