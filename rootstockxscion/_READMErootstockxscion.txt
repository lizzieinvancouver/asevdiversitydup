Started 5 May 2023
Started by Lizzie

Searched ISI ALL Fields on 5 May 2023 (at ETH Zurich):
rootstock* AND scion* AND grape*  
Returned 530 refs, saved as isi_5May2023.bib (when I searched via Harvard ISI on 7 May 2023 to get the Excel version, it was the same #)

See also: https://www6.inrae.fr/porte-greffe-vigne/Ressources/Publications-scientifiques/Articles-scientifiques

Then I subsampled for 100 entries in R using this code:
reviewthese <- sample(100, x=seq(1:530), replace=FALSE)
write.csv(reviewthese, "~/Desktop/rootstockscionlatlon.csv")

Next up! Entering the data in rootstockscionlatlon.csv
See rootstockscionlatlon_meta.txt for info on that. 
