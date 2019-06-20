# Frequently asked questions  

## Table of Contents  

[Where does the PCR-bias correction algorithm come from?](#where-does-the-pcr-bias-correction-algorithm-come-from)  
[What kind of data can be corrected by BiasCorrector?](#what-kind-of-data-can-be-corrected-by-biascorrector)   
[Do my input files need to be in a special format?](#do-my-input-files-need-to-be-in-a-special-format)  
[Are there any requirements for naming the files?](#are-there-any-requirements-for-naming-the-files)  
[What is exactly done during BiasCorrector's data preprocessing?](#what-is-exactly-done-during-biascorrectors-data-preprocessing)  
[What are the regression statistics?](#what-are-the-regression-statistics)  
[What are 'substitutions' in my final results?](#what-are-substitutions-in-my-final-results)  


## Where does the PCR-bias correction algorithm come from?  

BiasCorrector is the user friendly implementation of the algorithms, described by Moskalev et. al in their article *'Correction of PCR-bias in quantitative DNA methylation studies by means of cubic polynomial regression'*, published 2011 in *Nucleic acids research, Oxford University Press* (DOI: [https://doi.org/10.1093/nar/gkr213](https://doi.org/10.1093/nar/gkr213)).  

### Citation:  
```
@article{10.1093/nar/gkr213,
    author = {Moskalev, Evgeny A. and Zavgorodnij, Mikhail G. and Majorova, Svetlana P. and Vorobjev, Ivan A. and Jandaghi, Pouria and Bure, Irina V. and Hoheisel, Jörg D.},
    title = "{Correction of PCR-bias in quantitative DNA methylation studies by means of cubic polynomial regression}",
    journal = {Nucleic Acids Research},
    volume = {39},
    number = {11},
    pages = {e77-e77},
    year = {2011},
    month = {04},
    abstract = "{DNA methylation profiling has become an important aspect of biomedical molecular analysis. Polymerase chain reaction (PCR) amplification of bisulphite-treated DNA is a processing step that is common to many currently used methods of quantitative methylation analysis. Preferential amplification of unmethylated alleles—known as PCR-bias—may significantly affect the accuracy of quantification. To date, no universal experimental approach has been reported to overcome the problem. This study presents an effective method of correcting biased methylation data. The procedure includes a calibration performed in parallel to the analysis of the samples under investigation. DNA samples with defined degrees of methylation are analysed. The observed deviation of the experimental results from the expected values is used for calculating a regression curve. The equation of the best-fitting curve is then used for correction of the data obtained from the samples of interest. The process can be applied irrespective of the locus interrogated and the number of sites analysed, avoiding an optimization of the amplification conditions for each individual locus.}",
    issn = {0305-1048},
    doi = {10.1093/nar/gkr213},
    url = {https://dx.doi.org/10.1093/nar/gkr213},
    eprint = {http://oup.prod.sis.lan/nar/article-pdf/39/11/e77/16775711/gkr213.pdf},
}
```


## What kind of data can be corrected by BiasCorrector?  

BiasCorrector can handle two types of input data:  
  
- Type 1: one locus in many samples (e.g. pyrosequencing data)  
- Type 2: many loci in one sample (e.g. next-generation sequencing data or microarray data)


## Do my input files need to be in a special format?  

Yes, BiasCorrector places very strict requirements on the file format. Below is a description of the exact requirements for the two types of input data, which differ in several aspects. However, all uploaded files must  
- be in CSV format [file endings: *.csv and *.CSV] 
- contain the column headers in the first row  
- the number of CpG-sites per locus ID is equal in every provided file 


### Type 1: one locus in many samples (e.g. pyrosequencing data)  

- Experimental data:  

  -- the first column contains the sample IDs (alphanumeric)  
  -- sample IDs may occure more than once (indicating repeated measurements of the same sample; in this case, the mean-values of the repeated measurements will be used for bias correction)   
  -- all other columns contain the results of your methylation analysis for each CpG-side of the respective sample  
  -- missing values are not allowed (rows containing empty cells [= missing values] will be removed during the data preprocessing step)  
  
- Calibration data:  

  -- the first column contains the degrees of true methylation of the calibration sample (calibration steps, numeric)  
  -- calibration steps may occure more than once (indicating repeated measurements of the same calibration sample; in this case, the mean-values of the repeated measurements will be used for calculation of the calibration curve)  
  -- all other columns contain the results of the methylation analysis for each CpG-side of the respective calibration sample  
  -- missing values are not allowed (rows containing empty cells [= missing values] will be removed during the data preprocessing step)  
  -- a minimum of four distinct calibration steps are provided  
  -- the calibration steps (CS) must be in the range 0 <= CS <= 100  
  

### Type 2: many loci in one sample (e.g. next-generation sequencing data or microarray data)  

- Experimental data: 

  -- the first column contains the locus IDs (alphanumeric)  
  -- locus IDs may occure more than once (indicating repeated measurements of the same locus; in this case, the mean-values of the repeated measurements will be used for bias correction)
  -- all other columns contain the results of your methylation analysis for each CpG-side of the respective locus  
  
- Calibration data:  

  -- the first column contains the locus IDs (alphanumeric)  
  -- locus IDs may occure more than once (indicating repeated measurements of the same locus; in this case, the mean-values of the repeated measurements will be used for bias correction)
  -- all other columns contain the results of your methylation analysis for each CpG-side of the respective locus  
  -- for bias correction of type 2 data, you need to provide one separate calibration file for each degree of methylation  
  -- a minimum of four calibration data files (four distinct calibration steps) are provided  
  -- all provided calibration files have equal dimensions (number of rows * number of columns), equal column names and equal locus IDs  
  -- the calibration steps (CS) must be in the range 0 <= CS <= 100  


## Are there any requirements for naming the files?  

Files of the input data type 1 (one locus in many samples) do not place specific requirements for naming the files.  
On the contrary, files of the input data type 2 (many loci in one sample), and in particular the files containing the calibration data, place very strict requirements on the file naming:  

Every filname must be of the following pattern:  'anyfilename'_CS###.csv  

The suffix '_CS###.csv'  
- must begin with '_CS', otherwise the file is going to be rejected during the data preprocessing step (CS is the short form of 'calibration step')  
- the placeholder '###' must be replaced with the respective degree of methylation of the calibration data contained in the specific file  
-- it can be or an integer number between 0 and 100 (integers < 0 or > 100 will be rejected during the data preprocessing step)  
-- or a numeric number between 0 an 100, indicated by an underscore ('_') as decimal separator (e.g. '12_5' meaning '12.5')   

Example: to upload a file for bias correction of type 2, that contains the calibration data for the calibration step '12.5' (true degree of calibration = 12.5) it need to be named the following:  
  *'my-calibrationfile_CS12_5.csv'*  


## What is exactly done during BiasCorrector's data preprocessing?  

During the preprocessing, all requirements on the input files as stated in [Do my input files need to be formated?](#do-my-input-files-need-to-be-in-a-special-format) are checked. Furhtermore, the rowmeans of all CpG-columns are calculated for every provided file. 

If any of the abovementioned file requirements is not met, an error will occur, e.g. if any calibration step is not within the range of 0 <= CS <= 100 or if you provided less then four calibration steps with your input data. 


## What are the regression statistics?

The regression statistics table shows the regression parameters of the hyperbolic regression and the cubic regression. 
- Column 1 presents the CpG-site's name. 
- Column 2 presents the mean of the relative absolute errors for every CpG-site. 
- Columns 3-6 present the sum of squared error of the hyperbolic regression ('SSE [h]') and the regression parameters used to calculate the hyperbolic regression curves for the respective CpG-site. 
- Columns 7-11 present the sum of squared error of the cubic regression ('SSE [c]') and the regression parameters used to calculate the cubic regression curves. 
- The rows highlighted with a green background colour indicate the regression equation, that in comparison of the sum of squared errors better fits the data points for the respecitve CpG-site. 


## What are 'substitutions' in my final results?

Substitutions occur, when no result is found in the range of plausible values between 0 and 100 during the BiasCorrection. A 'border zone' is implemented in the ranges 0 – 10% and 100 + 10%. If a result is in the range -10 < x < 0 percentage or 100 < x < 110 percentage, the value is substituted in the final results with 0 percentage or 100 percentage respectively. 
Values beyond these border zones will be substituted with a blank value in the final output, as they seem implausible and could indicate substantial errors in the underlying data. 
For a detailed feedback, the substitutions table shows the results of the algorithm 'BiasCorrected value' and the corresponding substitution 'Substituted value' for the respective CpG-site. 
