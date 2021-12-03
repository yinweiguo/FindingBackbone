*
* Global variables for backbone.f
*
      implicit none

      INTEGER n, ndim, Linput 

      PARAMETER ( n=10 )
      PARAMETER ( ndim=2 )
      PARAMETER ( Linput=20 )

      INTEGER
     . Percolation, Nsample, DEBUG,
     . Ldat , Lfield , Lout , Ldata1 , Ldata2 , Loutput , Lerror

      REAL*8 p

      CHARACTER*50
     . Inputfile , Outputfile , Datafile1 , Datafile2  , Errorfile

      COMMON
     . Percolation, Nsample, p, DEBUG, 
     . Ldat , Lfield , Lout , Ldata1 , Ldata2 , Loutput , Lerror ,
     . Inputfile, Outputfile , Datafile1 , Datafile2  , Errorfile
