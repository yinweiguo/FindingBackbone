      SUBROUTINE ReadInputfile()
      Include 'globvar.inc'
*---------------------------------------------------------------------
* Reads the inputfile and initializes parameters.

      CHARACTER*40     Answer, FORM
      INTEGER          Iseed(4) , ScreenUnit, J
      PARAMETER        ( ScreenUnit = 6 )

      Inputfile = 'input.dat'
C      Read*, Inputfile

C      if ( n .gt. 0 ) J = 1
C      if ( n/10 .gt. 0 ) J = 2
C      if ( n/100 .gt. 0 ) J = 3
c      if ( n/1000 .gt. 0 ) J = 4
c      if ( n/10000 .gt. 0 ) J = 5
c      if ( n/100000 .gt. 0 ) stop 'n > 100000'
c      write (FORM, '(5H(A3,I,I1,4H,A4))') J
c      write (Inputfile, FORM) 'In', n, '.dat'
c      write (ScreenUnit,*) 'FORM = ', FORM

      write (ScreenUnit,*) 'Inputfile = ', Inputfile

      Open(Linput , file=Inputfile)

      Call CheckLine('FILENAMES')

      Read(Linput,10) Datafile1
      Read(Linput,10) Datafile2
      Read(Linput,10) Outputfile
      Read(Linput,10) Errorfile

 10   Format(a50)

      Call CheckLine('SEEDS')
      Read(Linput,*) Iseed(1)
      Read(Linput,*) Iseed(2)
      Read(Linput,*) Iseed(3)
      Read(Linput,*) Iseed(4)

* Immediately initialize random generator

      Call Setrn(Iseed)

      Call CheckLine('TYPE OF PERCOLATION')

      Read(Linput,*) Answer
      IF ( Answer(1:4) .eq. 'bond' ) THEN
         Percolation = 1
      ELSEIF ( Answer(1:4) .eq. 'site' ) THEN
         Percolation = 2
      ELSE
         Print*, 'ERROR in input in specifying TYPE OF PERCOLATION'
         Stop
      ENDIF

      Call CheckLine('PROBABILITY')

      Read(Linput,*) p

      Call CheckLine('VOLUME OF SAMPLES')

      Read(Linput,*) Nsample

      Call CheckLine('DEBUG')

      Read(Linput,*) DEBUG

      Call CheckLine('END OF INPUTFILE')

      Close(Linput)

* Open files

      Ldat    = 10
      Lout    = 10
      Lfield  = 15
      Ldata1  = 21
      Ldata2  = 22
      Loutput = 23
      Lerror  = 24

      Open(Ldata2,file=Datafile2,form='unformatted',
     .     access='direct',recl=28)

      Open(Loutput,file=Outputfile)

      IF ( Errorfile(1:12) .eq. 'screenoutput' ) THEN
         Lerror = ScreenUnit
      ELSE
         Open(Lerror , file=Errorfile)
      ENDIF
c
*      Open(Ldat , file='/dev/null')
c
c* Now write headers of output and error files
c
c*      Call Idate(Date)
c*      Call Itime(Time)
c
c      Write(Loutput, *) 'Output file of program network'
c      Write(Loutput, *) '------------------------------'
c      Write(Loutput, *)
c      Write(Loutput, *) 'Inputfile  : ' , Inputfile(1:40)
c      Write(Loutput,20) 'Started    : ' , 
c     .                  Date(1) , Date(2) , Date(3) ,
c     .                  Time(1) , Time(2) , Time(3)
c      Write(Loutput, *)
c      Write(Loutput, *) 'Filenames:'
c      Write(Loutput, *) '   Datafile 1 : ' , Datafile1
c      Write(Loutput, *) '   Datafile 2 : ' , Datafile2
c      Write(Loutput, *) '   Errorfile  : ' , Errorfile
c      Write(Loutput, *) '   Outputfile : ' , Outputfile
c      Write(Loutput, *)
cC      IF ( Periodic ) THEN
cC         Write(Loutput,*) '   Vertical BCs          : periodic'
cC      ELSE
cC         Write(Loutput,*) '   Vertical BCs          : free'
cC      ENDIF
c
c      Write(Lerror, *) 'Error file of program network'
c      Write(Lerror, *) '-----------------------------'
c      Write(Lerror, *)
c      Write(Lerror, *) 'Inputfile  : ' , Inputfile(1:40)
c      Write(Lerror,20) 'Started    : ' , 
c     .                  Date(1) , Date(2) , Date(3) ,
c     .                  Time(1) , Time(2) , Time(3)
c      Write(Lerror, *)
c
c 20   Format(a13,i2,'-',i2,'-',i4,' at ' , i2,':',i2,':',i2)
c
c* Immediately write all information of the input file to the data2
c* file. Also write the starting date and time, and consider this as 
c* a unique identification set, as to link output and datafile if
c* necessary.
c
c      Write(Ldata2,Rec=1) (Date(i),Time(i),i=1,3)
c      Write(Ldata2,Rec=2) (Iseed(i),i=1,4)

      return
      end

*---------------------------------------------------------------------
      SUBROUTINE Checkline(Words)
*---------------------------------------------------------------------
      Include 'globvar.inc'
*---------------------------------------------------------------------

      CHARACTER*(*) Words
      CHARACTER*60  Line

      Call Readline(Line)

      IF ( Line(1:Len(Words)) .ne. Words ) THEN
         Print*, 'Error in reading inputfile for'
         Print*, Words
         Close(Linput)
         Stop
      ENDIF

      RETURN
      END


*---------------------------------------------------------------------
      SUBROUTINE Readline(Line)
*---------------------------------------------------------------------
      Include 'globvar.inc'
*---------------------------------------------------------------------

      CHARACTER*(*) Line

 10   Read(Linput,'(a60)',END=100) Line

      IF ( Line(1:1) .eq. '#' .or. Line(1:1) .eq. ' ' ) GOTO 10

      RETURN

 100  Print*, 'ERROR: End of inputfile reached'
      Stop

      END
