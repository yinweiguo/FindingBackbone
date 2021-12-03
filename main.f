*************************************************************************
* A program to find the current-carrying backbone
* written by Wei-Guo Yin in 1999 @ Fudan University, Shanghai, China
* References:
*   [1] Wei-Guo Yin, and R. Tao, “Rapid Algorithm for identifying
*       backbones in the two-dimensional percolation model,” 
*       Int. J. Mod. Phys. C: Physics and Computers 14, 1427 (2003).
*   [2] Wei-Guo Yin, and R. Tao, “Algorithm for finding two-dimensional
*       site-percolation backbones,” Physica B 279, 84 (2000).
*************************************************************************
      PROGRAM  BACKBONE
      Include 'globvar.inc'

      INTEGER np1, nface, nedge, nvertice

      PARAMETER ( np1 = n+1 )
      PARAMETER ( nface = n*n+1 )
      PARAMETER ( nedge = ndim * n * np1 - n )
      integer*2 kres( n, np1, ndim )
      integer   I1( n, np1, ndim ), I2( n, np1, ndim )
      integer   J1( n, np1, ndim ), J2( n, np1, ndim )
      INTEGER   ID( n, np1, ndim )
      INTEGER  NUMCLUS, nloop, itrial, ix, iy
      real   time1, time2

      call  ReadInputfile()

      call  vertice( I1, I2, n, np1, ndim )

      call  face( J1, J2, n, np1, ndim )

C initialize graph (i.e., resistor network).

      nloop = 1
      itrial = 1

 666  if ( Percolation .eq. 1 ) then
         call bonds ( kres, p, n, np1, ndim )          ! bond percolation
      else 
         call sites ( ID, kres, p, n, np1, ndim )      ! site percolation
      endif

      if ( DEBUG .eq. 1 ) then
         write(Lerror, *)
         write(Lerror, *) 'Sample ', nloop, ' , trial ', itrial
        call  draw_lattice( kres, n, np1, ndim, Lerror )
      endif


C faces connecting

      call cputime(time1)

      call cchkold ( nface, nedge, J1, J2, kres, ID, NUMCLUS, 0 )


      if ( DEBUG .eq. 1 ) then
       write(Lerror, *) 'Face labeling...'
       write(Lerror, *)
       do iy = n, 1, -1
          write (Lerror, 101) (ID(ix,iy,1), ix=1,n )
  101     format(1x,10(i2,',')) 
       enddo
       write(Lerror, *)
      endif


C judge if the infinite cluster exists

      if ( NUMCLUS .eq. 0 ) then
         if ( DEBUG .eq. 1 )
     .      write(Lerror,*) 'No spanning cluster'
         itrial = itrial + 1
         goto 666
      else
         if ( DEBUG .eq. 1 ) 
     .      write(Lerror,*) 'Has spanning cluster'
      endif

C remove dangling ends and recognize dangling loops

      call   remove ( J1, J2, I1, I2, kres, ID, nface,
     .      n, np1, ndim, nvertice )

      if ( DEBUG .eq. 1 ) then
       write(Lerror, *)
       write(Lerror, *) 'After remove()'
       call  draw_lattice( kres, n, np1, ndim, Lerror )
C       write(Lerror, *) 'nvertice, n*np1 = ', nvertice, n*np1
      endif

C vertices connecting

      call cchkold ( nvertice, nedge, I1, I2, kres, ID, NUMCLUS, 1 )

      call cputime(time2)

      if ( DEBUG .eq. 1 ) then
       write(Lerror, *) 'The backbone:'
       call  draw_lattice( kres, n, np1, ndim, Lerror )
 
       write(Lerror, *) 'Vertice labeling...'
       write(Lerror, *)
       do iy = np1, 1, -1
          write (Lerror, 102) (ID(ix,iy,1), ix=1,n )
  102     format(1x,10(i2,',')) 
       enddo
       write(Lerror, *)
      endif

      write (Loutput, 100) nloop, time2-time1,
     .      (numclus - 2*n) / dble(nedge - 3*n + 1) 

 100  format (i5, f10.5, f12.7)

      nloop = nloop + 1
      itrial = 1

      if ( nloop .le. Nsample ) goto 666

      stop
      end

