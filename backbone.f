      subroutine face( J1, J2, n, np1, ndim )
      integer    J1( n, np1, ndim ), J2( n, np1, ndim )

      do 10 iy = 2, n
      do 10 ix = 1, n
         J1(ix,iy,1) = n*(iy-2) + ix
         J2(ix,iy,1) = J1(ix,iy,1) + n
  10  continue
      do ix = 1, n
         J2(ix,1,1) = ix
         J1(ix,np1,1) = n*(np1-2) + ix
      enddo

      nnp1 = n*n + 1

      do 20 iy = 1, n
         J1(1,iy,2) = nnp1
         J2(1,iy,2) = n*(iy-1) + 1
         do ix = 2, n
            J1(ix,iy,2) = n*(iy-1) + (ix-1)
            J2(ix,iy,2) = J1(ix,iy,2) + 1
         enddo
  20  continue

c      do iy = n, 2, -1
c         write (*, 100) (J1(ix,iy,1), J2(ix,iy,1), ix=1,n )
c 100     format(1x,10('(',i2,',',i2,')')) 
c      enddo
c      do iy = n, 1, -1
c         write (*, 100) (J1(ix,iy,2), J2(ix,iy,2), ix=1,n )
c      enddo
c      pause 'write (J1, J2)'
c      print *

      return
      end

      subroutine vertice( I1, I2, n, np1, ndim )
      integer    I1( n, np1, ndim ), I2( n, np1, ndim )

      nm1 = n-1
      do 10 iy = 1, np1
         do ix = 1, nm1
            I1(ix,iy,1) = n*(iy-1) + ix
            I2(ix,iy,1) = I1(ix,iy,1) + 1
         enddo
         I1(n,iy,1) = n*(iy-1) + n
         I2(n,iy,1) = n*(iy-1) + 1
  10  continue

      do 20 iy = 1, n
      do 20 ix = 1, n
         I1(ix,iy,2) = n*(iy-1) + ix
         I2(ix,iy,2) = I1(ix,iy,2) + n
  20  continue

C      do iy = np1, 1, -1
C         write (*, 100) (I1(ix,iy,1), I2(ix,iy,1), ix=1,n )
C 100     format(1x,10('(',i2,',',i2,')')) 
C      enddo
C      do iy = n, 1, -1
C         write (*, 100) (I1(ix,iy,2), I2(ix,iy,2), ix=1,n )
C      enddo
C      pause 'write (I1, I2)'
C      print *

      return
      end



      subroutine bonds ( kres, p, n, np1, ndim )
      integer*2  kres( n, np1, ndim )
      real*8 p, rannyu  

c
c   Choose new bond variables.
c   Here, the loop is over the edges of the graph. KRES is whether the
c   array exists or not.
c
      do 10 iy = 2, n
      do 10 ix = 1, n
         if ( rannyu() .le. p ) then
            kres(ix,iy,1) = 1
         else
            kres(ix,iy,1) = 0
         endif

         if ( rannyu() .le. p ) then
            kres(ix,iy,2) = 1
         else
            kres(ix,iy,2) = 0
         endif
  10  continue

c
c  Set the upper and lower rows of the resistances.
c  I'm doing something sneaky here. The resistances on the top and
c  bottom rows are actually the inverse resistances. KRES is just
c  a label whether the bond is activated or not.
c
      do 20 ix = 1, n
         kres(ix,1,1)   = 1
         kres(ix,np1,1) = 1
         kres(ix,np1,2) = 0

         if ( rannyu() .le. p ) then
            kres(ix,1,2) = 1
         else
            kres(ix,1,2) = 0
         endif
  20  continue

      do iy = 2, n
         kres(n,iy,1) = 0                      ! open boundary condition
      enddo

      return
      end


      subroutine sites ( ID, kres, p, n, np1, ndim )
      integer*2  kres ( n, np1, ndim )
      integer  ID( n, np1)
      real*8 p, rannyu

c
c   Choose new site variables.
c   Here, the loop is over the vertices of the graph. ID is whether the
c   array exists or not.
c
      do 10 iy = 2, n
      do 10 ix = 1, n
         if ( rannyu() .le. p ) then
            ID(ix,iy) = 1
         else
            ID(ix,iy) = 0
         endif
  10  continue

c
c  Set the upper and lower rows of the resistances.
c  I'm doing something sneaky here. The resistances on the top and
c  bottom rows are actually the inverse resistances. KRES is just
c  a label whether the bond is activated or not.
c
      do 20 ix = 1, n
         ID(ix,1)   = 1
         ID(ix,np1) = 1
  20  continue

      nm1 = n-1

      do 30 iy = 2, n
      do 30 ix = 1, nm1
         if ( ID(ix,iy) + ID(ix+1,iy) .eq. 2 ) then
            kres(ix,iy,1) = 1
         else
            kres(ix,iy,1) = 0
         endif

         if ( ID(ix,iy) + ID(ix,iy+1) .eq. 2 ) then 
            kres(ix,iy,2) = 1
         else
            kres(ix,iy,2) = 0
         endif
  30  continue

c
c  Set the upper and lower rows of the resistances.
c  I'm doing something sneaky here. The resistances on the top and
c  bottom rows are actually the inverse resistances. KRES is just
c  a label whether the bond is activated or not.
c
      do 40 ix = 1, n
         kres(ix,1,1)   = 1
         kres(ix,np1,1) = 1
         kres(ix,np1,2) = 0

         if ( ID(ix,1) + ID(ix,2) .eq. 2 ) then
            kres(ix,1,2) = 1
         else
            kres(ix,1,2) = 0
         endif
  40  continue

      do iy = 2, n
c         if ( ID(n,iy) + ID(1,iy) .eq. 2 ) then
c            kres(n,iy,1) = 1
c         else
c            kres(n,iy,1) = 0
c         endif
     
         kres(n,iy,1) = 0                      ! open boundary condition
      enddo
      return
      end


      subroutine remove ( J1, J2, I1, I2, kres, ID, nface,
     .      n, np1, ndim, nvertice )
      integer*2  kres( n, np1, ndim )
      integer    I1( n, np1, ndim ), I2( n, np1, ndim )
      integer    J1( n, np1, ndim ), J2( n, np1, ndim )
      INTEGER    ID( nface )


C ... STEP 1: Remove non-spanning clusters connected to the TOP and
C             BOTTOM raws.
C
C      goto 200                      !   skip STEP 1

      i = 1
 50   if ( i .gt. n ) goto 100
      do 40 ix = n, i , -1
         if ( ID(J1(i,1,2)) .eq. ID(J2(ix,1,2)) ) then
            do j = i, ix
               kres ( j, 1, 2 ) = 0
            enddo
            i = ix + 2
            goto 50
         endif
 40   continue
      i = i + 1
      goto 50

 100  CONTINUE     ! start checking the bottome raw

      i = 1
 70   if ( i .gt. n ) goto 200
      do 60 ix = n, i , -1
         if ( ID(J1(i,n,2)) .eq. ID(J2(ix,n,2)) ) then
            do j = i, ix
               kres ( j, n, 2 ) = 0
            enddo
            i = ix + 2
            goto 70
         endif
 60   continue
      i = i + 1
      goto 70

 200  CONTINUE     ! END checking the bottome raw

C ... STEP 2: Remove dangling ends
C 
      do 10 iy = 2, n
      do 10 ix = 1, n
         if ( kres(ix,iy,1) .eq. 1 ) then
            if ( ID(J1(ix,iy,1)) .eq. ID(J2(ix,iy,1)) )
     .         kres(ix,iy,1) = 0
         endif
  10  continue

      do 20 iy = 1, n
      do 20 ix = 1, n
         if ( kres(ix,iy,2) .eq. 1 ) then
            if ( ID(J1(ix,iy,2)) .eq. ID(J2(ix,iy,2)) )
     .         kres(ix,iy,2) = 0
         endif
  20  continue


C ... STEP 3: Recognize dangling loops

      nvertice = n * np1

      nm1 = n-1
      do 30 iy = 3, nm1                                          
      do 30 ix = 1, n                                            !     *
         ixp1 = ix + 1                                           !     |
         if ( kres(ix,iy,1) + kres(ixp1,iy,1) .eq. 2 ) then      !  *--*--*
            if ( ID(J1(ix,iy,1)) .eq. ID(J2(ixp1,iy,1)) ) then   !     |
               nvertice = nvertice + 1                           !     *
               I2 (ix,iy,1) = nvertice  
               I1 (ixp1,iy,2) = nvertice
            elseif ( ID(J2(ix,iy,1)) .eq. ID(J1(ixp1,iy,1)) ) then
               nvertice = nvertice + 1
               I1 (ixp1,iy,1) = nvertice
               I1 (ixp1,iy,2) = nvertice
            endif
         endif
  30  continue

      return
      end


c**************************************************************************
c**************************************************************************

c CCHKOLD -- uncompressed version (i.e. includes KB).
c
c Subroutine CCHK:  Finds the connected components of an arbitrary
c   undirected graph, using a generalized and streamlined version of
c   the Hoshen-Kopelman algorithm.  Follows ADS 10/25/87 notes, inspired
c   by ideas of Wang and Shiloach-Vishkin.
c
c Version 2  (10/26/87)
c
c
C Vertices are numbered 1, 2, ..., N.
c
c Each undirected edge  <I1, I2>  appears exactly once as an ordered pair
c   (I1, I2) in the input arrays -- it doesn't matter in which order.
c
c For flexibility in applications, we use a redundant data structure:
c   the edge  <I1, I2>  is in fact present only if the corresponding entry
c   in the array KB is set to 1.
C (We have in the back of our mind that we may be calling CCHK repeatedly,
C   for graphs G that are distinct subgraphs of a given graph G*.
c   Then the driver program can set up I1 and I2 once and for all
c   according to the structure of G*, and set KB to specify the subgraph G.)
c If this flexibility is not needed, references to KB can be eliminated.
C   
C
C
C INPUT ARGUMENTS:
C
C     N     - # OF VERTICES IN GRAPH
C	M	- # OF EDGES IN GRAPH
C     I1, I2- (POTENTIAL) EDGES IN GRAPH
C	KB	- KB = 1 FOR POTENTIAL EDGES WHICH ARE IN FACT "TURNED ON"
C     IFLAG - IFLAG = 0 FOR FACE CONNECTING, 1 FOR VERTICES CONNECTING
C
C OUTPUT ARGUMENTS:
C
C	ID	- COLOR CODES FOR CLUSTERS (= 1, 2, 3, ...)
C		    (ALSO USED DURING ALGORITHM AS POINTER ARRAY)
C
C     NUMCLUS - NUMBER OF BONDS BELONGING TO THE BACKBONE.
C
C
C	


      subroutine cchkold ( N, M, I1, I2, KB, ID, NUMCLUS, IFLAG )

	
	INTEGER  N, M
	INTEGER  I1(M), I2(M)
	INTEGER*2  KB(M)

	INTEGER  ID(N)
      INTEGER  NUMCLUS, IFLAG 


C
C STEP 0.  INITIALIZE POINTER ARRAY.
C
	DO 10  I = 1, N
	   ID(I) = I
10	CONTINUE

c
c Step 1.  For each undirected edge <I1, I2>, find the corresponding roots
c   in the pointer graph, IDINFI1 and IDINFI2.  Then make the higher-numbered
c   one of these roots point to the lower-numbered.  (While we're at it,
c   also make I1 and I2 also point to the lower-numbered root.  This is
c   not necessary, but it does speed up the computation at little cost.)
c
	do 20  k = 1, m
         if (kb(k) .eq. IFLAG)  then
	      i1temp = i1(k)
	      i2temp = i2(k)
c
25	      idinfi1 = id(i1temp)
	      if ( idinfi1 .ne. i1temp )  then
	         i1temp = idinfi1
	         goto 25
	      endif
c
27	      idinfi2 = id(i2temp)
	      if ( idinfi2 .ne. i2temp )  then
	         i2temp = idinfi2
	         goto 27
	      endif
c
	      if ( idinfi2 .le. idinfi1)  then
	         id(idinfi1) = idinfi2
	         id(i1(k))   = idinfi2
	         id(i2(k))   = idinfi2
	      else
	         id(idinfi2) = idinfi1
	         id(i1(k))   = idinfi1
	         id(i2(k))   = idinfi1
	      endif
	   endif
20	continue


c
c The pointer graph is now a forest of rooted trees with self-loops at
c   the roots, with the additional property that all directed edges
c   point "downwards", i.e.  ID(I) <= I  for all I.
c This means that we can find and renumber the clusters sequentially
c   in a single pass, with no additional storage.
c The lowest-numbered vertex in each cluster is its root;
c   and whenever any element of a cluster is about to be labelled,
c   its "parent" will already have been labelled.
c To save storage, we re-use the same array ID:  it contains the
c   cluster labels for the sites already handled, and the pointers
c   for the sites not yet handled.
c

      index = 1
      do 30  i = 1, n
         itemp = id(i)
         if ( itemp .eq. i )   then
            id(i) = index
            index = index + 1
         else
            id(i) = id(itemp)
         endif
30    continue
 
C      numclus = index - 1

      if ( IFLAG .eq. 0 ) then                        ! face connecting
         if ( id(n) .eq. id(n-1) ) then
            numclus = 0                               ! no infinite cluster
         else
            numclus = 1
         endif            
      else
         numclus = 0
         do k = 1, m
            if ( KB(k) .eq. 1 ) then
               if ( ID(I1(k)) .eq. 1 )  then
                  numclus = numclus + 1
               else
                  KB(k) = 0
               endif
            endif
         enddo
      endif  

 	return
	end



      subroutine draw_lattice( KB, n, np1, ndim, Lerror )
      integer*2  KB( n, np1, ndim )
      character*3 C (n), FORM*10

      if (n .ge. 100) then
         write(Lerror, *) 'n = ', n, ' is too large for plotting.'
         stop
      endif
      write(FORM, '(4H(1X,i2, 3HA3))')   n

      do 10 iy = np1, 1, -1
         do ix = 1, n
            if ( KB (ix, iy, 2) .eq. 1 ) then
               C (ix) = '|  '
            else
               C (ix) = '   '
            endif
         enddo
         write (Lerror, FORM) ( C(ix), ix = 1, n )

         do ix = 1, n
            if ( KB (ix, iy, 1) .eq. 1 ) then
               C (ix) = '*--'
            else
               C (ix) = '*  '
            endif
         enddo
         write (Lerror, FORM) ( C(ix), ix = 1, n )
  10  continue
      write(Lerror, *)

      return
      end
