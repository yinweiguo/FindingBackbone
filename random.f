      real*8 function rannyu()
      real*8 twom12
      parameter (twom12 = 1/4096.d0)
      common /rnyucm/ m1,m2,m3,m4,l1,l2,l3,l4
c
c     This is rannyu as modified by A. Sokal 9/26/85.
c        It is linear congruential with modulus m = 2**48, increment c = 1,
c        and multiplier a = (2**36)*m1 + (2**24)*m2 + (2**12)*m3 + m4. 
c        The multiplier is stored in common (see subroutine setrn)
c	 and is set to a = 31167285 (recommended by Knuth, vol. 2,
c	 2nd ed., p. 102).
c
      i1 = l1*m4 + l2*m3 + l3*m2 + l4*m1
      i2 = l2*m4 + l3*m3 + l4*m2
      i3 = l3*m4 + l4*m3
      i4 = l4*m4  +  1
      l4 = mod(i4, 4096)
      i3 = i3 + i4/4096
      l3 = mod(i3, 4096)
      i2 = i2 + i3/4096
      l2 = mod(i2, 4096)
      l1 = mod(i1 + i2/4096, 4096)
      rannyu = twom12*(dble(l1)+
     +         twom12*(dble(l2)+
     +         twom12*(dble(l3)+
     +         twom12*(dble(l4)))))
      return
      end


      BLOCK DATA init_rn
      common /rnyucm/ m(4),l(4)
      data m /   0,   1,3513, 821/
      data l /   0,   0,   0,   1/
      END


      subroutine Setrn(iseed)
      common /rnyucm/ m(4),l(4)
      integer iseed(4)
c
c     Multiplier is 31167285 = (2**24) + 3513*(2**12) + 821.
c        Recommended by Knuth, vol. 2, 2nd ed., p. 102.
c     (Generator is linear congruential with odd increment
c        and maximal period, so seed is unrestricted: it can be
c        either even or odd.)
c
c      data m /   0,   1,3513, 821/
c      data l /   0,   0,   0,   1/
c
      do 10 i = 1, 4
         l(i) = iseed(i)
10    continue
      return
      end


      subroutine savern(iseed)
      common /rnyucm/ m(4),l(4)
      integer iseed(4)
c
      do 10 i = 1, 4
         iseed(i) = l(i)
10    continue
      return
      end

