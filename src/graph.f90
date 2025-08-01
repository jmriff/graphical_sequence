! Test for a Graphical Sequence (pg 21)
! INPUT: a sequence S of nonnegatitive integers of length p.
! OUTPUT: YES if the sequence is graphical, NO otherwise.

module graphical_sequence
   implicit none
   private

   public :: graphical
contains
   subroutine graphical(S, p, test)
      ! S = the degree sequence of all verticies in Graph G
      ! p = number of verticies in Graph G (the "order" of G).

      use stdlib_sorting, only: sort
      integer, INTENT(IN) :: p
      integer, INTENT(IN), dimension(p) :: S
      logical, INTENT(OUT) :: test

      integer, dimension(p) :: ds
      logical :: reverse = .true.   ! sort in order of non-increasing values
      integer :: index = 1
      integer :: i, j

      ds = S

      ! (1) If there exists an integer d in S such that d > p - 1, then graphable = FALSE
      if (maxval(ds) > p - 1) then
         test = .false.
         print *, 'NOT GRAPHICAL: There is a degree sequence value  > number of verticies -1.'

      else
         do
            ! (2) if the sequence is all zeros, then graphable = TRUE
            if (sum(ds(index:p)) == 0) then
               test = .true.
               print *, 'GRAPHICAL: the degree sequence is all zeros.'
               exit

               ! (3) if the sequence contains a negative number, then graphable = FALSE
            else if (minval(ds(index:p)) < 0) then
               test = .false.
               print *, 'NOT GRAPHICAL: the degree sequence contains a negative number.'
               exit

               ! (4) reorder the sequence so that it is nonincreasing.
            else
               call sort(DS, reverse)
               print 100, 'Sorted degree sequence: ', index, (ds(j), j=1, p)

               ! (5) Delete the first term d(1) from the sequence and subtract '1' from the next d(1) terms
               !     to form a new sequence.  Go to Step (2).
               forall (i=index + 1:index + ds(index))
                  ds(i) = ds(i) - 1
               end forall
               print 100, 'new sequence: ', index, (ds(j), j=1, p)
               print *

               index = index + 1
            end if
         end do
      end if
100   format(a, t40, i4, 2x, *(i4))

   end subroutine graphical
end module graphical_sequence
