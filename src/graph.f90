! Test for a Graphical Sequence (pg 21)
! INPUT: a sequence S of nonnegatitive integers of length p.
! OUTPUT: YES if the sequence is graphical, NO otherwise.

module graphical_sequence
   use stdlib_logger, log => global_logger
   use stdlib_sorting, only: sort
   implicit none

   private

   ! Declare variables for logger
   integer :: unit, iostat

   public :: graphical, is_graphical
contains
   subroutine graphical(S, p, test)
      ! S = the degree sequence of all verticies in Graph G
      ! p = number of verticies in Graph G (the "order" of G).

      ! Declare dummy variables
      integer, INTENT(IN) :: p
      integer, INTENT(IN), dimension(p) :: S
      logical, INTENT(OUT) :: test

      ! Declaire local variables
      integer, dimension(p) :: ds
      logical :: reverse = .true.   ! sort in order of non-increasing values
      integer :: index = 1
      integer :: i, j

      ! Declaire logger variables
      character(len=256) :: message

      call log%add_log_file('log.txt', unit, &
                            position='asis', stat=iostat)
      if (iostat /= success) then
         error stop 'Unable to open "log.txt".'
      end if

      call log%configure(level=debug_level)

      ds = S

      ! (1) If there exists an integer d in S such that d > p - 1, then graphable = FALSE
      if (maxval(ds) > p - 1) then
         test = .false.
         call log%log_message('There is a degree sequence value  > number of verticies -1.', &
                              module='graphical_sequence', procedure='graphical', &
                              prefix='NOT GRAPHICAL')

      else
         do
            ! (2) if the sequence is all zeros, then graphable = TRUE
            if (sum(ds(index:p)) == 0) then
               test = .true.
               call log%log_message('The degree sequence is all zeros.', &
                                    module='graphical_sequence', procedure='graphical', &
                                    prefix='GRAPHICAL')
               exit

               ! (3) if the sequence contains a negative number, then graphable = FALSE
            else if (minval(ds(index:p)) < 0) then
               test = .false.
               call log%log_message('The degree sequence contains a negative number.', &
                                    module='graphical_sequence', procedure='graphical', &
                                    prefix='NOT GRAPHICAL')
               exit

               ! (4) reorder the sequence so that it is nonincreasing.
            else
               call sort(ds, reverse)
               write (message, 100) 'Sorted sequence: ', index, (ds(j), j=1, p)
               call log%log_DEBUG(trim(message), module='graphical_sequence', procedure='graphical')

               ! (5) Delete the first term d(1) from the sequence and subtract '1' from the next d(1) terms
               !     to form a new sequence.  Go to Step (2).
               forall (i=index + 1:index + ds(index))
                  ds(i) = ds(i) - 1
               end forall
               write (message, 100) 'New sequence: ', index, (ds(j), j=1, p)
               call log%log_DEBUG(trim(message), module='graphical_sequence', procedure='graphical')

               index = index + 1
            end if
         end do
      end if
100   format(a, t25, i4, 2x, *(i4))

   end subroutine graphical

   pure function is_graphical(S, p) result(test_result)
      ! S = the degree sequence of all verticies in Graph G
      ! p = number of verticies in Graph G (the "order" of G).

      ! Declare dummy variables
      integer, INTENT(IN) :: p
      integer, INTENT(IN), dimension(p) :: S
      logical :: test_result

      ! Declaire local variables
      integer, dimension(p) :: ds
      logical, PARAMETER :: reverse = .true.   ! sort in order of non-increasing values
      integer :: index
      integer :: i, j

      ds = S
      index = 1

      ! (1) If there exists an integer d in S such that d > p - 1, then graphable = FALSE
      if (maxval(ds) > p - 1) then
         test_result = .false.

      else
         do
            ! (2) if the sequence is all zeros, then graphable = TRUE
            if (sum(ds(index:p)) == 0) then
               test_result = .true.
               exit

               ! (3) if the sequence contains a negative number, then graphable = FALSE
            else if (minval(ds(index:p)) < 0) then
               test_result = .false.
               exit

               ! (4) reorder the sequence so that it is nonincreasing.
            else
               call sort(ds, reverse)

               ! (5) Delete the first term d(1) from the sequence and subtract '1' from the next d(1) terms
               !     to form a new sequence.  Go to Step (2).
               forall (i=index + 1:index + ds(index))
                  ds(i) = ds(i) - 1
               end forall

               index = index + 1
            end if
         end do
      end if

   end function is_graphical
end module graphical_sequence
