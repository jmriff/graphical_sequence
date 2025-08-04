program main
   use graphical_sequence

   implicit none

   integer, dimension(:), ALLOCATABLE :: dsequence
   integer :: order   ! order of G
   logical :: result

   integer :: i

   print *, "What is the order of G graph?"
   read (*, *) order

   allocate (dsequence(1:order))
   print '(a,i0,a)', 'Enter the degree sequence for ', order, ' verticies:'

   READ (*, *) (dsequence(i), i=1, order)

   print *, 'Thank you for the info.  Time to get busy...'

   call graphical(dsequence, order, result)
   print *, 'Result passed back from graphical() subroutine: ', result

   print *, 'Result passed back from is_graphical() function: ', is_graphical(dsequence, order)

   deallocate (dsequence)

end program main
