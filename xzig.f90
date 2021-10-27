program xzig
! 10/26/2021 01:38 PM demonstrate random number generator that is effectively pure
use ziggurat_mod, only: uni
implicit none
integer, parameter :: n = 5, nseeds = 2, seeds(nseeds) = [654321,123456]
integer :: iter, iseed
do iter=1,3
   do iseed=1,nseeds
      write (*,"(i8,1000f8.4)") seeds(iseed),uni(n,seeds(iseed))
   end do
end do
end program xzig
