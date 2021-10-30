program xzig_pure
! 10/27/2021 12:54 AM driver for random_int_32 and random_uni
use ziggurat_pure_mod, only: dp, random_int_32, random_uni
implicit none
integer, parameter :: seed = 123, nran = 10000000
integer            :: iran(nran),state
real(kind=dp)      :: xran(nran)
state = seed
call random_int_32(state,iran)
call random_uni(state,xran)
write (*,"(2a12)") "min","max"
write (*,"(2i12)") minval(iran),maxval(iran)
write (*,"(/,4a10)") "min","max","mean","mean_sq"
write (*,"(4f10.6)") minval(xran),maxval(xran),sum(xran)/nran,sum(xran**2)/nran
end program xzig_pure
