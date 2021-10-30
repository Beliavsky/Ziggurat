module ziggurat_pure_mod
! Marsaglia, G. & Tsang, W.W. (2000) `The ziggurat method for generating random variables', J. Statist. Software, v5(8).
implicit none
private
public             :: dp, random_int_32, random_uni
interface random_int_32
   module procedure random_int_32_scalar, random_int_32_vec
end interface random_int_32
interface random_uni
   module procedure random_uni_scalar,random_uni_vec
end interface random_uni
integer, parameter :: dp = selected_real_kind(12, 60)
contains
!
pure elemental subroutine random_int_32_scalar(jsr,iran)
! generate random 32-bit integers
integer, intent(in out) :: jsr  ! state of RNG
integer, intent(out)    :: iran ! random integer
integer                 :: jz
jz   = jsr
jsr  = ieor(jsr, ishft(jsr,  13))
jsr  = ieor(jsr, ishft(jsr, -17))
jsr  = ieor(jsr, ishft(jsr,   5))
iran = jz + jsr
end subroutine random_int_32_scalar
!
pure subroutine random_int_32_vec(jsr,iran)
! generate random 32-bit integers
integer, intent(in out) :: jsr     ! state of RNG
integer, intent(out)    :: iran(:) ! random integers
integer                 :: i
do i=1,size(iran)
   call random_int_32_scalar(jsr,iran(i))
end do
end subroutine random_int_32_vec
!
pure elemental subroutine random_uni_scalar(jsr,xran)
integer      , intent(in out) :: jsr  ! state of RNG
real(kind=dp), intent(out)    :: xran ! random uniform variate
integer                       :: iran
call random_int_32(jsr,iran)
xran = 0.2328306e-9_dp*iran + 0.5_dp
end subroutine random_uni_scalar
!
pure subroutine random_uni_vec(jsr,xran)
integer      , intent(in out) :: jsr     ! state of RNG
real(kind=dp), intent(out)    :: xran(:) ! random uniform variates
integer                       :: i
do i=1,size(xran)
   call random_uni_scalar(jsr,xran(i))
end do
end subroutine random_uni_vec
end module ziggurat_pure_mod
