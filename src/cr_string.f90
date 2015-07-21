MODULE cr_string
  IMPLICIT NONE

! STRING MANIPULATION FUNCTIONS
!   upcase (subroutine)

CONTAINS 

! ************* STRING MANIPULATION FUNCTIONS *****************
subroutine upcase(mystring)
! upcase modifies mystring
  IMPLICIT NONE
  character(*), INTENT(INOUT) :: mystring
  character(*), PARAMETER :: UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  character(*), PARAMETER :: LOWER = "abcdefghijklmnopqrstuvwxyz"
  INTEGER :: S, k
  DO S = 1, LEN(mystring)
    k = INDEX(LOWER, mystring(S:S))  
    IF (k .NE. 0) THEN
      mystring(S:S) = upper(k:k)
    ENDIF
  END DO
end subroutine upcase

END MODULE cr_string
