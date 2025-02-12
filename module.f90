module subroutines
    IMPLICIT NONE
    PUBLIC :: forward_diff , central_diff , backward_diff
    CONTAINS

    SUBROUTINE forward_diff(f, h, N, df)
        !this subroutine is for the forward difference method
        !f is the function to be differentiated
        !h is the step size
        !N is the number of points
        !df is the output array
        !k is the digit precision for the variables.
        IMPLICIT NONE
        INTEGER(8), INTENT(IN) :: N
        REAL(8),DIMENSION(N), INTENT(IN) :: f
        REAL(8),DIMENSION(N),INTENT(OUT) :: df
        REAL(8), INTENT(IN) :: h
        INTEGER(8) :: i
        
        DO i = 1, N-1
            df(i) = (f(i+1) - f(i))/h
        END DO
        df(N) = df(N-1)
    END SUBROUTINE forward_diff

    !-------------------------------------------------------------------

    SUBROUTINE central_diff(f, h, N, df)
        !this subroutine is for the central difference method
        !f is the function to be differentiated
        !h is the step size
        !N is the number of points
        !df is the output array
        !k is the digit precision for the variables.
        IMPLICIT NONE
        INTEGER(8), INTENT(IN) :: N
        REAL(8),DIMENSION(N), INTENT(IN) :: f
        REAL(8),DIMENSION(N),INTENT(OUT) :: df
        REAL(8), INTENT(IN) :: h
        INTEGER(8) :: i
        
        DO i = 2, N-1
            df(i) = (f(i+1) - f(i-1))/(2*h)
        END DO
        df(1) = df(2)
        df(N) = df(N-1)
    END SUBROUTINE central_diff
    !-------------------------------------------------------------------

    SUBROUTINE backward_diff(f, h, N, df)
        !this subroutine is for the backward difference method
        !f is the function to be differentiated
        !h is the step size
        !N is the number of points
        !df is the output array
        !k is the digit precision for the variables.
        IMPLICIT NONE
        INTEGER(8), INTENT(IN) :: N
        REAL(8),DIMENSION(N), INTENT(IN) :: f
        REAL(8),DIMENSION(N),INTENT(OUT) :: df
        REAL(8), INTENT(IN) :: h
        INTEGER(8) :: i
        
        DO i = 2, N
            df(i) = (f(i) - f(i-1))/h
        END DO
        df(1) = df(2)
    END SUBROUTINE backward_diff

    !-------------------------------------------------------------------





END module subroutines