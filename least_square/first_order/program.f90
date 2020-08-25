! this program is a test for the Least Square Method at the specific case of an straight.
! Here it is made the calculation of the constants a and b 
! of a polynomial function of the first degree that should minimize the error resulting from the adjustment.

program program_least_quare
    implicit none
    
    integer:: N, i
    real, allocatable:: X(:), Y(:)    ! points
    real:: A, B                       ! constants a and b we want find
    
    write(*,*) '--------------------------------------------------------------'
    write(*,*) 'This program is a test for the Least Square Method at the specific case'
    write(*,*) 'of an straight. Here it is made the calculation of the constants a and b '
    write(*,*) 'of a polynomial function of the first degree that should minimize '
    write(*,*) 'the error resulting from the adjustment.'
    write(*,*) '--------------------------------------------------------------'
    write(*,*) '                                                              '
    
    
    ! Reads the points (x,y)
    OPEN (unit = 10, file = 'input_points.dat', status = 'old')

    read(10,*) N
    
    allocate (X(N), Y(N))
    
    
    ! Here the reading of the values of X is realized
    do i = 1, N
        read(10,*) X(i)
    enddo
    
    
    ! Here the reading of the values of Y is realized
    do i = 1, N
        read(10,*) Y(i)
    enddo
    
    
    ! Asks for the values of a and b calculated for the subroutine Least Squares
    CALL sub_least_square(N,X,Y,A,B)
    
    
    ! Write out the values of a and b calculated for the subroutine sub_least_square
    write(*,*) 'the values of a and b, respectively'
    write(*,*) 'a =', A
    Write(*,*) 'b =', B
    
end program program_least_quare
