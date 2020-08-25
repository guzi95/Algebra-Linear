! This program tests the BACKSUBSTITUTION subroutine.
! Opens and reads an archive matriz.dat, containing the values of array A(i,j) and B(i)
! with objective from calculate x(i).

program test_backsubst
    implicit none

    ! Variables of the test_backsubs program
    integer:: N, i, j, Error
    real,allocatable:: A(:,:), B(:), X(:)

    !Reads N integer
    write(*,*) 'Order of the array'
    read(*,*) N

    ! Allocate A(N,N), (B(N)), X(N)
    allocate (A(N,N))
    allocate (B(N))
    allocate (X(N))

    ! Opens matriz.dat for read
    OPEN (UNIT = 10, File = 'output.dat', status = 'Old', IOSTAT = Error)

    ! Reads A(i,j) array contained in the matriz.dat
    do i = 1, n
        do j = 1, n
            read(10,*) A(i,j)
        end do
    end do

    ! Reads B(i) array contained in the matriz.dat
    do i = 1, n
        read(10,*) B(i)
    end do

    close(10)

    ! Call the BACKSUBSTITUTION subroutine for realize the calculus of x(i)
    CALL backsubstitution(N,A,B,X)

    ! Write out X(i) array
    write(*,*) 'In descending order, the X values are'
    write(*,*) (X(i), i = n, 1, -1)

! End Program
end program test_backsubst
