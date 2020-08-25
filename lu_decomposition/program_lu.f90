program LU_test
implicit none

    integer:: n
    integer:: i, j
    real, allocatable:: A(:,:), L(:,:), b(:), y(:)

    open (unit = 1, file = 'output.dat', status = 'old', action = 'read')
    read(1,*) n

    allocate (A(n,n), L(n,n), b(n), y(n))

    do i = 1, n
        read(1,*) (A(i,j), j = 1, n)
    enddo
    
    read(1,*) (b(i), i = 1, n)

    CALL sub1_decomp (n,A,L)

    close(10)
    
!    write(*,*) 'write out L array'
!    do i = 1, n
!        write(*,*) (L(i,j), j = 1, n)
!    enddo
    
!    write(*,*) 'write out U array'
!    do i = 1, n
!        write(*,*) (A(i,j), j = 1 , n)
!    enddo
    
    CALL sub2_solve (n,A,L,b,y)
    
    open (unit = 2, file = 'output2.dat', status = 'old')

    write(2,130)  (y(i), i = 1, n)
    130 FORMAT (1X,T4,F10.6)

end program LU_test
