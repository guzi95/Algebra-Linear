subroutine sub2_solve (n,A,L,b,x)

    implicit none
    integer, intent(in):: n
    real, intent(in):: A(n,n), L(n,n), b(n)
    real, intent(out):: x(n)
    integer:: i, j
    real::plus, y(n)
    
    y(1) = b(1)
    
    ! Y values
    do i = 2, n
        plus = 0.0
        do j = 1, n-1
            plus = plus + (L(i,j)*y(j))
        enddo
        y(i) = b(i) - plus
    enddo
    
    x(n) = y(n) / A(n,n)
    
    ! X values
    do i = n-1, 1, -1
        plus = 0.0
        do j = i+1, n
            plus = (A(i,j)*x(j)) + plus
        enddo
        x(i) = (y(i) - plus) / A(i,i)
        write(*,*) x(i)
    enddo
    
end subroutine sub2_solve
    
