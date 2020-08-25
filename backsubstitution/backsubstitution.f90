! This subroutine realize calculus of backsubstitution of an array
subroutine backsubstitution(n,a,b,x)
    implicit none
    integer, intent(in):: n
    real, intent(in):: a(n,n), b(n)
    real, intent(out):: x(n)
    integer:: i, j

    x(n)=b(n)/a(n,n)

    do i = n-1, 1, -1
        plus = 0.0
        do j = i+1, n
            plus = (a(i,j)*x(j)) + plus
        enddo
        x(i) = (b(i)-plus) / a(i,i)
    enddo

end subroutine backsubstitution
