subroutine sub1_decomp (n,A,L)

    implicit none
    integer, intent(in):: n
    real, intent(inout):: A(n,n)
    real, intent(out):: L(n,n)
    integer:: i, p
    
    ! LU decomposition
    do p = 1, n-1
        do i = p+1, n
            L(i,p) = A(i,p)/A(p,p)
            A(i,p:n) = A(i,p:n) - (L(i,p)*A(p,p:n))
        enddo
    enddo

end subroutine sub1_decomp
