! This subroutine performs Gaussian elimination, if the array is square.

subroutine Gaussian_elimination (n,a,b)    ! This are the variables
implicit none
integer, intent(in):: n                ! Array order
real, intent(inout):: a(n,n)           ! Array Aij
real, intent(inout):: b(n)             ! Array X(i)
real:: m
integer:: i, p
    
do p = 1, n-1
	do i = p+1, n
	m = a(i,p)/a(p,p)
	a(i,p+1:n) = a(i,p+1:n) - m*a(p,p+1:n)
	b(i) = b(i) - m*b(p)
	enddo
enddo
			
end subroutine Gaussian_elimination
