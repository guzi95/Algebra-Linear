MODULE lu
implicit none
CONTAINS
subroutine sub1_decomp (p,A,L)
integer, intent(in):: p
real, intent(inout):: A(p+1,p+1)
real, intent(out):: L(p+1,p+1)
integer:: i, j

do j = 1, (p+1)-1
	do i = j+1, p+1
            L(i,j) = A(i,j)/A(j,j)
            A(i,j+1:p+1) = A(i,j+1:p+1) - (L(i,j)*A(j,j+1:p+1))
        enddo
enddo
end subroutine sub1_decomp

subroutine sub2_solve (p,A,L,b,y)
implicit none
integer, intent(in):: p
real, intent(in):: A(p+1,p+1), L(p+1,p+1), b(p+1)
real, intent(out):: y(p+1)
integer:: i, j
real::plus
    
! y Array's first element
y(1) = b(1)   
do i = 2, p+1
	plus = 0.0
        do j = 1, (p+1)-1
        plus = plus + (L(i,j)*y(j))
        enddo
	y(i) = b(i) - plus
enddo
    
! x Array's last element
y(p+1) = y(p+1) / A(p+1,p+1)
do i = (p+1)-1, 1, -1
	plus = 0.0
        do j = i+1, p+1
            plus = (A(i,j)*y(j)) + plus
        enddo
        y(i) = (y(i) - plus) / A(i,i)
enddo
end subroutine sub2_solve
   
END MODULE lu
