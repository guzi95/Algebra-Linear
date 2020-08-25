MODULE min_quadrados
CONTAINS

subroutine a_array(p,n,x,y,a,f)
implicit none
integer, intent(in):: p, n
real, intent(in):: x(n), y(n)
real,intent(out):: a(n,p+1), f(p+1)
integer:: i, j

A(:,1) = 1.
do i = 1, n
	do j = 1, p
	a(i,j+1) = x(i)**j
	end do
end do

do j= 1, p+1
f(j)= dot_product(a(:,j),y(:))
end do

end subroutine


subroutine ata_array(p,n,a,ata)
implicit none
integer, intent(in):: p, n
real, intent(in):: a(n,p+1)
real, intent(out):: ata(p+1,p+1)
integer:: j, k

do k= 1, p+1
	do j= 1, p+1
	ata(k,j)= dot_product(a(:,k),a(:,j))
	end do
	ata(:,k)= ata(k,:)
end do

end subroutine

END MODULE min_quadrados
