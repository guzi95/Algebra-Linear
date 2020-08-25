! This program performs the Gaussian Elimination.
program test_gaussian
implicit none
integer:: i, j
integer::n
real,allocatable:: a(:,:), b(:)
    
write(*,*) 'Order of the array'
read(*,*) n
allocate (a(n,n))
allocate (b(n))
OPEN (file = 'input.dat', status = 'old', action = 'read', unit = 10)

do i = 1, n
	read(10,*) (a(i,j), j = 1, n), b(i)
enddo
    
 close(10)
    
CALL Gaussian_elimination (n,a,b)   

OPEN (file = 'output.dat', status = 'replace', action = 'write', unit = 20)

! write out values a(i,j) and x(i)
do i = 1, n
	write(20,*) (a(i,j), j = 1, n), b(i)
enddo

end program test_gaussian
