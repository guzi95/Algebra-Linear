! This subroutine does calculus of least square in specific caso of a straight.

    subroutine sub_least_square(n,x,y,a,b)
    implicit none
    real, intent(in):: x(n), y(n)   ! points
    real, intent(out):: a, b        ! constants we want find, that minimize the distance between the points and straight
    integer, intent(in):: n         ! number of variables
    real:: xy, xi, yi, x2
    integer:: i, j
	
    do i = 1, n
        xy = 0.0
        xi = 0.0
        yi = 0.0
        x2 = 0.0
        do j = 1, n
            xy = (x(j)*y(j)) + xy
            xi = x(j) + xi
            yi = y(j) + yi
            x2 = (x(j)*x(j))+ x2
        enddo
    enddo
	
    a = ((n*xy) - (xi*yi)) / ((n*x2) - (xi*xi))
    
    b = (((x2)*(yi)) - (xy*xi)) / ((n*x2) - (xi*xi)) 
	
end subroutine sub_least_square
	
