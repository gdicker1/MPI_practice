PROGRAM Area
!---------------------------------------------------------------------
!
!  This program computes the area of a circle given the input radius
!
!  Uses:  SUBROUTINE Compute_Area(r, Area)
!
!---------------------------------------------------------------------
IMPLICIT NONE

INTERFACE 
   SUBROUTINE Compute_Area(r, Area)
     REAL, INTENT(IN) :: r
     REAL, INTENT(OUT) :: Area
   END SUBROUTINE Compute_Area
END INTERFACE

! Declare local variables
REAL :: radius, Area_Circle

! Prompt user for radius of circle
write(*, '(A)', ADVANCE = "NO") "Enter the radius of the circle:  "
read(*,*) radius

! Call subroutine to compute area of circle
CALL Compute_Area(radius, Area_Circle)

! Write out area of circle 
write(*,100) "Area of circle with radius", radius, " is", &
            Area_Circle
100 format (A, 2x, F6.2, A, 2x, F11.2)

END PROGRAM Area

!-----Compute_Area---------------------------------------------------
!
!  Subroutine to compute the area of a circle of given radius
!
!---------------------------------------------------------------------
SUBROUTINE Compute_Area(r, Area)

IMPLICIT NONE
REAL, INTENT(IN) :: r
REAL, INTENT(OUT) :: Area

! Declare local constant Pi
REAL, PARAMETER :: Pi = 3.1415927

Area = Pi * r * r

END SUBROUTINE Compute_Area
