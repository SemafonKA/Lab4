!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Программа 4 лабы по фортрану. Вычисление интегралов различных функций!
! по различным методам (трапеция, симпсон, гаусса).                    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      program main
         ! implicit none
         ! integer ngetGrid,nmax,n,minput,moutput,ios
         ! real xbeg,xend,xstep, ans
         ! real trapezoidalSum,simpsonSum
         real grid(10000)
         nmax = 10000
         minput = 100
         moutput = 200

         ! open(minput, file='input.txt',action='READ',iostat=ios)
         ! if (ios .ne. 0) then
         !    print *, 'Error: program cannot open file [input.txt]'
         !    close (minput)
         !    RETURN
         ! end if

         ! read(minput, *, iostat=ios) xbeg,xend,xstep
         ! if (ios .ne. 0) then
         !    print *, 'Error: wrong data in [input.txt]'
         !    close (minput)
         !    RETURN
         ! end if

         ! n = ngetGrid(grid,nmax,xbeg,xend,xstep)
         ! if (n .eq. -1) RETURN

         open (moutput, file='output.txt',action='WRITE')
            ! ans = trapezoidalSum(n,grid)
            ! write (moutput,'(A11,E25.18)') 'Trapezoid: ',ans
            ! write (moutput,'(A11,E25.8)') 'Trapezoid: ',ans
            ! ans = simpsonSum(n,grid)
            ! write (moutput,'(A11,E25.18)')  'Simpson: ',ans
            ! write (moutput,'(A11,E25.8)')  'Simpson: ',ans
            n = ngetGrid(grid,nmax,1.0,3.0,2.0)
            ans_2h = trapezoidalSum(n,grid)

            n = ngetGrid(grid,nmax,1.0,3.0,1.0)
            ans_h = trapezoidalSum(n,grid)

            ans_prec = (4*ans_h-ans_2h) / 3.0

            write (moutput, '(A20,E25.8)') 'Trapezoid 2h: ', ans_2h
            write (moutput, '(A20,E25.8)') 'Trapezoid h: ', ans_h
            write (moutput, '(A20,E25.15)') 'Trapezoid prec.: ',ans_prec
            write (moutput, *) 

            n = ngetGrid(grid,nmax,1.0,3.0,2.0)
            ans_2h = simpsonSum(n,grid)

            n = ngetGrid(grid,nmax,1.0,3.0,1.0)
            ans_h = simpsonSum(n,grid)

            ans_prec = (4*ans_h-ans_2h) / 3.0

            write (moutput, '(A20,E25.8)') 'simpsonSum 2h: ', ans_2h
            write (moutput, '(A20,E25.18)') 'simpsonSum 2h: ', ans_2h  !
            write (moutput, '(A20,E25.8)') 'simpsonSum h: ', ans_h
            write (moutput, '(A20,E25.18)') 'simpsonSum h: ', ans_h    !
            write (moutput, '(A20,E25.15)')'simpsonSum prec.: ',ans_prec
         close (moutput)
      end !program main


      INTEGER function ngetGrid(grid,nmax,xbeg,xend,xstep)
         ! implicit none
         ! integer n,nmax,i,nint
         ! real xbeg,xend,xstep
         real grid(*)

         n = nint((xend - xbeg) / xstep) + 1

         if (n .gt. nmax) then 
            print *, 'Error: size of grid too large to fit in array'
            ngetGrid = -1
            RETURN
         end if

         do i = 1,n-1
            grid(i) = xbeg + xstep*(i-1)
         end do

         grid(n) = xend

         ngetGrid = n
         RETURN
      end !subroutine ngetGrid


      ! n - Размер сетки
      ! grid - сама сетка
      real function trapezoidalSum(n,grid)
         ! implicit none
         ! integer n, i
         ! real sum, step
         real grid(*)

         if (n .lt. 2) then
            print *, 'Error: grid in trapezoidalSum too short.'
            trapezoidalSum = 0.0
            RETURN
         end if

         sum = fun(grid(1))*(grid(2)-grid(1))
         sum = sum + fun(grid(n))*(grid(n)-grid(n-1))

         do i=2,n-1
            ! step1 = grid(i) - grid(i-1)
            ! step2 = grid(i+1) - grid(i)
            ! sum = sum + fun(grid(i))*(step1+step2)
         end do
         trapezoidalSum = sum * 0.5

         ! do i = 1,n-1
         !    step = grid(i+1) - grid(i)
         !    sum1 = sum1 + step*(fun(grid(i))+fun(grid(i+1)))/2.0
         ! end do
         ! trapezoidalSum = sum1

      end !real function trapezoidalSum(n,grid)


      real function simpsonSum(n,grid)
         ! implicit none
         ! integer n,i
         ! real sum, deltasum, step, step1,step2
         real grid(*)

         if (n .lt. 2) then
            print *, 'Error: grid in simpsonSum too short.'
            simpsonSum = 0.0
            RETURN
         end if

         sum = fun(grid(1))*(grid(2) - grid(1))
         sum = sum + fun(grid(n))*(grid(n) - grid(n-1))

         deltasum = 0
         do i=1,n-1
            step = grid(i+1) - grid(i)
            deltasum = deltasum + step*fun((grid(i)+grid(i+1))/2.0)
         end do
         sum = sum + 4.0*deltasum

         deltasum = 0
         do i=2,n-1
            step1 = grid(i) - grid(i-1)
            step2 = grid(i+1) - grid (i)
            deltasum = deltasum + fun(grid(i))*(step1 + step2)
         end do

         simpsonSum = (sum + deltasum) / 6.0
         RETURN
      end !real function simpsonSum(n,grid)


      real function fun(x)
         ! implicit none
         ! real x

         ! fun = x*x + 5*x
         ! fun = x**8 - 5*x**3
         ! fun = sin(x**8)
         ! fun = sin(x)
         ! fun = sin(5*x)*cos(x) + sin(4*x) + sin(x)*sin(x)

         fun = x**2 / 17.0
         RETURN
      end !real functiion fun()