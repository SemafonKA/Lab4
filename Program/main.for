!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Программа 4 лабы по фортрану. Вычисление интегралов различных функций!
! по различным методам (трапеция, симпсон, гаусс-4).                   !
!                                                                      !
! Как пользоваться:                                                    !
! 0) Вводим необходимые функции в fun(x) и в dfun(x)                   !
! 1) Компилим и запускаем прогу (компилятор: watcom wfl386)            !
! 2) Вводим диапазоны от, до                                           !
! 3) Вводим до скольки шагов нужно дойти по расчётам                   !
! 4) Выбираем как вывести (числа подряд для удобной вставки в эксель)  !
!    или же подробный вывод                                            !
! 5) Выбираем метод (если надо несколько, то складываем числа)         !
! 6) Выбираем используемую точность для метода                         !
! 7) Ждём результаты в файле output.txt                                !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         ! iform {
         !    1: linear, (только числа столбиком)
         !    2: with steps, (числа и соответствующий им шаг)
         ! }
         ! imethod {
         !    1: trapez, (только трапеции)
         !    2: simps, (только симпсон)
         !    4: gauss-4 (только гаусс-4)
         !    // Их сумма - комбинация методов
         ! }
         ! iprecision {
         !    1: real 
         !    2: semi-double (только сумма в double)
         !    4: double (всё в double)
         ! }

      program main
         ! implicit none
         ! integer ngetGrid,nmax,n,moutput, i,k
         ! real step, ansTrap,ansSimps, from,to
         moutput = 200

         print *, 'Input range FROM: '
         read *, from
         print *, 'Input range TO: '
         read *, to
         print *, 'Input number k (where maxsteps = 2^k): '
         read *, k

         print *, 'Choose form of output:'
         print *, '1: linear'
         print *, '2: with steps'
         read *, iform

         print *, 'Choose methods of output (sum it if u need several):'
         print *, '  1: trapezoidal'
         print *, '  2: simpson'
         print *, '  4: gauss-4'
         read *, imethod

         print *, 'Choose precision of methods (or sum it):'
         print *, '1: Real'
         print *, '2: Semi-double (sum is double)'
         print *, '4: Double (real*8)'
         read *, iprec

         open (moutput, file='output.txt',action='WRITE')

            write (moutput,*) 'Range of integral: [',from,', ',to,']'

            if (iand(iprec,1) .ne. 0) then
               write (moutput, *) 'REAL METHODS:'
               call real_menu(moutput, from, to, k, iform, imethod)
            end if

            if (iand(iprec,2) .ne. 0) then
               write(moutput,*)'***************************************'
               write (moutput, *) 'SEMI-DOUBLE METHODS:'
               call semiDouble_menu(moutput,from,to,k,iform,imethod)
            end if

            if (iand(iprec,4) .ne. 0) then
               write(moutput,*)'***************************************'
               write (moutput, *) 'DOUBLE METHODS:'
               call double_menu(moutput, from, to, k, iform, imethod)
            end if
            
         close (moutput)
      end !program main

   ! Область менюшек
      subroutine real_menu(moutput, from, to, k, iform, imethod)
         real grid(10000000)
         nmax = 10000000

         ! линейный вывод
         if (iform .eq. 1) then
            ! Для трапеции
            if (Iand(imethod,1) .ne. 0) then
               write(moutput, *) 'Trapezoidal method:'
               do i=0,k
                  step = (to - from)/(2**i)
                  n = ngetGrid(grid,nmax,from,to,step)
                  if (n .eq. -1) EXIT
                  rez = trapezSumReal(n,grid)
                  write(moutput, '(E20.8))') rez
               end do
            end if
            write(moutput, *)

            ! Для симпсона
            if (Iand(imethod,2) .ne. 0) then               
               write(moutput, *) 'Simpson method:'
               do i=0,k
                  step = (to - from)/(2**i)
                  n = ngetGrid(grid,nmax,from,to,step)
                  if (n .eq. -1) EXIT
                  rez = simpSumReal(n,grid)
                  write(moutput, '(E20.8))') rez
               end do
            end if            
            write(moutput, *)

            ! Для Гаусса-4
            if (Iand(imethod,4) .ne. 0) then               
               write(moutput, *) 'Gauss-4 method:'
               do i=0,k
                  step = (to - from)/(2**i)
                  n = ngetGrid(grid,nmax,from,to,step)
                  if (n .eq. -1) EXIT
                  rez = gauss4SumReal(n,grid)
                  write(moutput, '(E20.8))') rez
               end do
            end if            
            write(moutput, *)

         ! вывод с шагами
         else if (iform .eq. 2) then
            do i=0,k
               step = (to - from)/(2**i)
               n = ngetGrid(grid,nmax,from,to,step)
               if (n .eq. -1) EXIT

               write(moutput,*) 'Steps: ', 2**i, ', step size: ', step

               if (iand(imethod, 1) .ne. 0) then
                  rez = trapezSumReal(n,grid)
                  write(moutput, '(A10,E20.8))') 'Trapez:', rez
               end if
               if (iand(imethod, 2) .ne. 0) then
                  rez = simpSumReal(n,grid)
                  write(moutput, '(A10,E20.8))') 'Simson:', rez
               end if
               if (iand(imethod, 4) .ne. 0) then
                  rez = gauss4SumReal(n,grid)
                  write(moutput, '(A10,E20.8))') 'Gauss: ', rez
               end if
               write(moutput,*)
            end do
         end if
      end !subroutine real_menu()

      subroutine semiDouble_menu(moutput, from, to, k, iform, imethod)
         real grid(10000000)
         nmax = 10000000

         ! линейный вывод
         if (iform .eq. 1) then
            ! Для трапеции
            if (Iand(imethod,1) .ne. 0) then
               write(moutput, *) 'Trapezoidal method:'
               do i=0,k
                  step = (to - from)/(2**i)
                  n = ngetGrid(grid,nmax,from,to,step)
                  if (n .eq. -1) EXIT
                  rez = trapezSumSemiDouble(n,grid)
                  write(moutput, '(E20.8))') rez
               end do
            end if
            write(moutput, *)

            ! Для симпсона
            if (Iand(imethod,2) .ne. 0) then               
               write(moutput, *) 'Simpson method:'
               do i=0,k
                  step = (to - from)/(2**i)
                  n = ngetGrid(grid,nmax,from,to,step)
                  if (n .eq. -1) EXIT
                  rez = simpSumSemiDouble(n,grid)
                  write(moutput, '(E20.8))') rez
               end do
            end if            

            ! Для гаусса-4
            if (Iand(imethod,4) .ne. 0) then               
               write(moutput, *) 'Gauss-4 method:'
               do i=0,k
                  step = (to - from)/(2**i)
                  n = ngetGrid(grid,nmax,from,to,step)
                  if (n .eq. -1) EXIT
                  rez = gauss4SumSemiDouble(n,grid)
                  write(moutput, '(E20.8))') rez
               end do
            end if            
            write(moutput, *)

         ! вывод с шагами
         else if (iform .eq. 2) then
            do i=0,k
               step = (to - from)/(2**i)
               n = ngetGrid(grid,nmax,from,to,step)
               if (n .eq. -1) EXIT

               write(moutput,*) 'Steps: ', 2**i, ', step size: ', step

               if (iand(imethod, 1) .ne. 0) then
                  rez = trapezSumSemiDouble(n,grid)
                  write(moutput, '(A10,E20.8))') 'Trapez:', rez
               end if
               if (iand(imethod, 2) .ne. 0) then
                  rez = simpSumSemiDouble(n,grid)
                  write(moutput, '(A10,E20.8))') 'Simpson:', rez
               end if
               if (iand(imethod, 4) .ne. 0) then
                  rez = gauss4SumSemiDouble(n,grid)
                  write(moutput, '(A10,E20.8))') 'Gauss-4:', rez
               end if
               ! место под гаусса
               write(moutput,*)
            end do
         end if
      end !subroutine semiDouble_menu()

      subroutine double_menu(moutput, from, to, k, iform, imethod)
         real*8 trapezSumDouble, simpSumDouble, gauss4SumDouble, rez
         real grid(10000000)
         nmax = 10000000

         ! линейный вывод
         if (iform .eq. 1) then
            ! Для трапеции
            if (Iand(imethod,1) .ne. 0) then
               write(moutput, *) 'Trapezoidal method:'
               do i=0,k
                  step = (to - from)/(2**i)
                  n = ngetGrid(grid,nmax,from,to,step)
                  if (n .eq. -1) EXIT
                  rez = trapezSumDouble(n,grid)
                  write(moutput, '(E25.15)') rez
               end do
            end if
            write(moutput, *)

            ! Для симпсона
            if (Iand(imethod,2) .ne. 0) then               
               write(moutput, *) 'Simpson method:'
               do i=0,k
                  step = (to - from)/(2**i)
                  n = ngetGrid(grid,nmax,from,to,step)
                  if (n .eq. -1) EXIT
                  rez = simpSumDouble(n,grid)
                  write(moutput, '(E25.15)') rez
               end do
            end if            

            ! Для гаусса-4
            if (Iand(imethod,4) .ne. 0) then               
               write(moutput, *) 'Gauss-4 method:'
               do i=0,k
                  step = (to - from)/(2**i)
                  n = ngetGrid(grid,nmax,from,to,step)
                  if (n .eq. -1) EXIT
                  rez = gauss4SumDouble(n,grid)
                  write(moutput, '(E25.15)') rez
               end do
            end if            
            write(moutput, *)

         ! вывод с шагами
         else if (iform .eq. 2) then
            do i=0,k
               step = (to - from)/(2**i)
               n = ngetGrid(grid,nmax,from,to,step)
               if (n .eq. -1) EXIT

               write(moutput,*) 'Steps: ', 2**i, ', step size: ', step

               if (iand(imethod, 1) .ne. 0) then
                  rez = trapezSumDouble(n,grid)
                  write(moutput, '(A10,E25.15)') 'Trapez:', rez
               end if
               if (iand(imethod, 2) .ne. 0) then
                  rez = simpSumDouble(n,grid)
                  write(moutput, '(A10,E25.15)') 'Simson:', rez
               end if
               if (iand(imethod, 4) .ne. 0) then
                  rez = gauss4SumDouble(n,grid)
                  write(moutput, '(A10,E25.15)') 'Gauss-4:', rez
               end if
               write(moutput,*)
            end do
         end if
      end !subroutine semiDouble_menu()
   ! конец области менюшек


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


   ! Область функций, в которых всё выполняется в real
      ! n - Размер сетки
      ! grid - сама сетка
      real function trapezSumReal(n,grid)
         ! implicit none
         ! integer n, i
         ! real sum, step
         real grid(*)

         if (n .lt. 2) then
            print *, 'Error: grid in trapezSumReal too short.'
            trapezSumReal = 0.0
            RETURN
         end if

         sum = fun(grid(1))*(grid(2)-grid(1))
         sum = sum + fun(grid(n))*(grid(n)-grid(n-1))

         do i=2,n-1
            sum = sum + fun(grid(i))*(grid(i+1) - grid(i-1))
         end do
         trapezSumReal = sum * 0.5

      end !real function trapezSumReal(n,grid)


      real function simpSumReal(n,grid)
         ! implicit none
         ! integer n,i
         ! real deltasum, step, step1,step2
         real grid(*)

         if (n .lt. 2) then
            print *, 'Error: grid in simpSumReal too short.'
            simpSumReal = 0.0
            RETURN
         end if

         sum = fun(grid(1))*(grid(2) - grid(1))
         sum = sum + fun(grid(n))*(grid(n) - grid(n-1))

         deltasum = 0.0
         do i=1,n-1
            step = grid(i+1) - grid(i)
            deltasum = deltasum + step*fun((grid(i)+grid(i+1))/2.0)
         end do
         sum = sum + 4.0*deltasum

         deltasum = 0.0
         do i=2,n-1
            deltasum = deltasum + fun(grid(i))*(grid(i+1) - grid(i-1))
         end do

         simpSumReal = (sum + deltasum) / 6.0
         RETURN
      end !real function simpSumReal(n,grid)


      real function gauss4SumReal(n, grid)
         real roots(4), weights(4), grid(*)
         roots(1) = -0.86113631159405257522394648889281
         roots(2) = -0.33998104358485626480266575910324
         roots(3) =  0.33998104358485626480266575910324
         roots(4) =  0.86113631159405257522394648889281

         weights(1) = 0.347854845137453857373063949222
         weights(2) = 0.652145154862546142626936050778
         weights(3) = 0.652145154862546142626936050778
         weights(4) = 0.347854845137453857373063949222

         sum = 0.0
         do j=1,4
            tsum = 0.0
            do k=2,n
               step = grid(k) - grid(k-1)
               x_kj = ((grid(k-1)+grid(k)) + roots(j)*step)/2.0
               tsum = tsum + step*fun(x_kj)
            end do
            sum = sum + weights(j)*tsum
         end do

         gauss4SumReal = sum / 2.0
      end !real function gauss4SumReal(n, grid)
   ! конец области

   ! Область функций, в которых сумма выполняется в double, но 
   ! вычисления выполняются в real (semiDouble) 
      ! n - Размер сетки
      ! grid - сама сетка
      real function trapezSumSemiDouble(n,grid)
         ! implicit none
         ! integer n, i
         ! real sum, step
         real grid(*)
         real*8 summer

         if (n .lt. 2) then
            print *, 'Error: grid in trapezSumSemiDouble too short.'
            trapezSumSemiDouble = 0.0
            RETURN
         end if

         summer = fun(grid(1))*(grid(2)-grid(1))
         summer = summer + fun(grid(n))*(grid(n)-grid(n-1))

         do i=2,n-1
            sum = fun(grid(i))*(grid(i+1) - grid(i-1))
            summer = summer + sum
         end do
         trapezSumSemiDouble = summer * 0.5
      end !real function trapezSumSemiDouble(n,grid)


      real function simpSumSemiDouble(n,grid)
         ! implicit none
         ! integer n,i
         ! real deltasum, step, step1,step2
         real grid(*)
         real*8 summer, deltasummer

         if (n .lt. 2) then
            print *, 'Error: grid in simpSumSemiDouble too short.'
            simpSumSemiDouble = 0.0
            RETURN
         end if

         summer = fun(grid(1))*(grid(2) - grid(1))
         summer = summer + fun(grid(n))*(grid(n) - grid(n-1))

         deltasum = 0.0
         deltasummer = 0.0
         do i=1,n-1
            step = grid(i+1) - grid(i)
            deltasum = step*fun((grid(i)+grid(i+1))/2.0)
            deltasummer = deltasummer + deltasum
         end do
         summer = summer + 4.0*deltasummer

         deltasum = 0.0
         deltasummer = 0.0
         do i=2,n-1
            step1 = grid(i) - grid(i-1)
            step2 = grid(i+1) - grid (i)
            deltasum = fun(grid(i))*(grid(i+1) - grid(i-1))
            deltasummer = deltasummer + deltasum
         end do

         simpSumSemiDouble = (summer + deltasummer) / 6.0
         RETURN
      end !real function simpSumSemiDouble(n,grid)


      real function gauss4SumSemiDouble(n, grid)
         real roots(4), weights(4), grid(*)
         real*8 sum,tsum
         roots(1) = -0.86113631159405257522394648889281
         roots(2) = -0.33998104358485626480266575910324
         roots(3) =  0.33998104358485626480266575910324
         roots(4) =  0.86113631159405257522394648889281

         weights(1) = 0.347854845137453857373063949222
         weights(2) = 0.652145154862546142626936050778
         weights(3) = 0.652145154862546142626936050778
         weights(4) = 0.347854845137453857373063949222

         sum = 0.0
         do j=1,4
            tsum = 0.0
            do k=2,n
               step = grid(k) - grid(k-1)
               x_kj = (grid(k-1)+grid(k))/2.0 + roots(j)*step/2.0
               tsum = tsum + step*fun(x_kj)
            end do
            sum = sum + weights(j)*tsum
         end do

         gauss4SumSemiDouble = sum / 2.0
      end !real function gauss4SumSemiDouble(n, grid)
   ! конец области

   ! Область функций, в которых всё выполняется в double
      ! n - Размер сетки
      ! grid - сама сетка
      real*8 function trapezSumDouble(n,grid)
         ! implicit none
         ! integer n, i
         real grid(*)
         real*8 sum, dfun

         if (n .lt. 2) then
            print *, 'Error: grid in trapezSumDouble too short.'
            trapezSumDouble = 0.0
            RETURN
         end if

         sum = dfun(grid(1)*1D0)*(grid(2)-grid(1))
         sum = sum + dfun(grid(n)*1D0)*(grid(n)-grid(n-1))

         do i=2,n-1
            sum = sum + dfun(grid(i)*1D0)*(grid(i+1) - grid(i-1))
         end do
         trapezSumDouble = sum * 0.5
      end !real*8 function trapezSumDouble(n,grid)


      real*8 function simpSumDouble(n,grid)
         ! implicit none
         ! integer n,i
         real grid(*)
         real*8 sum,deltasum,step, dfun

         if (n .lt. 2) then
            print *, 'Error: grid in simpSumDouble too short.'
            simpSumDouble = 0.0
            RETURN
         end if

         sum = dfun(grid(1)*1D0)*(grid(2) - grid(1))
         sum = sum + dfun(grid(n)*1D0)*(grid(n) - grid(n-1))

         deltasum = 0.0
         do i=1,n-1
            step = grid(i+1) - grid(i)
            deltasum = deltasum + step*dfun((grid(i)+grid(i+1))/2.0D0)
         end do
         sum = sum + 4.0*deltasum

         deltasum = 0.0
         do i=2,n-1
            deltasum = deltasum+dfun(grid(i)*1D0)*(grid(i+1)-grid(i-1))
         end do

         simpSumDouble = (sum + deltasum) / 6.0
         RETURN
      end !real*8 function simpSumDouble(n,grid)

      
      real*8 function gauss4SumDouble(n, grid)
         real grid(*)
         real*8 roots(4),weights(4), sum,tsum,step,x_kj, dfun
         roots(1) = -0.86113631159405257522394648889281D0
         roots(2) = -0.33998104358485626480266575910324D0
         roots(3) =  0.33998104358485626480266575910324D0
         roots(4) =  0.86113631159405257522394648889281D0

         weights(1) = 0.347854845137453857373063949222D0
         weights(2) = 0.652145154862546142626936050778D0
         weights(3) = 0.652145154862546142626936050778D0
         weights(4) = 0.347854845137453857373063949222D0

         sum = 0.0
         do j=1,4
            tsum = 0.0
            do k=2,n
               step = grid(k) - grid(k-1)
               x_kj = ((grid(k-1)+grid(k)) + roots(j)*step)/2.0D0
               tsum = tsum + step*dfun(x_kj)
            end do

            sum = sum + weights(j)*tsum
         end do

         gauss4SumDouble = sum / 2.0
      end !real*8 function gauss4SumSemiDouble(n, grid)
   ! конец области

      real function fun(x)
         ! implicit none
         ! real x
         !x^10/7 - 4x^9	

         fun = 10.0*x**2/17.0 ! Менять здесь и в dfun
         RETURN
      end !real function fun()

      real*8 function dfun(x)
         ! implicit none
         real*8 x

         dfun = 2.0D0*sin(27.0D0*x/5.0D0) - cos(5.0D0*x-1.6D0)  ! Менять здесь и в fun
         RETURN
      end !real*8 function dfun()