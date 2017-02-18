program main
        implicit none
        ! Variables
        real*8,allocatable::a(:,:),b(:),x(:),y(:,:)
        real*8::t1,t2
        integer::i,j,m,n
        open(10,file='a.txt')!系数矩阵存储文件
        open(11,file='b.txt')!自由项存储文件
        read(10,*)m,n
        allocate(a(m,n),b(n),x(n))
        read(10,*)((a(i,j),j=1,n),i=1,m)!读入系数矩阵
        read(11,*)b!读入自由项
        write(*,*)'a=',((a(i,j),j=1,n),i=1,m)
        write(*,*)'b=',b
        x=gausscme(a,b)!高斯选主元消元求解
        y=inverse(a)
        write(*,*)'x=',x!输出解
        write(*,*)'y=',y!输出解
        read(*,*)
contains
        include 'gausscme.f90'
        include 'inverse.f90'
end program main
