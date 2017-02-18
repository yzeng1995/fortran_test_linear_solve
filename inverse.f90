!@@@此方法为消元法求矩阵的逆
function inverse(a)result(aa)
    implicit none
    real*8,intent(in)::a(:,:)
    real*8,allocatable::aa(:,:),ae(:,:)
    integer::n,i,j,kp
    n=size(a,1)
    allocate(aa(n,n),ae(n,2*n))
    ae(1:n,1:n)=a
    aa=0
    FORALL(i=1:n)
    aa(i,i)=1
    ENDFORALL
    ae(1:n,n+1:2*n)=aa
    do i=1,n-1,1
        kp=maxloc(abs(ae(i:n,i)),1)+i-1     !找到主元位置
        ae((/i,kp/),:)=ae((/kp,i/),:)       !换行
        ae(i,:)=ae(i,:)/ae(i,i);            !主元行主元归一
        do j=i+1,n,1
            ae(j,:)=ae(j,:)-ae(i,:)*ae(j,i) !消元
        enddo
    enddo
    ae(n,:)=ae(n,:)/ae(n,n)
    do i=n,2,-1
        do j=1,i-1
            ae(j,:)=ae(j,:)-ae(j,i)*ae(i,:)!反消元
        enddo
    enddo
    aa=ae(1:n,n+1:2*n)
end function inverse