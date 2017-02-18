!@@@此方法为选主消元法，输入矩阵a，自由端向量b即可
function gausscme(a,b)result(x)
        implicit none
        real*8,intent(in)::a(:,:),b(:)
        real*8,allocatable::x(:),ab(:,:)
        integer::n,i,j,kp(2)
        integer,allocatable::idx(:)
        n=size(b)
        allocate(ab(n,n+1),x(n),idx(n))
        idx=(/(i,i=1,n)/)
        ab(1:n,1:n)=a
        ab(1:n,n+1)=b
        do i=1,n-1,1
        kp=maxloc(abs(ab(i:n,i:n)))+i-1     !找到主元位置
        ab((/i,kp(1)/),:)=ab((/kp(1),i/),:) !换行
        ab(:,(/i,kp(2)/))=ab(:,(/kp(2),i/)) !换列
        idx((/kp(2),i/))=idx((/i,kp(2)/))   !换未知数下标
        ab(i,:)=ab(i,:)/ab(i,i);            !主元行主元归一
        do j=i+1,n,1
        ab(j,:)=ab(j,:)-ab(i,:)*ab(j,i) !消元
        enddo
        enddo
        ab(n,:)=ab(n,:)/ab(n,n)
        x=0;
        do i=n,1,-1
        x(i)=ab(i,n+1)-dot_product(ab(i,i+1:n),x(i+1:n))!回代
        enddo
        x(idx)=x
end function gausscme
