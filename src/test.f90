program main
  use m_cloud_label
  implicit none

  logical :: cld(5,5,2)
  integer :: label(size(cld,dim=1), size(cld,dim=2), size(cld,dim=3))

  integer :: i,k

  cld = .False.
  cld(2,2,1) = .True.

  cld(2,4,1) = .True.
  cld(2,4,2) = .True.

  cld(1,1,:) = .True.
  cld(5,1,:) = .True.

  call gen_labels(cld, label)

  do k=1,size(cld,dim=3)
    do i=1,size(cld,dim=1)
      print *,i,':',cld(i,:,k), '::', label(i,:,k)
    enddo
    print *,''
  enddo


end program
