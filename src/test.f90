program main
  use m_cloud_label
  implicit none


  integer, parameter :: Nx=10, Ny=10, Nz=1
  logical :: cld(Nx, Ny, Nz)
  integer :: label(size(cld,dim=1), size(cld,dim=2), size(cld,dim=3))

  integer :: i,k

  cld = .False.
  cld(Nx/2,2:Ny-1,:) = .True.
  cld(2:Nx-1,Ny/2,:) = .True.

  cld(Nx/2-1,2:Ny-2,:) = .True.
  cld(2:Nx-2,Ny/2-1,:) = .True.
  cld(Nx/2+1,2:Ny-2,:) = .True.
  cld(2:Nx-2,Ny/2+1,:) = .True.

  call gen_labels(cld, label)

  do k=1,size(cld,dim=3)
    do i=1,size(cld,dim=1)
      print *,k,',',i,':',cld(i,:,k), '::', label(i,:,k)
    enddo
    print *,''
  enddo
end program
