module m_py_cloud_label
  use m_cloud_label, only : gen_labels
  implicit none

  contains

    subroutine py_gen_labels(Nx, Ny, Nz, cld, label)
      integer, intent(in) :: Nx, Ny, Nz
      logical, intent(in) :: cld(Nx, Ny, Nz)
      integer, intent(out) :: label(Nx, Ny, Nz)

      !integer :: i,j,k

      call gen_labels(cld, label)

      !do k=1,Nz
      !  do i=1,Nx
      !    print *,k,',',i,':',cld(i,:,k), '::', label(i,:,k)
      !  enddo
      !  print *,''
      !enddo
    end subroutine

    subroutine py_test_gen_labels(Nx, Ny, Nz, label)
      integer,intent(in) :: Nx, Ny, Nz
      logical :: cld(Nx, Ny, Nz)
      integer,intent(out) :: label(Nx, Ny, Nz)

      integer :: i,k

      cld = .False.
      cld(2,2,1) = .True.

      cld(2,4,1) = .True.
      cld(2,4,2) = .True.

      cld(1,1,:) = .True.
      cld(Nx,1,:) = .True.
      cld(Nx,Ny,Nz) = .True.

      call gen_labels(cld, label)

      do k=1,2
      do i=1,size(cld,dim=1)
        print *,i,':',cld(i,:,k), '::', label(i,:,k)
      enddo
      enddo
    end subroutine
end module
