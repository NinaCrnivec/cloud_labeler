module m_py_cloud_label
  use m_cloud_label, only : gen_labels
  implicit none

  public :: a

  integer,parameter :: a=5

  contains

    subroutine py_gen_labels(Nx, Ny, Nz, cld, label)
      integer, intent(in) :: Nx, Ny, Nz
      logical, intent(in) :: cld(Nx, Ny, Nz)
      integer, intent(out) :: label(Nx, Ny, Nz)

      call gen_labels(cld, label)
    end subroutine

    subroutine py_test_gen_labels()
      logical :: cld(3,3,2)
      integer :: label(size(cld,dim=1), size(cld,dim=2), size(cld,dim=3))

      integer :: i,k

      cld = .False.
      cld(2,2,1) = .True.
      call gen_labels(cld, label)

      do k=1,2
      do i=1,size(cld,dim=1)
        print *,i,':',cld(i,:,k), '::', label(i,:,k)
      enddo
      enddo
    end subroutine
end module
