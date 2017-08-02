module m_cloud_label
  implicit none

  character(len=*), parameter :: help="Module to determine the index sets for cloud labels in a 3D Field"
  integer,parameter :: nil=-1


  contains

    subroutine gen_labels(cld, label)
      logical, intent(in) :: cld(:,:,:)

      integer, intent(out) :: label(:,:,:)

      integer :: i,j,k, ilabel
      integer :: Nx, Ny, Nz


      Nx=size(cld,dim=1); Ny=size(cld,dim=2); Nz=size(cld,dim=3)
      label = nil

      !print *,'Shape of label:', shape(label)
      !print *,'Shape of cloud:', shape(cld)

      ilabel=0

      do k=1,Nz
        do j=1,Ny
          do i=1,Nx
            if (cld(i,j,k) .and. label(i,j,k).eq.nil) then
              call fill_stencil(i,j,k, cld, ilabel, label)
              ilabel = ilabel+1
            endif
          enddo
        enddo
      enddo
    end subroutine

    recursive subroutine fill_stencil(i,j,k, cld, ilabel, label)
      logical, intent(in) :: cld(:,:,:)
      integer, intent(in) :: i,j,k, ilabel
      integer, intent(inout) :: label(:,:,:)

      integer im1, ip1, jm1, jp1, km1, kp1

      integer :: Nx, Ny, Nz

      Nx=size(cld,dim=1); Ny=size(cld,dim=2); Nz=size(cld,dim=3)

      km1 = max(1, k-1); kp1 = min(ubound(cld,3), k+1)
      jm1 = cyclic(j-1, Ny); jp1 = cyclic(j+1, Ny)
      im1 = cyclic(i-1, Nx); ip1 = cyclic(i+1, Nx)

      if (label(i, j, k).eq.ilabel) return ! have been here already
      if (label(i, j, k).ne.nil) then
        print *,i ,j, k, ':', ilabel, '::', label(i, j, k)
        stop 'already has a label! that should not happen?'
      endif

      if (cld(i, j, k)) label(i, j, k) = ilabel

      if (cld(im1, j, k)) then
        call fill_stencil(im1,j,k, cld, ilabel, label)
      endif
      if (cld(ip1, j, k)) then
        call fill_stencil(ip1,j,k, cld, ilabel, label)
      endif
      if (cld(i  , jm1, k  )) then
        call fill_stencil(i,jm1,k, cld, ilabel, label)
      endif
      if (cld(i  , jp1, k  )) then
        call fill_stencil(i,jp1,k, cld, ilabel, label)
      endif
      if (cld(i  , j  , km1)) then
        call fill_stencil(i,j,km1, cld, ilabel, label)
      endif
      if (cld(i  , j  , kp1)) then
        call fill_stencil(i,j,kp1, cld, ilabel, label)
      endif
    end subroutine

    pure integer function cyclic(i,N)
      integer,intent(in) :: i, N
      cyclic = modulo(modulo(i-1, N) + N, N)+1
    end function

end module
