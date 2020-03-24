program hisread
!---------------------------------------------------------------------
!   read lammps trajectory file and wirte the trajectory in PDB format
!   written by Francesco Tavanti, March 2020
!   CNR-NANO S3, Modena, Italy
!
!   USAGE:
!   build the fortran executable ==> gfortran lammpstraj2pdb.f90
!   then ./a.out
!   insert the name of input lammps file
!   insert the name of output file in pdb format
!---------------------------------------------------------------------
implicit none
integer natmax,ncouplemax

parameter(natmax=50000)


integer it,hkey,pdbkey,t,nat,itemp,j,k,i,num(natmax)
integer nn,index(natmax),n,numero(natmax)
integer fluct,h
character*8 ctemp
character*1 atyp,chain(natmax),catena
character*2 atype(natmax)
character*3 resi(natmax)
character*70 ctemp2
character*80 title,tfilename

real*8 pos(3,natmax),vel(3,natmax),for(3,natmax)
real*8 temptot,raggio(natmax),speed,force
real*8 xa,xb,xc,ya,yb,yc,za,zb,zc
character*8 chskip
character*4 res(natmax),residuo
integer skip,nca,mass,charge,replica,index1,timestep

character*1 option
character*4 atname
character*80 lammpsfile,pdbfile

!------------------------------------------------------------------
!     read the lammps file
!------------------------------------------------------------------
write(*,*)''
write(*,*)'*********************'
write(*,*)'  This script reads a lammps trajectory file and writes the trajectory in PDB format'
write(*,*)'  The trajectory could come both from serial and from parallel simulations'
write(*,*)'  written by Francesco Tavanti, March 2020'
write(*,*)'  CNR-NANO S3, Modena, Italy'
write(*,*)'*********************'
write(*,*)''

write(*,*)'Write name of lammps trajcetory file'
read(*,*)lammpsfile

open(10,file=lammpsfile,status='old')

write(*,*)'Write name of output file with .pdb extension'
read(*,*)pdbfile
open(12,file=pdbfile)

do it=1,1000
read(10,900)title
read(10,*) timestep
read(10,900)title
read(10,*)nat
read(10,900)title

read(10,*)xa,xb,zc
read(10,*)ya,yb,zc
read(10,*)za,zb,zc
read(10,900)title


write(12,'(a6,3f9.3,3f7.2,a16)')'CRYST1',xb,yb,zb,90.0,90.0,90.0,' P 1           1'
write(12,'(a5,i9)')'MODEL',it
do j=1,nat

speed=0
force=0
read(10,*)numero(j),nn,atype(j),(pos(k,j),k=1,3)
if(nn.eq.1)then
res(j)='Ge  '
else
res(j)='Se  '
endif

enddo

do j=1,nat
do i=1,nat
if(numero(i).eq.j)then

write(12,1070)'ATOM',numero(i),atype(i),' A ',j,(pos(k,i),k=1,3),&
&        speed,force
exit

endif
enddo
enddo


write(12,170)'ENDMDL'
enddo

100  close(10)
close(12)

stop

101  print*,'errore'
print*,(pos(k,j),k=1,3), j
stop


170  format(a6)

900  format(a80)

1070 format(a4,2x,i5,1x,a2,3x,a3,i6,4x,f8.3,f8.3,f8.3,f6.2,f6.2)

end
!------------------------------------------------------------------------

