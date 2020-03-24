program hisread
!---------------------------------------------------------------------
!   read lammps trajectory file and wirte the trajectory in DL_POLY_2 format
!   written by Francesco Tavanti, March 2020
!   CNR-NANO S3, Modena, Italy
!
!   USAGE:
!   build the fortran executable ==> gfortran lammpstraj2pdb.f90
!   then ./a.out
!   insert the name of input lammps file
!   insert the name of output file in pdb format
!   insert number of atoms in simulation
!   insert the type of the simulation box
!---------------------------------------------------------------------
implicit none
integer natmax,ncouplemax

parameter(natmax=50000)


integer it,hkey,pdbkey,t,nat,itemp,j,k,i,num(natmax)
integer nn,index(natmax),n,numero(natmax)
integer fluct,h,cell
character*8 ctemp
character*1 atyp,chain(natmax),catena
character*2 atype(natmax)
character*3 resi(natmax)
character*70 ctemp2
character*80 title,tfilename

real*8 pos(3,natmax),vel(3,natmax),for(3,natmax)
real*8 temptot,raggio(natmax),speed,force
real*8 xa,xb,xc,ya,yb,yc,za,zb,zc,mass(natmax)
character*8 chskip
character*4 res(natmax),residuo
integer skip,nca,charge,replica,index1,timestep

character*1 option
character*4 atname
character*80 lammpsfile,dlfile

!------------------------------------------------------------------
!     read the HISTORY file
!------------------------------------------------------------------
write(*,*)''
write(*,*)'*********************'
write(*,*)'  This script reads a lammps trajectory file and writes the trajectory in DL_POLY_4 format'
write(*,*)'  The trajectory could come both from serial and from parallel simulations'
write(*,*)'  written by Francesco Tavanti, March 2020'
write(*,*)'  CNR-NANO S3, Modena, Italy'
write(*,*)'*********************'
write(*,*)''

write(*,*)'Write name of lammps trajcetory file'
read(*,*)lammpsfile

open(10,file=lammpsfile,status='old')

write(*,*)'Write name of output file '
read(*,*)dlfile
open(12,file=dlfile)

write(*,*)'Insert number of atoms in trajectory'
read(*,*)nat

write(*,*)'Insert cell type:'
write(*,*)'0 = no cell'
write(*,*)'1 = cubic'
write(*,*)'2 = orthorhombic'
write(*,*)'3 = parallelepiped'
read(*,*)cell


write(12,*)'lammps2HISTORY'
write(12,'(3i10)')0,cell,nat

do it=1,1000
read(10,*)title
read(10,*)timestep
read(10,*)title
read(10,*)nat
read(10,*)title

read(10,*)xa,xb
read(10,*)ya,yb
read(10,*)za,zb
read(10,*)title


write(12,'(a8,4i10)')'timestep',it-1,nat,0,cell
write(12,'(f8.2,6x,f8.3,6x,f8.3)')xb,0.0,0.0
write(12,'(f8.3,6x,f8.2,6x,f8.3)')0.0,yb,0.0
write(12,'(f8.3,6x,f8.3,6x,f8.2)')0.0,0.0,zb
do j=1,nat

read(10,*)numero(j),nn,atype(j),(pos(k,j),k=1,3)
if(atype(j).eq.'Ge') mass(j)=72.5900
if(atype(j).eq.'Se') mass(j)=78.9600
if(atype(j).eq.'H ') mass(j)=1.008
if(atype(j).eq.'C ') mass(j)=12.011
if(atype(j).eq.'O ') mass(j)=15.999
if(atype(j).eq.'N ') mass(j)=14.007
if(atype(j).eq.'Si') mass(j)=28.086
if(atype(j).eq.'Na') mass(j)=22.99
if(atype(j).eq.'Cl') mass(j)=35.452
if(atype(j).eq.'Mg') mass(j)=24.31
if(atype(j).eq.'Al') mass(j)=26.98
if(atype(j).eq.'Li') mass(j)=6.941
if(atype(j).eq.'K ') mass(j)=39.10
if(atype(j).eq.'Ca') mass(j)=40.08
if(atype(j).eq.'S ') mass(j)=32.066

enddo

do j=1,nat
do i=1,nat
if(numero(i).eq.j)then
write(12,'(a2,6x,i10,2f12.6)')atype(i),j,mass(i),0.0
write(12,'(3e12.4)')(pos(k,i),k=1,3)
exit

endif
enddo
enddo

enddo

end
!------------------------------------------------------------------------

