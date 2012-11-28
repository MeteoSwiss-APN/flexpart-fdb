!**********************************************************************
! Copyright 1998,1999,2000,2001,2002,2005,2007,2008,2009,2010         *
! Andreas Stohl, Petra Seibert, A. Frank, Gerhard Wotawa,             *
! Caroline Forster, Sabine Eckhardt, John Burkhart, Harald Sodemann   *
!                                                                     *
! This file is part of FLEXPART.                                      *
!                                                                     *
! FLEXPART is free software: you can redistribute it and/or modify    *
! it under the terms of the GNU General Public License as published by*
! the Free Software Foundation, either version 3 of the License, or   *
! (at your option) any later version.                                 *
!                                                                     *
! FLEXPART is distributed in the hope that it will be useful,         *
! but WITHOUT ANY WARRANTY; without even the implied warranty of      *
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *
! GNU General Public License for more details.                        *
!                                                                     *
! You should have received a copy of the GNU General Public License   *
! along with FLEXPART.  If not, see <http://www.gnu.org/licenses/>.   *
!**********************************************************************

subroutine part0(dquer,dsigma,density,fract,schmi,cun,vsh)
  !                  i      i       i      o     o    o   o
  !*****************************************************************************
  !                                                                            *
  !      Calculation of time independent factors of the dry deposition of      *
  !      particles:                                                            *
  !      Log-Normal-distribution of mass [dM/dlog(dp)], unimodal               *
  !                                                                            *
  !      AUTHOR: Matthias Langer, adapted by Andreas Stohl, 13 November 1993   *
  !                                                                            *
  !      Literature:                                                           *
  !      [1]  Scire/Yamartino/Carmichael/Chang (1989),                         *
  !             CALGRID: A Mesoscale Photochemical Grid Model.                 *
  !             Vol II: User's Guide. (Report No.A049-1, June, 1989)           *
  !                                                                            *
  !*****************************************************************************
  !                                                                            *
  ! Variables:                                                                 *
  ! alpha            help variable                                             *
  ! cun              'slip-flow' correction after Cunningham                   *
  ! d01 [um]         upper diameter                                            *
  ! d02 [um]         lower diameter                                            *
  ! dc [m2/s]        coefficient of Brownian diffusion                         *
  ! delta            distance given in standard deviation units                *
  ! density [kg/m3]  density of the particle                                   *
  ! dmean            geometric mean diameter of interval                       *
  ! dquer [um]       geometric mass mean particle diameter                     *
  ! dsigma           e.g. dsigma=10 or dsigma=0.1 means that 68% of the mass   *
  !                  are between 0.1*dquer and 10*dquer                        *
  ! fract(ni)        mass fraction of each diameter interval                   *
  ! kn               Knudsen number                                            *
  ! ni               number of diameter intervals, for which deposition        *
  !                  is calculated                                             *
  ! schmidt          Schmidt number                                            *
  ! schmi            schmidt**2/3                                              *
  ! vsh [m/s]        gravitational settling velocity of the particle           *
  ! x01              normalized upper diameter                                 *
  ! x02              normalized lower diameter                                 *
  !                                                                            *
  ! Constants:                                                                 *
  ! g [m/s2]         Acceleration of gravity                                   *
  ! kb [J/K]         Stefan-Boltzmann constant                                 *
  ! lam [m]          mean free path of air molecules                           *
  ! myl [kg/m/s]     dynamical viscosity of air                                *
  ! nyl [m2/s]       kinematic viscosity of air                                *
  ! tr               reference temperature                                     *
  !                                                                            *
  ! Function:                                                                  *
  ! erf              calculates the integral of the Gauss function             *
  !                                                                            *
  !*****************************************************************************

  use par_mod

  implicit none

  real,parameter :: tr=293.15

  integer :: i
  real :: dquer,dsigma,density,xdummy,d01,d02,delta,x01,x02,fract(ni)
  real :: dmean,alpha,cun,dc,schmidt,schmi(ni),vsh(ni),kn,erf
  real,parameter :: myl=1.81e-5,nyl=0.15e-4
  real,parameter :: lam=6.53e-8,kb=1.38e-23,eps=1.2e-38


  ! xdummy constant for all intervals
  !**********************************

  xdummy=sqrt(2.)*alog(dsigma)


  ! particles diameters are split up to ni intervals between
  ! dquer-3*dsigma and dquer+3*dsigma
  !*********************************************************

  delta=6./real(ni)

  d01=dquer*dsigma**(-3)
  do i=1,ni
    d02=d01
    d01=dquer*dsigma**(-3.+delta*real(i))
    x01=alog(d01/dquer)/xdummy
    x02=alog(d02/dquer)/xdummy


  ! Area under Gauss-function is calculated and gives mass fraction of interval
  !****************************************************************************

    fract(i)=0.5*(erf(x01)-erf(x02))


  ! Geometric mean diameter of interval in [m]
  !*******************************************

    dmean=1.E-6*exp(0.5*alog(d01*d02))


  ! Calculation of time independent parameters of each interval
  !************************************************************

    kn=2.*lam/dmean
    if ((-1.1/kn).le.log10(eps)*log(10.)) then
      alpha=1.257
    else
      alpha=1.257+0.4*exp(-1.1/kn)
    endif
    cun=1.+alpha*kn
    dc=kb*tr*cun/(3.*pi*myl*dmean)
    schmidt=nyl/dc
    schmi(i)=schmidt**(-2./3.)
    vsh(i)=ga*density*dmean*dmean*cun/(18.*myl)

  end do

end subroutine part0
