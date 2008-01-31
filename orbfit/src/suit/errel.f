c ==================
c  {\bf errel} relative error
      double precision function errel(a1,a2)
      implicit double precision (a-h,o-z)
      amed=(a1+a2)/2.d0
      adif=a2-a1
      if(adif.eq.0.d0)then
          errel=0.d0
      else
          errel=adif/amed
      end if
      return
      end
