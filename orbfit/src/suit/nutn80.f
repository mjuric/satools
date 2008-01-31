* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 7, 1997
* ---------------------------------------------------------------------
*
*  ***************************************************************
*  *                                                             *
*  *                         N U T N 8 0                         *
*  *                                                             *
*  *    Nutation angles according to Wahr (IAU 1980) theory      *
*  *                                                             *
*  ***************************************************************
*
*  INPUT:    TJM    -   Modified Julian Time (TDT)
*
*  OUTPUT:   DPSI   -   Nutation in longitude (arcsec)
*            DEPS   -   Nutation in obliquity (arcsec)
*
      subroutine nutn80(tjm,dpsi,deps)
      double precision tjm,dpsi,deps,dl,dp,df,dd,dn,rs,p2,t1
      include 't2000.h'
      real l,n
      save
      data rs,p2/4.84813681109536d-6, 6.2831853071795865d0/
      data z1,z2/1.0, 2.0/


      t1=(tjm-t2000)/36525.d0
      t=sngl(t1)
      t2=t*t
      t3=t2*t
*
* Fundamental arguments (IAU 1980)
*
      dl=( 485866.733d0 +1717915922.633d0*t1 +31.310*t2 +0.064*t3)*rs
      dp=(1287099.804d0 + 129596581.224d0*t1 - 0.577*t2 -0.012*t3)*rs
      df=( 335778.877d0 +1739527263.137d0*t1 -13.257*t2 +0.011*t3)*rs
      dd=(1072261.307d0 +1602961601.328d0*t1 - 6.891*t2 +0.019*t3)*rs
      dn=( 450160.280d0 -   6962890.539d0*t1 + 7.455*t2 +0.008*t3)*rs
      l=sngl(dmod(dl,p2))
      p=sngl(dmod(dp,p2))
      x=sngl(dmod(df,p2)*2.d0)
      d=sngl(dmod(dd,p2))
      n=sngl(dmod(dn,p2))
      cl=cos(l)
      sl=sin(l)
      cp=cos(p)
      sp=sin(p)
      cx=cos(x)
      sx=sin(x)
      cd=cos(d)
      sd=sin(d)
      cn=cos(n)
      sn=sin(n)
      cp2=z2*cp*cp -z1
      sp2=z2*sp*cp
      cd2=z2*cd*cd -z1
      sd2=z2*sd*cd
      cn2=z2*cn*cn -z1
      sn2=z2*sn*cn
      cl2=z2*cl*cl -z1
      sl2=z2*sl*cl
      ca=cx*cd2 +sx*sd2
      sa=sx*cd2 -cx*sd2
      cb=ca*cn -sa*sn
      sb=sa*cn +ca*sn
      cc=cb*cn -sb*sn
      sc=sb*cn +cb*sn
      cv=cx*cd2 -sx*sd2
      sv=sx*cd2 +cx*sd2
      ce=cv*cn -sv*sn
      se=sv*cn +cv*sn
      cf=ce*cn -se*sn
      sf=se*cn +ce*sn
      cg=cl*cd2 +sl*sd2
      sg=sl*cd2 -cl*sd2
      ch=cx*cn2 -sx*sn2
      sh=sx*cn2 +cx*sn2
      cj=ch*cl -sh*sl
      sj=sh*cl +ch*sl
      ck=cj*cl -sj*sl
      sk=sj*cl +cj*sl
      cm=cx*cl2 +sx*sl2
      sm=sx*cl2 -cx*sl2
      cq=cl*cd +sl*sd
      sq=sl*cd -cl*sd
      cr=z2*cq*cq -z1
      sr=z2*sq*cq
      cs=cx*cn -sx*sn
      ss=sx*cn +cx*sn
      ct=cs*cl -ss*sl
      st=ss*cl +cs*sl
      cu=cf*cl +sf*sl
      su=sf*cl -cf*sl
      cw=cp*cg -sp*sg
      sw=sp*cg +cp*sg
*
* Series for DPSI
*
      dpsi=-(171996.+174.2*t)*sn +(2062.+0.2*t)*sn2 +46.*(sm*cn+cm*sn)
     1-11.*sm -3.*(sm*cn2+cm*sn2) -3.*(sq*cp-cq*sp) -2.*(sb*cp2-cb*sp2)
     2+(sn*cm-cn*sm) -(13187.+1.6*t)*sc +(1426.-3.4*t)*sp
     3-(517.-1.2*t)*(sc*cp+cc*sp) +(217.-0.5*t)*(sc*cp-cc*sp)
     4+(129.+0.1*t)*sb +48.*sr -22.*sa +(17.-0.1*t)*sp2
     5-15.*(sp*cn+cp*sn) -(16.-0.1*t)*(sc*cp2+cc*sp2) -12.*(sn*cp-cn*sp)
      dpsi=dpsi -6.*(sn*cr-cn*sr) -5.*(sb*cp-cb*sp) +4.*(sr*cn+cr*sn)
     1+4.*(sb*cp+cb*sp) -4.*sq +(sr*cp+cr*sp) +(sn*ca-cn*sa)
     2-(sp*ca-cp*sa) +(sp*cn2+cp*sn2) +(sn*cq-cn*sq) -(sp*ca+cp*sa)
     3-(2274.+0.2*t)*sh +(712.+0.1*t)*sl -(386.+0.4*t)*ss -301.*sj
     4-158.*sg +123.*(sh*cl-ch*sl) +63.*sd2 +(63.+0.1*t)*(sl*cn+cl*sn)
     5-(58.+0.1*t)*(sn*cl-cn*sl) -59.*su -51.*st -38.*sf +29.*sl2
      dpsi=dpsi +29.*(sc*cl+cc*sl) -31.*sk +26.*sx +21.*(ss*cl-cs*sl)
     1+16.*(sn*cg-cn*sg) -13.*(sn*cg+cn*sg) -10.*(se*cl-ce*sl)
     2-7.*(sg*cp+cg*sp) +7.*(sh*cp+ch*sp) -7.*(sh*cp-ch*sp)
     3-8.*(sf*cl+cf*sl) +6.*(sl*cd2+cl*sd2) +6.*(sc*cl2+cc*sl2)
     4-6.*(sn*cd2+cn*sd2) -7.*se +6.*(sb*cl+cb*sl) -5.*(sn*cd2-cn*sd2)
     5+5.*(sl*cp-cl*sp) -5.*(ss*cl2+cs*sl2) -4.*(sp*cd2-cp*sd2)
      dpsi=dpsi +4.*(sl*cx-cl*sx) -4.*sd -3.*(sl*cp+cl*sp)
     1+3.*(sl*cx+cl*sx) -3.*(sj*cp-cj*sp) -3.*(su*cp-cu*sp)
     2-2.*(sn*cl2-cn*sl2) -3.*(sk*cl+ck*sl) -3.*(sf*cp-cf*sp)
     3+2.*(sj*cp+cj*sp) -2.*(sb*cl-cb*sl)
      dpsi=dpsi +2.*(sn*cl2+cn*sl2) -2.*(sl*cn2+cl*sn2)
     1+2.*(sl*cl2+cl*sl2) +2.*(sh*cd+ch*sd) +(sn2*cl-cn2*sl)
     2-(sg*cd2-cg*sd2) +(sf*cl2-cf*sl2) -2.*(su*cd2+cu*sd2)
     3-(sr*cd2-cr*sd2) +(sw*ch+cw*sh) -(sl*ce+cl*se) -(sf*cr-cf*sr)
     4+(su*ca+cu*sa) +(sg*cp-cg*sp) +(sb*cl2+cb*sl2) -(sf*cl2+cf*sl2)
     5-(st*ca-ct*sa) +(sc*cx+cc*sx) +(sj*cr+cj*sr) -(sg*cx+cg*sx)
      dpsi=dpsi +(sp*cs+cp*ss) +(sn*cw-cn*sw) -(sn*cx-cn*sx)
     1-(sh*cd-ch*sd) -(sp*cd2+cp*sd2) -(sl*cv-cl*sv) -(ss*cp-cs*sp)
     2-(sw*cn+cw*sn) -(sl*ca-cl*sa) +(sl2*cd2+cl2*sd2)
     3-(sf*cd2+cf*sd2) +(sp*cd+cp*sd)
*
* Series for DEPS
*
      deps=(92025.+8.9*t)*cn -(895.-0.5*t)*cn2 -24.*(cm*cn-sm*sn)
     1+(cm*cn2-sm*sn2) +(cb*cp2+sb*sp2) +(5736.-3.1*t)*cc
     2+(54.-0.1*t)*cp +(224.-0.6*t)*(cc*cp-sc*sp)
     3-(95.-0.3*t)*(cc*cp+sc*sp) -70.*cb +cr +9.*(cp*cn-sp*sn)
     4+7.*(cc*cp2-sc*sp2) +6.*(cn*cp+sn*sp) +3.*(cn*cr+sn*sr)
     5+3.*(cb*cp+sb*sp) -2.*(cr*cn-sr*sn) -2.*(cb*cp-sb*sp)
      deps=deps +(977.-0.5*t)*ch -7.*cl +200.*cs +(129.-0.1*t)*cj -cg
     1-53.*(ch*cl+sh*sl) -2.*cd2 -33.*(cl*cn-sl*sn) +32.*(cn*cl+sn*sl)
     2+26.*cu +27.*ct +16.*cf -cl2 -12.*(cc*cl-sc*sl) +13.*ck -cx
     3-10.*(cs*cl+ss*sl) -8.*(cn*cg+sn*sg) +7.*(cn*cg-sn*sg)
     4+5.*(ce*cl+se*sl) -3.*(ch*cp-sh*sp) +3.*(ch*cp+sh*sp)
     5+3.*(cf*cl-sf*sl) -3.*(cc*cl2-sc*sl2) +3.*(cn*cd2-sn*sd2) +3.*ce
      deps=deps -3.*(cb*cl-sb*sl) +3.*(cn*cd2+sn*sd2)
     1+3.*(cs*cl2-ss*sl2) +(cj*cp+sj*sp) +(cu*cp+su*sp)
     2+(cn*cl2+sn*sl2) +(ck*cl-sk*sl) +(cf*cp+sf*sp) -(cj*cp-sj*sp)
     3+(cb*cl+sb*sl)
      deps=deps -(cn*cl2-sn*sl2) +(cl*cn2-sl*sn2) -(ch*cd-sh*sd)
     1-(cn2*cl+sn2*sl) -(cf*cl2+sf*sl2) +(cu*cd2-su*sd2) -(cw*ch-sw*sh)
     2+(cl*ce-sl*se) +(cf*cr+sf*sr) -(cb*cl2-sb*sl2)
*
      dpsi=dpsi*1.d-4
      deps=deps*1.d-4
      end
