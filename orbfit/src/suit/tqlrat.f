      subroutine tqlrat(n,d,e2,ierr)                                            
c                                                                               
      integer i,j,l,m,n,ii,l1,mml,ierr                                          
      double precision d(n),e2(n)                                                         
      double precision b,c,f,g,h,p,r,s,machep                                             
*      real*8 dsqrt,dabs,dsign                                                   
c                                                                               
c     this subroutine is a translation of the algol procedure tqlrat,           
c     algorithm 464, comm. acm 16, 689(1973) by reinsch.                        
c                                                                               
c     this subroutine finds the eigenvalues of a symmetric                      
c     tridiagonal matrix by the rational ql method.                             
c                                                                               
c     on input:                                                                 
c                                                                               
c        n is the order of the matrix;                                          
c                                                                               
c        d contains the diagonal elements of the input matrix;                  
c                                                                               
c        e2 contains the squares of the subdiagonal elements of the             
c          input matrix in its last n-1 positions.  e2(1) is arbitrary.         
c                                                                               
c      on output:                                                               
c                                                                               
c        d contains the eigenvalues in ascending order.  if an                  
c          error exit is made, the eigenvalues are correct and                  
c          ordered for indices 1,2,...ierr-1, but may not be                    
c          the smallest eigenvalues;                                            
c                                                                               
c        e2 has been destroyed;                                                 
c                                                                               
c        ierr is set to                                                         
c          zero       for normal return,                                        
c          j          if the j-th eigenvalue has not been                       
c                     determined after 30 iterations.                           
c                                                                               
c     questions and comments should be directed to b. s. garbow,                
c     applied mathematics division, argonne national laboratory                 
c                                                                               
c     ------------------------------------------------------------------        
c                                                                               
c     :::::::::: machep is a machine dependent parameter specifying             
c                the relative precision of floating point arithmetic.           
c                machep = 16.0d0**(-13) for long form arithmetic                
c                on s360 ::::::::::                                             
*      data machep/z'3410000000000000'/
      data machep/1.d-16/
c                                                                               
      ierr = 0                                                                  
      if (n .eq. 1) go to 1001                                                  
c                                                                               
      do 100 i = 2, n                                                           
  100 e2(i-1) = e2(i)                                                           
c                                                                               
      f = 0.0d0                                                                 
      b = 0.0d0                                                                 
      e2(n) = 0.0d0                                                             
c                                                                               
      do 290 l = 1, n                                                           
         j = 0                                                                  
         h = machep * (abs(d(l)) + sqrt(e2(l)))                               
         if (b .gt. h) go to 105                                                
         b = h                                                                  
         c = b * b                                                              
c     :::::::::: look for small squared sub-diagonal element ::::::::::         
  105    do 110 m = l, n                                                        
            if (e2(m) .le. c) go to 120                                         
c     :::::::::: e2(n) is always zero, so there is no exit                      
c                through the bottom of the loop ::::::::::                      
  110    continue                                                               
c                                                                               
  120    if (m .eq. l) go to 210                                                
  130    if (j .eq. 30) go to 1000                                              
         j = j + 1                                                              
c     :::::::::: form shift ::::::::::                                          
         l1 = l + 1                                                             
         s = sqrt(e2(l))                                                       
         g = d(l)                                                               
         p = (d(l1) - g) / (2.0d0 * s)                                          
         r = sqrt(p*p+1.0d0)                                                   
         d(l) = s / (p + sign(r,p))                                            
         h = g - d(l)                                                           
c                                                                               
         do 140 i = l1, n                                                       
  140    d(i) = d(i) - h                                                        
c                                                                               
         f = f + h                                                              
c     :::::::::: rational ql transformation ::::::::::                          
         g = d(m)                                                               
         if (g .eq. 0.0d0) g = b                                                
         h = g                                                                  
         s = 0.0d0                                                              
         mml = m - l                                                            
c     :::::::::: for i=m-1 step -1 until l do -- ::::::::::                     
         do 200 ii = 1, mml                                                     
            i = m - ii                                                          
            p = g * h                                                           
            r = p + e2(i)                                                       
            e2(i+1) = s * r                                                     
            s = e2(i) / r                                                       
            d(i+1) = h + s * (h + d(i))                                         
            g = d(i) - e2(i) / g                                                
            if (g .eq. 0.0d0) g = b                                             
            h = g * p / r                                                       
  200    continue                                                               
c                                                                               
         e2(l) = s * g                                                          
         d(l) = h                                                               
c     :::::::::: guard against underflow in convergence test ::::::::::         
         if (h .eq. 0.0d0) go to 210                                            
         if (abs(e2(l)) .le. abs(c/h)) go to 210                              
         e2(l) = h * e2(l)                                                      
         if (e2(l) .ne. 0.0d0) go to 130                                        
  210    p = d(l) + f                                                           
c     :::::::::: order eigenvalues ::::::::::                                   
         if (l .eq. 1) go to 250                                                
c     :::::::::: for i=l step -1 until 2 do -- ::::::::::                       
         do 230 ii = 2, l                                                       
            i = l + 2 - ii                                                      
            if (p .ge. d(i-1)) go to 270                                        
            d(i) = d(i-1)                                                       
  230    continue                                                               
c                                                                               
  250    i = 1                                                                  
  270    d(i) = p                                                               
  290 continue                                                                  
c                                                                               
      go to 1001                                                                
c     :::::::::: set error -- no convergence to an                              
c                eigenvalue after 30 iterations ::::::::::                      
 1000 ierr = l                                                                  
 1001 return                                                                    
c     :::::::::: last card of tqlrat ::::::::::                                 
      end                                                                       
