      subroutine tred2(nm,n,a,d,e,z)                                            
c                                                                               
      integer i,j,k,l,n,ii,nm,jp1                                               
      double precision a(nm,n),d(n),e(n),z(nm,n)                                          
      double precision f,g,h,hh,scale                                                     
*      real*8 dsqrt,dabs,dsign                                                   
c                                                                               
c     this subroutine is a translation of the algol procedure tred2,            
c     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson.           
c     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).           
c                                                                               
c     this subroutine reduces a real symmetric matrix to a                      
c     symmetric tridiagonal matrix using and accumulating                       
c     orthogonal similarity transformations.                                    
c                                                                               
c     on input:                                                                 
c                                                                               
c        nm must be set to the row dimension of two-dimensional                 
c          array parameters as declared in the calling program                  
c          dimension statement;                                                 
c                                                                               
c        n is the order of the matrix;                                          
c                                                                               
c        a contains the real symmetric input matrix.  only the                  
c          lower triangle of the matrix need be supplied.                       
c                                                                               
c     on output:                                                                
c                                                                               
c        d contains the diagonal elements of the tridiagonal matrix;            
c                                                                               
c        e contains the subdiagonal elements of the tridiagonal                 
c          matrix in its last n-1 positions.  e(1) is set to zero;              
c                                                                               
c        z contains the orthogonal transformation matrix                        
c          produced in the reduction;                                           
c                                                                               
c        a and z may coincide.  if distinct, a is unaltered.                    
c                                                                               
c     questions and comments should be directed to b. s. garbow,                
c     applied mathematics division, argonne national laboratory                 
c                                                                               
c     ------------------------------------------------------------------        
c                                                                               
      do 100 i = 1, n                                                           
c                                                                               
         do 100 j = 1, i                                                        
            z(i,j) = a(i,j)                                                     
  100 continue                                                                  
c                                                                               
      if (n .eq. 1) go to 320                                                   
c     :::::::::: for i=n step -1 until 2 do -- ::::::::::                       
      do 300 ii = 2, n                                                          
         i = n + 2 - ii                                                         
         l = i - 1                                                              
         h = 0.0d0                                                              
         scale = 0.0d0                                                          
         if (l .lt. 2) go to 130                                                
c     :::::::::: scale row (algol tol then not needed) ::::::::::               
         do 120 k = 1, l                                                        
  120    scale = scale + abs(z(i,k))                                           
c                                                                               
         if (scale .ne. 0.0d0) go to 140                                        
  130    e(i) = z(i,l)                                                          
         go to 290                                                              
c                                                                               
  140    do 150 k = 1, l                                                        
            z(i,k) = z(i,k) / scale                                             
            h = h + z(i,k) * z(i,k)                                             
  150    continue                                                               
c                                                                               
         f = z(i,l)                                                             
         g = -sign(sqrt(h),f)                                                 
         e(i) = scale * g                                                       
         h = h - f * g                                                          
         z(i,l) = f - g                                                         
         f = 0.0d0                                                              
c                                                                               
         do 240 j = 1, l                                                        
            z(j,i) = z(i,j) / h                                                 
            g = 0.0d0                                                           
c     :::::::::: form element of a*u ::::::::::                                 
            do 180 k = 1, j                                                     
  180       g = g + z(j,k) * z(i,k)                                             
c                                                                               
            jp1 = j + 1                                                         
            if (l .lt. jp1) go to 220                                           
c                                                                               
            do 200 k = jp1, l                                                   
  200       g = g + z(k,j) * z(i,k)                                             
c     :::::::::: form element of p ::::::::::                                   
  220       e(j) = g / h                                                        
            f = f + e(j) * z(i,j)                                               
  240    continue                                                               
c                                                                               
         hh = f / (h + h)                                                       
c     :::::::::: form reduced a ::::::::::                                      
         do 260 j = 1, l                                                        
            f = z(i,j)                                                          
            g = e(j) - hh * f                                                   
            e(j) = g                                                            
c                                                                               
            do 260 k = 1, j                                                     
               z(j,k) = z(j,k) - f * e(k) - g * z(i,k)                          
  260    continue                                                               
c                                                                               
  290    d(i) = h                                                               
  300 continue                                                                  
c                                                                               
  320 d(1) = 0.0d0                                                              
      e(1) = 0.0d0                                                              
c     :::::::::: accumulation of transformation matrices ::::::::::             
      do 500 i = 1, n                                                           
         l = i - 1                                                              
         if (d(i) .eq. 0.0d0) go to 380                                         
c                                                                               
         do 360 j = 1, l                                                        
            g = 0.0d0                                                           
c                                                                               
            do 340 k = 1, l                                                     
  340       g = g + z(i,k) * z(k,j)                                             
c                                                                               
            do 360 k = 1, l                                                     
               z(k,j) = z(k,j) - g * z(k,i)                                     
  360    continue                                                               
c                                                                               
  380    d(i) = z(i,i)                                                          
         z(i,i) = 1.0d0                                                         
         if (l .lt. 1) go to 500                                                
c                                                                               
         do 400 j = 1, l                                                        
            z(i,j) = 0.0d0                                                      
            z(j,i) = 0.0d0                                                      
  400    continue                                                               
c                                                                               
  500 continue                                                                  
c                                                                               
      return                                                                    
c     :::::::::: last card of tred2 ::::::::::                                  
      end                                                                       
