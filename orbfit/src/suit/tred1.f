      subroutine tred1(nm,n,a,d,e,e2)                                           
c                                                                               
      integer i,j,k,l,n,ii,nm,jp1                                               
      double precision a(nm,n),d(n),e(n),e2(n)                                            
      double precision f,g,h,scale                                                        
c                                                                               
c     this subroutine is a translation of the algol procedure tred1,            
c     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson.           
c     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).           
c                                                                               
c     this subroutine reduces a real symmetric matrix                           
c     to a symmetric tridiagonal matrix using                                   
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
c        a contains information about the orthogonal trans-                     
c          formations used in the reduction in its strict lower                 
c          triangle.  the full upper triangle of a is unaltered;                
c                                                                               
c        d contains the diagonal elements of the tridiagonal matrix;            
c                                                                               
c        e contains the subdiagonal elements of the tridiagonal                 
c          matrix in its last n-1 positions.  e(1) is set to zero;              
c                                                                               
c        e2 contains the squares of the corresponding elements of e.            
c          e2 may coincide with e if the squares are not needed.                
c                                                                               
c     questions and comments should be directed to b. s. garbow,                
c     applied mathematics division, argonne national laboratory                 
c                                                                               
c     ------------------------------------------------------------------        
c                                                                               
      do 100 i = 1, n                                                           
  100 d(i) = a(i,i)                                                             
c     :::::::::: for i=n step -1 until 1 do -- ::::::::::                       
      do 300 ii = 1, n                                                          
         i = n + 1 - ii                                                         
         l = i - 1                                                              
         h = 0.0d0                                                              
         scale = 0.0d0                                                          
         if (l .lt. 1) go to 130                                                
c     :::::::::: scale row (algol tol then not needed) ::::::::::               
         do 120 k = 1, l                                                        
  120    scale = scale + abs(a(i,k))                                           
c                                                                               
         if (scale .ne. 0.0d0) go to 140                                        
  130    e(i) = 0.0d0                                                           
         e2(i) = 0.0d0                                                          
         go to 290                                                              
c                                                                               
  140    do 150 k = 1, l                                                        
            a(i,k) = a(i,k) / scale                                             
            h = h + a(i,k) * a(i,k)                                             
  150    continue                                                               
c                                                                               
         e2(i) = scale * scale * h                                              
         f = a(i,l)                                                             
         g = -sign(sqrt(h),f)                                                 
         e(i) = scale * g                                                       
         h = h - f * g                                                          
         a(i,l) = f - g                                                         
         if (l .eq. 1) go to 270                                                
         f = 0.0d0                                                              
c                                                                               
         do 240 j = 1, l                                                        
            g = 0.0d0                                                           
c     :::::::::: form element of a*u ::::::::::                                 
            do 180 k = 1, j                                                     
  180       g = g + a(j,k) * a(i,k)                                             
c                                                                               
            jp1 = j + 1                                                         
            if (l .lt. jp1) go to 220                                           
c                                                                               
            do 200 k = jp1, l                                                   
  200       g = g + a(k,j) * a(i,k)                                             
c     :::::::::: form element of p ::::::::::                                   
  220       e(j) = g / h                                                        
            f = f + e(j) * a(i,j)                                               
  240    continue                                                               
c                                                                               
         h = f / (h + h)                                                        
c     :::::::::: form reduced a ::::::::::                                      
         do 260 j = 1, l                                                        
            f = a(i,j)                                                          
            g = e(j) - h * f                                                    
            e(j) = g                                                            
c                                                                               
            do 260 k = 1, j                                                     
               a(j,k) = a(j,k) - f * e(k) - g * a(i,k)                          
  260    continue                                                               
c                                                                               
  270    do 280 k = 1, l                                                        
  280    a(i,k) = scale * a(i,k)                                                
c                                                                               
  290    h = d(i)                                                               
         d(i) = a(i,i)                                                          
         a(i,i) = h                                                             
  300 continue                                                                  
c                                                                               
      return                                                                    
c     :::::::::: last card of tred1 ::::::::::                                  
      end                                                                       
