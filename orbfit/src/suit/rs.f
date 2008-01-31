      subroutine rs(nm,n,a,w,matz,z,fv1,fv2,ierr)                               
c                                                                               
      integer n,nm,ierr,matz                                                    
      double precision a(nm,n),w(n),z(nm,n),fv1(n),fv2(n)                                 
c                                                                               
c     this subroutine calls the recommended sequence of                         
c     subroutines from the eigensystem subroutine package (eispack)             
c     to find the eigenvalues and eigenvectors (if desired)                     
c     of a real symmetric matrix.                                               
c                                                                               
c     on input:                                                                 
c                                                                               
c        nm  must be set to the row dimension of the two-dimensional            
c        array parameters as declared in the calling program                    
c        dimension statement;                                                   
c                                                                               
c        n  is the order of the matrix  a;                                      
c                                                                               
c        a  contains the real symmetric matrix;                                 
c                                                                               
c        matz  is an integer variable set equal to zero if                      
c        only eigenvalues are desired;  otherwise it is set to                  
c        any non-zero integer for both eigenvalues and eigenvectors.            
c                                                                               
c     on output:                                                                
c                                                                               
c        w  contains the eigenvalues in ascending order;                        
c                                                                               
c        z  contains the eigenvectors if matz is not zero;                      
c                                                                               
c        ierr  is an integer output variable set equal to an                    
c        error completion code described in section 2b of the                   
c        documentation.  the normal completion code is zero;                    
c                                                                               
c        fv1  and  fv2  are temporary storage arrays.                           
c                                                                               
c     questions and comments should be directed to b. s. garbow,                
c     applied mathematics division, argonne national laboratory                 
c                                                                               
c     ------------------------------------------------------------------        
c                                                                               
      if (n .le. nm) go to 10                                                   
      ierr = 10 * n                                                             
      go to 50                                                                  
c                                                                               
   10 if (matz .ne. 0) go to 20                                                 
c     :::::::::: find eigenvalues only ::::::::::                               
      call  tred1(nm,n,a,w,fv1,fv2)                                             
      call  tqlrat(n,w,fv2,ierr)                                                
      go to 50                                                                  
c     :::::::::: find both eigenvalues and eigenvectors ::::::::::              
   20 call  tred2(nm,n,a,w,fv1,z)                                               
      call  tql2(nm,n,w,fv1,z,ierr)                                             
   50 return                                                                    
c     :::::::::: last card of rs ::::::::::                                     
      end                                                                       
