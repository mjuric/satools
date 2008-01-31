      subroutine transp(a,n,m,b)
      double precision a(n,m),b(m,n)
      do 50 i=1,n
        do 60 j=1,m
          b(j,i)=a(i,j)
 60     continue  
 50   continue  
      return
      end 
