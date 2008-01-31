c
      double precision function prscal(a,b)
c
c    Computes the scalar product of two 3d vectors
c
      double precision a(3),b(3)
      prscal=a(1)*b(1)+a(2)*b(2)+a(3)*b(3)
      return
      end
