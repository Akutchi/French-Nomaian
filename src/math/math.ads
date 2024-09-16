with Glib; use Glib;

package Math is

   function Linearize_Angle
     (I, N, a, k : Gdouble; Is_Derived : Boolean := False) return Gdouble;
   --  cut the interval [a, ka] in N parts to smooth the stretching of N
   --  elements. I is here to specify which value to get, knowing that
   --  f(1) = ka
   --  f(N) = a
   --
   --  this function contains its derivate for simplicity when calculating
   --  gradients.

   function Ln_Smooth
     (x : Gdouble; Is_Derived : Boolean := False) return Gdouble;
   --  Smooth the value f (I) as described above in Lienarize_Angle so that
   --  for greater set of points, the streching between E_i and E_i_1 is not
   --  as much reduced.
   --
   --  this function contains its derivate for simplicity when calculating
   --  gradients.

   function Linearize_Scale (x, N : Gdouble) return Gdouble;
   --  Akin to Linearize_Angle, but for scaling elements.

   function theta (I, N, Start_Angle, a, k : Gdouble) return Gdouble;

   function radius (theta_var : Gdouble) return Gdouble;

   function theta_prime (I, N, a, k : Gdouble) return Gdouble;

   function radius_prime (I, N, Start_Angle, a, k : Gdouble) return Gdouble;

end Math;
