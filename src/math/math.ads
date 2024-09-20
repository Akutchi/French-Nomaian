with Glib; use Glib;

package Math is

   type vector is record
      p1, p2 : Gdouble := 0.0;
   end record;

   type gradient is record
      dr, dtheta : Gdouble := 0.0;
   end record;

   function Linearize_Scale (x, N : Gdouble) return Gdouble;
   --  This function is used to adapt the scaling from 1.0 to 0.5 to the
   --  interval [|1; N|]

   function theta (I, N : Gdouble) return Gdouble;

   function radius (I, N : Gdouble) return Gdouble;

   function Calculate_Gradient
     (I, N : Gdouble; Is_Vowel : Boolean := False) return gradient;

   procedure Adjust_Element (Angle : in out Gdouble; I, N : Gdouble);

private

   function k (N : Gdouble) return Gdouble;
   --  This function is used to adapt the angle to the length of the spiral.
   --  The longer the spiral, the greater k will be, also increasing the
   --  interval between two elements.

   function s (N : Gdouble) return Gdouble;
   --  This function is used to adapt the spiral's radius to the length of said
   --  structure. The longer the spiral, the greater s will be, in turn also
   --  increasing the distance to the spiral's arm.

   function theta_prime (N : Gdouble) return Gdouble;

   function radius_prime (I, N : Gdouble) return Gdouble;

   function eps (N : Gdouble) return Gdouble;
   --  This function is used to adapt the gradient epsilon to the curve length.
   --  I would need to adapt to the scaling also, but this is not an important
   --  optimization.

   function norm (Point : vector) return Gdouble;

   function Normalize (u : vector) return vector;

   function dot (u : vector; v : vector) return Gdouble;

end Math;
