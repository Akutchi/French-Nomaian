with Glib; use Glib;

package Math is

   type gradient is record
      dx, dy : Gdouble;

   end record;

   function Linearize_Scale (x, N : Gdouble) return Gdouble;
   --  This function is used to adapt the scaling from 1.0 to 0.5 to the
   --  interval [|1; N|]

   function theta (I, N : Gdouble) return Gdouble;

   function radius (I, N : Gdouble) return Gdouble;

   function Calculate_Gradient
     (I, N : Gdouble; Is_Vowel : Boolean := False) return gradient;

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

end Math;
