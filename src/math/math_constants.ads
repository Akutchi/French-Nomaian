with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics;

with Glib; use Glib;

package Math_Constants is

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

   e_d : constant Gdouble := Gdouble (Ada.Numerics.e);
   Phi : constant Gdouble := (1.0 + Sqrt (5.0)) / 2.0;

   PI     : constant Gdouble := Gdouble (Ada.Numerics.Pi);
   TWO_PI : constant Gdouble := 2.0 * PI;
   PI_2   : constant Gdouble := PI / 2.0;
   PI_3   : constant Gdouble := PI / 3.0;
   PI_4   : constant Gdouble := PI / 4.0;
   PI_5   : constant Gdouble := PI / 5.0;
   PI_6   : constant Gdouble := PI / 6.0;
   PI_7   : constant Gdouble := PI / 7.0;

end Math_Constants;
