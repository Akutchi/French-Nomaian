with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;

package body Draw_Glyphs is

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);

   use Functions;

   procedure Background (Ctx : Cairo.Cairo_Context) is
   begin

      Cairo.Set_Source_Rgb (Ctx, 0.98, 0.92, 0.84);
      Cairo.Rectangle (Ctx, 0.0, 0.0, 100.0, 100.0);
      Cairo.Fill (Ctx);
      Cairo.Set_Source_Rgb (Ctx, 0.06, 0.30, 0.55);
      Cairo.Set_Line_Width (Ctx, 0.3);

   end Background;

   procedure Dot (Ctx : Cairo.Cairo_Context; X, Y : Gdouble) is
   begin

      Cairo.Move_To (Ctx, X, Y);
      Cairo.Arc (Ctx, X, Y, R_Dot, 0.0, 360.0);
      Cairo.Fill (Ctx);
      Cairo.Stroke (Ctx);

   end Dot;

   procedure Turn (X, Y : in out Gdouble; Angle : Gdouble) is
   begin

      X := X * Cos (Angle) - Y * Sin (Angle);
      Y := X * Sin (Angle) + Y * Cos (Angle);

   end Turn;

   procedure Ngone
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive)
   is

      Start_Angle : constant Gdouble := Gdouble (0.0);
      Increment   : constant Gdouble := Gdouble (2.0 * Ada.Numerics.Pi / N);

      r     : constant Gdouble := R_Poly;
      theta : Gdouble          := Start_Angle;

      I : Positive := 1;

   begin

      Cairo.Move_To (Ctx, X, Y);
      loop

         Dot (Ctx, r * Cos (theta) + X, r * Sin (theta) + Y);
         theta := theta + Increment;

      end loop;

   end Ngone;

end Draw_Glyphs;
