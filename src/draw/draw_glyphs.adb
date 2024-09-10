with Ada.Numerics.Generic_Elementary_Functions;

package body Draw_Glyphs is

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);

   use Functions;

   ----------------
   -- Background --
   ----------------

   procedure Background (Ctx : Cairo.Cairo_Context) is
   begin

      Cairo.Set_Source_Rgb (Ctx, 0.98, 0.92, 0.84);
      Cairo.Rectangle (Ctx, 0.0, 0.0, 100.0, 100.0);
      Cairo.Fill (Ctx);
      Cairo.Set_Source_Rgb (Ctx, 0.06, 0.30, 0.55);
      Cairo.Set_Line_Width (Ctx, 0.3);

   end Background;

   ---------------------
   -- Rotation_Around --
   ---------------------

   procedure Rotation_Around
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; Angle : Gdouble)
   is
   begin
      Cairo.Translate (Ctx, X, Y);
      Cairo.Rotate (Ctx, Angle);
      Cairo.Translate (Ctx, -X, -Y);

   end Rotation_Around;

   --------------------
   -- Scaling_Around --
   --------------------

   procedure Scaling_Around
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; Sx, Sy : Gdouble)
   is
   begin
      Cairo.Translate (Ctx, X, Y);
      Cairo.Scale (Ctx, Sx, Sy);
      Cairo.Translate (Ctx, -X, -Y);

   end Scaling_Around;

   ---------
   -- Dot --
   ---------

   procedure Dot (Ctx : Cairo.Cairo_Context; X, Y : Gdouble) is
   begin

      Cairo.Move_To (Ctx, X, Y);
      Cairo.Arc (Ctx, X, Y, R_Dot, 0.0, 360.0);
      Cairo.Fill (Ctx);
      Cairo.Stroke (Ctx);

   end Dot;

   ----------
   -- Line --
   ----------

   procedure Line (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin

      Dot (Ctx, X, Y);

      Cairo.Move_To (Ctx, X + R_Dot - 0.5, Y);
      Cairo.Line_To (Ctx, X + R_Poly, Y);
      Cairo.Stroke (Ctx);

      Dot (Ctx, X + R_Poly, Y);

   end Line;

   ----------
   -- Bend --
   ----------

   procedure Bend (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin

      Dot (Ctx, X, Y);

      Cairo.Move_To (Ctx, X + R_Dot - 0.5, Y);
      Cairo.Line_To (Ctx, X + R_Poly, Y);
      Cairo.Line_To (Ctx, X + 1.5 * R_Poly, Y + R_Poly);
      Cairo.Stroke (Ctx);

      Dot (Ctx, X + 1.5 * R_Poly, Y + R_Poly);

   end Bend;

   -----------
   -- Ngone --
   -----------

   procedure Ngone
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive;
      r   :        Gdouble := R_Poly)
   is

      Start_Angle : constant Gdouble := 0.0;
      Increment   : constant Gdouble := Gdouble (2.0 * Ada.Numerics.Pi / N);

      theta : Gdouble := Start_Angle;

      I : Positive := 1;

   begin

      Cairo.Move_To (Ctx, X, Y);
      loop

         declare

            X1 : constant Gdouble := X + r * Cos (theta);
            Y1 : constant Gdouble := Y + r * Sin (theta);

            X2 : constant Gdouble := X + r * Cos (theta + Increment);
            Y2 : constant Gdouble := Y + r * Sin (theta + Increment);

         begin

            exit when I > N + 1;

            Dot (Ctx, X1, Y1);

            Cairo.Move_To (Ctx, X1, Y1);
            Cairo.Line_To (Ctx, X2, Y2);
            Cairo.Stroke (Ctx);

            Dot (Ctx, X2, Y2);

            theta := theta + Increment;
            I     := I + 1;

         end;
      end loop;

   end Ngone;

   procedure NgoneLine
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive)
   is
   begin
      Ngone (Ctx, X, Y, N);
      Cairo.Move_To (Ctx, X + R_Poly, Y);
      Cairo.Line_To (Ctx, X + 2.0 * R_Poly, Y);
      Cairo.Stroke (Ctx);

   end NgoneLine;

   procedure NgoneBend
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive)
   is
   begin
      Ngone (Ctx, X, Y, N);
      Cairo.Move_To (Ctx, X + R_Poly, Y);
      Cairo.Line_To (Ctx, X + 2.0 * R_Poly, Y);
      Cairo.Move_To (Ctx, X + 2.0 * R_Poly, Y);
      Cairo.Line_To (Ctx, X + 2.0 * R_Poly, Y + 0.5 * R_Poly);

      Cairo.Stroke (Ctx);

   end NgoneBend;

   procedure SquareSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin
      Ngone (Ctx, X, Y, 4);
      Ngone (Ctx, X - R_Poly, Y - R_Poly, 4);
   end SquareSquare;

   procedure PentaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin

      Ngone (Ctx, X, Y, 5);
      Rotation_Around (Ctx, X, Y, 18.22);
      Ngone (Ctx, X - 1.315 * R_Poly, Y - 0.95 * R_Poly, 5);

   end PentaPenta;

   procedure HexaHexa (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin
      Ngone (Ctx, X, Y, 6);
      Ngone (Ctx, X - 1.5 * R_Poly, Y - 0.87 * R_Poly, 6);
   end HexaHexa;

   procedure HeptaHepta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin
      Ngone (Ctx, X, Y, 7);
      Rotation_Around (Ctx, X, Y, 18.4);
      Ngone (Ctx, X - 1.62 * R_Poly, Y - 0.79 * R_Poly, 7);
   end HeptaHepta;

end Draw_Glyphs;
