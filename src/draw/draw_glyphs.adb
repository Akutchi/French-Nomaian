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

      Rotation_Around (Ctx, X + R_Poly, Y, PI);

      Dot (Ctx, X, Y);

      Cairo.Move_To (Ctx, X + R_Dot - 0.5, Y);
      Cairo.Line_To (Ctx, X + R_Poly, Y);
      Cairo.Line_To (Ctx, X + 1.5 * R_Poly, Y + R_Poly);
      Cairo.Stroke (Ctx);

      Dot (Ctx, X + 1.5 * R_Poly, Y + R_Poly);

      Rotation_Around (Ctx, X + R_Poly, Y, -1.0 * PI);

   end Bend;

   --------------------
   -- Word_Separator --
   --------------------

   procedure Word_Separator (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble)
   is
   begin

      Dot (Ctx, X, Y);

      Cairo.Move_To (Ctx, X + R_Dot - 0.5, Y);
      Cairo.Line_To (Ctx, X + R_Poly, Y);
      Cairo.Line_To (Ctx, X + 2.0 * R_Poly, Y + 0.4 * R_Poly);
      Cairo.Stroke (Ctx);

      Dot (Ctx, X + R_Poly, Y);
      Dot (Ctx, X + 2.0 * R_Poly, Y + 0.4 * R_Poly);

   end Word_Separator;

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

   ---------------
   -- NgoneLine --
   ---------------

   procedure NgoneLine
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive)
   is
   begin
      Rotation_Around (Ctx, X, Y, -1.0 * PI / Gdouble (N));
      Ngone (Ctx, X, Y, N);
      Cairo.Move_To (Ctx, X + R_Poly, Y);
      Cairo.Line_To (Ctx, X + 2.0 * R_Poly, Y);
      Cairo.Stroke (Ctx);
      Rotation_Around (Ctx, X, Y, 1.0 * PI / Gdouble (N));

   end NgoneLine;

   ---------------
   -- NgoneBend --
   ---------------

   procedure NgoneBend
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive)
   is
   begin

      Rotation_Around (Ctx, X, Y, -1.0 * PI / Gdouble (N));
      Ngone (Ctx, X, Y, N);
      Cairo.Move_To (Ctx, X + R_Poly, Y);
      Cairo.Line_To (Ctx, X + 2.0 * R_Poly, Y);
      Cairo.Move_To (Ctx, X + 2.0 * R_Poly, Y);
      Cairo.Line_To (Ctx, X + 2.0 * R_Poly, Y + 0.5 * R_Poly);
      Cairo.Stroke (Ctx);
      Rotation_Around (Ctx, X, Y, 1.0 * PI / Gdouble (N));

   end NgoneBend;

   -----------------
   -- PentaSquare --
   -----------------

   procedure PentaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.16;
      Sx  : constant Gdouble := 0.84;
   begin

      Ngone (Ctx, X, Y, 5);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Scaling_Around (Ctx, X + R_Poly, Y, Sx, Sx);
      Ngone (Ctx, X + R_Poly, Y - R_Poly, 4);
      Scaling_Around (Ctx, X + R_Poly, Y, 2.0 - Sx, 2.0 - Sx);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end PentaSquare;

   ----------------
   -- HexaSquare --
   ----------------

   procedure HexaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.26;
      Sx  : constant Gdouble := 0.72;
   begin

      Ngone (Ctx, X, Y, 6);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Scaling_Around (Ctx, X + R_Poly, Y, Sx, Sx);
      Ngone (Ctx, X + R_Poly, Y - R_Poly, 4);
      Scaling_Around (Ctx, X + R_Poly, Y, 2.0 - Sx, 2.0 - Sx);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HexaSquare;

   ----------------
   -- HexaSquare --
   ----------------

   procedure HexaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.74;
      Sx  : constant Gdouble := 0.84;
   begin

      Ngone (Ctx, X, Y, 6);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Scaling_Around (Ctx, X + R_Poly, Y, Sx, Sx);
      Ngone (Ctx, X + 0.68 * R_Poly, Y - 0.95 * R_Poly, 5);
      Scaling_Around (Ctx, X + R_Poly, Y, 2.0 - Sx, 2.0 - Sx);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HexaPenta;

   -----------------
   -- HeptaSquare --
   -----------------

   procedure HeptaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.33;
      Sx  : constant Gdouble := 0.63;
   begin

      Ngone (Ctx, X, Y, 7);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Scaling_Around (Ctx, X + R_Poly, Y, Sx, Sx);
      Ngone (Ctx, X + R_Poly, Y - R_Poly, 4);
      Scaling_Around (Ctx, X + R_Poly, Y, 2.0 - Sx, 2.0 - Sx);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HeptaSquare;

   ----------------
   -- HeptaPenta --
   ----------------

   procedure HeptaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.80;
      Sx  : constant Gdouble := 0.72;
   begin

      Ngone (Ctx, X, Y, 7);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Scaling_Around (Ctx, X + R_Poly, Y, Sx, Sx);
      Ngone (Ctx, X + 0.68 * R_Poly, Y - 0.97 * R_Poly, 5);
      Scaling_Around (Ctx, X + R_Poly, Y, 2.0 - Sx, 2.0 - Sx);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HeptaPenta;

   ---------------
   -- HeptaHexa --
   ---------------

   procedure HeptaHexa (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.80;
      Sx  : constant Gdouble := 0.72;
   begin

      Ngone (Ctx, X, Y, 7);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Scaling_Around (Ctx, X + R_Poly, Y, Sx, Sx);
      Ngone (Ctx, X + 0.5 * R_Poly, Y - 0.85 * R_Poly, 6);
      Scaling_Around (Ctx, X + R_Poly, Y, 2.0 - Sx, 2.0 - Sx);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HeptaHexa;

   ------------------
   -- SquareSquare --
   ------------------

   procedure SquareSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin
      Ngone (Ctx, X, Y, 4);
      Ngone (Ctx, X - R_Poly, Y - R_Poly, 4);
   end SquareSquare;

   ----------------
   -- PentaPenta --
   ----------------

   procedure PentaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin

      Ngone (Ctx, X, Y, 5);
      Rotation_Around (Ctx, X, Y, 2.90 * TWO_PI);
      Ngone (Ctx, X - 1.315 * R_Poly, Y - 0.95 * R_Poly, 5);
      Rotation_Around (Ctx, X, Y, -2.90 * TWO_PI);

   end PentaPenta;

   --------------
   -- HexaHexa --
   --------------

   procedure HexaHexa (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin
      Ngone (Ctx, X, Y, 6);
      Ngone (Ctx, X - 1.5 * R_Poly, Y - 0.87 * R_Poly, 6);
   end HexaHexa;

   ----------------
   -- HeptaHepta --
   ----------------

   procedure HeptaHepta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin
      Ngone (Ctx, X, Y, 7);
      Rotation_Around (Ctx, X, Y, 2.93 * TWO_PI);
      Ngone (Ctx, X - 1.62 * R_Poly, Y - 0.79 * R_Poly, 7);
      Rotation_Around (Ctx, X, Y, -2.93 * TWO_PI);

   end HeptaHepta;

end Draw_Glyphs;
