with Ada.Numerics.Generic_Elementary_Functions;

with Math_Constants; use Math_Constants;

package body Draw_Glyphs is

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

   ----------------
   -- Background --
   ----------------

   procedure Background (Ctx : in out Cairo.Cairo_Context; W, H : Gdouble) is
   begin

      Cairo.Set_Source_Rgb (Ctx, 0.98, 0.92, 0.84);
      Cairo.Rectangle (Ctx, 0.0, 0.0, W, H);
      Cairo.Fill (Ctx);
      Cairo.Set_Source_Rgb (Ctx, 0.06, 0.30, 0.55);
      Cairo.Set_Line_Width (Ctx, Line_Width);

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

   procedure Dot
     (Ctx  : in out Cairo.Cairo_Context; X, Y : Gdouble;
      Rdot :        Gdouble := R_Dot)
   is
   begin

      Cairo.Move_To (Ctx, X, Y);
      Cairo.Arc (Ctx, X, Y, Rdot, 0.0, 360.0);
      Cairo.Fill (Ctx);
      Cairo.Stroke (Ctx);

   end Dot;

   ------------------------
   -- Line_Between_Words --
   ------------------------

   procedure Line_Between_Words
     (Ctx : in out Cairo.Cairo_Context; Parent, Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp :        Gdouble)
   is

      Xp_t : Gdouble := Xp;
      Yp_t : Gdouble := Yp;
      Xc_t : Gdouble := Xc;
      Yc_t : Gdouble := Yc;

      dx_Parent, dx_Child : Gdouble := 0.0;
      dy_Parent, dy_Child : Gdouble := 0.0;

   begin

      Get_Element_Displacement_For_Line (Parent, dx_Parent, dy_Parent, after);
      Get_Element_Displacement_For_Line (Child, dx_Child, dy_Child, before);

      Xp_t := Xp_t + dx_Parent;
      Yp_t := Yp_t + dy_Parent;

      Xc_t := Xc_t + dx_Child;
      Yc_t := Yc_t + dy_Child;

      Dot (Ctx, Xp_t, Yp_t);

      Cairo.Move_To (Ctx, Xp_t, Yp_t);
      Cairo.Line_To (Ctx, Xc_t, Yc_t);
      Cairo.Stroke (Ctx);

      Dot (Ctx, Xc_t, Yc_t);

   end Line_Between_Words;

   ----------
   -- Line --
   ----------

   procedure Line (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin

      Dot (Ctx, X, Y);

      Cairo.Move_To (Ctx, X, Y);
      Cairo.Line_To (Ctx, X + Line_Glyph_R_Poly, Y);
      Cairo.Stroke (Ctx);

      Dot (Ctx, X + Line_Glyph_R_Poly, Y);

   end Line;

   ----------
   -- Bend --
   ----------

   procedure Bend (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin

      Rotation_Around (Ctx, X + R_Poly, Y, PI);

      Dot (Ctx, X, Y);

      Cairo.Move_To (Ctx, X, Y);
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

      Cairo.Move_To (Ctx, X, Y);
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
      Increment   : constant Gdouble := TWO_PI / Gdouble (N);

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
      Cairo.Line_To (Ctx, X + 1.9 * R_Poly, Y);
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
      Cairo.Line_To (Ctx, X + 1.9 * R_Poly, Y);
      Cairo.Move_To (Ctx, X + 1.75 * R_Poly, Y);
      Cairo.Line_To (Ctx, X + 1.75 * R_Poly, Y + 0.5 * R_Poly);
      Cairo.Stroke (Ctx);
      Rotation_Around (Ctx, X, Y, 1.0 * PI / Gdouble (N));

   end NgoneBend;

   ----------------
   -- Draw_Ngone --
   ----------------

   procedure Draw_Ngone
     (Ctx : in out Cairo.Cairo_Context; GlyphName : String; X, Y : Gdouble;
      Has_Line, Has_Bend :        Boolean)
   is
      Sides : Positive;

      Sliced_GlyphName : constant String :=
        GlyphName (GlyphName'First .. GlyphName'Last - 4);

      GN_String : constant String :=
        (if Has_Line or else Has_Bend then Sliced_GlyphName else GlyphName);
   begin

      case GlyphRep'Value (GN_String) is
         when square =>
            Sides := 4;
         when penta =>
            Sides := 5;
         when hexa =>
            Sides := 6;
         when hepta =>
            Sides := 7;
         when octa =>
            Sides := 8;
         when others => --  Should not happen
            Sides := 4;
      end case;

      if Has_Line then
         NgoneLine (Ctx, X, Y, Sides);

      elsif Has_Bend then
         NgoneBend (Ctx, X, Y, Sides);

      else
         Ngone (Ctx, X, Y, Sides);
      end if;

   end Draw_Ngone;

   -----------------
   -- PentaSquare --
   -----------------

   procedure PentaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.16;
   begin

      Ngone (Ctx, X, Y, 5);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Ngone (Ctx, X + 1.0 * R_Poly, Y - 0.8 * R_Poly, 4, 0.8 * R_Poly);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end PentaSquare;

   ----------------
   -- HexaSquare --
   ----------------

   procedure HexaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.26;
   begin

      Ngone (Ctx, X, Y, 6);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Ngone (Ctx, X + R_Poly, Y - 0.75 * R_Poly, 4, 0.7 * R_Poly);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HexaSquare;

   ---------------
   -- HexaPenta --
   ---------------

   procedure HexaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.74;
   begin

      Ngone (Ctx, X, Y, 6);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Ngone (Ctx, X + 0.72 * R_Poly, Y - 0.8 * R_Poly, 5, 0.85 * R_Poly);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HexaPenta;

   -----------------
   -- HeptaSquare --
   -----------------

   procedure HeptaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.33;
   begin

      Ngone (Ctx, X, Y, 7);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Ngone (Ctx, X + 1.0 * R_Poly, Y - 0.6 * R_Poly, 4, 0.6 * R_Poly);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HeptaSquare;

   ----------------
   -- HeptaPenta --
   ----------------

   procedure HeptaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.80;
   begin

      Ngone (Ctx, X, Y, 7);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Ngone (Ctx, X + 0.8 * R_Poly, Y - 0.7 * R_Poly, 5, 0.7 * R_Poly);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HeptaPenta;

   ---------------
   -- HeptaHexa --
   ---------------

   procedure HeptaHexa (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 1.1;
   begin

      Ngone (Ctx, X, Y, 7);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Ngone (Ctx, X + 0.6 * R_Poly, Y - 0.8 * R_Poly, 6, 0.9 * R_Poly);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HeptaHexa;

   ---------------
   -- x2_Square --
   ---------------

   procedure x2_Square (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin
      Ngone (Ctx, X, Y, 4);
      Ngone (Ctx, X + R_Poly, Y + R_Poly, 4);
   end x2_Square;

   ----------------
   -- x2_Penta --
   ----------------

   procedure x2_Penta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin

      Ngone (Ctx, X, Y, 5);
      Rotation_Around (Ctx, X, Y, 2.90 * TWO_PI);
      Ngone (Ctx, X - 1.315 * R_Poly, Y - 0.95 * R_Poly, 5);
      Rotation_Around (Ctx, X, Y, -2.90 * TWO_PI);

   end x2_Penta;

   --------------
   -- x2_Hexa --
   --------------

   procedure x2_Hexa (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin
      Ngone (Ctx, X, Y, 6);
      Ngone (Ctx, X - 1.5 * R_Poly, Y - 0.87 * R_Poly, 6);
   end x2_Hexa;

   ----------------
   -- x2_Hepta --
   ----------------

   procedure x2_Hepta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin
      Ngone (Ctx, X, Y, 7);
      Rotation_Around (Ctx, X, Y, 2.93 * TWO_PI);
      Ngone (Ctx, X - 1.62 * R_Poly, Y - 0.79 * R_Poly, 7);
      Rotation_Around (Ctx, X, Y, -2.93 * TWO_PI);

   end x2_Hepta;

   ------------------
   -- Choose_Glyph --
   ------------------

   procedure Choose_Glyph
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; GN_String : String)
   is
   begin

      case GlyphRep'Value (GN_String) is

         when dot_start =>
            Dot (Ctx, X, Y);

         when line =>
            Line (Ctx, X, Y);

         when bend =>
            Bend (Ctx, X, Y);

         when linedotline =>
            Word_Separator (Ctx, X, Y);

         when square | penta | hexa | hepta | octa =>
            Draw_Ngone (Ctx, GN_String, X, Y, False, False);

         when squareline | pentaline | hexaline | heptaline | octaline =>
            Draw_Ngone (Ctx, GN_String, X, Y, True, False);

         when squarebend | pentabend | hexabend | heptabend | octabend =>
            Draw_Ngone (Ctx, GN_String, X, Y, False, True);

         when squaresquare =>
            x2_Square (Ctx, X, Y);

         when pentapenta =>
            x2_Penta (Ctx, X, Y);

         when hexahexa =>
            x2_Hexa (Ctx, X, Y);

         when heptahepta =>
            x2_Penta (Ctx, X, Y);

         when pentasquare =>
            PentaSquare (Ctx, X, Y);

         when hexasquare =>
            HexaSquare (Ctx, X, Y);

         when hexapenta =>
            HexaPenta (Ctx, X, Y);

         when heptasquare =>
            HeptaSquare (Ctx, X, Y);

         when heptapenta =>
            HeptaPenta (Ctx, X, Y);

         when heptahexa =>
            HeptaHexa (Ctx, X, Y);

      end case;

   end Choose_Glyph;

end Draw_Glyphs;
