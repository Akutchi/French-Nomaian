with Ada.Strings.Unbounded;
with Ada.Numerics.Generic_Elementary_Functions;

with Draw_Glyphs;

with Math_Constants; use Math_Constants;
with Math;           use Math;

package body Draw_Spiral_Utils is

   package S_U renames Ada.Strings.Unbounded;

   package DG renames Draw_Glyphs;

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

   -----------------------
   -- Draw_Vector_Field --
   -----------------------

   procedure Draw_Vector_Field
     (Ctx : Cairo.Cairo_Context; X, Y, I, N, radius_var, theta_var : Gdouble)
   is

      Grad, Tan_v : gradient;

   begin

      Grad := Calculate_Gradient (I, N);

      Grad  := (Grad.dr, Grad.dtheta);
      Tan_v := (Grad.dr, Grad.dtheta + PI_2);

      Cairo.Move_To (Ctx, X, Y);
      Cairo.Line_To
        (Ctx, X + 0.1 * (radius_var + Grad.dr) * Cos (theta_var - Grad.dtheta),
         Y - 0.1 * (radius_var + Grad.dr) * Sin (theta_var - Grad.dtheta));

      Cairo.Move_To (Ctx, X, Y);
      Cairo.Line_To
        (Ctx,
         X + 0.1 * (radius_var + Tan_v.dr) * Cos (theta_var - Tan_v.dtheta),
         Y - 0.1 * (radius_var + Tan_v.dr) * Sin (theta_var - Tan_v.dtheta));

      Cairo.Move_To (Ctx, X, Y);
      Cairo.Line_To (Ctx, X + 1.0, Y);
      Cairo.Stroke (Ctx);

   end Draw_Vector_Field;

   ----------------------------------------
   --  Get_Element_Displacement_For_Line --
   ----------------------------------------

   procedure Get_Element_Displacement_For_Line
     (Element : P2G.GlyphInfo; dx_e, dy_e : in out Gdouble; dp : dpos_Type)
   is
   begin

      dx_e := 0.0;
      dy_e := 0.0;

      case GlyphRep'Value (S_U.To_String (Element.GlyphName)) is

         when line =>

            if dp = after then
               dx_e := Line_Glyph_R_Poly;
            end if;

         when bend =>

            if dp = before then
               dx_e := R_Poly_2;
               dy_e := -R_Poly;
            else
               dx_e := 3.0 * R_Poly;
               dy_e := 0.0;
            end if;

         when linedotline =>

            if dp = after then
               dx_e := 2.0 * R_Poly;
               dy_e := 0.4 * R_Poly;
            end if;

         when square | hexa | octa =>

            dx_e := -R_Poly;
            if dp = after then
               dx_e := -dx_e;
            end if;

         when squareline | squarebend =>

            dx_e := R_Poly * Cos (PI_4);
            dy_e := R_Poly * Sin (PI_4);

            if dp = before then
               dx_e := -dx_e;
            end if;

         when squaresquare =>

            if dp = before then
               dx_e := -R_Poly;

            elsif dp = after then
               dx_e := dx_e + 2.0 * R_Poly;
               dy_e := R_Poly;

            end if;

         when penta | pentasquare =>

            dx_e := R_Poly;

            if dp = before then
               dx_e := R_Poly * Cos (4.0 * PI_5);
               dy_e := R_Poly * Sin (4.0 * PI_5);
            end if;

         when pentaline | pentabend =>

            if dp = before then
               dx_e := -R_Poly;
            else
               dx_e := R_Poly * Cos (PI_5);
               dy_e := R_Poly * Sin (PI_5);
            end if;

         when pentapenta =>

            if dp = before then
               dx_e := -2.5 * R_Poly;
            else
               dx_e := R_Poly;
            end if;

         when hexaline | hexabend =>

            dx_e := R_Poly * Cos (5.0 * PI_6);
            dy_e := R_Poly * Sin (5.0 * PI_6);

            if dp = after then
               dx_e := -dx_e;
            end if;

         when hexasquare =>

            dx_e := -R_Poly;
            if dp = after then
               dx_e := R_Poly * (1.0 + Cos (PI_6));
               dy_e := -R_Poly * Sin (PI_6);
            end if;

         when hexapenta =>

            dx_e := -R_Poly;
            if dp = after then
               dx_e := R_Poly * (1.0 + Cos (PI_15));
               dy_e := -R_Poly * Sin (PI_15);
            end if;

         when hexahexa =>

            if dp = before then
               dx_e := -R_Poly * (2.0 + Cos (-PI_3));
               dy_e := -R_Poly * Sin (PI_3);
            end if;

         when hepta =>

            dx_e := R_Poly;
            if dp = before then
               dx_e := R_Poly * Cos (6.0 * PI_7);
               dy_e := R_Poly * Sin (6.0 * PI_7);
            end if;

         when heptaline | heptabend =>

            if dp = before then
               dx_e := -R_Poly;
            else
               dx_e := R_Poly * Cos (5.0 * PI_7);
               dy_e := R_Poly * Sin (5.0 * PI_7);

            end if;

         when octaline =>

            dx_e := R_Poly * Cos (5.0 * PI_6);
            dy_e := R_Poly * Sin (5.0 * PI_6);

            if dp = after then
               dx_e := -dx_e;
            end if;

         when others =>
            dx_e := 0.0;
            dy_e := 0.0;

      end case;

   end Get_Element_Displacement_For_Line;

   ------------------------
   -- Line_Between_Words --
   ------------------------

   procedure Line_Between_Words
     (Ctx : in out Cairo.Cairo_Context; Parent, Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp :        Gdouble)
   is
   begin
      null;
   end Line_Between_Words;

   ---------------------------
   -- Draw_With_Coordinates --
   ---------------------------

   procedure Draw_With_Coordinates
     (I, N :        Gdouble; Line_Info : in out LineInfo;
      Ctx  : in out Cairo.Cairo_Context)
   is

      dx_Parent, dx_Child : Gdouble := 0.0;
      dy_Parent, dy_Child : Gdouble := 0.0;

      Sx : constant Gdouble := Linearize_Scale (I, N);

   begin

      Get_Element_Displacement_For_Line
        (Line_Info.Parent, dx_Parent, dy_Parent, after);
      Get_Element_Displacement_For_Line
        (Line_Info.Child, dx_Child, dy_Child, before);

      Line_Info.Xp := Line_Info.Xp + dx_Parent;
      Line_Info.Yp := Line_Info.Yp + dy_Parent;

      Line_Info.Xc := Line_Info.Xc + dx_Child;
      Line_Info.Yc := Line_Info.Yc + dy_Child;

      DG.Scaling_Around (Ctx, Line_Info.Xp, Line_Info.Yp, Sx, Sx);

      DG.Dot (Ctx, Line_Info.Xp, Line_Info.Yp);

      Cairo.Move_To (Ctx, Line_Info.Xp, Line_Info.Yp);
      Cairo.Line_To (Ctx, Line_Info.Xc, Line_Info.Yc);
      Cairo.Stroke (Ctx);

      DG.Dot (Ctx, Line_Info.Xc, Line_Info.Yc);

      DG.Scaling_Around (Ctx, Line_Info.Xp, Line_Info.Yp, 1.0 / Sx, 1.0 / Sx);

   end Draw_With_Coordinates;

end Draw_Spiral_Utils;
