with Ada.Strings.Unbounded;
with Ada.Numerics.Generic_Elementary_Functions;

with Glib; use Glib;

with Math_Constants; use Math_Constants;

package body Draw_Spiral_Utils is

   package S_U renames Ada.Strings.Unbounded;

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

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

   procedure Line_Between_Words
     (Ctx : in out Cairo.Cairo_Context; Parent, Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp :        Gdouble)
   is
   begin
      null;
   end Line_Between_Words;

   procedure Get_Displacement_For_Branch
     (Element              : P2G.GlyphInfo; dx_e, dy_e : in out Gdouble;
      Is_Vowel, Is_Numeral : Boolean)
   is
   begin
      null;
   end Get_Displacement_For_Branch;

   procedure Draw_Branch
     (Ctx : Cairo.Cairo_Context; Parent : P2G.GlyphInfo; Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp : Gdouble)
   is
   begin
      null;
   end Draw_Branch;

end Draw_Spiral_Utils;
