with Ada.Strings.Unbounded;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with Cairo;
with Glib; use Glib;

with Phonems2Glyphs;

package Draw_Utils is

   package S_U renames Ada.Strings.Unbounded;

   package P2G renames Phonems2Glyphs;

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

   type GlyphRep is
     (dot_start, line, bend, square, penta, hexa, hepta, octa, squareline,
      pentaline, hexaline, heptaline, octaline, squarebend, pentabend,
      hexabend, heptabend, octabend, squaresquare, pentapenta, hexahexa,
      heptahepta, pentasquare, hexasquare, hexapenta, heptasquare, heptapenta,
      heptahexa, linedotline);

   type dpos_Type is (before, after);

   R_Poly     : constant Gdouble := 0.5;
   Line_Width : constant Gdouble := 0.1 * R_Poly;
   R_Dot      : constant Gdouble := 0.2 * R_Poly;

   R_Poly_2          : constant Gdouble := R_Poly / 2.0;
   Line_Glyph_R_Poly : constant Gdouble := 1.3 * R_Poly;
   Line_Words_R_Poly : constant Gdouble := 2.0 * R_Poly;

   Offset_Leaf   : constant Gdouble := 0.5 * R_Poly;
   Offset_Branch : constant Gdouble := 1.5 * R_Poly;

   dy_vn : constant Gdouble := 3.0;

   function Is_CX
     (Parent, Child : P2G.GlyphInfo; X : Character) return Boolean;

   function Is_SX
     (Parent, Child : P2G.GlyphInfo; X : Character) return Boolean;

   function Is_DX
     (Parent, Child : P2G.GlyphInfo; X : Character) return Boolean;

   function Is_CS_V (Parent, Child : P2G.GlyphInfo) return Boolean;

   function Is_CS_N (Parent, Child : P2G.GlyphInfo) return Boolean;

   function Is_Start_Dot (E : P2G.GlyphInfo) return Boolean;

   function dx
     (GlyphName : S_U.Unbounded_String; dp : dpos_Type) return Gdouble;

   function dy
     (GlyphName : S_U.Unbounded_String; dp : dpos_Type) return Gdouble;

   function Offset (Element : P2G.GlyphInfo) return Gdouble;

   function Need_Line_Between_Phonems
     (Root, Child : P2G.Spiral_Model.Cursor) return Boolean;

   procedure Get_Displacement_For_Line
     (Element : P2G.GlyphInfo; dx_e, dy_e : in out Gdouble; dp : dpos_Type);
   --  Turn in the anti-trigonometric sense.
   --  Some values were not implemented because not necessary at the time of
   --  writing (17/09/24)

   procedure Draw_Branch
     (Ctx : Cairo.Cairo_Context; Parent : P2G.GlyphInfo; Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp : Gdouble);
   --  Also turn in the anti-trigonometric sense.

private

   procedure Get_Displacement_For_Branch
     (Element              : P2G.GlyphInfo; dx_e, dy_e : in out Gdouble;
      Is_Vowel, Is_Numeral : Boolean);

end Draw_Utils;
