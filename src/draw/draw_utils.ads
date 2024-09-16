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

   Phi : constant Gdouble := (1.0 + Sqrt (5.0)) / 2.0;

   e_d : constant Gdouble := Gdouble (Ada.Numerics.e);

   PI     : constant Gdouble := Gdouble (Ada.Numerics.Pi);
   TWO_PI : constant Gdouble := 2.0 * PI;
   PI_2   : constant Gdouble := PI / 2.0;
   PI_3   : constant Gdouble := PI / 3.0;
   PI_4   : constant Gdouble := PI / 4.0;
   PI_5   : constant Gdouble := PI / 5.0;
   PI_6   : constant Gdouble := PI / 6.0;
   PI_7   : constant Gdouble := PI / 7.0;

   R_Poly     : constant Gdouble := 0.5;
   Line_Width : constant Gdouble := 0.1 * R_Poly;
   R_Dot      : constant Gdouble := 0.2 * R_Poly;

   R_Poly_2          : constant Gdouble := R_Poly / 2.0;
   Line_Glyph_R_Poly : constant Gdouble := 1.3 * R_Poly;
   Line_Words_R_Poly : constant Gdouble := 2.0 * R_Poly;

   Offset_Leaf   : constant Gdouble := 0.5 * R_Poly;
   Offset_Branch : constant Gdouble := 1.5 * R_Poly;

   dy_vn : constant Gdouble := 5.0;

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

   procedure Draw_Branch
     (Ctx : Cairo.Cairo_Context; Parent : P2G.GlyphInfo; Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp : Gdouble);

private

   procedure Get_Displacement_For_Branch
     (Element              : P2G.GlyphInfo; dx_e, dy_e : in out Gdouble;
      Is_Vowel, Is_Numeral : Boolean);

end Draw_Utils;
