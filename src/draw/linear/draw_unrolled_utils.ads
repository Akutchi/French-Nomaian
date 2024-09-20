with Ada.Strings.Unbounded;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with Cairo;
with Glib; use Glib;

with Phonems2Glyphs;

with Draw_Constants; use Draw_Constants;

package Draw_Unrolled_Utils is

   package S_U renames Ada.Strings.Unbounded;

   package P2G renames Phonems2Glyphs;

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

   function Is_Start_Dot (E : P2G.GlyphInfo) return Boolean;

   function dx_For_Word_Separator
     (GlyphName : S_U.Unbounded_String; dp : dpos_Type) return Gdouble;

   function dy_For_Word_Separator
     (GlyphName : S_U.Unbounded_String; dp : dpos_Type) return Gdouble;
   --  I would only need one displacement function for lines if I allowed
   --  to draw lines between glyphs and word separators. However, I don't
   --  don't feel like there's a need for that*, thus I need another function
   --  to handle C-> S / S -> S and S -> C displacements.
   --
   --  *It's kinda ugly and useless.

   function Branch_Offset (Element : P2G.GlyphInfo) return Gdouble;

   procedure Get_Element_Displacement_For_Line
     (Element : P2G.GlyphInfo; dx_e, dy_e : in out Gdouble; dp : dpos_Type);
   --  Turn in the anti-trigonometric sense.
   --  Some values were not implemented because not necessary at the time of
   --  writing (17/09/24)

   procedure Line_Between_Words
     (Ctx : in out Cairo.Cairo_Context; Parent, Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp :        Gdouble);

   procedure Get_Displacement_For_Branch
     (Element              : P2G.GlyphInfo; dx_e, dy_e : in out Gdouble;
      Is_Vowel, Is_Numeral : Boolean);

   procedure Draw_Branch
     (Ctx : Cairo.Cairo_Context; Parent : P2G.GlyphInfo; Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp : Gdouble);
   --  Also turn in the anti-trigonometric sense.

end Draw_Unrolled_Utils;
