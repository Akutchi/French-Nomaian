with Cairo;
with Glib; use Glib;

with Phonems2Glyphs;

with Draw_Utils; use Draw_Utils;

package Draw_Glyphs is

   package P2G renames Phonems2Glyphs;

   type Machine_State is record

      Xv : Gdouble := 0.0;
      Xn : Gdouble := 0.0;
   end record;

   procedure Background (Ctx : Cairo.Cairo_Context);

   procedure Rotation_Around
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; Angle : Gdouble);

   procedure Scaling_Around
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; Sx, Sy : Gdouble);

   procedure Dot (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure Line_Between_Words
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure Line (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure Bend (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure Word_Separator (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure Ngone
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive;
      r   :        Gdouble := R_Poly) with
     Pre => N > 3;

   procedure NgoneLine
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive) with
     Pre => N > 3;

   procedure NgoneBend
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive) with
     Pre => N > 3;

   procedure PentaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HexaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HexaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HeptaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HeptaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HeptaHexa (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure x2_Square (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure x2_Penta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure x2_Hexa (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure x2_Hepta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure Draw_Unrolled_Spiral
     (Ctx   : in out Cairo.Cairo_Context; X, Y : Gdouble;
      state : in out Machine_State; Root : P2G.Spiral_Model.Cursor);

private

   procedure Draw_Ngone
     (Ctx : in out Cairo.Cairo_Context; GlyphName : String; X, Y : Gdouble;
      Has_Line, Has_Bend :        Boolean);

   procedure Draw_Spiral_Element
     (Ctx  : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      X, Y :        Gdouble);

   procedure Update_Child_Coordinates
     (Root   : P2G.Spiral_Model.Cursor; Xc, Yc : in out Gdouble;
      Xp, Yp : Gdouble);

   procedure Restore_To_Parent_Coordinates
     (Root   : P2G.Spiral_Model.Cursor; Xc, Yc : in out Gdouble;
      Xp, Yp : Gdouble);

   procedure Draw_CVSN
     (Ctx    : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      Xp, Yp :        Gdouble; state : in out Machine_State);

end Draw_Glyphs;
