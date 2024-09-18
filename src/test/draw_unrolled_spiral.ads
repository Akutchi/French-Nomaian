with Cairo;
with Glib; use Glib;

with Phonems2Glyphs;

package Draw_Unrolled_Spiral is

   package P2G renames Phonems2Glyphs;

   type Machine_State is record

      Xc, Yc : Gdouble := 0.0;

      Xv : Gdouble := 0.0;
      Xn : Gdouble := 0.0;

   end record;

   procedure Draw_Unrolled_Spiral
     (Ctx  : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      X, Y :        Gdouble; state : in out Machine_State);

private

   procedure Draw_Spiral_Element
     (Ctx  : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      X, Y :        Gdouble);

   procedure Draw_CVSN
     (Ctx    : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      Xp, Yp :        Gdouble; state : in out Machine_State);

   procedure Update_Branch_Coordinates
     (Root, Child :        P2G.Spiral_Model.Cursor; Xp, Yp : Gdouble;
      state       : in out Machine_State);

   procedure Restore_To_Parent_Coordinates_If_CS
     (Root, Child :        P2G.Spiral_Model.Cursor; Xp, Yp : Gdouble;
      state       : in out Machine_State);

end Draw_Unrolled_Spiral;
