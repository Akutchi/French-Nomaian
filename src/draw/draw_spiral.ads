with Cairo;
with Glib; use Glib;

with Phonems2Glyphs;

package Draw_Spiral is

   package P2G renames Phonems2Glyphs;

   type Machine_State is record

      Xv : Gdouble := 0.0;
      Xn : Gdouble := 0.0;
   end record;

   procedure Background (Ctx : in out Cairo.Cairo_Context);

   procedure Draw_Unrolled_Spiral
     (Ctx  : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      X, Y :        Gdouble; state : in out Machine_State);

private

   procedure Draw_Spiral_Element
     (Ctx  : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      X, Y :        Gdouble);

   procedure Update_Child_Coordinates
     (Root   : P2G.Spiral_Model.Cursor; Xc, Yc : in out Gdouble;
      Xp, Yp : Gdouble; state : Machine_State);

   procedure Restore_To_Parent_Coordinates_If_CS
     (Root, Child : P2G.Spiral_Model.Cursor; Xc, Yc : in out Gdouble;
      Xp, Yp      : Gdouble; state : Machine_State);

   procedure Draw_CVSN
     (Ctx    : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      Xp, Yp :        Gdouble; state : in out Machine_State);

end Draw_Spiral;
