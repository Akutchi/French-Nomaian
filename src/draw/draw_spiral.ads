with Cairo;
with Glib; use Glib;

with Phonems2Glyphs;

package Draw_Spiral is

   package P2G renames Phonems2Glyphs;

   type Machine_State is record

      theta     : Gdouble;
      Increment : Gdouble;

   end record;

   procedure Draw_Spiral
     (Ctx   : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      state : in out Machine_State);

private

   procedure Draw_Spiral_Element
     (Ctx   : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      state :        Machine_State);

   procedure Draw_CVSN
     (Ctx   : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      state :        Machine_State);

end Draw_Spiral;
