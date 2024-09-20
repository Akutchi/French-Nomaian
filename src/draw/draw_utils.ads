with Phonems2Glyphs;

package Draw_Utils is

   package P2G renames Phonems2Glyphs;

   function Is_CX
     (Parent, Child : P2G.GlyphInfo; X : Character) return Boolean;

   function Is_SX
     (Parent, Child : P2G.GlyphInfo; X : Character) return Boolean;

   function Is_DX
     (Parent, Child : P2G.GlyphInfo; X : Character) return Boolean;

   function Is_CS_V (Parent, Child : P2G.GlyphInfo) return Boolean;

   function Is_CS_N (Parent, Child : P2G.GlyphInfo) return Boolean;

   function Need_Line_Between_Phonems
     (Root, Child : P2G.Spiral_Model.Cursor; Is_Spiral : Boolean := False)
      return Boolean;

end Draw_Utils;
