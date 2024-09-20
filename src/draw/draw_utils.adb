with Ada.Strings.Unbounded;

with Draw_Constants; use Draw_Constants;

package body Draw_Utils is

   package S_U renames Ada.Strings.Unbounded;

   -----------
   -- Is_CX --
   -----------

   function Is_CX (Parent, Child : P2G.GlyphInfo; X : Character) return Boolean
   is
   begin
      return Parent.T = 'c' and then Child.T = X;
   end Is_CX;

   -----------
   -- Is_SX --
   -----------

   function Is_SX (Parent, Child : P2G.GlyphInfo; X : Character) return Boolean
   is
   begin
      return Parent.T = 's' and then Child.T = X;
   end Is_SX;

   -----------
   -- Is_DX --
   -----------

   function Is_DX (Parent, Child : P2G.GlyphInfo; X : Character) return Boolean
   is
   begin
      return Parent.T = P2G.Starting_Dot and then Child.T = X;
   end Is_DX;

   -------------
   -- Is_CS_V --
   -------------

   function Is_CS_V (Parent, Child : P2G.GlyphInfo) return Boolean is
   begin
      return
        Is_CX (Parent, Child, P2G.Vowel)
        or else Is_SX (Parent, Child, P2G.Vowel);
   end Is_CS_V;

   -------------
   -- Is_CS_N --
   -------------

   function Is_CS_N (Parent, Child : P2G.GlyphInfo) return Boolean is
   begin
      return
        Is_CX (Parent, Child, P2G.Numeral)
        or else Is_SX (Parent, Child, P2G.Numeral);
   end Is_CS_N;

   -------------------------------
   -- Need_Line_Between_Phonems --
   -------------------------------

   function Need_Line_Between_Phonems
     (Root, Child : P2G.Spiral_Model.Cursor; Is_Spiral : Boolean := False)
      return Boolean
   is
      Root_GlyphName : constant String :=
        S_U.To_String (P2G.Spiral_Model.Element (Root).GlyphName);

      Is_Start_Word : constant Boolean :=
        GlyphRep'Value (Root_GlyphName) = linedotline;

      Is_End_Branch : constant Boolean := P2G.Spiral_Model.Is_Leaf (Root);

   begin

      if not Is_End_Branch then

         declare

            E_Root : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);

            E_Child : constant P2G.GlyphInfo :=
              P2G.Spiral_Model.Element (Child);

            Child_GlyphName : constant String :=
              S_U.To_String (E_Child.GlyphName);

            Is_End_Word : constant Boolean :=
              GlyphRep'Value (Child_GlyphName) = linedotline;

         begin

            if Is_Spiral then
               return True;
            end if;

            return
              not
              (Is_Start_Word or else Is_End_Word
               or else Is_CS_V (E_Root, E_Child)
               or else Is_CS_N (E_Root, E_Child));
         end;
      end if;

      return False;

   end Need_Line_Between_Phonems;

end Draw_Utils;
