package body Tools is

   function To_Spiral_Model
     (Sentence : Unbounded_Wide_String; dict : Cmudict.Map;
      LM       : Language_Model.Map) return Spiral_Model.Tree
   is

      Phonems_Version : constant Unbounded_Wide_String :=
        Null_Unbounded_Wide_String;

   begin

      declare
         Phonems : constant Wide_String :=
           Simplify (To_Phonems (Sentence, Phonems_Version, dict));

         Glyphs : constant List_GlyphInfo.Vector := To_Glyphs (Phonems, LM);

         Spiral : Spiral_Model.Tree := Spiral_Model.Empty_Tree;

      begin

         Construct (Spiral, Glyphs);
         return Spiral;
      end;

   end To_Spiral_Model;

end Tools;
