with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;

with Sentence2Phonems; use Sentence2Phonems;
with Phonems2Glyphs;   use Phonems2Glyphs;

package Tools is

   function To_Spiral_Model
     (Sentence : Unbounded_Wide_String; dict : Cmudict.Map;
      LM       : Language_Model.Map) return Spiral_Model.Tree;

end Tools;
