with Ada.Strings.Wide_Unbounded;

with Sentence2Phonems;
with Phonems2Glyphs;

package Tools is

   package S_WU renames Ada.Strings.Wide_Unbounded;
   package S2P renames Sentence2Phonems;
   package P2G renames Phonems2Glyphs;

   procedure Create_Linear_SVG
     (Sentence : S_WU.Unbounded_Wide_String; dict : S2P.Cmudict.Map;
      LM       : P2G.Language_Model.Map);

   procedure Create_Spiral_SVG
     (Sentence : S_WU.Unbounded_Wide_String; dict : S2P.Cmudict.Map;
      LM       : P2G.Language_Model.Map);

private

   function To_Spiral_Model
     (Sentence : S_WU.Unbounded_Wide_String; dict : S2P.Cmudict.Map;
      LM       : P2G.Language_Model.Map) return P2G.Spiral_Model.Tree;

end Tools;
