with Sentence2Phonems;
with Phonems2Glyphs;
with Tools;
with Tui;

procedure French_Nomaian is

   package S2P renames Sentence2Phonems;
   package P2G renames Phonems2Glyphs;

   dict : S2P.Cmudict.Map;
   LM   : P2G.Language_Model.Map;

   Was_Initialized : Boolean;
   Current_Y       : Integer;
   Response        : Tui.Choice_State;

begin

   S2P.Init_Cmudict (dict);
   P2G.Init_Language_Model (LM);
   Was_Initialized := Tui.Init_Curses;

   if Was_Initialized then
      Current_Y := Tui.Print_Title;
      Response  := Tui.Propose (Current_Y);
   end if;

   if not Response.Quit then
      Tools.Create_Spiral_SVG (Response.Sentence, dict, LM);
   end if;

end French_Nomaian;
