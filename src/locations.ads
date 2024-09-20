package Locations is

   DEBUG : Boolean := False;

   Base : constant String := "../src/resources/";

   Cmudict_Location : constant String := Base & "cmudict_fr.txt";
   LM_Location      : constant String := Base & "lm.txt";
   Title_Location   : constant String := Base & "title.txt";

   SVG_FILE : constant String := "../result/nomai_sentence.svg";

end Locations;
