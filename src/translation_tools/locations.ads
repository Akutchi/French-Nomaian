package Locations is

   DEBUG : Boolean := False;

   Base : constant String :=
     (if DEBUG then "/home/akutchi/Desktop/Ada_programs/french_nomaian/"
      else "../");

   Cmudict_Location : constant String := Base & "src/cmudict_fr.txt";
   LM_Location      : constant String := Base & "src/lm.txt";
   SVG_FILE         : constant String := Base & "result/nomai_sentence.svg";

end Locations;
