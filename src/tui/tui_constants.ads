with Interfaces.C;

package Tui_Constants is

   package I_C renames Interfaces.C;

   GOLD  : constant I_C.short := 3;
   RED   : constant I_C.short := 1;
   GREEN : constant I_C.short := 2;
   RESET : constant I_C.short := 7;

   L1 : constant Natural := 1;
   L2 : constant Natural := 2;
   L3 : constant Natural := 3;
   L4 : constant Natural := 4;
   L5 : constant Natural := 5;
   L6 : constant Natural := 6;
   L7 : constant Natural := 7;
   L8 : constant Natural := 8;

   SENTENCE : constant I_C.int := 0;
   FILE_STR : constant I_C.int := 1;

end Tui_Constants;
