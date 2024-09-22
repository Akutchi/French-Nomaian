#include <ncurses_external.h>
#include <stdlib.h>
#include <string.h>

int InitScr_Wrp () {
   initscr ();

   if (has_colors () == FALSE) {
      endwin ();
      return -1;
   }
   cbreak ();
   noecho ();
   keypad (stdscr, TRUE);
   start_color ();

   return 0;
}

void Refresh_Wrp () {
   refresh ();
}

void Colored_Line (char Line[], short Color, int y) {

   init_pair (y, Color, COLOR_BLACK);
   attron (COLOR_PAIR (y));
   mvwprintw (stdscr, y - 1, 0, "%s", Line);
   attroff (COLOR_PAIR (y));
}


ITEM** Allocate_List (int N, char* choices[]) {

   ITEM** List;

   List = (ITEM**)calloc (N + 1, sizeof (ITEM*));
   for (int i = 0; i < N; ++i) {
      List[i] = new_item (choices[i], "");
   }

   return List;
}

void Free (int N, ITEM** List, MENU* menu) {

   unpost_menu (menu);
   free_menu (menu);
   for (int i = 0; i < N; i++) {
      free_item (List[i]);
   }
}

int Menu (int y) {

   ITEM** Item_List;

   WINDOW* menu_win;
   MENU* Option_Menu;

   menu_win = newwin (16, 50, y, 0);
   keypad (menu_win, TRUE);

   int N = sizeof (Choices) / sizeof (Choices[0]);
   Item_List = Allocate_List (N, Choices);

   Option_Menu = new_menu (Item_List);

   mvprintw (16, 0, "q pour quitter");
   set_menu_win (Option_Menu, menu_win);
   set_menu_sub (Option_Menu, menu_win);
   post_menu (Option_Menu);

   wrefresh (menu_win);

   int character;
   int Choosen_Option = 0;
   bool Has_Choosen = FALSE;

   menu_driver (Option_Menu, REQ_FIRST_ITEM);
   while ((character = getch ()) != 'q') {

      switch (character) {

      case KEY_UP:
         Choosen_Option = Choosen_Option > 0 ? Choosen_Option - 1 : 0;
         menu_driver (Option_Menu, REQ_PREV_ITEM);

      case KEY_DOWN:
         Choosen_Option = (Choosen_Option + 1) % N;
         menu_driver (Option_Menu, REQ_NEXT_ITEM);


      case KEY_ENTER:
         Has_Choosen = TRUE;
         break;

      }
   }

   Free (N, Item_List, Option_Menu);

   return Has_Choosen ? Choosen_Option : -1;

}

void EndScr_Wrp () {
   endwin ();
}
