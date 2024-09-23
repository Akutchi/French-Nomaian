#include "ncurses_external.h"
#include <ncursesw/ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>

#define NCURSES_WIDECHAR 1

int InitScr_Wrp () {

   setlocale (LC_ALL, "");

   initscr ();

   if (has_colors () == FALSE) {
      endwin ();
      return -1;
   }

   curs_set (INVISIBLE);
   cbreak ();
   noecho ();
   start_color ();
   keypad (stdscr, TRUE);

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

   set_menu_win (Option_Menu, menu_win);
   set_menu_sub (Option_Menu, menu_win);
   post_menu (Option_Menu);

   wrefresh (menu_win);

   int character;
   int Choosen_Option = 0;

   menu_driver (Option_Menu, REQ_FIRST_ITEM);
   do {

      character = wgetch (menu_win);

      switch (character) {

      case KEY_UP:
         Choosen_Option = Choosen_Option > 0 ? Choosen_Option - 1 : 0;
         menu_driver (Option_Menu, REQ_UP_ITEM);
         break;

      case KEY_DOWN:
         Choosen_Option = Choosen_Option < N ? Choosen_Option + 1 : N;
         menu_driver (Option_Menu, REQ_DOWN_ITEM);
         break;
      }

   } while (character != ENTER_CODE);

   Free (N, Item_List, Option_Menu);
   wclear (menu_win);
   wrefresh (menu_win);

   delwin (menu_win);

   return Choosen_Option;

}

wint_t* Get (int type, int y) {

   if (type == SENTENCE) {
      mvwprintw (stdscr, y, 0, "Entrer une phrase à traduire :");
   }
   else {
      mvwprintw (stdscr, y, 0, "Entrer un fichier à décoder :");
   }

   mvwprintw (stdscr, y + 1, 0, "> ");
   wrefresh (stdscr);
   curs_set (VISIBLE);
   echo ();

   wint_t* response = NULL;
   size_t N = 0;
   size_t curr_end_size = 255;
   size_t Block = 16;

   response = (wint_t*)realloc (NULL, sizeof (*response) * curr_end_size);

   if (!response) return response;

   wint_t character;
   do {

      get_wch (&character);

      response[N] = character;
      N++;

      if (N == curr_end_size) {
         curr_end_size += Block;
         response = (wint_t*)realloc (response, sizeof (*response) * curr_end_size);
         if (!response) return response;
      }

   } while (character != ENTER_CODE);

   return response;

}

void EndScr_Wrp () {
   endwin ();
}

