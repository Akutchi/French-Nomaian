#include <ncurses.h>
#include <menu.h>

#define INVISIBLE 0
#define ENTER_CODE 10

char* Choices[] = {
   "Traduire (FR->NO)",
   "Traduire (NO->FR)",
   "Quitter"
};

int InitScr_Wrp ();

void Refresh_Wrp ();

void Colored_Line (char Line[], short Color, int y);

int Menu (int y);

char* Enter_Sentence ();

void EndScr_Wrp ();

// private in regards to Ada

ITEM** Allocate_List (int N, char* choices[]);

void Free (int N, ITEM** List, MENU* menu);
