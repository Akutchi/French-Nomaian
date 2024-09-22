#include <ncurses.h>
#include <menu.h>

char* Choices[] = {
     "Traduire (FR->NO)",
     "Traduire (NO->FR)",
};

int InitScr_Wrp ();

void Refresh_Wrp ();

void Colored_Line (char Line[], short Color, int y);

int Menu (int y);

void EndScr_Wrp ();

// private in regards to Ada

ITEM** Allocate_List (int N, char* choices[]);

void Free (int N, ITEM** List, MENU* menu);
