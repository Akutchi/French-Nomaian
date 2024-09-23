#include <ncurses.h>
#include <menu.h>

#define INVISIBLE 0
#define VISIBLE 1
#define ENTER_CODE 10
#define SENTENCE 0
#define FILE_STR 1

char* Choices[] = {
   "Traduire (FR->NO)",
   "Traduire (NO->FR)",
   "Quitter"
};

int InitScr_Wrp ();

void Refresh_Wrp ();

void Colored_Line (char Line[], short Color, int y);

int Menu (int y);

wint_t* Get (int type, int y);
// the wint_t errors (underlined in an IDE) are IDE's specific error. Program
// compile.

void EndScr_Wrp ();

// private in regards to Ada

ITEM** Allocate_List (int N, char* choices[]);

void Free (int N, ITEM** List, MENU* menu);
