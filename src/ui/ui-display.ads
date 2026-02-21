--  UI.Display -- Gestion de la fenetre SDL2
--
--  Couche mince au-dessus de SDL2 : initialisation,
--  boucle d'evenements, et fermeture propre.
--  Utilise des imports C directs vers libSDL2.

with Interfaces.C;
with System;

package UI.Display is

   --  Types d'evenements simplifies
   type Event_Kind is
     (Evt_None, Evt_Quit, Evt_Key_Down, Evt_Mouse_Click);

   --  Evenement utilisateur simplifie
   type UI_Event is record
      Kind    : Event_Kind := Evt_None;
      Key     : Interfaces.C.int := 0;
      Mouse_X : Interfaces.C.int := 0;
      Mouse_Y : Interfaces.C.int := 0;
   end record;

   --  Initialise SDL2 et cree la fenetre + renderer.
   procedure Initialize
     (Title  : String;
      Width  : Positive := 1024;
      Height : Positive := 768);

   --  Interroge la file d'evenements SDL2 (non bloquant).
   --  Retourne le prochain evenement ou Evt_None.
   function Poll_Event return UI_Event;

   --  Bascule maximise / restaure (F11 ou bouton titre).
   procedure Toggle_Maximize;

   --  Ferme proprement la fenetre et libere SDL2.
   procedure Shutdown;

   --  Pointeur opaque vers le renderer SDL2.
   --  Utilise par UI.Renderer pour dessiner.
   function Get_Renderer return System.Address;

   --  Dimensions de la fenetre
   function Get_Width return Positive;
   function Get_Height return Positive;

   --  Scancodes SDL2 (publics pour la boucle d'evenements)
   SDL_SCANCODE_F11 : constant Interfaces.C.int := 68;
   SDL_SCANCODE_TAB : constant Interfaces.C.int := 43;

   --  Accesseur vers la police chargee (SDL_ttf).
   --  Retourne Null_Address si la police n'est pas chargee.
   function Get_Font return System.Address;

private

   --  Imports SDL2 via C
   SDL_INIT_VIDEO : constant := 16#00000020#;

   function SDL_Init
     (Flags : Interfaces.C.unsigned)
      return Interfaces.C.int
     with Import, Convention => C,
          External_Name => "SDL_Init";

   procedure SDL_Quit_C
     with Import, Convention => C,
          External_Name => "SDL_Quit";

   function SDL_CreateWindow
     (Title : Interfaces.C.char_array;
      X     : Interfaces.C.int;
      Y     : Interfaces.C.int;
      W     : Interfaces.C.int;
      H     : Interfaces.C.int;
      Flags : Interfaces.C.unsigned)
      return System.Address
     with Import, Convention => C,
          External_Name => "SDL_CreateWindow";

   function SDL_CreateRenderer
     (Window : System.Address;
      Index  : Interfaces.C.int;
      Flags  : Interfaces.C.unsigned)
      return System.Address
     with Import, Convention => C,
          External_Name => "SDL_CreateRenderer";

   procedure SDL_DestroyRenderer
     (Renderer : System.Address)
     with Import, Convention => C,
          External_Name => "SDL_DestroyRenderer";

   procedure SDL_DestroyWindow
     (Window : System.Address)
     with Import, Convention => C,
          External_Name => "SDL_DestroyWindow";

   --  SDL_PollEvent retourne 1 si evenement, 0 sinon
   function SDL_PollEvent
     (Event : System.Address)
      return Interfaces.C.int
     with Import, Convention => C,
          External_Name => "SDL_PollEvent";

   procedure SDL_GetWindowSize
     (Window : System.Address;
      W      : access Interfaces.C.int;
      H      : access Interfaces.C.int)
     with Import, Convention => C,
          External_Name => "SDL_GetWindowSize";

   function SDL_GetWindowFlags
     (Window : System.Address)
      return Interfaces.C.unsigned
     with Import, Convention => C,
          External_Name => "SDL_GetWindowFlags";

   procedure SDL_MaximizeWindow
     (Window : System.Address)
     with Import, Convention => C,
          External_Name => "SDL_MaximizeWindow";

   procedure SDL_RestoreWindow
     (Window : System.Address)
     with Import, Convention => C,
          External_Name => "SDL_RestoreWindow";

   --  SDL_ttf imports
   function TTF_Init return Interfaces.C.int
     with Import, Convention => C,
          External_Name => "TTF_Init";

   procedure TTF_Quit
     with Import, Convention => C,
          External_Name => "TTF_Quit";

   function TTF_OpenFont
     (File : Interfaces.C.char_array;
      Size : Interfaces.C.int)
      return System.Address
     with Import, Convention => C,
          External_Name => "TTF_OpenFont";

   procedure TTF_CloseFont
     (Font : System.Address)
     with Import, Convention => C,
          External_Name => "TTF_CloseFont";

end UI.Display;
