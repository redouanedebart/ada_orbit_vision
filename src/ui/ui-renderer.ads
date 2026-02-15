--  UI.Renderer -- Dessin sur le renderer SDL2
--
--  Trace les satellites, la texture terrestre et les
--  alertes de collision sur la surface SDL2.

with System;
with Interfaces.C;
with Concurrency.Ground_Station;

package UI.Renderer is

   --  Couleur RGBA
   type Colour is record
      R : Interfaces.C.unsigned_char := 0;
      G : Interfaces.C.unsigned_char := 0;
      B : Interfaces.C.unsigned_char := 0;
      A : Interfaces.C.unsigned_char := 255;
   end record;

   --  Efface le renderer avec une couleur de fond.
   procedure Clear
     (Rend : System.Address;
      BG   : Colour);

   --  Dessine tous les satellites depuis le Ground_Station.
   --  Mappe lat/lon vers coordonnees ecran.
   procedure Draw_Satellites
     (Rend      : System.Address;
      Snapshots : Concurrency.Ground_Station.Snapshot_Maps.Map;
      W         : Positive;
      H         : Positive);

   --  Dessine un bandeau d'alerte rouge en haut de l'ecran.
   procedure Draw_Alert_Overlay
     (Rend : System.Address;
      W    : Positive);

   --  Presente le buffer a l'ecran (flip).
   procedure Present (Rend : System.Address);

private

   --  Imports SDL2 bas niveau
   function SDL_SetRenderDrawColor
     (Renderer : System.Address;
      R        : Interfaces.C.unsigned_char;
      G        : Interfaces.C.unsigned_char;
      B        : Interfaces.C.unsigned_char;
      A        : Interfaces.C.unsigned_char)
      return Interfaces.C.int
     with Import, Convention => C,
          External_Name => "SDL_SetRenderDrawColor";

   function SDL_RenderClear
     (Renderer : System.Address)
      return Interfaces.C.int
     with Import, Convention => C,
          External_Name => "SDL_RenderClear";

   procedure SDL_RenderPresent
     (Renderer : System.Address)
     with Import, Convention => C,
          External_Name => "SDL_RenderPresent";

   function SDL_RenderFillRect
     (Renderer : System.Address;
      Rect     : System.Address)
      return Interfaces.C.int
     with Import, Convention => C,
          External_Name => "SDL_RenderFillRect";

   function SDL_RenderDrawLine
     (Renderer : System.Address;
      X1       : Interfaces.C.int;
      Y1       : Interfaces.C.int;
      X2       : Interfaces.C.int;
      Y2       : Interfaces.C.int)
      return Interfaces.C.int
     with Import, Convention => C,
          External_Name => "SDL_RenderDrawLine";

end UI.Renderer;
