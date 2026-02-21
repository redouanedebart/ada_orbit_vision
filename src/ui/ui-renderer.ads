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
   --  Selected_Sat : NORAD ID du satellite selectionne (0 = aucun).
   --  Font : handle SDL_ttf pour le nom du satellite selectionne.
   procedure Draw_Satellites
     (Rend         : System.Address;
      Snapshots    : Concurrency.Ground_Station.Snapshot_Maps.Map;
      W            : Positive;
      H            : Positive;
      Selected_Sat : Natural := 0;
      Font         : System.Address := System.Null_Address);

   --  Dessine un bandeau d'alerte rouge en haut de l'ecran.
   procedure Draw_Alert_Overlay
     (Rend : System.Address;
      W    : Positive);

   --  Dessine le panel lateral droit avec la liste des satellites.
   --  Font = handle SDL_ttf (Null_Address => pas de texte).
   --  Panel_Width = largeur en pixels du volet.
   --  Selected_Sat : NORAD ID du satellite selectionne (0 = aucun).
   procedure Draw_Panel
     (Rend         : System.Address;
      Font         : System.Address;
      Snapshots    : Concurrency.Ground_Station.Snapshot_Maps.Map;
      W            : Positive;
      H            : Positive;
      Panel_Width  : Positive := 280;
      Selected_Sat : Natural := 0);

   --  Dessine le footprint (empreinte au sol) du satellite selectionne.
   --  Snap : snapshot du satellite dont on dessine le footprint.
   --  Map_W : largeur effective de la zone carte (hors panel).
   procedure Draw_Footprint
     (Rend  : System.Address;
      Snap  : Concurrency.Ground_Station.Satellite_Snapshot;
      Map_W : Positive;
      H     : Positive);

   --  Hit-test sur la carte : retourne le NORAD ID du satellite
   --  le plus proche du clic (rayon 10 px), 0 si aucun.
   function Find_Sat_At_Map_Click
     (Snapshots : Concurrency.Ground_Station.Snapshot_Maps.Map;
      Mouse_X   : Interfaces.C.int;
      Mouse_Y   : Interfaces.C.int;
      Map_W     : Positive;
      H         : Positive) return Natural;

   --  Hit-test sur le panel : retourne le NORAD ID de la ligne
   --  cliquee, 0 si aucune ligne touchee.
   function Find_Sat_At_Panel_Click
     (Snapshots   : Concurrency.Ground_Station.Snapshot_Maps.Map;
      Mouse_X     : Interfaces.C.int;
      Mouse_Y     : Interfaces.C.int;
      W           : Positive;
      H           : Positive;
      Panel_Width : Positive := 280) return Natural;

   --  Presente le buffer a l'ecran (flip).
   procedure Present (Rend : System.Address);

   --  Charge la texture terrestre depuis un fichier image.
   --  Cree une SDL_Texture utilisee comme fond de carte.
   procedure Load_Earth_Texture
     (Rend : System.Address;
      Path : String);

   --  Dessine la texture terrestre en fond d'ecran.
   --  Ne fait rien si aucune texture n'est chargee.
   procedure Draw_Earth_Background
     (Rend : System.Address);

   --  Libere la texture terrestre.
   procedure Destroy_Earth_Texture;

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

   function SDL_RenderDrawRect
     (Renderer : System.Address;
      Rect     : System.Address)
      return Interfaces.C.int
     with Import, Convention => C,
          External_Name => "SDL_RenderDrawRect";

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
