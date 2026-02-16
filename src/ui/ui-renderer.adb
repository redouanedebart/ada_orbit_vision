--  UI.Renderer -- Implementation

with Ada.Directories;
with Common.Logging;
with Interfaces.C.Strings;
with Vision.Transformations;

package body UI.Renderer is

   use Interfaces.C;
   use type System.Address;

   ----------------------------------------------------------
   --  Imports SDL2 supplementaires (texture / image)
   ----------------------------------------------------------

   function IMG_Load
     (File : Interfaces.C.Strings.chars_ptr)
      return System.Address
     with Import, Convention => C,
          External_Name => "IMG_Load";

   procedure SDL_FreeSurface (Surface : System.Address)
     with Import, Convention => C,
          External_Name => "SDL_FreeSurface";

   function SDL_CreateTextureFromSurface
     (Renderer : System.Address;
      Surface  : System.Address)
      return System.Address
     with Import, Convention => C,
          External_Name => "SDL_CreateTextureFromSurface";

   procedure SDL_DestroyTexture (Texture : System.Address)
     with Import, Convention => C,
          External_Name => "SDL_DestroyTexture";

   function SDL_RenderCopy
     (Renderer : System.Address;
      Texture  : System.Address;
      Srcrect  : System.Address;
      Dstrect  : System.Address)
      return Interfaces.C.int
     with Import, Convention => C,
          External_Name => "SDL_RenderCopy";

   --  Texture terrestre (fond de carte)
   Earth_Tex : System.Address := System.Null_Address;

   --  Rectangle SDL2 (4 x C.int)
   type SDL_Rect is record
      X : Interfaces.C.int;
      Y : Interfaces.C.int;
      W : Interfaces.C.int;
      H : Interfaces.C.int;
   end record
     with Convention => C;

   ----------------------------------------------------------
   --  Clear
   ----------------------------------------------------------

   procedure Clear
     (Rend : System.Address;
      BG   : Colour)
   is
      Ignore : Interfaces.C.int;
   begin
      Ignore := SDL_SetRenderDrawColor
        (Rend, BG.R, BG.G, BG.B, BG.A);
      Ignore := SDL_RenderClear (Rend);
   end Clear;

   ----------------------------------------------------------
   --  Draw_Satellites
   ----------------------------------------------------------
   --  Pour chaque satellite dans la map :
   --  1. Convertir ECI -> lat/lon
   --  2. Mapper lat/lon vers coordonnees ecran
   --  3. Dessiner un carre colore + croix de reticule

   procedure Draw_Satellites
     (Rend      : System.Address;
      Snapshots : Concurrency.Ground_Station.Snapshot_Maps.Map;
      W         : Positive;
      H         : Positive)
   is
      use Concurrency.Ground_Station;
      use Concurrency.Ground_Station.Snapshot_Maps;

      C : Cursor := Snapshots.First;
      Lat  : Long_Float;
      Lon  : Long_Float;
      Row  : Positive;
      Col  : Positive;
      Ignore : Interfaces.C.int;
      Rect   : SDL_Rect;

      --  Couleurs par index de satellite (cycle)
      type Colour_Array is
        array (Positive range <>) of Colour;
      Colours : constant Colour_Array :=
        ((R => 255, G => 100, B => 100, A => 255),
         (R => 100, G => 255, B => 100, A => 255),
         (R => 100, G => 100, B => 255, A => 255),
         (R => 255, G => 255, B => 100, A => 255),
         (R => 255, G => 100, B => 255, A => 255),
         (R => 100, G => 255, B => 255, A => 255));

      Idx : Positive := 1;
      Clr : Colour;

      Sx : Interfaces.C.int;
      Sy : Interfaces.C.int;
   begin
      while C /= No_Element loop
         declare
            Snap : constant Satellite_Snapshot :=
              Element (C);
         begin
            Vision.Transformations.ECI_To_Lat_Lon
              (Snap.State.Position, Lat, Lon);
            Vision.Transformations.Lat_Lon_To_Pixel
              (Lat, Lon, W, H, Row, Col);

            Clr := Colours
              (((Idx - 1) mod Colours'Length) + 1);
            Ignore := SDL_SetRenderDrawColor
              (Rend, Clr.R, Clr.G, Clr.B, Clr.A);

            --  Carre 6x6 centre sur la position
            Sx := Interfaces.C.int (Col) - 3;
            Sy := Interfaces.C.int (Row) - 3;
            Rect := (X => Sx, Y => Sy, W => 6, H => 6);
            Ignore := SDL_RenderFillRect
              (Rend, Rect'Address);

            --  Croix de reticule 20px
            Ignore := SDL_RenderDrawLine
              (Rend,
               Sx + 3, Sy - 7,
               Sx + 3, Sy + 13);
            Ignore := SDL_RenderDrawLine
              (Rend,
               Sx - 7, Sy + 3,
               Sx + 13, Sy + 3);

            Idx := Idx + 1;
         end;
         C := Next (C);
      end loop;
   end Draw_Satellites;

   ----------------------------------------------------------
   --  Draw_Alert_Overlay
   ----------------------------------------------------------

   procedure Draw_Alert_Overlay
     (Rend : System.Address;
      W    : Positive)
   is
      Ignore : Interfaces.C.int;
      Rect   : SDL_Rect;
   begin
      --  Bandeau rouge semi-transparent en haut
      Ignore := SDL_SetRenderDrawColor
        (Rend, 220, 30, 30, 200);
      Rect := (X => 0, Y => 0,
               W => Interfaces.C.int (W),
               H => 30);
      Ignore := SDL_RenderFillRect
        (Rend, Rect'Address);
   end Draw_Alert_Overlay;

   ----------------------------------------------------------
   --  Present
   ----------------------------------------------------------

   procedure Present (Rend : System.Address) is
   begin
      SDL_RenderPresent (Rend);
   end Present;

   ----------------------------------------------------------
   --  Load_Earth_Texture
   ----------------------------------------------------------

   procedure Load_Earth_Texture
     (Rend : System.Address;
      Path : String)
   is
      use Interfaces.C.Strings;
      C_Path  : chars_ptr;
      Surface : System.Address;
   begin
      if not Ada.Directories.Exists (Path) then
         Common.Logging.Log_Warning
           ("Renderer",
            "Texture terrestre absente : " & Path);
         return;
      end if;

      C_Path := New_String (Path);
      Surface := IMG_Load (C_Path);
      Free (C_Path);

      if Surface = System.Null_Address then
         Common.Logging.Log_Error
           ("Renderer",
            "IMG_Load a echoue pour " & Path);
         return;
      end if;

      Earth_Tex := SDL_CreateTextureFromSurface (Rend, Surface);
      SDL_FreeSurface (Surface);

      if Earth_Tex = System.Null_Address then
         Common.Logging.Log_Error
           ("Renderer",
            "SDL_CreateTextureFromSurface a echoue");
         return;
      end if;

      Common.Logging.Log_Info
        ("Renderer",
         "Texture terrestre chargee : " & Path);
   end Load_Earth_Texture;

   ----------------------------------------------------------
   --  Draw_Earth_Background
   ----------------------------------------------------------

   procedure Draw_Earth_Background
     (Rend : System.Address)
   is
      Ignore : Interfaces.C.int;
   begin
      if Earth_Tex = System.Null_Address then
         return;
      end if;

      --  NULL src/dst : etire la texture sur tout le renderer
      Ignore := SDL_RenderCopy
        (Rend, Earth_Tex,
         System.Null_Address, System.Null_Address);
   end Draw_Earth_Background;

   ----------------------------------------------------------
   --  Destroy_Earth_Texture
   ----------------------------------------------------------

   procedure Destroy_Earth_Texture is
   begin
      if Earth_Tex /= System.Null_Address then
         SDL_DestroyTexture (Earth_Tex);
         Earth_Tex := System.Null_Address;
      end if;
   end Destroy_Earth_Texture;

end UI.Renderer;
