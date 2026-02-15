--  UI.Renderer -- Implementation

with Vision.Transformations;

package body UI.Renderer is

   use Interfaces.C;

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

end UI.Renderer;
