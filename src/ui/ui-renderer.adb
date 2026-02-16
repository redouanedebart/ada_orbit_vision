--  UI.Renderer -- Implementation

with Ada.Directories;
with Common.Logging;
with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;
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

   function SDL_GetRendererInfo
     (Renderer : System.Address;
      Info     : System.Address)
      return Interfaces.C.int
     with Import, Convention => C,
          External_Name => "SDL_GetRendererInfo";

   function SDL_CreateRGBSurfaceWithFormat
     (Flags  : Interfaces.Unsigned_32;
      Width  : Interfaces.C.int;
      Height : Interfaces.C.int;
      Depth  : Interfaces.C.int;
      Format : Interfaces.Unsigned_32)
      return System.Address
     with Import, Convention => C,
          External_Name => "SDL_CreateRGBSurfaceWithFormat";

   function SDL_UpperBlitScaled
     (Src     : System.Address;
      Srcrect : System.Address;
      Dst     : System.Address;
      Dstrect : System.Address)
      return Interfaces.C.int
     with Import, Convention => C,
          External_Name => "SDL_UpperBlitScaled";

   function SDL_GetError return Interfaces.C.Strings.chars_ptr
     with Import, Convention => C,
          External_Name => "SDL_GetError";

   --  En-tete partiel de SDL_Surface (layout C x86_64)
   type SDL_Surface_Header is record
      Flags  : Interfaces.Unsigned_32;
      Format : System.Address;
      W      : Interfaces.C.int;
      H      : Interfaces.C.int;
      Pitch  : Interfaces.C.int;
      Pixels : System.Address;
   end record
     with Convention => C;

   package Srf_Conv is new System.Address_To_Access_Conversions
     (SDL_Surface_Header);

   --  SDL_RendererInfo (layout C)
   type U32_Array_16 is
     array (0 .. 15) of Interfaces.Unsigned_32
     with Convention => C;

   type SDL_RendererInfo is record
      Name      : System.Address;
      Flags     : Interfaces.Unsigned_32;
      Num_Fmts  : Interfaces.Unsigned_32;
      Fmts      : U32_Array_16;
      Max_W     : Interfaces.C.int;
      Max_H     : Interfaces.C.int;
   end record
     with Convention => C;

   --  SDL_PIXELFORMAT_RGBA32 (byte-order RGBA, little-endian)
   SDL_FMT_RGBA32 : constant Interfaces.Unsigned_32 :=
     16#16762004#;

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
      Srf     : Srf_Conv.Object_Pointer;
      Src_W   : int;
      Src_H   : int;
      Info    : SDL_RendererInfo;
      Max_W   : int;
      Max_H   : int;
      Ignore  : int;
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
            "IMG_Load a echoue pour " & Path
            & " : " & Value (SDL_GetError));
         return;
      end if;

      --  Lire les dimensions de l'image chargee
      Srf := Srf_Conv.To_Pointer (Surface);
      Src_W := Srf.W;
      Src_H := Srf.H;

      Common.Logging.Log_Info
        ("Renderer",
         "Image chargee :"
         & Src_W'Image & "x" & Src_H'Image);

      --  Limiter a la taille max du GPU
      Ignore := SDL_GetRendererInfo (Rend, Info'Address);
      Max_W := Info.Max_W;
      Max_H := Info.Max_H;

      if Max_W <= 0 then
         Max_W := 8192;
      end if;
      if Max_H <= 0 then
         Max_H := 8192;
      end if;

      if Src_W > Max_W or else Src_H > Max_H then
         declare
            Scale_W : constant Long_Float :=
              Long_Float (Max_W) / Long_Float (Src_W);
            Scale_H : constant Long_Float :=
              Long_Float (Max_H) / Long_Float (Src_H);
            Scale   : constant Long_Float :=
              Long_Float'Min (Scale_W, Scale_H);
            Dst_W   : constant int :=
              int (Long_Float (Src_W) * Scale);
            Dst_H   : constant int :=
              int (Long_Float (Src_H) * Scale);
            Scaled  : System.Address;
         begin
            Common.Logging.Log_Info
              ("Renderer",
               "Redimensionnement :"
               & Dst_W'Image & "x" & Dst_H'Image
               & " (max GPU :"
               & Max_W'Image & "x" & Max_H'Image & ")");

            Scaled := SDL_CreateRGBSurfaceWithFormat
              (0, Dst_W, Dst_H, 32, SDL_FMT_RGBA32);

            if Scaled = System.Null_Address then
               Common.Logging.Log_Error
                 ("Renderer",
                  "SDL_CreateRGBSurfaceWithFormat a echoue");
               SDL_FreeSurface (Surface);
               return;
            end if;

            Ignore := SDL_UpperBlitScaled
              (Surface, System.Null_Address,
               Scaled, System.Null_Address);
            SDL_FreeSurface (Surface);
            Surface := Scaled;
         end;
      end if;

      Earth_Tex := SDL_CreateTextureFromSurface (Rend, Surface);
      SDL_FreeSurface (Surface);

      if Earth_Tex = System.Null_Address then
         Common.Logging.Log_Error
           ("Renderer",
            "SDL_CreateTextureFromSurface a echoue : "
            & Value (SDL_GetError));
         return;
      end if;

      Common.Logging.Log_Info
        ("Renderer",
         "Texture terrestre prete");
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
