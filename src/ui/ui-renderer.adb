--  UI.Renderer -- Implementation

with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with Common.Logging;
with Common.Physics;
with Engine.Footprint;
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

   function SDL_SetRenderDrawBlendMode
     (Renderer  : System.Address;
      BlendMode : Interfaces.C.int)
      return Interfaces.C.int
     with Import, Convention => C,
          External_Name => "SDL_SetRenderDrawBlendMode";

   function SDL_SetTextureBlendMode
     (Texture   : System.Address;
      BlendMode : Interfaces.C.int)
      return Interfaces.C.int
     with Import, Convention => C,
          External_Name => "SDL_SetTextureBlendMode";

   SDL_BLENDMODE_BLEND : constant Interfaces.C.int := 1;

   --  SDL_Color pour SDL_ttf (4 octets, Convention C)
   type SDL_Color is record
      R : Interfaces.C.unsigned_char;
      G : Interfaces.C.unsigned_char;
      B : Interfaces.C.unsigned_char;
      A : Interfaces.C.unsigned_char;
   end record
     with Convention => C, Size => 32;

   function TTF_RenderUTF8_Blended
     (Font : System.Address;
      Text : Interfaces.C.Strings.chars_ptr;
      Fg   : SDL_Color)
      return System.Address
     with Import, Convention => C,
          External_Name => "TTF_RenderUTF8_Blended";

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

   --  Palette satellite (cycle de 6 couleurs)
   type Colour_Array is
     array (Positive range <>) of Colour;
   Sat_Colours : constant Colour_Array :=
     ((R => 255, G => 100, B => 100, A => 255),
      (R => 100, G => 255, B => 100, A => 255),
      (R => 100, G => 100, B => 255, A => 255),
      (R => 255, G => 255, B => 100, A => 255),
      (R => 255, G => 100, B => 255, A => 255),
      (R => 100, G => 255, B => 255, A => 255));

   --  Couleurs texte recurrentes
   White_Clr : constant SDL_Color := (220, 220, 220, 255);
   Gray_Clr  : constant SDL_Color := (160, 160, 160, 255);

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
   --  Render_Text — helper interne pour dessiner du texte
   ----------------------------------------------------------

   procedure Render_Text
     (Rend : System.Address;
      Font : System.Address;
      Text : String;
      X    : Interfaces.C.int;
      Y    : Interfaces.C.int;
      Clr  : SDL_Color)
   is
      use Interfaces.C.Strings;
      C_Text  : chars_ptr;
      Surface : System.Address;
      Texture : System.Address;
      Srf     : Srf_Conv.Object_Pointer;
      Dst     : SDL_Rect;
      Ignore  : Interfaces.C.int;
   begin
      if Font = System.Null_Address
        or else Text'Length = 0
      then
         return;
      end if;

      C_Text := New_String (Text);
      Surface := TTF_RenderUTF8_Blended (Font, C_Text, Clr);
      Free (C_Text);

      if Surface = System.Null_Address then
         return;
      end if;

      Srf := Srf_Conv.To_Pointer (Surface);
      Dst := (X => X, Y => Y,
              W => Srf.W, H => Srf.H);

      Texture := SDL_CreateTextureFromSurface (Rend, Surface);
      SDL_FreeSurface (Surface);

      if Texture = System.Null_Address then
         return;
      end if;

      Ignore := SDL_SetTextureBlendMode
        (Texture, SDL_BLENDMODE_BLEND);
      Ignore := SDL_RenderCopy
        (Rend, Texture,
         System.Null_Address, Dst'Address);
      SDL_DestroyTexture (Texture);
   end Render_Text;

   ----------------------------------------------------------
   --  Draw_Satellites
   ----------------------------------------------------------
   --  Pour chaque satellite dans la map :
   --  1. Convertir ECI -> lat/lon
   --  2. Mapper lat/lon vers coordonnees ecran
   --  3. Dessiner un carre colore + croix de reticule
   --     (ou surbrillance si satellite selectionne)

   procedure Draw_Satellites
     (Rend         : System.Address;
      Snapshots    : Concurrency.Ground_Station.Snapshot_Maps.Map;
      W            : Positive;
      H            : Positive;
      Selected_Sat : Natural := 0;
      Font         : System.Address := System.Null_Address)
   is
      use Concurrency.Ground_Station;
      use Concurrency.Ground_Station.Snapshot_Maps;

      C   : Cursor := Snapshots.First;
      Lat : Long_Float;
      Lon : Long_Float;
      Row : Positive;
      Col : Positive;

      Ignore : Interfaces.C.int;
      Rect   : SDL_Rect;
      Idx    : Positive := 1;
      Clr    : Colour;
      Sx     : Interfaces.C.int;
      Sy     : Interfaces.C.int;
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

            Clr := Sat_Colours
              (((Idx - 1) mod Sat_Colours'Length) + 1);
            Ignore := SDL_SetRenderDrawColor
              (Rend, Clr.R, Clr.G, Clr.B, Clr.A);

            if Natural (Snap.Id) = Selected_Sat then
               --  Satellite selectionne : grand carre 12x12 +
               --  bordure blanche + nom
               Sx := Interfaces.C.int (Col) - 6;
               Sy := Interfaces.C.int (Row) - 6;

               --  Remplissage 12x12
               Rect := (X => Sx, Y => Sy, W => 12, H => 12);
               Ignore := SDL_RenderFillRect
                 (Rend, Rect'Address);

               --  Bordure blanche 16x16
               Ignore := SDL_SetRenderDrawColor
                 (Rend, 255, 255, 255, 255);
               Rect :=
                 (X => Sx - 2, Y => Sy - 2,
                  W => 16, H => 16);
               Ignore := SDL_RenderDrawRect
                 (Rend, Rect'Address);

               --  Nom du satellite au-dessus
               declare
                  Raw : constant String :=
                    Ada.Strings.Fixed.Trim
                      (Snap.Name (1 .. Snap.Name_Len),
                       Ada.Strings.Both);
                  Label : constant String :=
                    (if Raw'Length = 0
                     then "#"
                       & Ada.Strings.Fixed.Trim
                           (Snap.Id'Image, Ada.Strings.Left)
                     elsif Raw'Length > 14
                     then Raw (Raw'First .. Raw'First + 13)
                     else Raw);
               begin
                  Render_Text
                    (Rend, Font, Label,
                     Sx - 2, Sy - 16,
                     White_Clr);
               end;

            else
               --  Satellite normal : carre 6x6 + croix 20 px
               Sx := Interfaces.C.int (Col) - 3;
               Sy := Interfaces.C.int (Row) - 3;
               Rect := (X => Sx, Y => Sy, W => 6, H => 6);
               Ignore := SDL_RenderFillRect
                 (Rend, Rect'Address);

               Ignore := SDL_RenderDrawLine
                 (Rend,
                  Sx + 3, Sy - 7,
                  Sx + 3, Sy + 13);
               Ignore := SDL_RenderDrawLine
                 (Rend,
                  Sx - 7, Sy + 3,
                  Sx + 13, Sy + 3);
            end if;

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

   ----------------------------------------------------------
   --  Draw_Footprint
   ----------------------------------------------------------
   --  Calcule et dessine le footprint (empreinte au sol) du
   --  satellite selectionne :
   --  1. Convertir position ECI -> lat/lon + altitude
   --  2. Calculer le demi-angle de vision (Half_Angle_Rad)
   --  3. Calculer le polygone (Compute_Polygon, 72 points)
   --  4. Remplissage fan semi-transparent depuis le nadir
   --  5. Contour du polygone (en sautant les aretes qui
   --     franchissent l'antimeridien)

   procedure Draw_Footprint
     (Rend  : System.Address;
      Snap  : Concurrency.Ground_Station.Satellite_Snapshot;
      Map_W : Positive;
      H     : Positive)
   is
      use Engine.Footprint;

      Lat    : Long_Float;
      Lon    : Long_Float;
      Alt_Km : Long_Float;
      Radius : Long_Float;
      Pts    : Footprint_Array;

      Ignore   : Interfaces.C.int;
      Ctr_Row  : Positive;
      Ctr_Col  : Positive;
      Half_W   : constant Interfaces.C.int :=
        Interfaces.C.int (Map_W / 2);
   begin
      --  Position au sol et altitude
      Vision.Transformations.ECI_To_Lat_Lon
        (Snap.State.Position, Lat, Lon);

      Alt_Km :=
        Long_Float (Common.Physics.Norm (Snap.State.Position))
        - Long_Float (Common.Physics.Earth_Radius_Km);

      Radius := Half_Angle_Rad (Alt_Km);

      if Radius <= 0.0 then
         return;
      end if;

      Compute_Polygon (Lat, Lon, Radius, Pts);

      --  Pixel du nadir (centre du footprint)
      Vision.Transformations.Lat_Lon_To_Pixel
        (Lat, Lon, Map_W, H, Ctr_Row, Ctr_Col);

      --  Activer le blend mode
      Ignore := SDL_SetRenderDrawBlendMode
        (Rend, SDL_BLENDMODE_BLEND);

      --  Remplissage fan (semi-transparent jaune)
      Ignore := SDL_SetRenderDrawColor
        (Rend, 255, 255, 100, 30);

      for I in 1 .. Max_Points loop
         declare
            Pt_Row : Positive;
            Pt_Col : Positive;
         begin
            Vision.Transformations.Lat_Lon_To_Pixel
              (Pts (I).Lat, Pts (I).Lon,
               Map_W, H, Pt_Row, Pt_Col);

            --  Sauter si l'arrete franchit l'antimeridien
            if abs (Interfaces.C.int (Pt_Col)
                    - Interfaces.C.int (Ctr_Col)) < Half_W
            then
               Ignore := SDL_RenderDrawLine
                 (Rend,
                  Interfaces.C.int (Ctr_Col),
                  Interfaces.C.int (Ctr_Row),
                  Interfaces.C.int (Pt_Col),
                  Interfaces.C.int (Pt_Row));
            end if;
         end;
      end loop;

      --  Contour du polygone (jaune opaque)
      Ignore := SDL_SetRenderDrawColor
        (Rend, 255, 255, 100, 200);

      for I in 1 .. Max_Points loop
         declare
            J      : constant Positive :=
              (if I = Max_Points then 1 else I + 1);
            Row_1  : Positive;
            Col_1  : Positive;
            Row_2  : Positive;
            Col_2  : Positive;
         begin
            Vision.Transformations.Lat_Lon_To_Pixel
              (Pts (I).Lat, Pts (I).Lon,
               Map_W, H, Row_1, Col_1);
            Vision.Transformations.Lat_Lon_To_Pixel
              (Pts (J).Lat, Pts (J).Lon,
               Map_W, H, Row_2, Col_2);

            --  Sauter les segments franchissant l'antimeridien
            if abs (Interfaces.C.int (Col_1)
                    - Interfaces.C.int (Col_2)) < Half_W
            then
               Ignore := SDL_RenderDrawLine
                 (Rend,
                  Interfaces.C.int (Col_1),
                  Interfaces.C.int (Row_1),
                  Interfaces.C.int (Col_2),
                  Interfaces.C.int (Row_2));
            end if;
         end;
      end loop;

      --  Restaurer blend mode par defaut
      Ignore := SDL_SetRenderDrawBlendMode (Rend, 0);
   end Draw_Footprint;

   ----------------------------------------------------------
   --  Find_Sat_At_Map_Click
   ----------------------------------------------------------
   --  Itere sur les snapshots, calcule la position pixel de
   --  chaque satellite et retourne l'Id du premier satellite
   --  dont le point est a moins de 10 px du clic.

   function Find_Sat_At_Map_Click
     (Snapshots : Concurrency.Ground_Station.Snapshot_Maps.Map;
      Mouse_X   : Interfaces.C.int;
      Mouse_Y   : Interfaces.C.int;
      Map_W     : Positive;
      H         : Positive) return Natural
   is
      use Concurrency.Ground_Station;
      use Concurrency.Ground_Station.Snapshot_Maps;

      C   : Cursor := Snapshots.First;
      Lat : Long_Float;
      Lon : Long_Float;
      Row : Positive;
      Col : Positive;
      Dx  : Integer;
      Dy  : Integer;
   begin
      while C /= No_Element loop
         declare
            Snap : constant Satellite_Snapshot :=
              Element (C);
         begin
            Vision.Transformations.ECI_To_Lat_Lon
              (Snap.State.Position, Lat, Lon);
            Vision.Transformations.Lat_Lon_To_Pixel
              (Lat, Lon, Map_W, H, Row, Col);

            Dx := Integer (Interfaces.C.int (Col) - Mouse_X);
            Dy := Integer (Interfaces.C.int (Row) - Mouse_Y);

            --  Rayon de clic : 10 px
            if Dx * Dx + Dy * Dy <= 100 then
               return Natural (Snap.Id);
            end if;
         end;
         C := Next (C);
      end loop;

      return 0;
   end Find_Sat_At_Map_Click;

   ----------------------------------------------------------
   --  Find_Sat_At_Panel_Click
   ----------------------------------------------------------
   --  Le panel occupe X in [W - Panel_Width, W].
   --  Les items commencent a Y = Header_H + 4 = 36,
   --  espaces de Row_Height = 22 px.

   function Find_Sat_At_Panel_Click
     (Snapshots   : Concurrency.Ground_Station.Snapshot_Maps.Map;
      Mouse_X     : Interfaces.C.int;
      Mouse_Y     : Interfaces.C.int;
      W           : Positive;
      H           : Positive;
      Panel_Width : Positive := 280) return Natural
   is
      use Concurrency.Ground_Station;
      use Concurrency.Ground_Station.Snapshot_Maps;

      Panel_X  : constant Interfaces.C.int :=
        Interfaces.C.int (W - Panel_Width);
      Y_Start  : constant Interfaces.C.int := 36;
      Row_H    : constant Interfaces.C.int := 22;
      Bot_Lim  : constant Interfaces.C.int :=
        Interfaces.C.int (H) - 20;

      Index    : Interfaces.C.int;
      C        : Cursor;
      I        : Natural;
   begin
      --  Verifier que le clic est dans la zone panel
      if Mouse_X < Panel_X then
         return 0;
      end if;

      --  Verifier que le clic est dans la zone items
      --  (hors en-tete et hors compteur en bas)
      if Mouse_Y < Y_Start or else Mouse_Y >= Bot_Lim then
         return 0;
      end if;

      Index := (Mouse_Y - Y_Start) / Row_H;

      C := Snapshots.First;
      I := 0;

      while C /= No_Element loop
         if I = Natural (Index) then
            return Natural (Element (C).Id);
         end if;
         I := I + 1;
         C := Next (C);
      end loop;

      return 0;
   end Find_Sat_At_Panel_Click;

   ----------------------------------------------------------
   --  Draw_Panel
   ----------------------------------------------------------

   procedure Draw_Panel
     (Rend         : System.Address;
      Font         : System.Address;
      Snapshots    : Concurrency.Ground_Station.Snapshot_Maps.Map;
      W            : Positive;
      H            : Positive;
      Panel_Width  : Positive := 280;
      Selected_Sat : Natural := 0)
   is
      use Concurrency.Ground_Station;
      use Concurrency.Ground_Station.Snapshot_Maps;

      Ignore  : Interfaces.C.int;
      Panel_X : constant Interfaces.C.int :=
        Interfaces.C.int (W - Panel_Width);
      Rect    : SDL_Rect;

      Header_Clr : constant SDL_Color := (100, 200, 255, 255);

      --  Layout
      Row_Height : constant := 22;
      Header_H   : constant := 32;
      Pad        : constant := 8;
      Y_Start    : Interfaces.C.int :=
        Interfaces.C.int (Header_H + 4);

      C   : Cursor := Snapshots.First;
      Idx : Positive := 1;
      Clr : Colour;
   begin
      --  Activer le blend mode pour la transparence
      Ignore := SDL_SetRenderDrawBlendMode
        (Rend, SDL_BLENDMODE_BLEND);

      --  Fond du panel (semi-transparent)
      Ignore := SDL_SetRenderDrawColor (Rend, 15, 20, 40, 210);
      Rect := (X => Panel_X, Y => 0,
               W => Interfaces.C.int (Panel_Width),
               H => Interfaces.C.int (H));
      Ignore := SDL_RenderFillRect (Rend, Rect'Address);

      --  Bordure gauche
      Ignore := SDL_SetRenderDrawColor (Rend, 60, 80, 120, 255);
      Ignore := SDL_RenderDrawLine
        (Rend, Panel_X, 0,
         Panel_X, Interfaces.C.int (H));

      --  Titre "SATELLITES"
      Render_Text
        (Rend, Font, "SATELLITES",
         Panel_X + Pad, 6, Header_Clr);

      --  Ligne de separation sous le titre
      Ignore := SDL_SetRenderDrawColor (Rend, 60, 80, 120, 255);
      Ignore := SDL_RenderDrawLine
        (Rend,
         Panel_X + Pad,
         Interfaces.C.int (Header_H),
         Interfaces.C.int (W) - Pad,
         Interfaces.C.int (Header_H));

      --  Liste des satellites
      while C /= No_Element loop
         exit when Integer (Y_Start) + Row_Height > H;

         declare
            Snap : constant Satellite_Snapshot := Element (C);
            Name : constant String :=
              Ada.Strings.Fixed.Trim
                (Snap.Name (1 .. Snap.Name_Len),
                 Ada.Strings.Both);
            Disp_Name : constant String :=
              (if Name'Length > 18
               then Name (Name'First .. Name'First + 17)
               else Name);
            Id_Str : constant String :=
              Ada.Strings.Fixed.Trim
                (Snap.Id'Image, Ada.Strings.Left);
            Alt_F  : Long_Float;
            Alt_Km : Integer;
         begin
            --  Surbrillance si satellite selectionne
            if Natural (Snap.Id) = Selected_Sat then
               Ignore := SDL_SetRenderDrawColor
                 (Rend, 30, 60, 120, 180);
               Rect :=
                 (X => Panel_X + 2,
                  Y => Y_Start - 1,
                  W => Interfaces.C.int (Panel_Width) - 4,
                  H => Interfaces.C.int (Row_Height));
               Ignore := SDL_RenderFillRect
                 (Rend, Rect'Address);
            end if;

            --  Couleur indicateur (meme cycle que la carte)
            Clr := Sat_Colours
              (((Idx - 1) mod Sat_Colours'Length) + 1);

            --  Carre couleur indicateur
            Ignore := SDL_SetRenderDrawColor
              (Rend, Clr.R, Clr.G, Clr.B, Clr.A);
            Rect := (X => Panel_X + Pad,
                     Y => Y_Start + 3,
                     W => 10, H => 10);
            Ignore := SDL_RenderFillRect
              (Rend, Rect'Address);

            --  Nom du satellite
            Render_Text
              (Rend, Font, Disp_Name,
               Panel_X + Pad + 16, Y_Start, White_Clr);

            --  NORAD ID + altitude
            Alt_F := Long_Float
              (Common.Physics.Norm
                 (Snap.State.Position))
              - Long_Float (Common.Physics.Earth_Radius_Km);
            Alt_Km := Integer (Alt_F);
            declare
               Alt_Raw : constant String :=
                 Ada.Strings.Fixed.Trim
                   (Alt_Km'Image, Ada.Strings.Left);
               Info : constant String :=
                 "#" & Id_Str & "  "
                 & Alt_Raw & "km";
            begin
               Render_Text
                 (Rend, Font, Info,
                  Panel_X + Pad + 16,
                  Y_Start + 11, Gray_Clr);
            end;
         end;

         Y_Start := Y_Start + Interfaces.C.int (Row_Height);
         Idx := Idx + 1;
         C := Next (C);
      end loop;

      --  Compteur en bas
      declare
         Count_Str : constant String :=
           Ada.Strings.Fixed.Trim
             (Natural'Image (Natural (Snapshots.Length)),
              Ada.Strings.Left)
           & " satellite(s)";
      begin
         Render_Text
           (Rend, Font, Count_Str,
            Panel_X + Pad,
            Interfaces.C.int (H) - 20, Gray_Clr);
      end;

      --  Restaurer le blend mode par defaut (none)
      Ignore := SDL_SetRenderDrawBlendMode (Rend, 0);
   end Draw_Panel;

end UI.Renderer;
