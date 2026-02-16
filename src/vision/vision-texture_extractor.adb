--  Vision.Texture_Extractor -- Implementation
--
--  Charge une image equirectangulaire (Blue Marble) via SDL2_image
--  et copie les pixels dans un buffer Pixel_Grid interne.
--  Si le fichier est absent ou le chargement echoue, une texture
--  de test (damier 360x180) est generee en remplacement.

with Ada.Directories;
with Ada.Numerics;
with Common.Logging;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

package body Vision.Texture_Extractor is

   use type System.Address;
   use type Interfaces.C.int;

   --  SDL_PIXELFORMAT_ABGR8888 : bytes R, G, B, A en memoire
   --  (little-endian x86_64, correspond au layout de Pixel)
   SDL_FMT_ABGR8888 : constant Interfaces.Unsigned_32 :=
     16#16762004#;

   ----------------------------------------------------------
   --  Imports SDL2 / SDL2_image
   ----------------------------------------------------------

   function IMG_Load
     (File : Interfaces.C.Strings.chars_ptr)
      return System.Address
     with Import, Convention => C,
          External_Name => "IMG_Load";

   procedure SDL_FreeSurface (Surface : System.Address)
     with Import, Convention => C,
          External_Name => "SDL_FreeSurface";

   function SDL_ConvertSurfaceFormat
     (Src    : System.Address;
      Format : Interfaces.Unsigned_32;
      Flags  : Interfaces.Unsigned_32)
      return System.Address
     with Import, Convention => C,
          External_Name => "SDL_ConvertSurfaceFormat";

   function SDL_LockSurface
     (Surface : System.Address)
      return Interfaces.C.int
     with Import, Convention => C,
          External_Name => "SDL_LockSurface";

   procedure SDL_UnlockSurface (Surface : System.Address)
     with Import, Convention => C,
          External_Name => "SDL_UnlockSurface";

   ----------------------------------------------------------
   --  En-tete partiel de SDL_Surface (layout C x86_64)
   ----------------------------------------------------------

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

   package Byte_Conv is new System.Address_To_Access_Conversions
     (Interfaces.Unsigned_8);

   ----------------------------------------------------------
   --  Generate_Test_Pattern
   ----------------------------------------------------------

   procedure Generate_Test_Pattern (Img : out Image_Handle) is
   begin
      Img.Dims := (Width => 360, Height => 180);
      Img.Pixels := new Pixel_Grid (1 .. 180, 1 .. 360);

      for Row in 1 .. 180 loop
         for Col in 1 .. 360 loop
            if (Row / 30 + Col / 30) mod 2 = 0 then
               Img.Pixels (Row, Col) :=
                 (R => 30, G => 80, B => 180, A => 255);
            else
               Img.Pixels (Row, Col) :=
                 (R => 40, G => 160, B => 60, A => 255);
            end if;
         end loop;
      end loop;
   end Generate_Test_Pattern;

   ----------------------------------------------------------
   --  Load_Image
   ----------------------------------------------------------

   procedure Load_Image
     (Path : String;
      Img  : out Image_Handle)
   is
      use Interfaces.C;
      use Interfaces.C.Strings;
      use System.Storage_Elements;

      C_Path   : chars_ptr;
      Raw_Srf  : System.Address;
      Conv_Srf : System.Address;
      Srf      : Srf_Conv.Object_Pointer;
      Img_W    : Positive;
      Img_H    : Positive;
      Pitch    : Natural;
      Ignore   : int;
   begin
      if not Ada.Directories.Exists (Path) then
         Common.Logging.Log_Warning
           ("Texture_Extractor",
            "Fichier absent : " & Path
            & " -- generation d'une texture de test");
         Generate_Test_Pattern (Img);
         return;
      end if;

      Common.Logging.Log_Info
        ("Texture_Extractor",
         "Chargement de " & Path & "...");

      --  Charger via SDL2_image (PNG, BMP, JPG, etc.)
      C_Path := New_String (Path);
      Raw_Srf := IMG_Load (C_Path);
      Free (C_Path);

      if Raw_Srf = System.Null_Address then
         Common.Logging.Log_Error
           ("Texture_Extractor",
            "IMG_Load a echoue pour " & Path);
         Generate_Test_Pattern (Img);
         return;
      end if;

      --  Convertir en ABGR8888 (bytes : R, G, B, A)
      Conv_Srf := SDL_ConvertSurfaceFormat
        (Raw_Srf, SDL_FMT_ABGR8888, 0);
      SDL_FreeSurface (Raw_Srf);

      if Conv_Srf = System.Null_Address then
         Common.Logging.Log_Error
           ("Texture_Extractor",
            "SDL_ConvertSurfaceFormat a echoue");
         Generate_Test_Pattern (Img);
         return;
      end if;

      --  Lire les dimensions depuis l'en-tete SDL_Surface
      Srf := Srf_Conv.To_Pointer (Conv_Srf);
      Img_W := Positive (Srf.W);
      Img_H := Positive (Srf.H);
      Pitch := Natural (Srf.Pitch);

      Img.Dims := (Width => Img_W, Height => Img_H);
      Img.Pixels := new Pixel_Grid (1 .. Img_H, 1 .. Img_W);

      --  Verrouiller pour acces direct aux pixels
      Ignore := SDL_LockSurface (Conv_Srf);

      --  Copier les pixels depuis le buffer SDL
      declare
         Base : constant System.Address := Srf.Pixels;
         Off  : Storage_Offset;
      begin
         for Row in 1 .. Img_H loop
            for Col in 1 .. Img_W loop
               Off := Storage_Offset
                 ((Row - 1) * Pitch + (Col - 1) * 4);
               Img.Pixels (Row, Col) :=
                 (R => Component
                    (Byte_Conv.To_Pointer
                       (Base + Off).all),
                  G => Component
                    (Byte_Conv.To_Pointer
                       (Base + Off + 1).all),
                  B => Component
                    (Byte_Conv.To_Pointer
                       (Base + Off + 2).all),
                  A => Component
                    (Byte_Conv.To_Pointer
                       (Base + Off + 3).all));
            end loop;
         end loop;
      end;

      SDL_UnlockSurface (Conv_Srf);
      SDL_FreeSurface (Conv_Srf);

      Common.Logging.Log_Info
        ("Texture_Extractor",
         "Image chargee :"
         & Img_W'Image & "x" & Img_H'Image);
   end Load_Image;

   ----------------------------------------------------------
   --  Extract_Region
   ----------------------------------------------------------

   procedure Extract_Region
     (Source    : Image_Handle;
      Lat       : Long_Float;
      Lon       : Long_Float;
      Half_Size : Positive;
      Region    : out Pixel_Grid)
   is
      use Ada.Numerics;

      W : constant Positive := Source.Dims.Width;
      H : constant Positive := Source.Dims.Height;

      --  Projection equirectangulaire : lat/lon -> pixel
      Center_Col : Integer :=
        Integer (Long_Float (W) * (Lon + Pi) / (2.0 * Pi));
      Center_Row : Integer :=
        Integer (Long_Float (H) * (Pi / 2.0 - Lat) / Pi);

      Src_Row : Integer;
      Src_Col : Integer;
   begin
      --  Clamper au centre de l'image
      if Center_Col < 1 then
         Center_Col := 1;
      elsif Center_Col > W then
         Center_Col := W;
      end if;

      if Center_Row < 1 then
         Center_Row := 1;
      elsif Center_Row > H then
         Center_Row := H;
      end if;

      for Dr in Region'Range (1) loop
         for Dc in Region'Range (2) loop
            Src_Row := Center_Row + Dr - Half_Size;
            Src_Col := Center_Col + Dc - Half_Size;

            --  Wrapping horizontal, clamping vertical
            Src_Col := ((Src_Col - 1) mod W) + 1;
            if Src_Row < 1 then
               Src_Row := 1;
            elsif Src_Row > H then
               Src_Row := H;
            end if;

            Region (Dr, Dc) :=
              Source.Pixels (Src_Row, Src_Col);
         end loop;
      end loop;
   end Extract_Region;

end Vision.Texture_Extractor;
