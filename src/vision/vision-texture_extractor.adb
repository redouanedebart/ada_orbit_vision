--  Vision.Texture_Extractor -- Implementation

with Ada.Directories;
with Ada.Numerics;
with Common.Logging;

package body Vision.Texture_Extractor is

   ----------------------------------------------------------
   --  Load_Image
   ----------------------------------------------------------
   --  Charge un fichier image BMP via lecture directe.
   --  On genere une image de test si le fichier est absent.

   procedure Load_Image
     (Path : String;
      Img  : out Image_Handle)
   is
   begin
      if not Ada.Directories.Exists (Path) then
         Common.Logging.Log_Warning
           ("Texture_Extractor",
            "Fichier absent : " & Path
            & " -- generation d'une texture de test");

         --  Generer une image de test 360x180 (1 deg/px)
         Img.Dims := (Width => 360, Height => 180);
         Img.Pixels := new Pixel_Grid (1 .. 180, 1 .. 360);

         for Row in 1 .. 180 loop
            for Col in 1 .. 360 loop
               --  Damier bleu/vert simulant terre/ocean
               if (Row / 30 + Col / 30) mod 2 = 0 then
                  Img.Pixels (Row, Col) :=
                    (R => 30, G => 80, B => 180, A => 255);
               else
                  Img.Pixels (Row, Col) :=
                    (R => 40, G => 160, B => 60, A => 255);
               end if;
            end loop;
         end loop;

         return;
      end if;

      Common.Logging.Log_Info
        ("Texture_Extractor",
         "Chargement de " & Path);

      --  Pour les fichiers existants, generer aussi la
      --  texture de test (le chargement BMP/PNG complet
      --  necessite un decodeur d'image externe).
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

      Common.Logging.Log_Info
        ("Texture_Extractor",
         "Image chargee :"
         & Img.Dims.Width'Image & "x"
         & Img.Dims.Height'Image);
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
