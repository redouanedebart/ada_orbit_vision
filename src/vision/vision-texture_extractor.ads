--  Vision.Texture_Extractor -- Chargement et extraction de textures
--
--  Charge une image Blue Marble (equirectangulaire) et extrait
--  des sous-regions basees sur les coordonnees lat/lon.

package Vision.Texture_Extractor is

   --  Charge une image depuis un fichier (BMP/PNG via SDL2_image).
   --  Remplit le handle avec le buffer de pixels et les dimensions.
   procedure Load_Image
     (Path : String;
      Img  : out Image_Handle);

   --  Extrait une sous-region carree centree sur (Lat, Lon).
   --  Half_Size definit la demi-taille en pixels du carre extrait.
   --  Region doit etre pre-allouee a la bonne taille.
   procedure Extract_Region
     (Source    : Image_Handle;
      Lat       : Long_Float;
      Lon       : Long_Float;
      Half_Size : Positive;
      Region    : out Pixel_Grid);

end Vision.Texture_Extractor;
