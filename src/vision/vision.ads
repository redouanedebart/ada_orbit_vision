--  Vision -- Paquetage racine pour le traitement d'images
--
--  Definit les types fondamentaux pour les buffers image
--  (couleur et niveaux de gris) utilises par l'extracteur
--  de texture, les transformations et les filtres.

with Interfaces;

package Vision is

   --  Composante couleur 8 bits
   subtype Component is Interfaces.Unsigned_8;

   --  Pixel RGBA
   type Pixel is record
      R : Component := 0;
      G : Component := 0;
      B : Component := 0;
      A : Component := 255;
   end record;

   --  Pixel niveaux de gris
   type Greyscale_Pixel is mod 256;

   --  Buffer image 2D couleur (indexe depuis 1)
   type Pixel_Grid is
     array (Positive range <>, Positive range <>) of Pixel;
   type Pixel_Grid_Access is access Pixel_Grid;

   --  Buffer image 2D niveaux de gris
   type Grey_Grid is
     array (Positive range <>, Positive range <>)
     of Greyscale_Pixel;
   type Grey_Grid_Access is access Grey_Grid;

   --  Dimensions d'une image
   type Image_Dimensions is record
      Width  : Positive := 1;
      Height : Positive := 1;
   end record;

   --  Handle image : buffer + dimensions
   type Image_Handle is record
      Pixels : Pixel_Grid_Access := null;
      Dims   : Image_Dimensions;
   end record;

   --  Conversion RGB vers niveaux de gris (luminance)
   function To_Greyscale (P : Pixel) return Greyscale_Pixel;

end Vision;
