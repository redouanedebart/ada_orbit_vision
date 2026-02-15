--  Vision.Filters -- Filtres de detection de contours
--
--  Operateurs Sobel et Canny pour la reconnaissance de
--  structures (cotes, frontieres) sur les textures extraites.
--  Les boucles de convolution sont parallelisees via des
--  taches Ada dediees.

package Vision.Filters is

   --  Convertit un buffer couleur en niveaux de gris.
   procedure To_Grey
     (Src : Pixel_Grid;
      Dst : out Grey_Grid);

   --  Filtre Sobel (gradient 3x3).
   --  Calcule la magnitude du gradient en chaque pixel.
   procedure Sobel
     (Src : Grey_Grid;
      Dst : out Grey_Grid);

   --  Filtre Canny complet :
   --  1. Lissage gaussien
   --  2. Gradient Sobel
   --  3. Suppression des non-maxima
   --  4. Hysteresis (seuils Low et High)
   procedure Canny
     (Src  : Grey_Grid;
      Dst  : out Grey_Grid;
      Low  : Long_Float := 0.1;
      High : Long_Float := 0.3);

end Vision.Filters;
