--  Vision.Filters -- Implementation des filtres de detection de contours

with Ada.Numerics.Elementary_Functions;

package body Vision.Filters is

   package EF renames Ada.Numerics.Elementary_Functions;

   ----------------------------------------------------------
   --  To_Grey
   ----------------------------------------------------------

   procedure To_Grey
     (Src : Pixel_Grid;
      Dst : out Grey_Grid)
   is
   begin
      for R in Src'Range (1) loop
         for C in Src'Range (2) loop
            Dst (R, C) := To_Greyscale (Src (R, C));
         end loop;
      end loop;
   end To_Grey;

   ----------------------------------------------------------
   --  Sobel
   ----------------------------------------------------------
   --  Noyaux Sobel 3x3 :
   --  Gx = [-1 0 1; -2 0 2; -1 0 1]
   --  Gy = [-1 -2 -1; 0 0 0; 1 2 1]
   --  Magnitude = sqrt(Gx^2 + Gy^2), clampee a 255.

   procedure Sobel_Rows
     (Src   : Grey_Grid;
      Dst   : in out Grey_Grid;
      First : Positive;
      Last  : Positive)
   is
      Gx  : Integer;
      Gy  : Integer;
      Mag : Integer;
   begin
      for R in First .. Last loop
         for C in Src'First (2) + 1
           .. Src'Last (2) - 1
         loop
            Gx :=
              -Integer (Src (R - 1, C - 1))
              + Integer (Src (R - 1, C + 1))
              - 2 * Integer (Src (R, C - 1))
              + 2 * Integer (Src (R, C + 1))
              - Integer (Src (R + 1, C - 1))
              + Integer (Src (R + 1, C + 1));

            Gy :=
              -Integer (Src (R - 1, C - 1))
              - 2 * Integer (Src (R - 1, C))
              - Integer (Src (R - 1, C + 1))
              + Integer (Src (R + 1, C - 1))
              + 2 * Integer (Src (R + 1, C))
              + Integer (Src (R + 1, C + 1));

            Mag := Integer
              (EF.Sqrt (Float (Gx * Gx + Gy * Gy)));
            if Mag > 255 then
               Mag := 255;
            end if;

            Dst (R, C) := Greyscale_Pixel (Mag);
         end loop;
      end loop;
   end Sobel_Rows;

   procedure Sobel
     (Src : Grey_Grid;
      Dst : out Grey_Grid)
   is
      H : constant Positive := Src'Length (1);
      Num_Workers : constant := 4;

      --  Initialiser les bords a zero
   begin
      Dst := (others => (others => 0));

      if H < 4 then
         --  Image trop petite pour paralleliser
         if H = 3 then
            Sobel_Rows
              (Src,
               Dst,
               Src'First (1) + 1,
               Src'Last (1) - 1);
         end if;
         return;
      end if;

      --  Paralleliser via 4 taches
      declare
         Inner_First : constant Positive :=
           Src'First (1) + 1;
         Inner_Last  : constant Positive :=
           Src'Last (1) - 1;
         Chunk : constant Positive :=
           (Inner_Last - Inner_First + 1 + Num_Workers - 1)
           / Num_Workers;

         task type Sobel_Worker is
            entry Run (F, L : Positive);
         end Sobel_Worker;

         task body Sobel_Worker is
            WF, WL : Positive;
         begin
            accept Run (F, L : Positive) do
               WF := F;
               WL := L;
            end Run;
            Sobel_Rows (Src, Dst, WF, WL);
         end Sobel_Worker;

         Workers : array (1 .. Num_Workers)
           of Sobel_Worker;
         Row_Start : Positive := Inner_First;
         Row_End   : Positive;
      begin
         for I in Workers'Range loop
            Row_End := Row_Start + Chunk - 1;
            if Row_End > Inner_Last then
               Row_End := Inner_Last;
            end if;
            if Row_Start <= Row_End then
               Workers (I).Run (Row_Start, Row_End);
            else
               Workers (I).Run
                 (Inner_Last, Inner_Last);
            end if;
            Row_Start := Row_End + 1;
         end loop;
      end;
   end Sobel;

   ----------------------------------------------------------
   --  Canny
   ----------------------------------------------------------
   --  Implementation simplifiee :
   --  1. Lissage gaussien 3x3
   --  2. Gradient via Sobel
   --  3. Suppression des non-maxima (simplifiee)
   --  4. Hysteresis par seuils

   procedure Canny
     (Src  : Grey_Grid;
      Dst  : out Grey_Grid;
      Low  : Long_Float := 0.1;
      High : Long_Float := 0.3)
   is
      H : constant Positive := Src'Length (1);
      W : constant Positive := Src'Length (2);

      --  Buffers intermediaires
      Blurred  : Grey_Grid (Src'Range (1), Src'Range (2));
      Gradient : Grey_Grid (Src'Range (1), Src'Range (2));

      Low_Thresh  : constant Integer :=
        Integer (Low * 255.0);
      High_Thresh : constant Integer :=
        Integer (High * 255.0);

      Acc : Integer;
   begin
      Dst := (others => (others => 0));

      --  1. Lissage gaussien 3x3 approche
      --     [1 2 1; 2 4 2; 1 2 1] / 16
      Blurred := (others => (others => 0));
      if H >= 3 and then W >= 3 then
         for R in Src'First (1) + 1
           .. Src'Last (1) - 1
         loop
            for C in Src'First (2) + 1
              .. Src'Last (2) - 1
            loop
               Acc :=
                 Integer (Src (R - 1, C - 1))
                 + 2 * Integer (Src (R - 1, C))
                 + Integer (Src (R - 1, C + 1))
                 + 2 * Integer (Src (R, C - 1))
                 + 4 * Integer (Src (R, C))
                 + 2 * Integer (Src (R, C + 1))
                 + Integer (Src (R + 1, C - 1))
                 + 2 * Integer (Src (R + 1, C))
                 + Integer (Src (R + 1, C + 1));
               Acc := Acc / 16;
               if Acc > 255 then
                  Acc := 255;
               end if;
               Blurred (R, C) :=
                 Greyscale_Pixel (Acc);
            end loop;
         end loop;
      else
         Blurred := Src;
      end if;

      --  2. Gradient via Sobel
      Sobel (Blurred, Gradient);

      --  3-4. Hysteresis simplifiee
      --  Pixel fort (> High) -> blanc
      --  Pixel faible (< Low) -> noir
      --  Pixel entre les deux -> blanc si voisin fort
      for R in Dst'Range (1) loop
         for C in Dst'Range (2) loop
            if Integer (Gradient (R, C)) >= High_Thresh
            then
               Dst (R, C) := 255;
            elsif Integer (Gradient (R, C)) >= Low_Thresh
            then
               --  Verifier les 8 voisins
               declare
                  Has_Strong : Boolean := False;
               begin
                  for Dr in -1 .. 1 loop
                     for Dc in -1 .. 1 loop
                        if (Dr /= 0 or else Dc /= 0)
                          and then
                            R + Dr >= Dst'First (1)
                          and then
                            R + Dr <= Dst'Last (1)
                          and then
                            C + Dc >= Dst'First (2)
                          and then
                            C + Dc <= Dst'Last (2)
                        then
                           if Integer (Gradient
                             (R + Dr, C + Dc))
                             >= High_Thresh
                           then
                              Has_Strong := True;
                           end if;
                        end if;
                     end loop;
                  end loop;
                  if Has_Strong then
                     Dst (R, C) := 255;
                  end if;
               end;
            end if;
         end loop;
      end loop;
   end Canny;

end Vision.Filters;
