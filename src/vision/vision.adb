--  Vision -- Implementation

package body Vision is

   ----------------------------------------------------------
   --  To_Greyscale
   ----------------------------------------------------------
   --  Luminance standard (ITU-R BT.601) :
   --  Y = 0.299*R + 0.587*G + 0.114*B

   function To_Greyscale (P : Pixel) return Greyscale_Pixel
   is
      Y : constant Natural :=
        (Natural (P.R) * 299
         + Natural (P.G) * 587
         + Natural (P.B) * 114) / 1000;
   begin
      if Y > 255 then
         return 255;
      end if;
      return Greyscale_Pixel (Y);
   end To_Greyscale;

end Vision;
