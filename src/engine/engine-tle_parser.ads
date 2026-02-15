--  Engine.TLE_Parser — Parseur de Two-Line Element Sets (format NORAD)
--
--  Un TLE est composé de trois lignes :
--    Ligne 0 : Nom du satellite (optionnelle, 24 caractères max)
--    Ligne 1 : Données d'identification et d'époque
--    Ligne 2 : Éléments orbitaux képlériens
--
--  Référence format : https://celestrak.org/columns/v04n03/
--  Colonnes a largeur fixe (format herite des cartes perforees).

with Ada.Calendar;
with Common.Physics;

package Engine.TLE_Parser is

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Numéro de catalogue NORAD (1 à 99999, bientôt 9 chiffres)
   subtype Catalog_Number is Positive range 1 .. 999_999;

   --  Classification du satellite
   type Classification is (Unclassified, Classified, Secret);

   --  Record complet issu du parsing d'un TLE
   type TLE_Record is record
      --  Ligne 0
      Name              : String (1 .. 24) := (others => ' ');
      Name_Length        : Natural := 0;

      --  Ligne 1
      Catalog_Id         : Catalog_Number := 1;
      Object_Class       : Classification := Unclassified;
      Intl_Designator    : String (1 .. 8) := (others => ' ');
      Epoch              : Ada.Calendar.Time;
      Mean_Motion_Dot    : Long_Float := 0.0;  --  rev/day² / 2
      Mean_Motion_DDot   : Long_Float := 0.0;  --  rev/day³ / 6
      BStar              : Long_Float := 0.0;   --  coefficient de traînée
      Element_Set_Number : Natural := 0;

      --  Ligne 2 (éléments képlériens)
      Elements           : Common.Physics.Keplerian_Elements;
      Rev_Number         : Natural := 0;
   end record;

   ---------------------------------------------------------------------------
   --  Exceptions
   ---------------------------------------------------------------------------

   --  Levée quand le format TLE est invalide (longueur, checksum, etc.)
   TLE_Format_Error : exception;

   ---------------------------------------------------------------------------
   --  Sous-programmes publics
   ---------------------------------------------------------------------------

   --  Parse un TLE complet (3 lignes).
   --  Line_0 : nom du satellite (peut être vide "")
   --  Line_1 : première ligne TLE (69 caractères)
   --  Line_2 : deuxième ligne TLE (69 caractères)
   --  Lève TLE_Format_Error si le format est invalide.
   function Parse
     (Line_0 : String;
      Line_1 : String;
      Line_2 : String) return TLE_Record;

   --  Vérifie le checksum modulo 10 d'une ligne TLE.
   --  Chaque chiffre compte pour sa valeur, '-' compte pour 1,
   --  tous les autres caractères comptent pour 0.
   --  Le dernier caractère est le checksum attendu.
   function Valid_Checksum (Line : String) return Boolean;

private

   --  Convertit l'époque TLE (année 2 chiffres + jour fractionnaire)
   --  en Ada.Calendar.Time.
   --  Exemple : année=21, jour=264.51782528 → 21 sept 2021, 12:25:...
   function Parse_Epoch
     (Year_2D : Natural;
      Day_Frac : Long_Float) return Ada.Calendar.Time;

   --  Décode la notation exponentielle implicite du TLE.
   --  Format : "12345-6" signifie 0.12345 × 10^(-6)
   --  Utilisé pour BStar et Mean_Motion_DDot.
   function Parse_Implicit_Decimal (S : String) return Long_Float;

   --  Convertit un caractère de classification en type énuméré.
   function To_Classification (C : Character) return Classification;

end Engine.TLE_Parser;
