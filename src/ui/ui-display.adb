--  UI.Display -- Implementation

with Common.Logging;

package body UI.Display is

   use type System.Address;

   --  Etat interne du module
   Win_Ptr       : System.Address := System.Null_Address;
   Rend_Ptr      : System.Address := System.Null_Address;
   Win_W : Positive := 1024;
   Win_H : Positive := 768;

   --  Constantes SDL2
   SDL_WINDOWPOS_CENTERED : constant := 16#2FFF0000#;
   SDL_WINDOW_SHOWN       : constant := 16#00000004#;
   SDL_RENDERER_ACCELERATED  : constant := 16#00000002#;
   SDL_RENDERER_PRESENTVSYNC : constant := 16#00000004#;
   SDL_WINDOW_RESIZABLE      : constant := 16#00000020#;
   SDL_WINDOW_MAXIMIZED      : constant := 16#00000080#;

   --  Buffer structure pour SDL_Event (64 octets)
   --  On superpose les champs utiles de SDL_KeyboardEvent
   --  et SDL_MouseButtonEvent.
   type Uint32 is mod 2 ** 32
     with Size => 32;

   type Event_Buffer is record
      Evt_Type  : Uint32 := 0;            --  offset  0
      Timestamp : Uint32 := 0;            --  offset  4
      Window_ID : Uint32 := 0;            --  offset  8
      Field_12  : Uint32 := 0;            --  offset 12
      Field_16  : Uint32 := 0;            --  offset 16  (scancode)
      Field_20  : Interfaces.C.int := 0;  --  offset 20  (mouse x)
      Field_24  : Interfaces.C.int := 0;  --  offset 24  (mouse y)
      Rest      : String (1 .. 36) := (others => ASCII.NUL);
   end record
     with Convention => C, Size => 512;

   --  Codes d'evenements SDL2
   SDL_QUIT_EVENT            : constant Uint32 := 16#100#;
   SDL_WINDOWEVENT           : constant Uint32 := 16#200#;
   SDL_KEYDOWN_EVENT         : constant Uint32 := 16#300#;
   SDL_MOUSEBUTTONDOWN_EVENT : constant Uint32 := 16#401#;

   --  Sous-types SDL_WindowEvent (octet a l'offset 12)
   SDL_WINDOWEVENT_SIZE_CHANGED : constant Uint32 := 6;

   ----------------------------------------------------------
   --  Initialize
   ----------------------------------------------------------

   procedure Initialize
     (Title  : String;
      Width  : Positive := 1024;
      Height : Positive := 768)
   is
      use Interfaces.C;
      Ret : int;
   begin
      Win_W := Width;
      Win_H := Height;

      Ret := SDL_Init (SDL_INIT_VIDEO);
      if Ret /= 0 then
         Common.Logging.Log_Error
           ("Display",
            "Echec SDL_Init :" & int'Image (Ret));
         return;
      end if;

      Win_Ptr := SDL_CreateWindow
        (To_C (Title),
         int (SDL_WINDOWPOS_CENTERED),
         int (SDL_WINDOWPOS_CENTERED),
         int (Width),
         int (Height),
         SDL_WINDOW_SHOWN or SDL_WINDOW_RESIZABLE);

      if Win_Ptr = System.Null_Address then
         Common.Logging.Log_Error
           ("Display", "Echec SDL_CreateWindow");
         return;
      end if;

      Rend_Ptr := SDL_CreateRenderer
        (Win_Ptr,
         -1,
         SDL_RENDERER_ACCELERATED
           or SDL_RENDERER_PRESENTVSYNC);

      if Rend_Ptr = System.Null_Address then
         Common.Logging.Log_Error
           ("Display", "Echec SDL_CreateRenderer");
         return;
      end if;

      Common.Logging.Log_Info
        ("Display",
         "Fenetre creee :"
         & Width'Image & "x" & Height'Image);
   end Initialize;

   ----------------------------------------------------------
   --  Poll_Event
   ----------------------------------------------------------

   function Poll_Event return UI_Event
   is
      use Interfaces.C;
      Buf    : Event_Buffer;
      Ret    : int;
      Result : UI_Event :=
        (Kind => Evt_None, others => <>);
   begin
      Ret := SDL_PollEvent (Buf'Address);
      if Ret = 0 then
         return Result;
      end if;

      if Buf.Evt_Type = SDL_QUIT_EVENT then
         Result.Kind := Evt_Quit;

      elsif Buf.Evt_Type = SDL_WINDOWEVENT then
         --  Sous-type dans l'octet bas de Field_12
         if (Buf.Field_12 and 16#FF#)
           = SDL_WINDOWEVENT_SIZE_CHANGED
         then
            Win_W := Positive'Max
              (1, Positive (Buf.Field_16));
            Win_H := Positive'Max
              (1, Positive (Buf.Field_20));
         end if;

      elsif Buf.Evt_Type = SDL_KEYDOWN_EVENT then
         Result.Kind := Evt_Key_Down;
         Result.Key  := int (Buf.Field_16);   --  scancode

      elsif Buf.Evt_Type = SDL_MOUSEBUTTONDOWN_EVENT
      then
         Result.Kind    := Evt_Mouse_Click;
         Result.Mouse_X := Buf.Field_20;
         Result.Mouse_Y := Buf.Field_24;
      end if;

      return Result;
   end Poll_Event;

   ----------------------------------------------------------
   --  Toggle_Maximize
   ----------------------------------------------------------

   procedure Toggle_Maximize is
      use Interfaces.C;
      Flags : unsigned;
   begin
      if Win_Ptr = System.Null_Address then
         return;
      end if;

      Flags := SDL_GetWindowFlags (Win_Ptr);

      if (Flags and SDL_WINDOW_MAXIMIZED) /= 0 then
         SDL_RestoreWindow (Win_Ptr);
      else
         SDL_MaximizeWindow (Win_Ptr);
      end if;
      --  Win_W / Win_H seront mis a jour par
      --  l'evenement SDL_WINDOWEVENT_SIZE_CHANGED.
   end Toggle_Maximize;

   ----------------------------------------------------------
   --  Shutdown
   ----------------------------------------------------------

   procedure Shutdown is
   begin
      if Rend_Ptr /= System.Null_Address then
         SDL_DestroyRenderer (Rend_Ptr);
         Rend_Ptr := System.Null_Address;
      end if;

      if Win_Ptr /= System.Null_Address then
         SDL_DestroyWindow (Win_Ptr);
         Win_Ptr := System.Null_Address;
      end if;

      SDL_Quit_C;

      Common.Logging.Log_Info
        ("Display", "SDL2 ferme proprement");
   end Shutdown;

   ----------------------------------------------------------
   --  Accesseurs
   ----------------------------------------------------------

   function Get_Renderer return System.Address is
   begin
      return Rend_Ptr;
   end Get_Renderer;

   function Get_Width return Positive is
   begin
      return Win_W;
   end Get_Width;

   function Get_Height return Positive is
   begin
      return Win_H;
   end Get_Height;

end UI.Display;
