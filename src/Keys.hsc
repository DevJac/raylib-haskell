{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Keys where

#include "raylib.h"

data KeyboardKey = Apostrophe
                 | Comma
                 | Minus
                 | Period
                 | Slash
                 | Zero
                 | One
                 | Two
                 | Three
                 | Four
                 | Five
                 | Six
                 | Seven
                 | Eight
                 | Nine
                 | Semicolon
                 | Equal
                 | A
                 | B
                 | C
                 | D
                 | E
                 | F
                 | G
                 | H
                 | I
                 | J
                 | K
                 | L
                 | M
                 | N
                 | O
                 | P
                 | Q
                 | R
                 | S
                 | T
                 | U
                 | V
                 | W
                 | X
                 | Y
                 | Z
                 | Space
                 | Escape
                 | Enter
                 | Tab
                 | Backspace
                 | Insert
                 | Delete
                 | ArrowRight
                 | ArrowLeft
                 | ArrowDown
                 | ArrowUp
                 | PageUp
                 | PageDown
                 | Home
                 | End
                 | CapsLock
                 | ScrollLock
                 | NumLock
                 | PrintScreen
                 | Pause
                 | F1
                 | F2
                 | F3
                 | F4
                 | F5
                 | F6
                 | F7
                 | F8
                 | F9
                 | F10
                 | F11
                 | F12
                 | LeftShift
                 | LeftControl
                 | LeftAlt
                 | LeftSuper
                 | RightShift
                 | RightControl
                 | RightAlt
                 | RightSuper
                 | Menu
                 | LeftBracket
                 | RightBracket
                 | Backslash
                 | Grave
                 | KeyPadZero
                 | KeyPadOne
                 | KeyPadTwo
                 | KeyPadThree
                 | KeyPadFour
                 | KeyPadFive
                 | KeyPadSix
                 | KeyPadSeven
                 | KeyPadEight
                 | KeyPadNine
                 | KeyPadDecimal
                 | KeyPadDivide
                 | KeyPadMultiply
                 | KeyPadSubtract
                 | KeyPadAdd
                 | KeyPadEnter
                 | KeyPadEqual
                 | OtherKey Int
                 deriving (Show, Eq)

instance Enum KeyboardKey where
  fromEnum Apostrophe     = #{const KEY_APOSTROPHE}
  fromEnum Comma          = #{const KEY_COMMA}
  fromEnum Minus          = #{const KEY_MINUS}
  fromEnum Period         = #{const KEY_PERIOD}
  fromEnum Slash          = #{const KEY_SLASH}
  fromEnum Zero           = #{const KEY_ZERO}
  fromEnum One            = #{const KEY_ONE}
  fromEnum Two            = #{const KEY_TWO}
  fromEnum Three          = #{const KEY_THREE}
  fromEnum Four           = #{const KEY_FOUR}
  fromEnum Five           = #{const KEY_FIVE}
  fromEnum Six            = #{const KEY_SIX}
  fromEnum Seven          = #{const KEY_SEVEN}
  fromEnum Eight          = #{const KEY_EIGHT}
  fromEnum Nine           = #{const KEY_NINE}
  fromEnum Semicolon      = #{const KEY_SEMICOLON}
  fromEnum Equal          = #{const KEY_EQUAL}
  fromEnum A              = #{const KEY_A}
  fromEnum B              = #{const KEY_B}
  fromEnum C              = #{const KEY_C}
  fromEnum D              = #{const KEY_D}
  fromEnum E              = #{const KEY_E}
  fromEnum F              = #{const KEY_F}
  fromEnum G              = #{const KEY_G}
  fromEnum H              = #{const KEY_H}
  fromEnum I              = #{const KEY_I}
  fromEnum J              = #{const KEY_J}
  fromEnum K              = #{const KEY_K}
  fromEnum L              = #{const KEY_L}
  fromEnum M              = #{const KEY_M}
  fromEnum N              = #{const KEY_N}
  fromEnum O              = #{const KEY_O}
  fromEnum P              = #{const KEY_P}
  fromEnum Q              = #{const KEY_Q}
  fromEnum R              = #{const KEY_R}
  fromEnum S              = #{const KEY_S}
  fromEnum T              = #{const KEY_T}
  fromEnum U              = #{const KEY_U}
  fromEnum V              = #{const KEY_V}
  fromEnum W              = #{const KEY_W}
  fromEnum X              = #{const KEY_X}
  fromEnum Y              = #{const KEY_Y}
  fromEnum Z              = #{const KEY_Z}
  fromEnum Space          = #{const KEY_SPACE}
  fromEnum Escape         = #{const KEY_ESCAPE}
  fromEnum Enter          = #{const KEY_ENTER}
  fromEnum Tab            = #{const KEY_TAB}
  fromEnum Backspace      = #{const KEY_BACKSPACE}
  fromEnum Insert         = #{const KEY_INSERT}
  fromEnum Delete         = #{const KEY_DELETE}
  fromEnum ArrowRight     = #{const KEY_RIGHT}
  fromEnum ArrowLeft      = #{const KEY_LEFT}
  fromEnum ArrowDown      = #{const KEY_DOWN}
  fromEnum ArrowUp        = #{const KEY_UP}
  fromEnum PageUp         = #{const KEY_PAGE_UP}
  fromEnum PageDown       = #{const KEY_PAGE_DOWN}
  fromEnum Home           = #{const KEY_HOME}
  fromEnum End            = #{const KEY_END}
  fromEnum CapsLock       = #{const KEY_CAPS_LOCK}
  fromEnum ScrollLock     = #{const KEY_SCROLL_LOCK}
  fromEnum NumLock        = #{const KEY_NUM_LOCK}
  fromEnum PrintScreen    = #{const KEY_PRINT_SCREEN}
  fromEnum Pause          = #{const KEY_PAUSE}
  fromEnum F1             = #{const KEY_F1}
  fromEnum F2             = #{const KEY_F2}
  fromEnum F3             = #{const KEY_F3}
  fromEnum F4             = #{const KEY_F4}
  fromEnum F5             = #{const KEY_F5}
  fromEnum F6             = #{const KEY_F6}
  fromEnum F7             = #{const KEY_F7}
  fromEnum F8             = #{const KEY_F8}
  fromEnum F9             = #{const KEY_F9}
  fromEnum F10            = #{const KEY_F10}
  fromEnum F11            = #{const KEY_F11}
  fromEnum F12            = #{const KEY_F12}
  fromEnum LeftShift      = #{const KEY_LEFT_SHIFT}
  fromEnum LeftControl    = #{const KEY_LEFT_CONTROL}
  fromEnum LeftAlt        = #{const KEY_LEFT_ALT}
  fromEnum LeftSuper      = #{const KEY_LEFT_SUPER}
  fromEnum RightShift     = #{const KEY_RIGHT_SHIFT}
  fromEnum RightControl   = #{const KEY_RIGHT_CONTROL}
  fromEnum RightAlt       = #{const KEY_RIGHT_ALT}
  fromEnum RightSuper     = #{const KEY_RIGHT_SUPER}
  fromEnum Menu           = #{const KEY_MENU}
  fromEnum LeftBracket    = #{const KEY_LEFT_BRACKET}
  fromEnum RightBracket   = #{const KEY_RIGHT_BRACKET}
  fromEnum Backslash      = #{const KEY_BACKSLASH}
  fromEnum Grave          = #{const KEY_GRAVE}
  fromEnum KeyPadZero     = #{const KEY_KP_0}
  fromEnum KeyPadOne      = #{const KEY_KP_1}
  fromEnum KeyPadTwo      = #{const KEY_KP_2}
  fromEnum KeyPadThree    = #{const KEY_KP_3}
  fromEnum KeyPadFour     = #{const KEY_KP_4}
  fromEnum KeyPadFive     = #{const KEY_KP_5}
  fromEnum KeyPadSix      = #{const KEY_KP_6}
  fromEnum KeyPadSeven    = #{const KEY_KP_7}
  fromEnum KeyPadEight    = #{const KEY_KP_8}
  fromEnum KeyPadNine     = #{const KEY_KP_9}
  fromEnum KeyPadDecimal  = #{const KEY_KP_DECIMAL}
  fromEnum KeyPadDivide   = #{const KEY_KP_DIVIDE}
  fromEnum KeyPadMultiply = #{const KEY_KP_MULTIPLY}
  fromEnum KeyPadSubtract = #{const KEY_KP_SUBTRACT}
  fromEnum KeyPadAdd      = #{const KEY_KP_ADD}
  fromEnum KeyPadEnter    = #{const KEY_KP_ENTER}
  fromEnum KeyPadEqual    = #{const KEY_KP_EQUAL}
  fromEnum (OtherKey i)   = i
  toEnum #{const KEY_APOSTROPHE}    = Apostrophe
  toEnum #{const KEY_COMMA}         = Comma
  toEnum #{const KEY_MINUS}         = Minus
  toEnum #{const KEY_PERIOD}        = Period
  toEnum #{const KEY_SLASH}         = Slash
  toEnum #{const KEY_ZERO}          = Zero
  toEnum #{const KEY_ONE}           = One
  toEnum #{const KEY_TWO}           = Two
  toEnum #{const KEY_THREE}         = Three
  toEnum #{const KEY_FOUR}          = Four
  toEnum #{const KEY_FIVE}          = Five
  toEnum #{const KEY_SIX}           = Six
  toEnum #{const KEY_SEVEN}         = Seven
  toEnum #{const KEY_EIGHT}         = Eight
  toEnum #{const KEY_NINE}          = Nine
  toEnum #{const KEY_SEMICOLON}     = Semicolon
  toEnum #{const KEY_EQUAL}         = Equal
  toEnum #{const KEY_A}             = A
  toEnum #{const KEY_B}             = B
  toEnum #{const KEY_C}             = C
  toEnum #{const KEY_D}             = D
  toEnum #{const KEY_E}             = E
  toEnum #{const KEY_F}             = F
  toEnum #{const KEY_G}             = G
  toEnum #{const KEY_H}             = H
  toEnum #{const KEY_I}             = I
  toEnum #{const KEY_J}             = J
  toEnum #{const KEY_K}             = K
  toEnum #{const KEY_L}             = L
  toEnum #{const KEY_M}             = M
  toEnum #{const KEY_N}             = N
  toEnum #{const KEY_O}             = O
  toEnum #{const KEY_P}             = P
  toEnum #{const KEY_Q}             = Q
  toEnum #{const KEY_R}             = R
  toEnum #{const KEY_S}             = S
  toEnum #{const KEY_T}             = T
  toEnum #{const KEY_U}             = U
  toEnum #{const KEY_V}             = V
  toEnum #{const KEY_W}             = W
  toEnum #{const KEY_X}             = X
  toEnum #{const KEY_Y}             = Y
  toEnum #{const KEY_Z}             = Z
  toEnum #{const KEY_SPACE}         = Space
  toEnum #{const KEY_ESCAPE}        = Escape
  toEnum #{const KEY_ENTER}         = Enter
  toEnum #{const KEY_TAB}           = Tab
  toEnum #{const KEY_BACKSPACE}     = Backspace
  toEnum #{const KEY_INSERT}        = Insert
  toEnum #{const KEY_DELETE}        = Delete
  toEnum #{const KEY_RIGHT}         = ArrowRight
  toEnum #{const KEY_LEFT}          = ArrowLeft
  toEnum #{const KEY_DOWN}          = ArrowDown
  toEnum #{const KEY_UP}            = ArrowUp
  toEnum #{const KEY_PAGE_UP}       = PageUp
  toEnum #{const KEY_PAGE_DOWN}     = PageDown
  toEnum #{const KEY_HOME}          = Home
  toEnum #{const KEY_END}           = End
  toEnum #{const KEY_CAPS_LOCK}     = CapsLock
  toEnum #{const KEY_SCROLL_LOCK}   = ScrollLock
  toEnum #{const KEY_NUM_LOCK}      = NumLock
  toEnum #{const KEY_PRINT_SCREEN}  = PrintScreen
  toEnum #{const KEY_PAUSE}         = Pause
  toEnum #{const KEY_F1}            = F1
  toEnum #{const KEY_F2}            = F2
  toEnum #{const KEY_F3}            = F3
  toEnum #{const KEY_F4}            = F4
  toEnum #{const KEY_F5}            = F5
  toEnum #{const KEY_F6}            = F6
  toEnum #{const KEY_F7}            = F7
  toEnum #{const KEY_F8}            = F8
  toEnum #{const KEY_F9}            = F9
  toEnum #{const KEY_F10}           = F10
  toEnum #{const KEY_F11}           = F11
  toEnum #{const KEY_F12}           = F12
  toEnum #{const KEY_LEFT_SHIFT}    = LeftShift
  toEnum #{const KEY_LEFT_CONTROL}  = LeftControl
  toEnum #{const KEY_LEFT_ALT}      = LeftAlt
  toEnum #{const KEY_LEFT_SUPER}    = LeftSuper
  toEnum #{const KEY_RIGHT_SHIFT}   = RightShift
  toEnum #{const KEY_RIGHT_CONTROL} = RightControl
  toEnum #{const KEY_RIGHT_ALT}     = RightAlt
  toEnum #{const KEY_RIGHT_SUPER}   = RightSuper
  toEnum #{const KEY_MENU}          = Menu
  toEnum #{const KEY_LEFT_BRACKET}  = LeftBracket
  toEnum #{const KEY_RIGHT_BRACKET} = RightBracket
  toEnum #{const KEY_BACKSLASH}     = Backslash
  toEnum #{const KEY_GRAVE}         = Grave
  toEnum #{const KEY_KP_0}          = KeyPadZero
  toEnum #{const KEY_KP_1}          = KeyPadOne
  toEnum #{const KEY_KP_2}          = KeyPadTwo
  toEnum #{const KEY_KP_3}          = KeyPadThree
  toEnum #{const KEY_KP_4}          = KeyPadFour
  toEnum #{const KEY_KP_5}          = KeyPadFive
  toEnum #{const KEY_KP_6}          = KeyPadSix
  toEnum #{const KEY_KP_7}          = KeyPadSeven
  toEnum #{const KEY_KP_8}          = KeyPadEight
  toEnum #{const KEY_KP_9}          = KeyPadNine
  toEnum #{const KEY_KP_DECIMAL}    = KeyPadDecimal
  toEnum #{const KEY_KP_DIVIDE}     = KeyPadDivide
  toEnum #{const KEY_KP_MULTIPLY}   = KeyPadMultiply
  toEnum #{const KEY_KP_SUBTRACT}   = KeyPadSubtract
  toEnum #{const KEY_KP_ADD}        = KeyPadAdd
  toEnum #{const KEY_KP_ENTER}      = KeyPadEnter
  toEnum #{const KEY_KP_EQUAL}      = KeyPadEqual
  toEnum i                          = OtherKey i

noKey :: KeyboardKey
noKey = OtherKey 0