library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

use work.pBus_savestates.all;

package pReg_savestates is

   --   (                                                   adr   upper    lower    size   default)

   -- T80 CPU (212 bits total, split across 4 DWORDS)
   constant REG_SAVESTATE_T80_1           : regmap_type := (  0,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_T80_2           : regmap_type := (  1,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_T80_3           : regmap_type := (  2,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_T80_4           : regmap_type := (  3,   19,      0,        1, x"0000000000000000");

   -- System state (banking, mappers, control)
   constant REG_SAVESTATE_SysBank         : regmap_type := (  4,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_SysMapper       : regmap_type := (  5,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_SysCtrl         : regmap_type := (  6,   63,      0,        1, x"0000000000000000");

   -- VDP1 registers and state
   constant REG_SAVESTATE_VDP1_Ctrl1      : regmap_type := (  7,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_VDP1_Ctrl2      : regmap_type := (  8,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_VDP1_State1     : regmap_type := (  9,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_VDP1_State2     : regmap_type := ( 10,   63,      0,        1, x"0000000000000000");

   -- VDP2 registers and state (for System E dual VDP)
   constant REG_SAVESTATE_VDP2_Ctrl1      : regmap_type := ( 11,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_VDP2_Ctrl2      : regmap_type := ( 12,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_VDP2_State1     : regmap_type := ( 13,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_VDP2_State2     : regmap_type := ( 14,   63,      0,        1, x"0000000000000000");

   -- PSG1 state (3 tone channels + 1 noise channel)
   constant REG_SAVESTATE_PSG1_Ch0        : regmap_type := ( 15,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_PSG1_Ch1        : regmap_type := ( 16,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_PSG1_Ch2        : regmap_type := ( 17,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_PSG1_Noise      : regmap_type := ( 18,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_PSG1_Mux        : regmap_type := ( 19,   63,      0,        1, x"0000000000000000");

   -- PSG2 state (for System E)
   constant REG_SAVESTATE_PSG2_Ch0        : regmap_type := ( 20,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_PSG2_Ch1        : regmap_type := ( 21,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_PSG2_Ch2        : regmap_type := ( 22,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_PSG2_Noise      : regmap_type := ( 23,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_PSG2_Mux        : regmap_type := ( 24,   63,      0,        1, x"0000000000000000");

   -- FM sound (VM2413/OPLL) - complex state across multiple registers
   constant REG_SAVESTATE_FM_Ctrl         : regmap_type := ( 25,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_FM_Regs1        : regmap_type := ( 26,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_FM_Regs2        : regmap_type := ( 27,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_FM_Regs3        : regmap_type := ( 28,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_FM_Regs4        : regmap_type := ( 29,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_FM_State1       : regmap_type := ( 30,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_FM_State2       : regmap_type := ( 31,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_FM_State3       : regmap_type := ( 32,   63,      0,        1, x"0000000000000000");

   -- I/O controller state
   constant REG_SAVESTATE_IO_Ctrl         : regmap_type := ( 33,   63,      0,        1, x"0000000000000000");

   -- Reserved for expansion
   constant REG_SAVESTATE_Reserved1       : regmap_type := ( 34,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_Reserved2       : regmap_type := ( 35,   63,      0,        1, x"0000000000000000");

end package;
