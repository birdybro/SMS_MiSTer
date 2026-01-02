library IEEE;
use IEEE.std_logic_1164.all;  
use IEEE.numeric_std.all;     

use work.pBus_savestates.all;

package pReg_savestates_sms is

   --   (                                                   adr   upper    lower    size   default)  

   -- CPU core registers (Z80)
   constant REG_SAVESTATE_CPU0           : regmap_type := (  0,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_CPU1           : regmap_type := (  1,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_CPU2           : regmap_type := (  2,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_CPU3           : regmap_type := (  3,   63,      0,        1, x"0000000000000000");

   -- System control / mapper flags (bank registers, nvram enables, etc.)
   constant REG_SAVESTATE_SYS0           : regmap_type := (  4,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_SYS1           : regmap_type := (  5,   31,      0,        1, x"0000000000000000");

   -- VDP registers (primary VDP)
   constant REG_SAVESTATE_VDP0           : regmap_type := (  6,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_VDP1           : regmap_type := (  7,   63,      0,        1, x"0000000000000000");

   -- VDP2 (System-E / dual VDP mode)
   constant REG_SAVESTATE_VDP2_0         : regmap_type := (  8,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_VDP2_1         : regmap_type := (  9,   63,      0,        1, x"0000000000000000");

   -- PSG / FM blocks
   constant REG_SAVESTATE_PSG0           : regmap_type := ( 10,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_PSG1           : regmap_type := ( 11,   63,      0,        1, x"0000000000000000");
   constant REG_SAVESTATE_FM0            : regmap_type := ( 12,   63,      0,       12, x"0000000000000000"); -- multiple words reserved

end package;
