------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                  A S I S T A N T . I N T E R F A C I N G                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1997-2013, Free Software Foundation, Inc.         --
--                                                                          --
-- ASIStant is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIStant is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- ASIStant is an evolution of ASIStint tool that was created by            --
-- Vasiliy Fofanov as part of a collaboration between Software Engineering  --
-- Laboratory of the Swiss Federal Institute of Technology in Lausanne,     --
-- Switzerland, and the Scientific Research Computer Center of the Moscow   --
-- University, Russia, supported by the Swiss National Science Foundation   --
-- grant #7SUPJ048247, "Development of ASIS for GNAT with industry quality" --
--                                                                          --
-- ASIStant is distributed as a part of the ASIS implementation for GNAT    --
-- (ASIS-for-GNAT) and is maintained by AdaCore (http://www.adacore.com).   --
--                                                                          --
------------------------------------------------------------------------------

package ASIStant.Interfacing is

------------------------------------------------------------------------------
--  This package is an upper-level interface to ASIStant
------------------------------------------------------------------------------

   function Parse_Cmd_Line (ScriptName : Wide_String := "";
                            No_Cmd_Line : Boolean := False) return Integer;
   --  This is a single entry point to ASIStant. A call to this procedure
   --  activates ASIStant and starts its session. If ScriptName is
   --  specified, the file is treated as an ASIStant script; otherwise,
   --  interactive session is started.
   --  If No_Cmd_Line is set to True ASIStant will always quit whenever
   --  it would normally switch to interactive mode.

end ASIStant.Interfacing;
