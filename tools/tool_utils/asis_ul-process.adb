------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                      A S I S _ U L . P R O C E S S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2004-2007, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING. If not, write to the Free Software Foundation, 59 Temple Place  --
--  - Suite 330, Boston,                                                    --
--                                                                          --
-- ASIS UL is maintained by ACT Europe (http://www.act-europe.fr).          --
--                                                                          --
------------------------------------------------------------------------------

with ASIS_UL.Environment;

package body ASIS_UL.Process is

   procedure ASIS_Processing;
   --  This procedure incapsulates all the actions performed in the opened
   --  Context with ASIS_UL.Common.The_CU pointing to the main unit of the tree
   --  that makes up this Context. Each tool should provide its own subunit
   --  as the actual implementation of this routine.

   ---------------------
   -- ASIS_Processing --
   ---------------------

   procedure ASIS_Processing is separate;

   ---------------------
   -- Process_One_Arg --
   ---------------------

   procedure Process_One_Arg is
      Success : Boolean;
   begin
      ASIS_UL.Environment.Prepare_Context (Success);

      if Success then
         ASIS_Processing;
      end if;
   end Process_One_Arg;

end ASIS_UL.Process;
