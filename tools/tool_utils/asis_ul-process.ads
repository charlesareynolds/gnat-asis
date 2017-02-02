------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                      A S I S _ U L . P R O C E S S                       --
--                                                                          --
--                                 S p e c                                  --
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

--  This package contains "templates" for the main processing routine(s) of
--  a tool. For each tool the needed routine(s) should be (re)written by
--  providing the corresponding subunit

package ASIS_UL.Process is

   procedure Process_One_Arg;
   --  This routine is supposed to implement the main ASIS-based processing for
   --  the tool provided that the tool has only one argument that is
   --  stored in ASIS_UL.Common.Arg_File

end ASIS_UL.Process;
