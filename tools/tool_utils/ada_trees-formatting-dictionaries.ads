------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--                  G N A T P P . D I C T I O N A R I E S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
--                                                                          --
-- GNATPP is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNATPP is  distributed in the  hope that it will  be  useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or  FITNESS  FOR A  PARTICULAR  PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston,                                                                  --
--                                                                          --
-- GNATPP is maintained by ACT Europe (http://www.act-europe.fr).           --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines for dealing with the casing exception
--  dictionaries

with Asis; use Asis;

with Ada_Trees.Formatting; use Ada_Trees.Formatting;

package Ada_Trees.Formatting.Dictionaries is

   procedure Scan_Dictionary (Dictionary_Name : String);
   --  Scans the dictionary file which hame is passed as a parameter and stores
   --  all the casing exceptions in the exception tables.

   procedure Check_With_Dictionary
     (Ada_Name : in out Program_Text;
      Casing   : PP_Casing);
   --  Checks if Ada_Name as a whole or some its subname (that is, a part of
   --  the AdA_Name surrounded by '_' is in exception dictionary, and if it is,
   --  changes the casing of Ada_Name or of its part to what is defined in the
   --  dictionary. For the names or name parts which are not in the dictionary,
   --  changes their casing according to the value of Casing parameter

end Ada_Trees.Formatting.Dictionaries;
