------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                     G N A T 2 X M L . G N A T 2 X S D                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                  Copyright (C) 2012-2013, AdaCore, Inc.                  --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

pragma Ada_2012;

with Gnat2xml.Xsd;

procedure Gnat2xml.Gnat2xsd is
begin
   Gnat2xml.Xsd.Generate_Schema;
   Main_Done := True;
end Gnat2xml.Gnat2xsd;
