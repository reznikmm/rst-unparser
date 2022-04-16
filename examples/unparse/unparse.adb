--  SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.Strings;

with XML.SAX.Constants;
with XML.SAX.Input_Sources.Streams.Files;
with XML.SAX.Simple_Readers;
with XML.SAX.String_Output_Destinations;

with RST_Unparsers;

procedure Unparse is
   File     : constant League.Strings.Universal_String :=
     League.Application.Arguments.Element (1);
   Output   : aliased
     XML.SAX.String_Output_Destinations.String_Output_Destination;
   Unparser : aliased RST_Unparsers.RST_Unparser (Output'Access);
   Source   : aliased XML.SAX.Input_Sources.Streams.Files.File_Input_Source;
   Reader   : aliased XML.SAX.Simple_Readers.Simple_Reader;
begin
   RST_Unparsers.Read_URI_Map
     (Unparser, League.Application.Arguments.Element (2));

   Reader.Set_Feature (XML.SAX.Constants.Load_External_DTD_Feature, False);
   Reader.Set_Content_Handler (Unparser'Unchecked_Access);
   Source.Open_By_File_Name (File);
   Reader.Parse (Source'Access);

   Ada.Wide_Wide_Text_IO.Put_Line (Output.Get_Text.To_Wide_Wide_String);
end Unparse;
