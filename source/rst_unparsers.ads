--  SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

with League.String_Vectors;
with League.Strings;
with League.Strings.Hash;

with XML.SAX.Attributes;
with XML.SAX.Content_Handlers;
with XML.SAX.Locators;
with XML.SAX.Output_Destinations;
with XML.SAX.String_Output_Destinations;

package RST_Unparsers is

   type RST_Unparser
     (Output : access XML.SAX.Output_Destinations.SAX_Output_Destination'Class)
   is limited new XML.SAX.Content_Handlers.SAX_Content_Handler with private;

   procedure Read_URI_Map
     (Handler : in out RST_Unparsers.RST_Unparser;
      File    : League.Strings.Universal_String);

   overriding procedure Characters
    (Self    : in out RST_Unparser;
     Text    : League.Strings.Universal_String;
     Success : in out Boolean);

   overriding procedure End_Element
    (Self           : in out RST_Unparser;
     Namespace_URI  : League.Strings.Universal_String;
     Local_Name     : League.Strings.Universal_String;
     Qualified_Name : League.Strings.Universal_String;
     Success        : in out Boolean);

   overriding function Error_String
    (Self : RST_Unparser)
       return League.Strings.Universal_String;

   overriding procedure Start_Document
    (Self    : in out RST_Unparser;
     Success : in out Boolean);

   overriding procedure Start_Element
    (Self           : in out RST_Unparser;
     Namespace_URI  : League.Strings.Universal_String;
     Local_Name     : League.Strings.Universal_String;
     Qualified_Name : League.Strings.Universal_String;
     Attributes     : XML.SAX.Attributes.SAX_Attributes;
     Success        : in out Boolean);

   overriding procedure Set_Document_Locator
    (Self    : in out RST_Unparser;
     Locator : XML.SAX.Locators.SAX_Locator);

private

   subtype Column_Count is Natural range 0 .. 5;
   type Natural_Array is array (Column_Count range <>) of Positive;

   type String_Output_Destination_Access is access all
     XML.SAX.String_Output_Destinations.String_Output_Destination;

   type Table_State (Columns : Column_Count := 0) is record
      Parent : access XML.SAX.Output_Destinations.SAX_Output_Destination'Class;
      Output : String_Output_Destination_Access;
      Column : Column_Count := 0;
      Text   : League.String_Vectors.Universal_String_Vector;
      Width  : Natural_Array (1 .. Columns);
   end record;

   type Unparser_State_Kind is
     (Document, Section, Admonition, Hint, Note,
      Title, Paragraph, Inline, Literal,
      Emphasis, Strong, Reference,
      Attention, Literal_Block, Bullet_List, Enumerated_List, List_Item,
      Table, Row, Column, Ignore);

   type Unparser_State (Kind : Unparser_State_Kind := Document) is record
      case Kind is
         when Literal =>
            Empty_Role : Boolean;
         when Reference =>
            URI        : League.Strings.Universal_String;
            Internal   : Boolean;
         when Table =>
            Table      : Table_State;
         when Enumerated_List =>
            Item       : Positive;
         when others =>
            null;
      end case;
   end record;

   package State_Vectors is
     new Ada.Containers.Vectors (Positive, Unparser_State);

   package String_Maps is new Ada.Containers.Hashed_Maps
     (League.Strings.Universal_String,
      League.Strings.Universal_String,
      League.Strings.Hash,
      League.Strings."=",
      League.Strings."=");

   type RST_Unparser
     (Output : access XML.SAX.Output_Destinations.SAX_Output_Destination'Class)
   is limited new XML.SAX.Content_Handlers.SAX_Content_Handler with record
      Current    : access
        XML.SAX.Output_Destinations.SAX_Output_Destination'Class;
      Stack      : State_Vectors.Vector;
      Characters : League.Strings.Universal_String;
      Locator    : XML.SAX.Locators.SAX_Locator;
      URI_Map    : String_Maps.Map;
      Table      : Table_State;
   end record;

end RST_Unparsers;
