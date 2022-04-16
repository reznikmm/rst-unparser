--  SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------

with Ada.Containers.Multiway_Trees;

with League.Strings;
with League.String_Vectors;
with XML.SAX.Attributes;
with XML.SAX.Content_Handlers;
with XML.SAX.Locators;

package Events_Printers is

   type DOM_Element_Kind is (Character_Kind, Element_Kind);

   type DOM_Element (Kind : DOM_Element_Kind := Character_Kind) is record
      case Kind is
         when Character_Kind =>
            Lines          : League.String_Vectors.Universal_String_Vector;

         when Element_Kind =>
            Qualified_Name : League.Strings.Universal_String;
            Attributes     : XML.SAX.Attributes.SAX_Attributes;

      end case;
   end record;

   package DOM_Trees is new Ada.Containers.Multiway_Trees (DOM_Element);

   type Events_Printer is limited
     new XML.SAX.Content_Handlers.SAX_Content_Handler with
   record
      DOM_Tree  : DOM_Trees.Tree;
      Current   : DOM_Trees.Cursor;
      Lines     : League.Strings.Universal_String;
      Locator   : XML.SAX.Locators.SAX_Locator;
   end record;

   overriding procedure Characters
    (Self    : in out Events_Printer;
     Text    : League.Strings.Universal_String;
     Success : in out Boolean);

   overriding procedure End_Element
    (Self           : in out Events_Printer;
     Namespace_URI  : League.Strings.Universal_String;
     Local_Name     : League.Strings.Universal_String;
     Qualified_Name : League.Strings.Universal_String;
     Success        : in out Boolean);

   overriding procedure End_Prefix_Mapping
    (Self    : in out Events_Printer;
     Prefix  : League.Strings.Universal_String;
     Success : in out Boolean);

   overriding function Error_String
    (Self : Events_Printer)
       return League.Strings.Universal_String;

   overriding procedure Ignorable_Whitespace
    (Self    : in out Events_Printer;
     Text    : League.Strings.Universal_String;
     Success : in out Boolean);

   overriding procedure Processing_Instruction
    (Self    : in out Events_Printer;
     Target  : League.Strings.Universal_String;
     Data    : League.Strings.Universal_String;
     Success : in out Boolean);

   overriding procedure Set_Document_Locator
    (Self    : in out Events_Printer;
     Locator : XML.SAX.Locators.SAX_Locator);

   overriding procedure Start_Element
    (Self           : in out Events_Printer;
     Namespace_URI  : League.Strings.Universal_String;
     Local_Name     : League.Strings.Universal_String;
     Qualified_Name : League.Strings.Universal_String;
     Attributes     : XML.SAX.Attributes.SAX_Attributes;
     Success        : in out Boolean);

   overriding procedure Start_Prefix_Mapping
    (Self          : in out Events_Printer;
     Prefix        : League.Strings.Universal_String;
     Namespace_URI : League.Strings.Universal_String;
     Success       : in out Boolean);

end Events_Printers;
