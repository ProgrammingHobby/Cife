{***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************}
unit XMLSettings;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Laz2_DOM, laz2_XMLRead, laz2_XMLWrite, Forms;

type
    TXMLSettings = class

    public    // Attribute

    public    // Methoden
        procedure Clear;
        procedure Flush;
        procedure OpenKey(const aPath: DOMString);
        procedure CloseKey;
        procedure ResetKey;
        procedure SaveFormState(var Form: TForm);
        procedure RestoreFormState(var Form: TForm);
        function GetValue(const APath: DOMString; const ADefault: DOMString): DOMString; overload;
        function GetValue(const APath: DOMString; ADefault: integer): integer; overload;
        function GetValue(const APath: DOMString; ADefault: boolean): boolean; overload;
        function GetAttribute(const APath: DOMString; const ADefault: DOMString): DOMString; overload;
        function GetAttribute(const APath: DOMString; ADefault: integer): integer; overload;
        function GetAttribute(const APath: DOMString; ADefault: boolean): boolean; overload;
        procedure SetValue(const APath: DOMString; const AValue: DOMString); overload;
        procedure SetValue(const APath: DOMString; AValue: integer); overload;
        procedure SetValue(const APath: DOMString; AValue: boolean); overload;
        procedure SetAttribute(const APath: DOMString; const AValue: DOMString); overload;
        procedure SetAttribute(const APath: DOMString; AValue: integer); overload;
        procedure SetAttribute(const APath: DOMString; AValue: boolean); overload;
        procedure SetDeleteValue(const APath: DOMString; const AValue, DefValue: DOMString); overload;
        procedure SetDeleteValue(const APath: DOMString; AValue, DefValue: integer); overload;
        procedure SetDeleteValue(const APath: DOMString; AValue, DefValue: boolean); overload;
        procedure SetDeleteAttribute(const APath: DOMString; const AValue, DefValue: DOMString); overload;
        procedure SetDeleteAttribute(const APath: DOMString; AValue, DefValue: integer); overload;
        procedure SetDeleteAttribute(const APath: DOMString; AValue, DefValue: boolean); overload;
        procedure DeletePath(const APath: DOMString);
        procedure DeleteValue(const APath: DOMString);
        procedure DeleteAttribute(const APath: DOMString);

    public  // Konstruktor/Destruktor
        constructor Create(const AFilename: string);
        destructor Destroy; override;

    protected // Attribute

    protected // Methoden

    private   // Attribute
    type
        TNodeFlags = set of (nfHasValue, nfWriteAccess);

    var
        m_Modified: boolean;
        m_FileName: string;
        m_RootName: DOMString;
        m_XmlDoc: TXMLDocument;
        m_NodePathDirty: boolean;
        m_NodePathCount: integer;
        m_NodePathStack: array of DOMString;
        m_DomElement: TDOMElement;

    private   // Methoden
        function CompareDOMStrings(const s1, s2: DOMPChar; l1, l2: integer): integer;
        function FindNode(const APath: DOMString; out Ident: DOMString; Flags: TNodeFlags): TDOMElement;
        function DoFindNode(const APath: DOMString; var Ident: DOMString; Flags: TNodeFlags): TDomElement;

    end;

implementation

uses StrUtils;

// --------------------------------------------------------------------------------
procedure TXMLSettings.Clear;
begin
    m_XmlDoc.ReplaceChild(m_XmlDoc.CreateElement(m_RootName), m_XmlDoc.DocumentElement);
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.Flush;
begin
    if (m_Modified and not m_FileName.IsEmpty) then begin
        WriteXMLFile(m_XmlDoc, m_FileName);
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.OpenKey(const aPath: DOMString);
begin
    if not aPath.IsEmpty then begin
        if m_NodePathCount >= Length(m_NodePathStack) then begin
            SetLength(m_NodePathStack, m_NodePathCount + 5);
        end;
        m_NodePathStack[m_NodePathCount] := APath;
        Inc(m_NodePathCount);
        m_DomElement := nil;
        m_NodePathDirty := True;
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.CloseKey;
begin
    if m_NodePathCount > 0 then begin
        m_NodePathStack[m_NodePathCount - 1] := '';
        Dec(m_NodePathCount);
        m_DomElement := nil;
        m_NodePathDirty := True;
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.ResetKey;
var
    I: integer;
begin
    for I := Length(m_NodePathStack) - 1 downto 0 do begin
        m_NodePathStack[I] := '';
    end;
    m_DomElement := nil;
    m_NodePathDirty := False;
    m_NodePathCount := 0;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.SaveFormState(var Form: TForm);
var
    FormState: string;
begin
    FormState := IntToHex(Form.Left, 6);
    FormState := FormState + IntToHex(Form.Top, 6);
    FormState := FormState + IntToHex(Form.Width, 6);
    FormState := FormState + IntToHex(Form.Height, 6);
    FormState := FormState + IntToHex(Form.RestoredLeft, 6);
    FormState := FormState + IntToHex(Form.RestoredTop, 6);
    FormState := FormState + IntToHex(Form.RestoredWidth, 6);
    FormState := FormState + IntToHex(Form.RestoredHeight, 6);
    FormState := FormState + IntToHex(integer(Form.WindowState), 6);
    OpenKey('Forms/' + Form.Name);
    SetValue('State', FormState);
    CloseKey;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.RestoreFormState(var Form: TForm);
var
    LastWindowState: TWindowState;
    FormState: string;
begin
    OpenKey('Forms/' + Form.Name);
    FormState := GetValue('State', '000000000000000000000000000000000000000000000000000000');
    CloseKey;
    LastWindowState := TWindowState(Hex2Dec(FormState.Substring(48, 6)));
    if LastWindowState = wsMaximized then begin
        Form.WindowState := wsNormal;
        Form.BoundsRect := Bounds(Hex2Dec(FormState.Substring(24, 6)), Hex2Dec(FormState.Substring(30, 6)),
            Hex2Dec(FormState.Substring(36, 6)), Hex2Dec(FormState.Substring(42, 6)));
        Form.WindowState := wsMaximized;
    end
    else begin
        Form.WindowState := wsNormal;
        Form.BoundsRect := Bounds(Hex2Dec(FormState.Substring(0, 6)), Hex2Dec(FormState.Substring(6, 6)),
            Hex2Dec(FormState.Substring(12, 6)), Hex2Dec(FormState.Substring(18, 6)));
    end;
end;

// --------------------------------------------------------------------------------
function TXMLSettings.GetValue(const APath: DOMString; const ADefault: DOMString): DOMString;
var
    Node: TDOMElement;
    Data: TDOMNode;
    Ident: DOMString;
begin
    Result := ADefault;
    Node := FindNode(APath, Ident, [nfHasValue]);
    Data := Node.FindNode(Ident);
    if Assigned(Data) then begin
        Result := Data.TextContent;
    end;
end;

// --------------------------------------------------------------------------------
function TXMLSettings.GetValue(const APath: DOMString; ADefault: integer): integer;
begin
    Result := StrToIntDef(string(GetValue(APath, '')), ADefault);
end;

// --------------------------------------------------------------------------------
function TXMLSettings.GetValue(const APath: DOMString; ADefault: boolean): boolean;
var
    Value: DOMString;
begin
    Value := GetValue(APath, '');
    if SameText(Value, 'true') then begin
        Result := True;
    end
    else if SameText(Value, 'false') then begin
        Result := False;
    end
    else begin
        Result := ADefault;
    end;
end;

// --------------------------------------------------------------------------------
function TXMLSettings.GetAttribute(const APath: DOMString; const ADefault: DOMString): DOMString;
var
    Node: TDOMElement;
    Attr: TDOMAttr;
    Ident: DOMString;
begin
    Result := ADefault;
    Node := FindNode(APath, Ident, [nfHasValue]);
    if Assigned(Node) then begin
        Attr := Node.GetAttributeNode(Ident);
        if Assigned(Attr) then begin
            Result := Attr.NodeValue;
        end;
    end;
end;

// --------------------------------------------------------------------------------
function TXMLSettings.GetAttribute(const APath: DOMString; ADefault: integer): integer;
begin
    Result := StrToIntDef(GetAttribute(APath, ''), ADefault);
end;

// --------------------------------------------------------------------------------
function TXMLSettings.GetAttribute(const APath: DOMString; ADefault: boolean): boolean;
var
    s: DOMString;
begin
    s := GetAttribute(APath, '');

    if SameText(s, 'true') then begin
        Result := True;
    end
    else if SameText(s, 'true') then begin
        Result := False;
    end
    else begin
        Result := ADefault;
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.SetValue(const APath: DOMString; const AValue: DOMString);
var
    Node: TDOMElement;
    KeyNode, DataNode, OldNode: TDOMNode;
    Ident: DOMString;
begin
    Node := FindNode(APath, Ident, [nfHasValue, nfWriteAccess]);
    KeyNode := Node.FindNode(Ident);
    if Assigned(KeyNode) then begin
        if KeyNode.FirstChild.NodeType = TEXT_NODE then begin
            OldNode := KeyNode.FirstChild;
            DataNode := m_XmlDoc.CreateTextNode(AValue);
            if (KeyNode.ReplaceChild(DataNode, OldNode) <> nil) then begin
                m_Modified := True;
            end;
        end;
    end
    else begin
        KeyNode := m_XmlDoc.CreateElement(Ident);
        DataNode := m_XmlDoc.CreateTextNode(AValue);
        if (KeyNode.AppendChild(DataNode) <> nil) and (Node.AppendChild(KeyNode) <> nil) then begin
            m_Modified := True;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.SetValue(const APath: DOMString; AValue: integer);
begin
    SetValue(APath, IntToStr(AValue));
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.SetValue(const APath: DOMString; AValue: boolean);
begin
    if AValue then begin
        SetValue(APath, 'true');
    end
    else begin
        SetValue(APath, 'false');
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.SetAttribute(const APath: DOMString; const AValue: DOMString);
var
    Node: TDOMElement;
    Attr: TDOMAttr;
    Ident: DOMString;
begin
    Node := FindNode(APath, Ident, [nfHasValue, nfWriteAccess]);
    Attr := Node.GetAttributeNode(Ident);
    if (Attr = nil) or (Attr.NodeValue <> AValue) then begin
        Node[Ident] := AValue;
        m_Modified := True;
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.SetAttribute(const APath: DOMString; AValue: integer);
begin
    SetAttribute(APath, IntToStr(AValue));
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.SetAttribute(const APath: DOMString; AValue: boolean);
begin
    if AValue then begin
        SetAttribute(APath, 'true');
    end
    else begin
        SetAttribute(APath, 'false');
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.SetDeleteValue(const APath: DOMString; const AValue, DefValue: DOMString);
begin
    if AValue = DefValue then begin
        DeleteValue(APath);
    end
    else begin
        SetValue(APath, AValue);
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.SetDeleteValue(const APath: DOMString; AValue, DefValue: integer);
begin
    if AValue = DefValue then begin
        DeleteValue(APath);
    end
    else begin
        SetValue(APath, AValue);
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.SetDeleteValue(const APath: DOMString; AValue, DefValue: boolean);
begin
    if AValue = DefValue then begin
        DeleteValue(APath);
    end
    else begin
        SetValue(APath, AValue);
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.SetDeleteAttribute(const APath: DOMString; const AValue, DefValue: DOMString);
begin
    if AValue = DefValue then begin
        DeleteAttribute(APath);
    end
    else begin
        SetAttribute(APath, AValue);
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.SetDeleteAttribute(const APath: DOMString; AValue, DefValue: integer);
begin
    if AValue = DefValue then begin
        DeleteAttribute(APath);
    end
    else begin
        SetAttribute(APath, AValue);
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.SetDeleteAttribute(const APath: DOMString; AValue, DefValue: boolean);
begin
    if AValue = DefValue then begin
        DeleteAttribute(APath);
    end
    else begin
        SetAttribute(APath, AValue);
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.DeletePath(const APath: DOMString);
var
    Node: TDomNode;
    Ident: DOMString;
begin
    Node := FindNode(APath, Ident, []);
    if Assigned(Node) and Assigned(Node.ParentNode) then begin
        Node.ParentNode.RemoveChild(Node);
        m_NodePathDirty := True;
        m_DomElement := nil;
        m_Modified := True;
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.DeleteValue(const APath: DOMString);
var
    Node: TDOMElement;
    Parent: TDOMNode;
    Ident: DOMString;
begin
    Node := FindNode(APath, Ident, [nfHasValue]);
    if Assigned(Node) then begin
        Parent := Node.FindNode(Ident);
        if Assigned(Parent) then begin
            Node.RemoveChild(Parent);
            m_NodePathDirty := True;
            m_DomElement := nil;
            m_Modified := True;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TXMLSettings.DeleteAttribute(const APath: DOMString);
var
    Node: TDOMElement;
    Ident: DOMString;
    Parent: TDOMNode;
begin
    Node := FindNode(APath, Ident, [nfHasValue]);
    if Assigned(Node) then begin
        if Assigned(Node.GetAttributeNode(Ident)) then begin
            Node.RemoveAttribute(Ident);
            m_Modified := True;
        end;
        while (Node.FirstChild = nil) and Assigned(Node.ParentNode) and Assigned(Node.ParentNode.ParentNode) do begin
            if Node.HasAttributes then begin
                Break;
            end;
            Parent := Node.ParentNode;
            Parent.RemoveChild(Node);
            Node := TDOMElement(Parent);
            m_NodePathDirty := True;
            m_DomElement := nil;
            m_Modified := True;
        end;
    end;
end;

// --------------------------------------------------------------------------------
constructor TXMLSettings.Create(const AFilename: string);
begin
    m_FileName := AFilename;
    m_Modified := False;
    m_NodePathDirty := False;

    if FileExists(m_FileName) then begin
        ReadXMLFile(m_XmlDoc, m_FileName);
    end
    else begin
        m_RootName := 'config';
        m_XmlDoc := TXMLDocument.Create;
        m_XmlDoc.AppendChild(m_XmlDoc.CreateElement(m_RootName));
    end;
end;

// --------------------------------------------------------------------------------
destructor TXMLSettings.Destroy;
begin
    if Assigned(m_XmlDoc) then begin
        Flush;
        m_XmlDoc.Free;
    end;
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
function TXMLSettings.CompareDOMStrings(const s1, s2: DOMPChar; l1, l2: integer): integer;
var
    i: integer;
begin
    Result := l1 - l2;
    i := 0;
    while (i < l1) and (Result = 0) do begin
        Result := Ord(s1[i]) - Ord(s2[i]);
        Inc(i);
    end;
end;

// --------------------------------------------------------------------------------
function TXMLSettings.FindNode(const APath: DOMString; out Ident: DOMString; Flags: TNodeFlags): TDOMElement;
var
    i: integer;
    dummy: DOMString;
begin
    if m_NodePathDirty then begin
        for i := 0 to m_NodePathCount - 1 do begin
            m_DOMElement := DoFindNode(m_NodePathStack[i], dummy, Flags - [nfHasValue]);
        end;
        if Assigned(m_DOMElement) then begin
            m_NodePathDirty := False;
        end;
    end;
    Result := DoFindNode(APath, Ident, Flags);
end;

// --------------------------------------------------------------------------------
function TXMLSettings.DoFindNode(const APath: DOMString; var Ident: DOMString; Flags: TNodeFlags): TDomElement;
var
    StartPos, EndPos: integer;
    PathLen: integer;
    Child: TDOMNode;
begin
    if Assigned(m_DOMElement) and (Length(APath) > 0) and (APath[1] <> '/') then begin
        Result := m_DOMElement;
    end
    else begin
        Result := m_XmlDoc.DocumentElement;
    end;

    PathLen := Length(APath);
    StartPos := 1;
    if APath[StartPos] = '/' then begin
        Inc(StartPos);
    end;
    while Assigned(Result) do begin
        EndPos := StartPos;
        while (EndPos <= PathLen) and (APath[EndPos] <> '/') do begin
            Inc(EndPos);
        end;
        if (EndPos > PathLen) and (nfHasValue in Flags) then begin
            SetString(Ident, PChar(@APath[StartPos]), PathLen - StartPos + 1);
            exit;
        end;
        if EndPos = StartPos then begin
            break;
        end;
        Child := Result.FirstChild;
        while Assigned(Child) and not ((Child.NodeType = ELEMENT_NODE) and
                (0 = CompareDOMStrings(DOMPChar(TDOMElement(Child).TagName), @APath[StartPos],
                Length(TDOMElement(Child).TagName), EndPos - StartPos))) do begin
            Child := Child.NextSibling;
        end;
        if (Child = nil) and (nfWriteAccess in Flags) then begin
            Child := m_XmlDoc.CreateElementBuf(@APath[StartPos], EndPos - StartPos);
            Result.AppendChild(Child);
        end;
        Result := TDOMElement(Child);
        StartPos := EndPos + 1;
        if StartPos > PathLen then begin
            exit;
        end;
    end;
    Result := nil;
end;

// --------------------------------------------------------------------------------
end.
