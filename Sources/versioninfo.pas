{*
 *  Copyright (C) 2023  Uwe Merker
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *}
unit VersionInfo;

{$mode objfpc}

interface

uses
    Classes, SysUtils;

function GetFileVersion: string;
function GetProductVersion: string;
function GetCompiledDate: string;
function GetCompilerInfo: string;
function GetTargetInfo: string;
function GetOS: string;
function GetResourceStrings(oStringList: TStringList): boolean;
function GetLCLVersion: string;
function GetWidgetSet: string;

const
    WIDGETSET_GTK = 'GTK widget set';
    WIDGETSET_GTK2 = 'GTK 2 widget set';
    WIDGETSET_WIN = 'Win32/Win64 widget set';
    WIDGETSET_WINCE = 'WinCE widget set';
    WIDGETSET_CARBON = 'Carbon widget set';
    WIDGETSET_QT = 'QT widget set';
    WIDGETSET_fpGUI = 'fpGUI widget set';
    WIDGETSET_OTHER = 'Other gui';

implementation

uses
    resource, versiontypes, versionresource, LCLVersion, InterfaceBase, lclplatformdef;

// --------------------------------------------------------------------------------
type
    TVersionInfo = class
    private
        FBuildInfoAvailable: boolean;
        FVersResource: TVersionResource;
        function GetFixedInfo: TVersionFixedInfo;
        function GetStringFileInfo: TVersionStringFileInfo;
        function GetVarFileInfo: TVersionVarFileInfo;
    public
        constructor Create;
        destructor Destroy; override;

        procedure Load(Instance: THandle);

        property BuildInfoAvailable: boolean read FBuildInfoAvailable;

        property FixedInfo: TVersionFixedInfo read GetFixedInfo;
        property StringFileInfo: TVersionStringFileInfo read GetStringFileInfo;
        property VarFileInfo: TVersionVarFileInfo read GetVarFileInfo;
    end;

// --------------------------------------------------------------------------------
function GetWidgetSet: string;
begin
    case WidgetSet.LCLPlatform of
        lpGtk: Result := WIDGETSET_GTK;
        lpGtk2: Result := WIDGETSET_GTK2;
        lpWin32: Result := WIDGETSET_WIN;
        lpWinCE: Result := WIDGETSET_WINCE;
        lpCarbon: Result := WIDGETSET_CARBON;
        lpQT: Result := WIDGETSET_QT;
        lpfpGUI: Result := WIDGETSET_fpGUI;
        else Result := WIDGETSET_OTHER;
    end;
end;

// --------------------------------------------------------------------------------
function GetCompilerInfo: string;
begin
    Result := 'FPC ' + {$I %FPCVERSION%};
end;

// --------------------------------------------------------------------------------
function GetTargetInfo: string;
begin
    Result := {$I %FPCTARGETCPU%} + ' - ' + {$I %FPCTARGETOS%};
end;

// --------------------------------------------------------------------------------
function GetOS: string;
begin
    Result := {$I %FPCTARGETOS%};
end;

// --------------------------------------------------------------------------------
function GetLCLVersion: string;
begin
    Result := 'LCL ' + lcl_version;
end;

// --------------------------------------------------------------------------------
function GetCompiledDate: string;
var
    sDate, sTime: string;
begin
    sDate := {$I %DATE%};
    sTime := {$I %TIME%};

    Result := sDate + ' at ' + sTime;
end;

{ Routines to expose TVersionInfo data }

var
    FInfo: TVersionInfo;

// --------------------------------------------------------------------------------
procedure CreateInfo;
begin
    if not Assigned(FInfo) then begin
        FInfo := TVersionInfo.Create;
        FInfo.Load(HINSTANCE);
    end;
end;

// --------------------------------------------------------------------------------
function GetResourceStrings(oStringList: TStringList): boolean;
var
    i, j: integer;
    oTable: TVersionStringTable;
begin
    CreateInfo;

    oStringList.Clear;
    Result := False;

    if FInfo.BuildInfoAvailable then begin
        Result := True;
        for i := 0 to FInfo.StringFileInfo.Count - 1 do begin
            oTable := FInfo.StringFileInfo.Items[i];

            for j := 0 to oTable.Count - 1 do
                if Trim(oTable.ValuesByIndex[j]) <> '' then
                    oStringList.Values[oTable.Keys[j]] := oTable.ValuesByIndex[j];
        end;
    end;
end;

// --------------------------------------------------------------------------------
function ProductVersionToString(PV: TFileProductVersion): string;
begin
    Result := Format('%d.%d.%d.%d', [PV[0], PV[1], PV[2], PV[3]]);
end;

// --------------------------------------------------------------------------------
function GetProductVersion: string;
begin
    CreateInfo;

    if FInfo.BuildInfoAvailable then
        Result := ProductVersionToString(FInfo.FixedInfo.ProductVersion)
    else
        Result := 'No build information available';
end;

// --------------------------------------------------------------------------------
function GetFileVersion: string;
begin
    CreateInfo;

    if FInfo.BuildInfoAvailable then
        Result := ProductVersionToString(FInfo.FixedInfo.FileVersion)
    else
        Result := 'No build information available';
end;

{ TVersionInfo }

// --------------------------------------------------------------------------------
function TVersionInfo.GetFixedInfo: TVersionFixedInfo;
begin
    Result := FVersResource.FixedInfo;
end;

// --------------------------------------------------------------------------------
function TVersionInfo.GetStringFileInfo: TVersionStringFileInfo;
begin
    Result := FVersResource.StringFileInfo;
end;

// --------------------------------------------------------------------------------
function TVersionInfo.GetVarFileInfo: TVersionVarFileInfo;
begin
    Result := FVersResource.VarFileInfo;
end;

// --------------------------------------------------------------------------------
constructor TVersionInfo.Create;
begin
    inherited Create;

    FVersResource := TVersionResource.Create;
    FBuildInfoAvailable := False;
end;

// --------------------------------------------------------------------------------
destructor TVersionInfo.Destroy;
begin
    FVersResource.Free;

    inherited Destroy;
end;

// --------------------------------------------------------------------------------
procedure TVersionInfo.Load(Instance: THandle);
var
    Stream: TResourceStream;
    ResID: integer;
    Res: TFPResourceHandle;
begin
    FBuildInfoAvailable := False;
    ResID := 1;

    // Defensive code to prevent failure if no resource available...
    Res := FindResource(Instance, PChar(PtrInt(ResID)), PChar(RT_VERSION));
    if Res = 0 then
        Exit;

    Stream := TResourceStream.CreateFromID(Instance, ResID, PChar(RT_VERSION));
    try
        FVersResource.SetCustomRawDataStream(Stream);

        // access some property to load from the stream
        FVersResource.FixedInfo;

        // clear the stream
        FVersResource.SetCustomRawDataStream(nil);

        FBuildInfoAvailable := True;
    finally
        Stream.Free;
    end;
end;

// --------------------------------------------------------------------------------
initialization
    FInfo := nil;

finalization
    if Assigned(FInfo) then
        FInfo.Free;
end.
