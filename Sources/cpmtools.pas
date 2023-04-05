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
unit CpmTools;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, CpmFileSystem, CpmDevice, CommonStructures;

type

    { TCpmTools }

    TCpmTools = class
    public    // Attribute

    public    // Methoden
        function OpenImage(const AFileName: string; const AFileType: string; AUpperCase: boolean): boolean;
        function GetFileSystemInfo: TFileSystemInfo;

    public  // Konstruktor/Destruktor
        constructor Create; overload;
        destructor Destroy; override;

    protected // Attribute

    protected // Methoden

    private   // Attribute
        FCpmDevice: TCpmDevice;
        FCpmFileSystem: TCpmFileSystem;
        FFileName: string;
        FFileType: string;

    private   // Methoden

    end;

implementation

{ TCpmTools }

uses Dialogs, Controls;

// --------------------------------------------------------------------------------
function TCpmTools.OpenImage(const AFileName: string; const AFileType: string; AUpperCase: boolean): boolean;
begin
    FFileName := AFileName;
    FFileType := AFileType;
    Result := False;

    if not (FCpmDevice.Open(AFileName, dmOpenReadWrite)) then begin
        if MessageDlg(Format('cannot open %s' + LineEnding + '(%s)', [ExtractFileName(AFileName), FCpmDevice.GetErrorMsg()])
            , mtError, [mbOK], 0) = mrOk then begin
            Exit;
        end;
    end;

    if not (FCpmFileSystem.ReadDiskdefData(AFileType)) then begin
        if MessageDlg(Format('cannot read superblock' + LineEnding + '(%s)', [FCpmFileSystem.GetErrorMsg()])
            , mtError, [mbOK], 0) = mrOk then begin
            Exit;
        end;
    end;

    if not (FCpmFileSystem.InitDriveData(AUpperCase)) then begin
        if MessageDlg(Format('cannot init filesystem' + LineEnding + '(%s)', [FCpmFileSystem.GetErrorMsg()])
            , mtError, [mbOK], 0) = mrOk then begin
            Exit;
        end;
    end;

    Result := True;
end;

// --------------------------------------------------------------------------------
function TCpmTools.GetFileSystemInfo: TFileSystemInfo;
var
    Info: TFileSystemInfo;
begin
    Info := FCpmFileSystem.GetFileSystemInfo;
    Info.FileName := FFileName;
    Info.FileType := FFileType;
    Result := Info;
end;

// --------------------------------------------------------------------------------
constructor TCpmTools.Create;
begin
    inherited Create;
    FCpmDevice := TCpmDevice.Create;
    FCpmFileSystem := TCpmFileSystem.Create(FCpmDevice);
end;

// --------------------------------------------------------------------------------
destructor TCpmTools.Destroy;
begin
    FreeAndNil(FCpmFileSystem);
    FreeAndNil(FCpmDevice);
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
end.
