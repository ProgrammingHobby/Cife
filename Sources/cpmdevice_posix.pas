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
unit CpmDevice_Posix;

{$mode ObjFPC}
{$H+}

interface

uses
    Classes, SysUtils, CpmDevice;

type

    { TCpmDevice_Posix }

    TCpmDevice_Posix = class(TCpmDevice)
    public    // Attribute

    public    // Methoden
        function Open(const AFilename: string; const ADeviceOptions: TLibdskDeviceOptions): boolean; override;
        function SetGeometry(const ASecLength, ASecTrk, ATracks, AOffset: integer;
            const ALibdskGeometry: TLibdskGeometry): boolean; override;
        function Close: boolean; override;
        function ReadSector(const ATrack, ASector: integer; var ABuffer: array of byte): boolean; override;
        function WriteSector(const ATrack, ASector: integer; const ABuffer: array of byte): boolean; override;
        function IsOpen(): boolean; override;
        function GetErrorMsg(): string; override;
    public  // Konstruktor/Destruktor

    protected // Attribute

    protected // Methoden

    private   // Attribute
    type
        TDevice = record
            Opened: boolean;
            SecLength: integer;
            Tracks: integer;
            SecTrk: integer;
            Offset: integer;
            Imagefile: file of byte;
        end;

    var
        FDevice: TDevice;
        FDeviceError: string;

    private   // Methoden

    end;

implementation

{ TCpmDevice_Posix }

// --------------------------------------------------------------------------------
function TCpmDevice_Posix.Open(const AFilename: string; const ADeviceOptions: TLibdskDeviceOptions): boolean;
begin
    FileMode := fmOpenReadWrite;
    FDevice.Opened := False;

    try
        AssignFile(FDevice.Imagefile, AFilename);
        Reset(FDevice.Imagefile, 1);
        FDevice.Opened := True;
    except

        on e: Exception do begin
            FDeviceError := Format(LineEnding + '%s', [e.Message]);
        end;

    end;
    Result := FDevice.Opened;
end;

// --------------------------------------------------------------------------------
function TCpmDevice_Posix.SetGeometry(const ASecLength, ASecTrk, ATracks, AOffset: integer;
    const ALibdskGeometry: TLibdskGeometry): boolean;
begin
    FDeviceError := EmptyStr;
    FDevice.SecLength := ASecLength;
    FDevice.SecTrk := ASecTrk;
    FDevice.Tracks := ATracks;
    FDevice.Offset := AOffset;
    Result := True;
end;

// --------------------------------------------------------------------------------
function TCpmDevice_Posix.Close: boolean;
begin

    try
        CloseFile(FDevice.Imagefile);
        FDevice.Opened := False;
    except

        on e: Exception do begin
            FDeviceError := Format(LineEnding + '%s', [e.Message]);
        end;

    end;

    Result := not FDevice.Opened;
end;

// --------------------------------------------------------------------------------
function TCpmDevice_Posix.ReadSector(const ATrack, ASector: integer; var ABuffer: array of byte): boolean;
var
    Count: integer;
begin
    Count := 0;
    FDeviceError := EmptyStr;

    try
        Reset(FDevice.Imagefile, 1);
        Seek(FDevice.Imagefile, (((ASector + (ATrack * FDevice.sectrk)) * FDevice.SecLength) + FDevice.Offset));
        BlockRead(FDevice.Imagefile, ABuffer[0], FDevice.SecLength, Count);
        Result := True;
    except

        on e: Exception do begin

            if (Count <> FDevice.SecLength) then begin
                FDeviceError := Format('Read Sector: %d bytes lost.', [(FDevice.SecLength - Count)]);
            end;

            FDeviceError := FDeviceError + Format(LineEnding + '%s', [e.Message]);
            Result := False;
        end;

    end;

end;

// --------------------------------------------------------------------------------
function TCpmDevice_Posix.WriteSector(const ATrack, ASector: integer; const ABuffer: array of byte): boolean;
var
    Count: integer;
begin
    Count := 0;
    FDeviceError := EmptyStr;

    try
        Reset(FDevice.Imagefile, 1);
        Seek(FDevice.Imagefile, (((ASector + (ATrack * FDevice.SecTrk)) * FDevice.SecLength) + FDevice.Offset));
        BlockWrite(FDevice.Imagefile, ABuffer[0], FDevice.SecLength, Count);
        Result := True;
    except

        on e: Exception do begin

            if (Count <> FDevice.SecLength) then begin
                FDeviceError := Format('Write Sector: %d bytes lost.', [(FDevice.SecLength - Count)]);
            end;

            FDeviceError := FDeviceError + Format(LineEnding + '%s', [e.Message]);
            Result := False;
        end;

    end;

end;

// --------------------------------------------------------------------------------
function TCpmDevice_Posix.IsOpen: boolean;
begin
    Result := FDevice.Opened;
end;

// --------------------------------------------------------------------------------
function TCpmDevice_Posix.GetErrorMsg: string;
begin
    Result := FDeviceError;
    FDeviceError := EmptyStr;
end;

// --------------------------------------------------------------------------------
end.
