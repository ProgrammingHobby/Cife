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
unit CpmDevice_Libdsk;

{$mode ObjFPC}
{$H+}

interface

uses
    Classes, SysUtils, CpmDevice, libdsk;

type

    { TCpmDevice_Libdsk }

    TCpmDevice_Libdsk = class(TCpmDevice)
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
        constructor Create(ALibdskFile: string); overload;
        destructor Destroy; override;

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
            Geometry: Tdsk_geometry;
            Driver: Tdsk_pdriver;
            Imagefile: file of byte;
        end;

    var
        FDevice: TDevice;
        FDeviceError: string;
        FLibdskHandle: TLibHandle;
        Fdg_stdformat: Tdg_stdformat;
        Fdsk_open: Tdsk_open;
        Fdsk_close: Tdsk_close;
        Fdsk_strerror: Tdsk_strerror;
        Fdsk_getgeom: Tdsk_getgeom;
        Fdsk_lread: Tdsk_lread;
        Fdsk_lwrite: Tdsk_lwrite;

    private   // Methoden
        function LookupFormat(var AGeom: Tdsk_geometry; const AName: string): boolean;

    end;

implementation

{ TCpmDevice_Libdsk }

uses dynlibs;

// --------------------------------------------------------------------------------
function TCpmDevice_Libdsk.Open(const AFilename: string; const ADeviceOptions: TLibdskDeviceOptions): boolean;
var
    Err: dsk_err_t;
    DeviceOptions: TStringArray;
    DriverName: string;
    FmtName: string;
begin
    FDevice.Opened := False;
    FileMode := fmOpenReadWrite;

    if ((FLibdskHandle = dynlibs.NilHandle) or not (Assigned(Fdg_stdformat) and Assigned(Fdsk_open) and
        Assigned(Fdsk_close) and Assigned(Fdsk_strerror) and Assigned(Fdsk_getgeom) and Assigned(Fdsk_lread) and
        Assigned(Fdsk_lwrite))) then begin
        FDeviceError := 'cannot load Libdsk library';
        Result := FDevice.Opened;
        exit;
    end;

    if (ADeviceOptions[0] = #0) then begin
        Err := Fdsk_open(FDevice.Driver, PChar(AFilename), nil, nil);
        FmtName := '';
    end
    else begin
        DeviceOptions := string(ADeviceOptions).Trim.Split(',');
        DriverName := DeviceOptions[0];

        if (Length(DeviceOptions) > 1) then begin
            FmtName := DeviceOptions[1];
        end;

        Err := Fdsk_open(FDevice.Driver, PChar(AFilename), PChar(DriverName), nil);
    end;

    if (Err <> DSK_ERR_OK) then begin
        FDeviceError := string(Fdsk_strerror(Err));
        Result := FDevice.Opened;
        exit;
    end;

    if (FmtName <> '') then begin

        if not LookupFormat(FDevice.Geometry, FmtName) then begin
            Result := FDevice.Opened;
            exit;
        end;

    end
    else begin
        Fdsk_getgeom(FDevice.Driver, FDevice.Geometry);
    end;

    FDevice.Opened := True;
    Result := FDevice.Opened;
end;

// --------------------------------------------------------------------------------
function TCpmDevice_Libdsk.SetGeometry(const ASecLength, ASecTrk, ATracks, AOffset: integer;
    const ALibdskGeometry: TLibdskGeometry): boolean;
begin
    FDevice.SecLength := ASecLength;
    FDevice.SecTrk := ASecTrk;
    FDevice.Tracks := ATracks;
    FDevice.Offset := AOffset;

    { If a geometry is named in diskdefs, use it  }
    if (ALibdskGeometry[0] <> #0) then begin
        Result := LookupFormat(FDevice.Geometry, string(ALibdskGeometry));
        exit;
    end;

    FDevice.Geometry.dg_secsize := ASecLength;
    FDevice.Geometry.dg_sectors := ASecTrk;

    { Did the autoprobe guess right about the number of sectors & cylinders?  }
    if ((FDevice.Geometry.dg_cylinders * FDevice.Geometry.dg_heads) = ATracks) then begin
        Result := True;
        exit;
    end;

    { Otherwise we guess: <= 43 tracks: single-sided. Else double. This
      fails for 80-track single-sided if there are any such beasts  }
    if (ATracks <= 43) then begin
        FDevice.Geometry.dg_cylinders := ATracks;
        FDevice.Geometry.dg_heads := 1;
    end
    else begin
        FDevice.Geometry.dg_cylinders := (ATracks div 2);
        FDevice.Geometry.dg_heads := 2;
    end;

    Result := True;
end;

// --------------------------------------------------------------------------------
function TCpmDevice_Libdsk.Close: boolean;
var
    Err: dsk_err_t;
begin

    if (FDevice.Opened) then begin
        Err := Fdsk_close(FDevice.Driver);

        if (Err <> DSK_ERR_OK) then begin
            FDeviceError := string(Fdsk_strerror(Err));
            FDevice.Opened := False;
        end;

    end;

    Result := FDevice.Opened;
end;

// --------------------------------------------------------------------------------
function TCpmDevice_Libdsk.ReadSector(const ATrack, ASector: integer; var ABuffer: array of byte): boolean;
var
    Err: dsk_err_t;
begin
    Result := True;
    Err := Fdsk_lread(FDevice.Driver, FDevice.Geometry, @ABuffer, ((ATrack * FDevice.SecTrk) +
        ASector + (FDevice.Offset div FDevice.SecLength)));

    if (Err <> DSK_ERR_OK) then begin
        FDeviceError := string(Fdsk_strerror(Err));
        Result := False;
    end;

end;

// --------------------------------------------------------------------------------
function TCpmDevice_Libdsk.WriteSector(const ATrack, ASector: integer; const ABuffer: array of byte): boolean;
var
    Err: dsk_err_t;
begin
    Result := True;
    Err := Fdsk_lwrite(FDevice.Driver, FDevice.Geometry, @ABuffer, ((ATrack * FDevice.SecTrk) +
        ASector + (FDevice.Offset div FDevice.SecLength)));

    if (Err <> DSK_ERR_OK) then begin
        FDeviceError := string(Fdsk_strerror(Err));
        Result := False;
    end;

end;

// --------------------------------------------------------------------------------
function TCpmDevice_Libdsk.IsOpen: boolean;
begin
    Result := FDevice.Opened;
end;

// --------------------------------------------------------------------------------
function TCpmDevice_Libdsk.GetErrorMsg: string;
begin
    Result := FDeviceError;
    FDeviceError := EmptyStr;
end;

// --------------------------------------------------------------------------------
constructor TCpmDevice_Libdsk.Create(ALibdskFile: string);
begin
    inherited Create;
    FLibdskHandle := LoadLibrary(ALibdskFile);

    if (FLibdskHandle <> dynlibs.NilHandle) then begin
        Fdg_stdformat := Tdg_stdformat(GetProcedureAddress(FLibdskHandle, 'dg_stdformat'));
        Fdsk_open := Tdsk_open(GetProcedureAddress(FLibdskHandle, 'dsk_open'));
        Fdsk_close := Tdsk_close(GetProcedureAddress(FLibdskHandle, 'dsk_close'));
        Fdsk_strerror := Tdsk_strerror(GetProcedureAddress(FLibdskHandle, 'dsk_strerror'));
        Fdsk_getgeom := Tdsk_getgeom(GetProcedureAddress(FLibdskHandle, 'dsk_getgeom'));
        Fdsk_lread := Tdsk_lread(GetProcedureAddress(FLibdskHandle, 'dsk_lread'));
        Fdsk_lwrite := Tdsk_lwrite(GetProcedureAddress(FLibdskHandle, 'dsk_lwrite'));
    end;

end;

// --------------------------------------------------------------------------------
destructor TCpmDevice_Libdsk.Destroy;
begin

    if (FLibdskHandle <> dynlibs.NilHandle) then begin

        if (FreeLibrary(FLibdskHandle)) then begin
            FLibdskHandle := dynlibs.NilHandle;
        end;

    end;

    inherited Destroy;
end;

// --------------------------------------------------------------------------------
function TCpmDevice_Libdsk.LookupFormat(var AGeom: Tdsk_geometry; const AName: string): boolean;
var
    Fmt: dsk_format_t;
    FmtName: PChar;
    PDsk_geometry: Tdsk_pgeometry;
begin
    Result := False;
    Fmt := FMT_180K;

    while (Fdg_stdformat(PDsk_geometry, Fmt, @FmtName, nil) = DSK_ERR_OK) do begin

        if (AName = FmtName) then begin
            Fdg_stdformat(AGeom, Fmt, @FmtName, nil);
            Result := True;
            exit;
        end;

        Inc(Fmt);
    end;

    FDeviceError := 'Unrecognised LibDsk geometry specification';
end;

// --------------------------------------------------------------------------------
end.
