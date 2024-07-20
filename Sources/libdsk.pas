{***************************************************************************
 *                                                                         *
 *    LIBDSK: General floppy and diskimage access library                  *
 *    Copyright (C) 2001-2021 John Elliott <seasip.webmaster@gmail.com>    *
 *                                                                         *
 *    Modifications to add dsk_dirty()                                     *
 *    (c) 2005 Philip Kendall <pak21-spectrum@srcf.ucam.org>               *
 *                                                                         *
 *    This library is free software; you can redistribute it and/or        *
 *    modify it under the terms of the GNU Library General Public          *
 *    License as published by the Free Software Foundation; either         *
 *    version 2 of the License, or (at your option) any later version.     *
 *                                                                         *
 *    This library is distributed in the hope that it will be useful,      *
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU    *
 *    Library General Public License for more details.                     *
 *                                                                         *
 *    You should have received a copy of the GNU Library General Public    *
 *    License along with this library; if not, write to the Free           *
 *    Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,      *
 *    MA 02111-1307, USA                                                   *
 *                                                                         *
 *************************************************************************** }
unit libdsk;

interface

{ **************************** CONSTANTS ***************************** }
const
    LIBDSK_VERSION = '1.5.19';

    DSK_ERR_OK = 0;             { No error  }
    DSK_ERR_BADPTR = -(1);      { Bad pointer  }
    DSK_ERR_DIVZERO = -(2);     { Division by zero  }
    DSK_ERR_BADPARM = -(3);     { Bad parameter  }
    DSK_ERR_NODRVR = -(4);      { Driver not found  }
    DSK_ERR_NOTME = -(5);       { Driver rejects disc  }
    DSK_ERR_SYSERR = -(6);      { System error, use errno  }
    DSK_ERR_NOMEM = -(7);       { Null return from malloc  }
    DSK_ERR_NOTIMPL = -(8);     { Function not implemented  }
    DSK_ERR_MISMATCH = -(9);    { Check sectors: No match }
    DSK_ERR_NOTRDY = -(10);     { Drive not ready  }
    DSK_ERR_RDONLY = -(11);     { Read-only disc  }
    DSK_ERR_SEEKFAIL = -(12);   { Seek fail  }
    DSK_ERR_DATAERR = -(13);    { CRC data error  }
    DSK_ERR_NODATA = -(14);     { No data  }
    DSK_ERR_NOADDR = -(15);     { Missing address mark  }
    DSK_ERR_BADFMT = -(16);     { Bad format  }
    DSK_ERR_CHANGED = -(19);    { Disc changed  }
    DSK_ERR_ECHECK = -(20);     { Equipment check  }
    DSK_ERR_OVERRUN = -(21);    { Overrun  }
    DSK_ERR_ACCESS = -(22);     { Access denied  }
    DSK_ERR_CTRLR = -(23);      { Failed controller  }
    DSK_ERR_COMPRESS = -(24);   { Compressed file is corrupt  }
    DSK_ERR_RPC = -(25);        { Error encoding / decoding RPC  }
    DSK_ERR_BADOPT = -(26);     { Requested optional feature not present  }
    DSK_ERR_BADVAL = -(27);     { Bad value for requested option  }
    DSK_ERR_ABORT = -(28);      { User abort requested  }
    DSK_ERR_TIMEOUT = -(29);    { Communications timed out  }
    DSK_ERR_UNKRPC = -(30);     { RPC server does not recognise function  }
    DSK_ERR_BADMEDIA = -(31);   { Unsuitable media for drive  }
    DSK_ERR_CORRUPT = -(32);    { Disk image file is corrupt  }
    DSK_ERR_NULLOPT = -(33);    { Option is valid, but has no value  }
    DSK_ERR_UNKNOWN = -(99);    { Unknown error  }

    { Disc sidedness (logical/physical mapping). Use SIDES_ALT for single-sided floppies.  }
    SIDES_ALT = 0;        { Track n is cylinder (n/heads) head (n%heads)  }
    SIDES_OUTBACK = 1;    { Tracks go (head 0) 0,1,2,3,...37,38,39, then (head 1) 39,38,37,...,2,1,0  }
    SIDES_OUTOUT = 2;     { Tracks go (head 0) 0,1,2,3,...37,38,39, then (head 1) 0,1,2,3,...37,38,39  }
    SIDES_EXTSURFACE = 3; { As SIDES_ALT, but sectors on head 1 identify
                            as head 0, with numbers in sequence
                            eg: Head 0 has sectors 1-9, head 1 has 10-18  }

    RATE_HD = 0;  { Data rate for 1.4Mb 3.5"  in 3.5"  drive  }
    RATE_DD = 1;  { Data rate for 360k  5.25" in 1.2Mb drive  }
    RATE_SD = 2;  { Data rate for 720k  3.5"  in 3.5"  drive  }
    RATE_ED = 3;  { Data rate for 2.8Mb 3.5"  in 3.5"  drive  }

    FMT_180K = 0;         { 9 sectors, 1 side  }
    FMT_CPCSYS = 1;       { CPC system 180K  }
    FMT_CPCDATA = 2;      { CPC data 180K  }
    FMT_720K = 3;         { 9 sectors, 80 tracks, 2 sides  }
    FMT_1440K = 4;        { 1.4M  }
    FMT_160K = 5;         { 8 sectors, 1 side  }
    FMT_320K = 6;         { 8 sectors, 2 sides  }
    FMT_360K = 7;         { 9 sectors, 2 sides  }
    FMT_720F = 8;         { 9 sectors, 2 sides out-and-back  }
    FMT_1200F = 9;        { 15 sectors, 2 sides out-and-back  }
    FMT_1440F = 10;       { 18 sectors, 2 sides out-and-back  }
    FMT_ACORN160 = 11;    { 16 sectors,  256 bytes/sector, 1 side  }
    FMT_ACORN320 = 12;    { 16 sectors,  256 bytes/sector, 1 side  }
    FMT_ACORN640 = 13;    { 16 sectors,  256 bytes/sector, 2 sides  }
    FMT_ACORN800 = 14;    {  5 sectors, 1024 bytes/sector, 2 sides  }
    FMT_ACORN1600 = 15;   { 10 sectors, 1024 bytes/sector, 2 sides  }
    FMT_800K = 16;        { 10 sectors, 80 tracks, 2 sides  }
    FMT_200K = 17;        { 10 sectors, 40 tracks, 1 side  }
    FMT_BBC100 = 18;      { 10 sectors, 40 tracks, 1 side, FM  }
    FMT_BBC200 = 19;      { 10 sectors, 80 tracks, 1 side, FM  }
    FMT_MBEE400 = 20;     { 10 sectors, 80 tracks, 1 side  }
    FMT_MGT800 = 21;      { 10 sectors, 80 tracks, 2 sides out and out  }
    FMT_TRDOS640 = 22;    { 16 sectors,  256 bytes/sector, 2 sides  }
    FMT_AMPRO200 = 23;    { 10 sectors, 512 bytes/sector, 1 side  }
    FMT_AMPRO400D = 24;   { 10 sectors, 512 bytes/sector, 2 sides  }
    FMT_AMPRO400S = 25;   { 5 sectors, 1024 bytes/sector, 1 side  }
    FMT_AMPRO800 = 26;    { 5 sectors, 1024 bytes/sector, 2 sides  }
    FMT_1200K = 27;       { 15 sectors, 2 sides   }
    FMT_MAC400 = 28;      { Apple GCR 400k  }
    FMT_MAC800 = 29;      { Apple GCR 800k  }
    FMT_UNKNOWN = -(1);

    { Low byte of dg_fm: Recording mode  }
    RECMODE_MASK = $00FF;
    RECMODE_MFM = $0000;
    RECMODE_FM = $0001;
    { Annoyingly, LDBS recording modes match LibDsk ones except for MFM,
      which is 2 in LDBS rather than 0. To avoid any possible conflict,
      don't use 2 for any future recording mode in this structure; use
      3+ for MFM variants  }

    { Recording modes 0x10-0x2F are GCR, defined as the GCR format byte
      (byte 0x51 of an Apple Disk Copy file) masked with 0x1F, plus 0x10.
      These fall into the range RECMODE_GCR_FIRST - RECMODE_GCR_LAST  }
    RECMODE_GCR_FIRST = $0010;
    RECMODE_GCR_MAC = $0012;      { Macintosh 400k / 800k GCR  }
    RECMODE_GCR_PRODOS = $0014;   { Apple IIgs Prodos 800k GCR  }
    RECMODE_GCR_LISA = $0022;     { Apple Lisa 400k GCR  }
    RECMODE_GCR_LAST = $002F;

    { High byte of dg_fm: Other data recording flags  }
    RECMODE_FLAGMASK = $FF00;
    RECMODE_COMPLEMENT = $0100;

    DSK_ST3_FAULT = $80;    { Drive fault  }
    DSK_ST3_RO = $40;       { Read only  }
    DSK_ST3_READY = $20;    { Drive ready  }
    DSK_ST3_TRACK0 = $10;   { Head is over track 0 (not all drivers)  }
    DSK_ST3_DSDRIVE = $08;  { Drive is double-sided  }
    DSK_ST3_HEAD1 = $04;    { Current head is head 1, not head 0  }

    { **************************** DATA and RECORD TYPES ***************************** }

type
    size_t = QWord;
    dsk_err_t = longint;        { Error number  }
    dsk_lsect_t = dword;        { Logical sector  }
    dsk_ltrack_t = dword;       { Logical track  }
    dsk_psect_t = dword;        { Physical sector  }
    dsk_pcyl_t = dword;         { Physical cylinder  }
    dsk_phead_t = dword;        { Physical head  }
    dsk_gap_t = byte;           { Gap length  }
    dsk_cchar_t = PChar;        { Const char *  }
    dsk_sides_t = longint;     { Disc sidedness  }
    dsk_rate_t = longint;      { Data rate  }
    dsk_format_t = longint;    { Disk formats  }
    dsk_recmode_t = longint;   { Disk Recordmode  }

    { DSK_GEOMETRY holds information used to convert physical to/from logical
      sectors and to access the disc  }
    Tdsk_geometry = record
        dg_sidedness: dsk_sides_t; { How to handle multisided discs?  }
        dg_cylinders: dsk_pcyl_t;  { Number of cylinders  }
        dg_heads: dsk_phead_t;     { Number of heads  }
        dg_sectors: dsk_psect_t;   { Sectors per track  }
        dg_secbase: dsk_psect_t;   { First physical sector number  }
        dg_secsize: size_t;        { Sector size in bytes  }
        dg_datarate: dsk_rate_t;   { Data rate  }
        dg_rwgap: dsk_gap_t;       { Read/write gap length  }
        dg_fmtgap: dsk_gap_t;      { Format gap length  }
        dg_fm: longint;            { Really a dsk_recmode_t, kept as int for backward compatibility   }
        dg_nomulti: longint;       { Disable multitrack?  }
        dg_noskip: longint;        { Set to 0 to skip deleted data  }
    end;

    { Used when formatting a sector or reading its ID  }
    Tdsk_format = record
        fmt_cylinder: dsk_pcyl_t;
        fmt_head: dsk_phead_t;
        fmt_sector: dsk_psect_t;
        fmt_secsize: size_t;
    end;

    Tdsk_pdriver = PtrUInt;
    Tdsk_pgeometry = Tdsk_geometry;

    { **************************** HELPER FUNCTIONS ***************************** }

{ Is this error a transient error, that may be cleared by a retry?  }
{ 1.1.3: Get this the right way round; they're negative numbers!  }
function DSK_TRANSIENT_ERROR(e: integer): boolean;

{ Callbacks from LibDsk to program  }
//DSK_REPORTFUNC = procedure (message:Pchar);cdecl;
//DSK_REPORTEND = procedure (_para1:pointer);cdecl;

{ **************************** GLOBAL FUNCTIONS ***************************** }
type
    { Convert physical C/H/S to logical sector  }
    Tdg_ps2ls = function(var self: Tdsk_pgeometry; cyl: dsk_pcyl_t; head: dsk_phead_t; sec: dsk_psect_t;
        var logical: dsk_lsect_t): dsk_err_t; stdcall;

    { Convert logical sector to physical C/H/S  }
    Tdg_ls2ps = function(var self: Tdsk_geometry; logical: dsk_lsect_t; var cyl: dsk_pcyl_t;
        var head: dsk_phead_t; var sec: dsk_psect_t): dsk_err_t; stdcall;

    { Convert physical cylinder/head to logical track  }
    Tdg_pt2lt = function(var self: Tdsk_pgeometry; cyl: dsk_pcyl_t; head: dsk_phead_t;
        var logical: dsk_ltrack_t): dsk_err_t; stdcall;

    { Convert logical track to physical cylinder/head  }
    Tdg_lt2pt = function(var self: Tdsk_pgeometry; logical: dsk_ltrack_t; var cyl: dsk_pcyl_t;
        var head: dsk_phead_t): dsk_err_t; stdcall;

    { Expand error message  }
    Tdsk_strerror = function(err: dsk_err_t): PChar; stdcall;

    { Initialise a DSK_GEOMETRY with one of the standard formats.
      If name / desc are not null, these are populated with the format's
      short name and a brief description.  }
    Tdg_stdformat = function(var self: Tdsk_pgeometry; formatid: dsk_format_t; Name: dsk_cchar_t;
        desc: dsk_cchar_t): dsk_err_t; stdcall;

    { Convert sector size to a physical sector shift as used by the controller.  }
    Tdsk_get_psh = function(sector_size: size_t): byte; stdcall;

    { Convert physical sector shift back to sector size.
      0-8: Returns 128 << psh. 9 and higher: Returns 32768.  }
    Tdsk_expand_psh = function(psh: byte): size_t; stdcall;

    { Register callbacks for LibDsk functions to display information on the screen.  }
    //Tdsk_reportfunc_set=procedure (report:DSK_REPORTFUNC; repend:DSK_REPORTEND); StdCall;

    { Retrieve the values of the callbacks  }
    //Tdsk_reportfunc_get=procedure (var report:DSK_REPORTFUNC; var repend:DSK_REPORTEND); StdCall;

    { Calls to these functions (or no-op, as appropriate)
      Report a work-in-progress message - LibDsk does this when starting a long
      operation such as decompression  }
    Tdsk_report = procedure(s: PChar); stdcall;

    { Remove a work-in-progress message, if appropriate  }
    Tdsk_report_end = procedure; stdcall;

    { Open a DSK file, POSIX image, drive or whatever.
      "type" is:
        NULL    : Autodetect file type from name or magic number
        "dsk"   : CPCEMU DSK or EDSK format
        "raw"   : Raw dd if=foo of=bar file
        "floppy": Host system's floppy drive
        "myz80" : MYZ80 image file
        "cfi"   : RLE-encoded raw file. Not done as a transparent compression
                driver, because it has no magic number.
      "compress" is:
        NULL     : Autodetect compression system.
        "sq"     : Huffman (Squeezed)
        "gz"     : Gzip
      Will allocate a DSK_DRIVER object.  }
    Tdsk_open = function(var self: Tdsk_pdriver; filename: PChar; _type: PChar; compress: PChar): dsk_err_t; stdcall;

    { As for "open", but creates a new image file. On a floppy drive
      this will act exactly as "open"; for an image this will attempt to
      create the file new or truncate it.
      Note that "type" cannot be NULL. "compress" can be (to create an
      uncompresed file).  }
    Tdsk_creat = function(var self: Tdsk_pdriver; filename: PChar; _type: PChar; compress: PChar): dsk_err_t; stdcall;

    { As for "open", but performs a diagnostic dump of the disk image to the output callback  }
    //Tdsk_diagopen=function (var self:Tdsk_pdriver; filename:Pchar; _type:Pchar; compress:Pchar; diagfunc:DSK_REPORTFUNC; diagend:DSK_REPORTEND):dsk_err_t; StdCall;

    { Close a DSK file. Frees the pointer and sets it to null.  }
    Tdsk_close = function(var self: Tdsk_pdriver): dsk_err_t; stdcall;

    { Returns whether the disk has been modified since it was opened  }
    Tdsk_dirty = function(self: Tdsk_pdriver): longint; stdcall;


    { Get drive status (Ready, Read-Only etc.). The actual status is
      based on the FDC's ST3 register. The following bits should be available:  }
    Tdsk_drive_status = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; head: dsk_phead_t;
        var status: byte): dsk_err_t; stdcall;

    { Read a sector. There are three alternative versions:
      One that uses physical sectors
      One that uses logical sectors
      One that uses physical sectors *which can have numbers not matching
      their positions on disc* - this functionality is only exposed by
      drivers which can manipulate the FDC directly.  }
    Tdsk_pread = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; buf: pointer; cylinder: dsk_pcyl_t;
        head: dsk_phead_t; sector: dsk_psect_t): dsk_err_t; stdcall;
    Tdsk_lread = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; buf: pointer; sector: dsk_lsect_t): dsk_err_t; stdcall;
    Tdsk_xread = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; buf: pointer; cylinder: dsk_pcyl_t;
        head: dsk_phead_t; cyl_expected: dsk_pcyl_t; head_expected: dsk_phead_t; sector: dsk_psect_t;
        sector_len: size_t; var deleted: longint): dsk_err_t; stdcall;

    { Write a sector. There are three alternative versions:
      One that uses physical sectors
      One that uses logical sectors
      One that uses physical sectors *which can have numbers not matching
      their positions on disc* - this functionality is only exposed by
      drivers which can manipulate the FDC directly.  }
    Tdsk_pwrite = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; buf: pointer; cylinder: dsk_pcyl_t;
        head: dsk_phead_t; sector: dsk_psect_t): dsk_err_t; stdcall;
    Tdsk_lwrite = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; buf: pointer; sector: dsk_lsect_t): dsk_err_t; stdcall;
    Tdsk_xwrite = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; buf: pointer; cylinder: dsk_pcyl_t;
        head: dsk_phead_t; cyl_expected: dsk_pcyl_t; head_expected: dsk_phead_t; sector: dsk_psect_t;
        sector_len: size_t; deleted: longint): dsk_err_t; stdcall;

    { Verify sector against memory buffer. There are three alternative versions:
      One that uses physical sectors
      One that uses logical sectors
      One that uses physical sectors *which can have numbers not matching
      their positions on disc* - this functionality is only exposed by
      drivers which can manipulate the FDC directly.  }
    Tdsk_pcheck = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; buf: pointer; cylinder: dsk_pcyl_t;
        head: dsk_phead_t; sector: dsk_psect_t): dsk_err_t; stdcall;
    Tdsk_lcheck = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; buf: pointer; sector: dsk_lsect_t): dsk_err_t; stdcall;
    Tdsk_xcheck = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; buf: pointer; cylinder: dsk_pcyl_t;
        head: dsk_phead_t; cyl_expected: dsk_pcyl_t; head_expected: dsk_phead_t; sector: dsk_psect_t;
        sector_len: size_t): dsk_err_t; stdcall;

    { Format a track.
      Note that the geometry in these functions is not const; the CPCEMU driver
      will increase the number of tracks/heads as the disc image file outgrows
      the geometry.   }
    Tdsk_pformat = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; cylinder: dsk_pcyl_t;
        head: dsk_phead_t; var format: Tdsk_format; filler: byte): dsk_err_t; stdcall;
    Tdsk_lformat = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; track: dsk_ltrack_t;
        var format: Tdsk_format; filler: byte): dsk_err_t; stdcall;

    { Read a track.   }
    Tdsk_xtread = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; buf: pointer; cylinder: dsk_pcyl_t;
        head: dsk_phead_t; cyl_expected: dsk_pcyl_t; head_expected: dsk_phead_t): dsk_err_t; stdcall;
    Tdsk_ptread = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; buf: pointer; cylinder: dsk_pcyl_t;
        head: dsk_phead_t): dsk_err_t; stdcall;
    Tdsk_ltread = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; buf: pointer; track: dsk_ltrack_t): dsk_err_t; stdcall;

    { Auto-format: generates the sector headers from "geom"  }
    Tdsk_apform = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; cylinder: dsk_pcyl_t;
        head: dsk_phead_t; filler: byte): dsk_err_t; stdcall;
    Tdsk_alform = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; track: dsk_ltrack_t; filler: byte): dsk_err_t; stdcall;

    { Probe the geometry of a disc. This will use the boot sector and any
      information the driver can give.  }
    Tdsk_getgeom = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry): dsk_err_t; stdcall;

    { Convert various types of boot sector to DSK_GEOMETRY
      Return DSK_ERR_OK if successful, else DSK_ERR_BADFMT  }
    Tdg_dosgeom = function(var self: Tdsk_pgeometry; var bootsect: byte): dsk_err_t; stdcall;
    Tdg_pcwgeom = function(var self: Tdsk_pgeometry; var bootsect: byte): dsk_err_t; stdcall;
    Tdg_cpm86geom = function(var self: Tdsk_pgeometry; var bootsect: byte): dsk_err_t; stdcall;
    Tdg_aprigeodsk_ptrackidsm = function(var self: Tdsk_pgeometry; var bootsect: byte): dsk_err_t; stdcall;
    Tdg_opusgeom = function(var self: Tdsk_pgeometry; var bootsect: byte): dsk_err_t; stdcall;
    Tdg_dfsgeom = function(var self: Tdsk_pgeometry; var sector0: byte; var sector1: byte): dsk_err_t; stdcall;
    Tdg_hfsgeom = function(var self: Tdsk_pgeometry; var superblock: byte): dsk_err_t; stdcall;

    { Tries all the above, in approximate order of detectability. This is
      intended to assist in parsing flat file disc images that contain no
      metadata, so the first 512 bytes are all you have to determine the
      format.  }
    Tdg_bootsecgeom = function(var self: Tdsk_pgeometry; var bootsect: byte): dsk_err_t; stdcall;

    { Read a random sector header from current track  }
    Tdsk_psecid = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; cylinder: dsk_pcyl_t;
        head: dsk_phead_t; var Result: Tdsk_format): dsk_err_t; stdcall;
    Tdsk_lsecid = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; track: dsk_ltrack_t;
        var Result: Tdsk_format): dsk_err_t; stdcall;

    { Read all sector headers from current track in the order they appear.
      Not implemented yet; this is for future expansion.  }
    Tdsk_ptrackids = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; cylinder: dsk_pcyl_t;
        head: dsk_phead_t; var Count: dsk_psect_t; var results: Tdsk_format): dsk_err_t; stdcall;
    Tdsk_ltrackids = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; track: dsk_ltrack_t;
        var Count: dsk_psect_t; var Result: Tdsk_format): dsk_err_t; stdcall;

    { Read a track as it appears on the disk, including sector headers.
      Not implemented yet; this is for future expansion.  }
    Tdsk_rtread = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; buf: pointer; cylinder: dsk_pcyl_t;
        head: dsk_phead_t; reserved: longint): dsk_err_t; stdcall;

    { Seek to a cylinder  }
    Tdsk_lseek = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; track: dsk_ltrack_t): dsk_err_t; stdcall;
    Tdsk_pseek = function(self: Tdsk_pdriver; var geom: Tdsk_pgeometry; cylinder: dsk_pcyl_t;
        head: dsk_phead_t): dsk_err_t; stdcall;

    { If "index" is in range, returns the n'th driver name in (*drvname).
      Else sets (*drvname) to null.  }
    Tdsk_type_enum = function(idx: longint; drvname: PPchar): dsk_err_t; stdcall;

    { If "index" is in range, returns the human-readable description of the
      n'th driver name in (*drvdesc).  Else sets (*drvdesc) to null.  }
    Tdsk_typedesc_enum = function(idx: longint; drvdesc: PPchar): dsk_err_t; stdcall;

    { If "index" is in range, returns the n'th compressor name in (*compname).
      Else sets (*drvname) to null.  }
    Tdsk_comp_enum = function(idx: longint; compname: PPchar): dsk_err_t; stdcall;

    { If "index" is in range, returns the n'th option name in (*optname).
      Else sets (*optname) to null.  }
    Tdsk_option_enum = function(self: Tdsk_pdriver; idx: longint; optname: PPchar): dsk_err_t; stdcall;

    { Force a drive to use head 0 or head 1 only for single-sided discs
      Pass 0 or 1, or -1 to unset it.
      Deprecated: Use dsk_set,get_option(self, "HEAD", n) instead  }
    Tdsk_set_forcehead = function(self: Tdsk_pdriver; force: longint): dsk_err_t; stdcall;
    tdsk_get_forcehead = function(self: Tdsk_pdriver; var force: longint): dsk_err_t; stdcall;

    { Set a named option to an integer value. Returns DSK_ERR_BADOPT if the
      driver doesn't support the option; DSK_ERR_BADVAL if the value is out
      of range  }
    Tdsk_set_option = function(self: Tdsk_pdriver; Name: PChar; Value: longint): dsk_err_t; stdcall;
    Tdsk_get_option = function(self: Tdsk_pdriver; Name: PChar; var Value: longint): dsk_err_t; stdcall;

    { Get or set the comment for a disc image file. Not supported by all
      file formats.  }
    Tdsk_set_comment = function(self: Tdsk_pdriver; comment: PChar): dsk_err_t; stdcall;
    Tdsk_get_comment = function(self: Tdsk_pdriver; comment: PPchar): dsk_err_t; stdcall;

    { Set / get the retry count.  }
    Tdsk_set_retry = function(self: Tdsk_pdriver; Count: dword): dsk_err_t; stdcall;
    Tdsk_get_retry = function(self: Tdsk_pdriver; var Count: dword): dsk_err_t; stdcall;

    { Get the driver name and description  }
    Tdsk_drvname = function(self: Tdsk_pdriver): PChar; stdcall;
    Tdsk_drvdesc = function(self: Tdsk_pdriver): PChar; stdcall;

    { Get the compression system name and description  }
    Tdsk_compname = function(self: Tdsk_pdriver): PChar; stdcall;
    Tdsk_compdesc = function(self: Tdsk_pdriver): PChar; stdcall;

    { Decode and act on RPC packets sent by another LIBDSK.
      This function is used to implement the 16-bit server app.  }
    Tdsk_rpc_server = function(var input: byte; inp_len: longint; var output: byte; var out_len: longint;
        var ref_count: longint): dsk_err_t; stdcall;

    { Map a DSK_PDRIVER to an integer handle. Used for RPC and JNI.
      The null pointer is always mapped to zero.
      dsk_map_dtoi will add the pointer to the map if it isn't found.  }
    Tdsk_map_dtoi = function(ptr: Tdsk_pdriver; var n: dword): dsk_err_t; stdcall;

    { Given an integer handle, retrieve the corresponding DSK_PDRIVER.
      If the handle is not found, returns *ptr = NULL.  }
    Tdsk_map_itod = function(n: dword; var ptr: Tdsk_pdriver): dsk_err_t; stdcall;

    { Remove an integer <--> DSK_DRIVER mapping. If it was the last one, free
      all the memory used by the mapping  }
    Tdsk_map_delete = function(index: dword): dsk_err_t; stdcall;

    { Copy one entire disk image to another. Currently this can only be done if
      both of them are based internally on LDBS; DSK_ERR_NOTIMPL will be returned
      otherwise.

      geom can be null. If it is not null, it will be used when converting
      from file formats that don't specify their own geometry (such as raw)  }
    Tdsk_copy = function(var geom: Tdsk_pgeometry; Source: Tdsk_pdriver; dest: Tdsk_pdriver): dsk_err_t; stdcall;

    { Helper method: Copy string 's' to a dsk_malloced buffer  }
    Tdsk_malloc_string = function(s: PChar): PChar; stdcall;

implementation

function DSK_TRANSIENT_ERROR(e: integer): boolean;
begin
    Result := ((e > DSK_ERR_COMPRESS) and (e <= DSK_ERR_NOTRDY));
end;

end.
