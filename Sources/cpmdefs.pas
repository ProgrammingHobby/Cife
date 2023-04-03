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
unit CpmDefs;

{$mode ObjFPC}{$H+}

interface

type
    off_t = longint;
    size_t = cardinal;
    ino_t = cardinal;
    mode_t = nativeuint;
    cpm_attr_t = integer;
    time_t = longint;

const
    INTBITS = (sizeof(integer) * 8);

    CPMFS_HI_USER = ($01 shl 0);     // has user numbers up to 31
    CPMFS_CPM3_DATES = ($01 shl 1);  // has CP/M+ style time stamps
    CPMFS_CPM3_OTHER = ($01 shl 2);  // has passwords and disc label
    CPMFS_DS_DATES = ($01 shl 3);    // has datestamper timestamps
    CPMFS_EXACT_SIZE = ($01 shl 4);  // has reverse exact file size

    CPMFS_DR22 = (CPMFS_HI_USER);
    CPMFS_P2DOS = (CPMFS_CPM3_DATES or CPMFS_HI_USER);
    CPMFS_DR3 = (CPMFS_CPM3_DATES or CPMFS_CPM3_OTHER or CPMFS_HI_USER);
    CPMFS_ISX = (CPMFS_EXACT_SIZE);
    CPMFS_ZSYS = (CPMFS_HI_USER);

    // CP/M file attributes
    CPM_ATTR_F1 = 1;
    CPM_ATTR_F2 = 2;
    CPM_ATTR_F3 = 4;
    CPM_ATTR_F4 = 8;
    // F5-F8 are banned in CP/M 2 & 3, F7 is used by ZSDOS

    CPM_ATTR_RO = 256;        // Read-only
    CPM_ATTR_SYS = 512;       // System
    CPM_ATTR_ARCV = 1024;     // Archive */
    CPM_ATTR_PWDEL = 2048;    // Password required to delete */
    CPM_ATTR_PWWRITE = 4096;  // Password required to write */
    CPM_ATTR_PWREAD = 8192;   // Password required to read */

    PASSWD_RECLEN = 24;
    RESERVED_ENTRIES = 4;
    RESERVED_INODES = 3;

    OK = 0;
    MODIFIED = 1;
    BROKEN = 2;

    S_IFDIR = 16384;
    S_IFREG = 32768;

    C0 = 'G';
    C1 = 'E';
    C2 = 'H';
    C3 = 'E';
    C4 = 'I';
    C5 = 'M';
    C6 = ' ';
    C7 = ' ';

//PB = ((PChar)(C0+C1+C2+C3+C4+C5+C6+C7));
//PC0 =((char)(C7^PB));
//PC1 =((char)(C6^PB));
//PC2 =((char)(C5^PB));
//PC3 =((char)(C4^PB));
//PC4 =((char)(C3^PB));
//PC5 =((char)(C2^PB));
//PC6 =((char)(C1^PB));
//PC7 =((char)(C0^PB));

//function BCD2BIN(x):
//function BIN2BCD(x):
//function ISFILECHAR(notFirst,c):
//function EXTENT(low,high):
//function EXTENTL(extent):
//function EXTENTH(extent):

implementation

end.
