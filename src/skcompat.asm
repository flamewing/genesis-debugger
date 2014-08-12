; ===========================================================================
; Copyright (C) Flamewing 2014 <flamewing.sonic@gmail.com>
;
; This program is free software: you can redistribute it and/or modify it
; under the terms of the GNU Lesser General Public License as published
; by the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
; See the GNU Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
; ===========================================================================
; A few constants
palette_line_0      =      (0<<13)
palette_line_1      =      (1<<13)
palette_line_2      =      (2<<13)
palette_line_3      =      (3<<13)
tile_mask           =      $07FF

; Remapped function names
KosDec EQU Kos_Decomp
EniDec EQU Eni_Decomp

; Remapped RAM locations
Chunk_Table EQU Chunk_table
System_Stack EQU System_stack

; VRAM constants
VRAM_Plane_A_Name_Table               = $C000	; Extends until $CFFF
ArtTile_VRAM_Start                    = $0000


; simplifying macros and functions

; makes a VDP address difference
vdpCommDelta function addr,((addr&$3FFF)<<16)|((addr&$C000)>>14)

; macros to convert from tile index to art tiles, block mapping or VRAM address.
make_art_tile function addr,pal,pri,((pri&1)<<15)|((pal&3)<<13)|(addr&tile_mask)
tiles_to_bytes function addr,((addr&$7FF)<<5)

; macro to declare an offset table
offsetTable macro {INTLABEL}
current_offset_table := __LABEL__
__LABEL__ label *
    endm

; macro to declare an entry in an offset table
offsetTableEntry macro ptr
	dc.ATTRIBUTE ptr-current_offset_table
    endm

