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
	CPU 68000
	supmode on			; We don't need warnings about privileged instructions
	listing purecode	; Want listing file, but only the final code in expanded macros

paddingSoFar set 0

; 128 = 80h = z80, 32988 = 80DCh = z80unDoC
notZ80 function cpu,(cpu<>128)&&(cpu<>32988)

; define the even pseudo-instruction
even macro
	if notZ80(MOMCPU)
		if (*)&1
paddingSoFar		set paddingSoFar+1
			dc.b 0 ;ds.b 1
		endif
	else
		if ($)&1
			db 0
		endif
	endif
    endm

; makes a VDP address difference
vdpCommDelta function addr,((addr&$3FFF)<<16)|((addr&$C000)>>14)

; makes a VDP command
vdpComm function addr,type,rwd,(((type&rwd)&3)<<30)|((addr&$3FFF)<<16)|(((type&rwd)&$FC)<<2)|((addr&$C000)>>14)

; values for the type argument
VRAM = %100001
CRAM = %101011
VSRAM = %100101

; values for the rwd argument
READ = %001100
WRITE = %000111
DMA = %100111

; tells the VDP to copy a region of 68k memory to VRAM or CRAM or VSRAM
dma68kToVDP macro source,dest,length,type
	lea	(VDP_control_port).l,a5
	move.l	#(($9400|((((length)>>1)&$FF00)>>8))<<16)|($9300|(((length)>>1)&$FF)),(a5)
	if source>=0
	lea	source(pc),a1
	movea.l	a1,d1
	move.w	#$9500,d0						; command to specify source address & $0001FE
	lsr.l	#1,d1
	move.b	d1,d0
	move.w	d0,(a5)							; Send to VDP

	move.w	#$9600,d0						; command to specify source address & $01FE00
	lsr.w	#8,d1
	move.b	d1,d0
	move.w	d0,(a5)							; Send to VDP

	move.w	#$9700,d0						; command to specify source address & $FE0000
	swap	d1
	move.b	d1,d0
	move.w	d0,(a5)							; Send to VDP
	else
	move.l	#(($9600|((((source)>>1)&$FF00)>>8))<<16)|($9500|(((source)>>1)&$FF)),(a5)
	move.w	#$9700|(((((source)>>1)&$FF0000)>>16)&$7F),(a5)
	endif
	move.w	#((vdpComm(dest,type,DMA)>>16)&$FFFF),(a5)
	move.w	#(vdpComm(dest,type,DMA)&$FFFF),(a5)
    endm

; tells the VDP to fill a region of VRAM with a certain byte
dmaFillVRAM macro byte,addr,length
	lea	(VDP_control_port).l,a5
	move.w	#$8F01,(a5) ; VRAM pointer increment: $0001
	move.l	#(($9400|((((length)-1)&$FF00)>>8))<<16)|($9300|(((length)-1)&$FF)),(a5) ; DMA length ...
	move.w	#$9780,(a5) ; VRAM fill
	move.l	#$40000080|(((addr)&$3FFF)<<16)|(((addr)&$C000)>>14),(a5) ; Start at ...
	move.w	#(byte)<<8,(VDP_data_port).l ; Fill with byte
.loop:	move.w	(a5),d1
	btst	#1,d1
	bne.s	.loop ; busy loop until the VDP is finished filling...
	move.w	#$8F02,(a5) ; VRAM pointer increment: $0002
    endm

; calculates initial loop counter value for a dbf loop
; that writes n bytes total at 4 bytes per iteration
bytesToLcnt function n,n>>2-1

; tells the Z80 to stop, and waits for it to finish stopping (acquire bus)
stopZ80 macro
	move.w	#$100,(Z80_Bus_Request).l ; stop the Z80
.loop:	btst	#0,(Z80_Bus_Request).l
	bne.s	.loop ; loop until it says it's stopped
    endm

; tells the Z80 to start again
startZ80 macro
	move.w	#0,(Z80_Bus_Request).l    ; start the Z80
    endm

; A few constants
palette_line_0      =      (0<<13)
palette_line_1      =      (1<<13)
palette_line_2      =      (2<<13)
palette_line_3      =      (3<<13)
tile_mask           =      $07FF

ramaddr function x,-(-x)&$FFFFFFFF

; Remapped RAM locations
Chunk_Table = ramaddr($FFFF0000)
System_Stack = ramaddr($FFFFFE00)

VDP_data_port =				$C00000 ; (8=r/w, 16=r/w)
VDP_control_port =			$C00004 ; (8=r/w, 16=r/w)

Z80_Bus_Request =			$A11100
Z80_Reset =					$A11200

; VRAM constants
VRAM_Plane_A_Name_Table               = $C000	; Extends until $CFFF
ArtTile_VRAM_Start                    = $0000

; simplifying macros and functions

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

ROMEndLoc = $1A4

debugger_blob = 1

; Remapped function names
KosDec:
	dc.w	$4EF9, $0BAD, $F00D
EniDec:
	dc.w	$4EF9, $DEAD, $BEEF

	include "Debugger.asm"

