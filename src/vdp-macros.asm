; ===========================================================================
; Copyright (C) 2011-2020 by flamewing
;
; Permission to use, copy, modify, and/or distribute this software for any
; purpose with or without fee is hereby granted.
;
; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
; OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
; ===========================================================================
    ifndef VDP_Macros
; Note: if you are adding those to S2 and S3&K disassemblies, make sure to
; also add this constant definition.
VDP_Macros := 1
; For register $80 - Mode Register 1
COL0_BLANK_OFF   = %00000000	; $00
COL0_BLANK_ON    = %00100000	; $20
HINT_OFF         = %00000000	; $00
HINT_ON          = %00010000	; $10
PALSEL_OFF       = %00000000	; $00
PALSEL_ON        = %00000100	; $04
HVLATCH_OFF      = %00000000	; $00
HVLATCH_ON       = %00000010	; $02
ECSYNC_OFF       = %00000000	; $00
ECSYNC_ON        = %00000001	; $01
MODE1_MASK       = COL0_BLANK_ON|HINT_ON|PALSEL_ON|HVLATCH_ON|ECSYNC_ON

; For register $81 - Mode Register 2
VRAM_128KB_OFF   = %00000000	; $00
VRAM_128KB_ON    = %10000000	; $80
DISPLAY_OFF      = %00000000	; $00
DISPLAY_ON       = %01000000	; $40
VINT_OFF         = %00000000	; $00
VINT_ON          = %00100000	; $20
DMA_OFF          = %00000000	; $00
DMA_ON           = %00010000	; $10
V30_OFF          = %00000000	; $00
V30_ON           = %00001000	; $08
MODE_SMS         = %00000000	; $00
MODE_GEN         = %00000100	; $04
MODE2_MASK       = VRAM_128KB_ON|DISPLAY_ON|VINT_ON|DMA_ON|V30_ON|MODE_GEN

; For register $86 - Sprite Pattern Generator Base Address
SPRITES_LOW      = %00000000	; $00
SPRITES_HIGH     = %00100000	; $20

; For register $8B - Mode Register 3
EXINT_OFF        = %00000000	; $00
EXINT_ON         = %00001000	; $08
VSCROLL_FULL     = %00000000	; $00
VSCROLL_CELL     = %00000100	; $04
HSCROLL_FULL     = %00000000	; $00
HSCROLL_TILE     = %00000010	; $02
HSCROLL_LINE     = %00000011	; $03
MODE3_MASK       = EXINT_ON|VSCROLL_CELL|HSCROLL_LINE

; For register $8C - Mode Register 4
MODE_H32         = %00000000	; $00
MODE_H32_FAST    = %10000000	; $80	; Out of spec for most TVs
MODE_H40_FAST    = %00000001	; $01	; OK for most TVs
MODE_H40         = %10000001	; $81
VSYNC_NORMAL     = %00000000 ; $00
PIXEL_CLOCK      = %01000000 ; $40
HSYNC_NORMAL     = %00000000 ; $00
HSYNC_OFF        = %00100000 ; $20
PIXEL_BUS_OFF    = %00000000 ; $00
PIXEL_BUS_ON     = %00010000 ; $10
SHADOWHILITE_OFF = %00000000 ; $00
SHADOWHILITE_ON  = %00001000 ; $08
INTERLACE_OFF    = %00000000 ; $00
INTERLACE_NORMAL = %00000010 ; $02
INTERLACE_DOUBLE = %00000110 ; $06
MODE4_MASK       = MODE_H40|PIXEL_CLOCK|HSYNC_OFF|PIXEL_BUS_ON|SHADOWHILITE_ON|INTERLACE_DOUBLE

; For register $8E - Nametable Pattern Generator Base Address
PLANE_B_LOW      = %00000000	; $00
PLANE_B_HIGH     = %00010000	; $10
PLANE_A_LOW      = %00000000	; $00
PLANE_A_HIGH     = %00000001	; $00
BASE_PLANE_MASK  = PLANE_B_HIGH|PLANE_A_HIGH

; For register $91 - Window Plane Horizontal Position
DOCK_LEFT        = %00000000	; $00
DOCK_RIGHT       = %10000000	; $80

; For register $92 - Window Plane Vertical Position
DOCK_TOP         = %00000000	; $00
DOCK_BOTTOM      = %10000000	; $80

WINDOW_CELL_MASK = %00011111	; $1F

; For register $97 - DMA Source
DMA_FLAG         = %00000000	; $00
FILL_FLAG        = %10000000	; $80
COPY_FLAG        = %11000000	; $C0

valMode1 function mode,((mode&MODE1_MASK)|PALSEL_ON)
valMode2 function mode,(mode&MODE2_MASK)
addrPlaneA function addr,(addr/$400)
addrWindow function addr,(addr/$400)
addrPlaneB function addr,(addr/$2000)
addrSprite function addr,(addr/$200)
addrScroll function addr,(addr/$400)
valBaseSprite function val,(val&SPRITES_HIGH)
valBGColor function pal,index,((pal&3)<<4)|(index&$F)
valMode3 function mode,(mode&MODE3_MASK)
valMode4 function mode,(mode&MODE4_MASK)
valBasePlane function val,(val&BASE_PLANE_MASK)
valPlaneSize function width,height,(((height-32)/32)<<4)|((width-32)/32)
valWindowLoc function dir,cells,(dir&$80)|(cells&WINDOW_CELL_MASK)
valDMALenLow function length,length&$FF
valDMALenHigh function length,(length&$FF00)>>8
valDMASrcLow function length,length&$FF
valDMASrcMid function length,(length&$FF00)>>8
valDMASrcHigh function type,length,type|((length&$7F0000)>>16)

regMode1 function mode,$8000|valMode1(mode)
regMode2 function mode,$8100|valMode2(mode)
regPlaneA function addr,$8200|addrPlaneA(addr)
regWindow function addr,$8300|addrWindow(addr)
regPlaneB function addr,$8400|addrPlaneB(addr)
regSprite function addr,$8500|addrSprite(addr)
regBaseSprite function val,$8600|valBaseSprite(val)
regBGColor function pal,index,$8700|valBGColor(pal,index)
regMode4HScroll function val,$8800|(val&$FF)
regMode4VScroll function val,$8900|(val&$FF)
regHIntLine function line,$8A00|(line&$FF)
regMode3 function mode,$8B00|valMode3(mode)
regMode4 function mode,$8C00|valMode4(mode)
regScroll function addr,$8D00|addrScroll(addr)
regBasePlane function val,$8E00|valBasePlane(val)
regAutoIncr function delta,$8F00|(delta&$FF)
regPlaneSize function width,height,$9000|valPlaneSize(width,height)
regWindowHLoc function dir,cells,$9100|valWindowLoc(dir,cells)
regWindowVLoc function dir,cells,$9200|valWindowLoc(dir,cells)
regDMALenLow function length,$9300|valDMALenLow(length)
regDMALenHigh function length,$9400|valDMALenHigh(length)
regDMASrcLow function length,$9500|valDMASrcLow(length)
regDMASrcMid function length,$9600|valDMASrcMid(length)
regDMASrcHigh function type,length,$9700|valDMASrcHigh(type,length)

planeSizeBytes function width,height,((height-32)/32)*((width-32)/32)*2

; Status register bits
IS_PAL_BIT      = 0
DMA_ACTIVE_BIT  = 1
HBLANK_BIT      = 2
VBLANK_BIT      = 3

IS_PAL_MASK     = 1<<IS_PAL_BIT
DMA_ACTIVE_MASK = 1<<DMA_ACTIVE_BIT
HBLANK_MASK     = 1<<HBLANK_BIT
VBLANK_MASK     = 1<<VBLANK_BIT

; Tells the VDP to copy from a region of VRAM to another.
dmaCopyVRAM macro src,dest,length
	if MOMPASS>1
		if (length)==0
			fatal "DMA is copying 0 bytes (becomes a 64kB copy). If you really mean it, pass 64kB (65536) instead."
		endif
	endif
	lea	(VDP_control_port).l,a5
	move.l	#dmaCommLength(2*length),(a5)
	move.l	#dmaCommSrcLow(src),(a5)
	move.l	#makeLong(regAutoIncr(1),regDMASrcHigh(COPY_FLAG,0)),(a5) ; VRAM pointer increment: $0001, VRAM copy
	move.l	#vdpComm(addr,VRAM,DMA),(a5)
.loop:
	moveq	#DMA_ACTIVE_MASK,d1
	and.w	(a5),d1
	bne.s	.loop ; busy loop until the VDP is finished filling...
	move.w	#regAutoIncr(2),(a5) ; VRAM pointer increment: $0002
	endm
    endif
