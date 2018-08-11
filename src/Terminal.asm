; ===========================================================================
; Copyright (C) 2011-2018 by flamewing
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
; Make sure these point to the correct locations.
; ---------------------------------------------------------------------------
; Kosinski compressed art (60 tiles)
; Terminal font; contains all ASCII characters. For the full list of characters,
; see https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters
ArtKos_TerminalFont:	BINCLUDE	"TerminalFont.bin"
	even
; ---------------------------------------------------------------------------
; Palette
; The terminal palette
TerminalPalette:		BINCLUDE	"TerminalPal.bin"
TerminalPalette_Len := *-TerminalPalette
	even
; ===========================================================================
; Location of art in VRAM. Just use these defaults.
ArtTile_ArtKos_TerminalFont      = $0021
TerminalFont_Len                 = tiles_to_bytes($60)
; ===========================================================================
; Location in RAM. Just use this default.
TerminalBuffer                   = ramaddr($FFFF0000)
; ===========================================================================
; NO USER SERVICEABLE PARTS BELOW THIS POINT
; ===========================================================================
WHITE = palette_line_0
BLUE  = palette_line_1
RED   = palette_line_2
GREEN = palette_line_3
; ===========================================================================
; Function to aid in moving the cursor to a given location.
nCols = 40
nRows = 28
DisplayCell function col,line,((2 * nCols * line) + (2 * col))
; ===========================================================================
; Macro for defining strings suitable for efficient use with the functions in
; this module. Adds a length prefix and maps characters to have the correct
; colors, as needed.
vtstring macro pal,letters
c := 0
	dc.w	strlen(letters)
	rept strlen(letters)
t := substr(letters,c,1)
	; Note: ? is not on the list
	if strstr("*@[\\]()+,-./:;#$%&! ",t)>=0
	dc.w	t							; raw character (no palette modifier)
	else
	dc.b	pal>>8,t					; output letter code
	endif
c := c+1
	endm
	endm
; ===========================================================================
; Macro that puts a character in the buffer with the given color, if any.
; In general, punctuation characters are uncolored by default.
vtputc macro char,clr
.r := charfromstr(char,0)
	; Note: ? is not on the list
	if strstr("*@[\\]()+,-./:;#$%&! ",char)>=0
	move.w	#.r,(a5)+					; raw character (no palette modifier)
	elseif "clr"<>""
	move.w	#clr|.r,(a5)+				; output letter code
	else
	move.w	#.r,(a5)+					; output letter code
	endif
	endm
; ===========================================================================
; Macro that puts a string of characters in the buffer with the given color, if
; any. Uses vtputc internally.
vtputs macro letters,clr
.c := 0
	rept strlen(letters)
.t := substr(letters,.c,1)
	vtputc	.t,clr
.c := .c+1
	endm
	endm
; ===========================================================================
; Macro that puts the character or pair of characters at the given effective
; address using the given color, if any.
vtput macro reg,clr
	if "ATTRIBUTE"=="b"
		andi.w	#$FF,reg
	endif
	if "ATTRIBUTE"<>"l"
		if "clr"<>""
			addi.w	#clr,reg
		endif
		move.w	reg,(a5)+					; output letter code
	else
		if "clr"<>""
			addi.l	#(clr<<16)|clr,reg
		endif
		move.l	reg,(a5)+					; output letter codes
	endif
	endm
; ===========================================================================
; Converts the given effective address from numbers to ASCII numbers, using the
; given color if any.
itoa macro reg,clr
	if "ATTRIBUTE"=="b"
		andi.w	#$FF,reg
	endif
	if "ATTRIBUTE"<>"l"
		if "clr"<>""
			addi.w	#'0'|clr,reg
		else
			addi.w	#'0',reg
		endif
	else
		if "clr"<>""
			addi.l	#(('0'|clr)<<16)|('0'|clr),reg
		else
			addi.l	#('0'<<16)|'0',reg
		endif
	endif
	endm
; ===========================================================================
; Gets current position of the put pointer.
tellp macro dst
	move.ATTRIBUTE	a5,dst
	endm
; ===========================================================================
; Moves the put pointer by the given distance.
seekp macro off,dst
	if "dst"<>""
	add.ATTRIBUTE	#DisplayCell(off,0),dst
	else
	adda.ATTRIBUTE	#DisplayCell(off,0),a5
	endif
	endm
; ===========================================================================
; Sets the put pointer based on the given effective address.
seekset macro src
	movea.l	src,a5
	endm
; ===========================================================================
; Sets the put pointer to the given column and row.
setcursor	macro col,row,dst
	if "dst"<>""
	lea	(TerminalBuffer + DisplayCell(col,row)).l,dst
	else
	lea	(TerminalBuffer + DisplayCell(col,row)).l,a5
	endif
	endm
; ===========================================================================
; Gets the distance, in characters, between the put pointer and the value given
; in the effective address.
getdst macro dst
	sub.ATTRIBUTE	a5,dst
	asr.w	#1,dst
	endm
; ===========================================================================
; As getdst, but gives the return in bytes instead.
vtchkp macro dst
	sub.ATTRIBUTE	a5,dst
	endm
; ===========================================================================
; Puts cn copies of the given character, in sequence, on the terminal.
fillr macro src,cnt
.fill_loop:
	vtput.w	src
	dbra	cnt,.fill_loop
	endm
; ===========================================================================
; Clears the entire terminal.
vtcls macro
	move.w	#bytesToLcnt(2 * nRows * nCols),d0
	moveq	#0,d1
	setcursor	0,0
.clr_screen:
	vtput.l	d1
	dbra	d0,.clr_screen
	endm
; ===========================================================================
; Initialize Terminal
InitTerminal:
	stopZ80

	lea	(VDP_control_port).l,a3
	tst.w	(a3)						; Reset "write-pending" flag
	move.w	#$8004,(a3)					; H-INT disabled
	move.w	#$8114,(a3)					; Genesis mode, DMA enabled, VBLANK-INT disabled, blanking mode on.
	move.w	#$8230,(a3)					; PNT A base: $C000
	move.w	#$833C,(a3)					; Window Address: $F000
	move.w	#$8407,(a3)					; PNT B base: $E000
	move.w	#$857C,(a3)					; Sprite attribute table base: $F800
	move.w	#$8600,(a3)					; Sprite Pattern Generator Base Address: low 64KB VRAM
	move.w	#$8700,(a3)					; Background palette/color: 0/0
	move.w	#$8800,(a3)					; Null
	move.w	#$8900,(a3)					; Null
	move.w	#$8AFF,(a3)					; H-INT every $FF scanlines
	move.w	#$8B00,(a3)					; EXT-INT disabled, V scroll by screen, H scroll by screen
	move.w	#$8C81,(a3)					; H res 40 cells, no interlace
	move.w	#$8D37,(a3)					; HScroll Table Address: $DC00
	move.w	#$8E00,(a3)					; Nametable Pattern Generator Base Address: low 64KB VRAM
	move.w	#$8F02,(a3)					; VDP auto increment 2 bytes
	move.w	#$9001,(a3)					; Scroll table size: 64x32
	move.w	#$9100,(a3)					; Window H left side, Base Point 0
	move.w	#$9200,(a3)					; Window V upside, Base Point 0

	; Copy the palette to CRAM
	dma68kToVDP TerminalPalette,$0000,TerminalPalette_Len,CRAM

	; Clear vertical scroll
	moveq	#0,d0
	lea	(VDP_data_port).l,a1
	move.l	#vdpComm($0000,VSRAM,WRITE),(a3)
	move.l	d0,(a1)

	; Fill VRAM with 0
	dmaFillVRAM 0,$0000,$10000

	; Decode and load graphics.
	lea	ArtKos_TerminalFont(pc),a0
	lea	(Chunk_Table).l,a1
	ifdef debugger_blob
	jsr	KosDec(pc)
	else
	jsr	(KosDec).w
	endif
	dma68kToVDP TerminalBuffer,tiles_to_bytes(ArtTile_ArtKos_TerminalFont),TerminalFont_Len,VRAM

	vtcls								; Clear screen
	rts
; ===========================================================================
planeLocH40_ function col,line,(($80 * line) + (2 * col))
; ---------------------------------------------------------------------------
DrawTerminal:
	; Inlined the call to PlaneMapToVRAM_H40 for one less external dependency.
	lea	(TerminalBuffer).l,a1
	lea	(VDP_data_port).l,a3
	lea	(VDP_control_port).l,a2
	move.l	#vdpCommDelta(planeLocH40_(0,1)),d4
	move.l	#vdpComm(VRAM_Plane_A_Name_Table,VRAM,WRITE),d0
	moveq	#nCols-1,d1					; Width
	moveq	#nRows-1,d2					; Height

.row_loop:
	move.l	d0,(a2)						; Set write destination
	move.w	d1,d3

.col_loop:
	move.w	(a1)+,(a3)
	dbra	d3,.col_loop
	add.l	d4,d0						; increase destination address by 1 line
	dbra	d2,.row_loop

	; Exit blanking mode.
	move.w	#$8154,(a2)					; Genesis mode, DMA enabled, VBLANK-INT disabled, blanking mode off.
	rts
; ===========================================================================
; SUBROUTINE
; Prints a message to the screen.
; Input:
; 	a1	Message to print
; 	a5	Write destination (in RAM)
Print_Message:
	move.w	d1,-(sp)
	move.w	(a1)+,d1					; Get size of string
	beq.s	.done						; Branch out for empty strings
	subq.w	#1,d1						; Make it into a loop counter
	fillr (a1)+,d1

.done:
	move.w	(sp)+,d1
	rts
; ===========================================================================
; Macro to dump a hex number. Prints the nibbles stored in d0 according to the
; specified attribute.
HexDump	macro
	; Select number of nibbles to print based on attribute given.
	if "ATTRIBUTE"=="b"
	moveq	#1,d2
	elseif "ATTRIBUTE"=="w"
	moveq	#3,d2
	elseif "ATTRIBUTE"=="l"
	moveq	#7,d2
	endif
	move.w	d1,-(sp)

.put_digit_loop:
	rol.ATTRIBUTE	#4,d0				; Get a new nibble to print
	move.b	d0,d1						; Move to d1 so the other nibbles are kept
	andi.w	#$F,d1						; Want only the lowest nibble
	move.b	Hex2Char(pc,d1.w),d1		; Convert to character
	addi.w	#RED,d1						; Set number color
	vtput.w	d1
	dbra	d2,.put_digit_loop
	move.w	(sp)+,d1
	endm
; ===========================================================================
; Macro to define functions that print hex numbers.
HexPrint macro {INTLABEL}
Print___LABEL___Signed label *
	tst.ATTRIBUTE	d0
	bpl.s	Print___LABEL__
	vtputc	"-"
	neg.ATTRIBUTE	d0

Print___LABEL__ label *
	vtputc	"$"

HexDump___LABEL__ label *
	HexDump.ATTRIBUTE
	rts
	endm
; ===========================================================================
; SUBROUTINE
; Prints a byte to the screen. Defines the functions Print_Byte_Signed,
; Print_Byte and HexDump_Byte.
; Input:
; 	d0	Byte to print
; 	a5	Write destination (in RAM)
Byte:	HexPrint.b
; ===========================================================================
; SUBROUTINE
; Prints a word to the screen. Defines the functions Print_Word_Signed,
; Print_Word and HexDump_Word.
; Input:
; 	d0	Word to print
; 	a5	Write destination (in RAM)
Word:	HexPrint.w
; ===========================================================================
Hex2Char:
	dc.b	'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
; ===========================================================================
; SUBROUTINE
; Prints a 24-bit address to the screen, including dollar sign.
; Input:
; 	d0	Address to print
; 	a5	Write destination (in RAM)
Print_Address:
	vtputc	"$"
	swap	d0
	HexDump.b
	swap	d0
	HexDump.w
	rts
; ===========================================================================
; SUBROUTINE
; Prints a long to the screen. Defines the functions Print_Long_Signed,
; Print_Long and HexDump_Long.
; Input:
; 	d0	Long to print
; 	a5	Write destination (in RAM)
Long:	HexPrint.l
; ===========================================================================

