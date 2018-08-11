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
; For all the "unhandled" vectors.
ErrorTrap:
	nop
	nop
	bra.s	ErrorTrap

; These get called from the binary blob. Do not edit them, or move them
; relative to the binary blobs below.
	jmp	(KosDec).l
	jmp	(EniDec).l

; This is the terminal code and graphics, plus the disassembler and the plane
; mappings for the debugger.
	incbin "_debugger/Part1.bin"

WHITE EQU 0<<13
BLUE  EQU 1<<13
RED   EQU 2<<13
GREEN EQU 3<<13
; Strings are word arrays: length followed by characters. You can change the
; length, but do NOT change the number of characters! The wasted space is the
; price to pay for a binary blob...
; The high byte of each word used for a character is the palette line to use:
HackerName:
	dc.w 11
	dc.w WHITE|'Y',	 WHITE|'o',	 WHITE|'u',	 WHITE|'r',	 WHITE|' ',	 WHITE|'N'
	dc.w WHITE|'a',	 WHITE|'m',	 WHITE|'e',	 WHITE|' ',	 WHITE|' '
	even
EMailmsg:
	dc.w 33
	dc.w BLUE|'y',	BLUE|'o',	BLUE|'u',	BLUE|'r',	BLUE|'.',	BLUE|'e'
	dc.w BLUE|'m',	BLUE|'a',	BLUE|'i',	BLUE|'l',	BLUE|'@',	BLUE|'s'
	dc.w BLUE|'e',	BLUE|'r',	BLUE|'v',	BLUE|'e',	BLUE|'r',	BLUE|'.'
	dc.w BLUE|'d',	BLUE|'o',	BLUE|'m',	BLUE|'a',	BLUE|'i',	BLUE|'n'
	dc.w BLUE|' ',	BLUE|' ',	BLUE|' ',	BLUE|' ',	BLUE|' ',	BLUE|' '
	dc.w BLUE|' ',	BLUE|' ',	BLUE|' '
	even

; Do not move or add padding between the code that follows. The debugger is
; split into these many parts because asm68k sucks.
BusErrorMsg:
	incbin "_debugger/Part2.bin"

BusError:
	incbin "_debugger/Part3.bin"

AddressError:
	incbin "_debugger/Part4.bin"

TraceError:
	incbin "_debugger/Part5.bin"

SpuriousException:
	incbin "_debugger/Part6.bin"

ZeroDivideError:
	incbin "_debugger/Part7.bin"

CHKExceptionError:
	incbin "_debugger/Part8.bin"

TRAPVError:
	incbin "_debugger/Part9.bin"

IllegalInstrError:
	incbin "_debugger/PartA.bin"

PrivilegeViolation:
	incbin "_debugger/PartB.bin"

LineAEmulation:
	incbin "_debugger/PartC.bin"

LineFEmulation:
	incbin "_debugger/PartD.bin"

TrapVector:
	incbin "_debugger/PartE.bin"

; Edit this to something sensible. One suggestion is the SVN revision.
RevisionNumber:
	dc.w	1
	incbin "_debugger/PartF.bin"

