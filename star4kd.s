
;A starfield in 4 bitplanes (16 shades of gray)
; - replaced keyboard interrupt with input handler
; - moved drawing/clearing/updating screen pointers to vbl interrupt
; - wrapped blitter usage with OwnBlitter/DisOwnBlitter
; - escape key also quits
; - added keyboard interrupt
; - added triple buffering
; - removed clearpixel, was causing flickering
; - double buffered, off-screen cleared by blitter
; - added grabing topaz font and use it to print raster position
;
;http://eab.abime.net/643245-post12.html
;Leffmann on 32/64 bytes width screens
;we don't need a ytable for the offsets, we just shift
w	=64*8
h	=256
bplsize	=w*h/8
NSTARS	= 900
SPEED	= 7

;this says how many frames the speed should be displayed
;when the speed changes (with cursor up/cursor down)
; 100 frames is 100/50 = 2 seconds 
SHOWSPEED = 100


	SECTION	tut,CODE
init:	
	bsr	initstars	;initialise star's data
	bsr	setupInputHandler
	move.l	4.w,a6		;get execbase
	clr.l	d0
	move.l	#dosname,a1
	jsr	-408(a6)
	move.l	d0,pDOSBase
	clr.l	d0
	move.l	#gfxname,a1
	jsr	-408(a6)	;OldOpenLibrary()
	move.l	d0,pGraphicsBase
	move.l	d0,a6
	move.l	38(a6),oldcopper;original copper list pointer
	lea.l   lTextAttr,a0
	lea.l   TopazName,a4
	move.l	a4,(a0)
	jsr	OpenFont(a6)
	move.l	d0,a0
	move.l	SU_tf_CharData(a0),TopazCharData-TopazName(a4)
	move.w	tf_Modulo(a0),ltf_Modulo-TopazName(a4)
	;jsr	-414(a6)	;CloseLibrary()
	bsr	GetFont
	;bsr	load1
	lea.l	_custom,a5
.loop	move.l	vposr(a5),d0	;wait for vbl here as well, to see if
	and.l	#$1ff00,d0	;it helps to not have the damn sprite stripe
	cmp.l	#303<<8,d0	;(i.e just before stoping sprite dma on dmacon)
	bne.b	.loop		;

	;move.l	pDOSBase,a6
	;moveq	#100,d1		;wait two seconds
	;jsr	_LVODelay(a6)

	move.w	intenar(a5),oINTENA	;save system intena
	move.w	intreqr(a5),oldintreq	;save system intreq
	move.w	dmaconr(a5),oDMA	;save system dmacon

   *	The disk controller can issue three kinds of interrupts:
   *   DSKSYNC  (level 5,  INTREQ  bit 12) -- input stream matches the
   *                                         DSKSYNC register.
   *   DSKBLK  (level 1,  INTREQ  bit 1) -- disk DMA has completed.
   *   INDEX  (level 6, 8520 Flag pin) -- index sensor triggered.

	;#%1100000000101000
	;  +--Set
	;   +--Master
	;            +-- Vbl
	;	       +-- Ports
MyInts =  %1100000000101000
	move.w	#$7fff,intena(a5);disable all bits in INTENA
	move.w	#MyInts,intena(a5)
	move.w	#$7fff,intreq(a5)	;disable all bits in INTREQ
	move.w	#$7fff,intreq(a5)	;disable all bits in INTREQ
	move.w	#$7fff,dmacon(a5)	;disable all bits in DMACON
	move.w	#$87d0,dmacon(a5)	;enable disk dma as well
	;asmone doesn't like the big line 
	;move.w	#DMAF_SETCLR|DMAF_BLITHOG|DMAF_MASTER|DMAF_RASTER|DMAF_BLITTER|DMAF_COPPER|DMAF_DISK,dmacon(a5)
	move.w	#$C40,bplcon3(a5)
	move.w	#$11,bplcon4(a5)

	bsr	setup_VBL_interrupt

	;create palette, 16 colors from $FFF to $111
	moveq	#15-1,d7
	moveq	#15,d0
	move.l	#$dff182,a0
.lc	moveq	#0,d2
	move.l	d0,d1
	add	d1,d2
	lsl	#4,d1
	add	d1,d2
	lsl	#4,d1
	add	d1,d2
	move.w	d2,(a0)+
	subq	#1,d0
	dbf	d7,.lc
	move.w	#$000,$dff180
	move.l	#Copper,$dff080
*****************************
mainloop:
	btst	#6,$bfe001	;left mouse button pressed?
	beq.b	exit
	cmp.b	#69,Key		;escape key pressed?
	beq.b	exit
	cmp.b	#1,Key 		;'1' key pressed
	bne	.skip
	bsr	load1		;let's load '1.raw' then...


.skip	bra.w	mainloop	;keep going
exit:
;shutdown code lifted from leffmann's SetSystem.s 0.1 07-February-2009
	bsr	removeInputHandler
	lea.l	_custom,a5
	move.l	oldcopper,cop1lc(a5)
	
	move.w	#$7fff,intena(a5)	; Disable all DMA and
	move.w	#$7fff,intreq(a5)	; interrupts
	move.w	#$7fff,dmacon(a5)
	bsr	GetVBR
	move.l	d0,a0
	move.l	old_intvbl,$6c(a0)	;restore system's $6C (LVL3)

	move.w	oINTENA,d0
	move.w	oldintreq,d1		; Restore previous DMA and
	move.w	oDMA,d2			; interrupt settings
	or.w	#int_set,d0
	or.w	#int_set,d1
	or.w	#dma_set,d2
	move.w	d0,intena(a5)
	move.w	d1,intreq(a5)
	move.w	d2,dmacon(a5)
	
	moveq	#0,d0
	rts

actOnKeys:
	cmp.b	#76,Key		;cursor up?
	bne	.next2
	bsr	cursorUp
	bra	.break
.next2	cmp.b	#77,Key		;cursor down?
	bne	.next3
	bsr	cursorDown
	bra	.break
.next3	cmp.b	#79,Key		;cursor left?
	bne	.next4
	bsr	cursorLeft
	bra	.break
.next4	cmp.b	#78,Key		;cursor right?
	bne	.break
	bsr	cursorRight
	bra	.break
.break	rts
	

cursorUp:
	;; move.b	#0,Key
	move.w	speed,d0
	add.w	#1,d0
	cmp.w	#100,d0
	beq.s	.exit
	move.w	d0,speed
	move.w  #SHOWSPEED,speedframes
.exit	rts

cursorDown:
	;; move.b	#0,Key
	move.w	speed,d0
	sub.w	#1,d0
	bmi.s	.exit
	move.w	d0,speed
	move.w  #SHOWSPEED,speedframes
.exit	rts

cursorRight:
	;; move.b	#0,Key
	move.w	hslide,d0
	add.w	#1,d0
	cmp.w	#(26-2)*8,d0
	beq.s	.exit
	move.w	d0,hslide
.exit	rts

cursorLeft:
	;; move.b	#0,Key
	move.w	hslide,d0
	sub.w	#1,d0
	bmi.s	.exit
	move.w	d0,hslide
.exit	rts

load1:	
	move.b	#0,Key		;so that mainloop won't keep calling us
	move.l	pDOSBase,a6	;gimme dos in a6
	move.l	#file1,d1	;filename
	move.l	#MODE_OLDFILE,d2;mode to open it
	jsr	_LVOOpen(a6)	;Open it already
	move.l	d0,fh		;grab filehandle
	beq	.exit		;0? Something didn't work, exit
	move.l	d0,d1		;filehandle
	move.l	#filebuffer,d2	;buffer address
	move.l	#24*1024,d3	;buffer's length
	jsr	_LVORead(a6)	;bring me the data
	;will check the returned length on a sunny day :P
	move.l	#Screen1,a1	;a1 will play "To"

	moveq.l #3-1,d1		;triple-buffering is...3 screens
.scrlp	move.l	#filebuffer,a0	;a0 will play "From"
				;copy from buffer to screen

	;can't call Delay
	;movem.l	d0-d1/a0-a1,-(sp)
	;moveq	#1,d1		;
	;jsr	_LVODelay(a6)
	;movem.l (sp)+,d0-d1/a0-a1

	move.l	#1024-1,d0	;4 planes, 256 height
.copy:	
	lea.l	40(a1),a1	;skip 40 bytes (starfield)
	move.l	(a0)+,(a1)+	;6 longwords = 24 bytes
	move.l	(a0)+,(a1)+
	move.l	(a0)+,(a1)+
	move.l	(a0)+,(a1)+
	move.l	(a0)+,(a1)+
	move.l	(a0)+,(a1)+
	dbf	d0,.copy
	dbf	d1,.scrlp

	move.l	fh,d1		;grab the filehandle
	jsr	_LVOClose(a6)	;close the file
.exit	rts

file1:	dc.b	"1.raw",0
	EVEN
fh:	dc.l	0



psubs:	dc.l	plot1,plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8
	dc.l	plot9,plot10,plot11,plot12,plot13,plot14,plot15
	
plot1:	bfset	(a0,d1.w){d0:1}
	bra	cont
	
plot2:	bfset	(a1,d1.w){d0:1}
	bra	cont
	
plot3:	bfset	(a0,d1.w){d0:1}
	bfset	(a1,d1.w){d0:1}
	bra	cont
	
plot4:	bfset	(a2,d1.w){d0:1}
	bra	cont
	
plot5:	bfset	(a0,d1.w){d0:1}
	bfset	(a2,d1.w){d0:1}
	bra	cont
	
plot6:  bfset	(a1,d1.w){d0:1}
	bfset	(a2,d1.w){d0:1}
	bra	cont
	
plot7:  bfset	(a0,d1.w){d0:1}
	bfset	(a1,d1.w){d0:1}
	bfset	(a2,d1.w){d0:1}
	bra	cont
	
plot8:  bfset	(a3,d1.w){d0:1}
	bra	cont
	
plot9:	bfset	(a0,d1.w){d0:1}
	bfset	(a3,d1.w){d0:1}
	bra	cont
	
plot10:	bfset	(a1,d1.w){d0:1}
	bfset	(a3,d1.w){d0:1}
	bra	cont
	
plot11:	bfset	(a0,d1.w){d0:1}
	bfset	(a1,d1.w){d0:1}
	bfset	(a3,d1.w){d0:1}
	bra	cont
	
plot12:	bfset	(a2,d1.w){d0:1}
	bfset	(a3,d1.w){d0:1}
	bra	cont
	
plot13:	bfset	(a0,d1.w){d0:1}
	bfset	(a2,d1.w){d0:1}
	bfset	(a3,d1.w){d0:1}
	bra	cont
	
plot14:	bfset	(a1,d1.w){d0:1}
	bfset	(a2,d1.w){d0:1}
	bfset	(a3,d1.w){d0:1}
	bra	cont
	
plot15:	bfset	(a0,d1.w){d0:1}
	bfset	(a1,d1.w){d0:1}
	bfset	(a2,d1.w){d0:1}
	bfset	(a3,d1.w){d0:1}
	bra	cont

;http://ada.untergrund.net/forum/index.php?action=vthread&forum=4&topic=31#msg246
;Cyf
interruptlev3: 
	movem.l d0-d7/a0-a6,-(sp) ; creat the macro Push d0-a6 :)) 
	; optional 
	move.w 	$dff01c,d0 ; intena 
	btst 	#14,d0 ; bit 14 Intena - authorized interruption ? 
	beq.w 	novbl 
	and.w 	$dff01e,d0 ; intreq 
	; or replace lines above by "move.w $dff01e,d0" 
	btst 	#5,d0 ; interrupt vbl ? 
	beq.w 	novbl

	;-------  start     ----

	bsr	actOnKeys
	
	lea.l	psubs,a4	;a4 is the table with the PlotPixels addresses
	btst 	#10,$DFF016	;press right mouse button to enable raster
	bne 	.noraster1	; timing
	move.w	#$300,$dff180	;
.noraster1:
	
	;shift screenpointers on our tripple screen system
	move.l	#activeScreen,a0
	move.l  (a0),a1		;save first
	move.l	4(a0),(a0)	;shift 2-->1
	move.l	8(a0),4(a0)	;shift 3-->2
	move.l  a1,8(a0)	;move saved first to 3rd

	;we always display 1st, we draw on 2nd and we clear 3rd
	move.l  activeScreen+8,a0 ;clear 3 screen
	bsr	clrscr		;clear it with blitter

	move.l  activeScreen,a0 ;point copperlist at 1st screen

	move.w	hslide,d1
	lsr.w	#4,d1
	add.w	d1,d1
	lea.l	(a0,d1.w),a0	;comment this to center starfield

	moveq	#4-1,d1		;
	move.l	a0,d0		;
	lea.l	CopBplP,a0	;the following loop will update the bitplane
.lb	move.w	d0,6(a0)	;pointers inside the copperlist
	swap	d0
	move.w	d0,2(a0)
	swap	d0
	add.l	#bplsize,d0
	adda	#8,a0
	dbf	d1,.lb	

	move.w	hslide,d1
	not.w	d1
	and.w	#$f,d1
	move.w	d1,d0
	lsl.w	#4,d0
	or.w	d0,d1
	move.w	d1,2(a0)

	move.l  activeScreen+4,a0; point drawing at 2nd screen
	lea.l	bplsize(a0),a1	;a1 at plane1 and so on
	lea.l	bplsize(a1),a2
	lea.l	bplsize(a2),a3



	lea.l	tabs,a6		;a6 points at our table of stars
	move.w	#NSTARS-1,d7	;ok boys and girls we're gonna do N stars
lp	move.w	starz(a6),d2	;lets move the stars towards viewer
	;sub.w	#SPEED,d2	;z = z - speed
	sub.w	speed,d2	;z = z - speed

	move.w	d2,starz(a6)	;
	bgt	.zIsOk		; z>0?, then z is still ok
.news	bsr	newstar		;random new star
.zIsOk:		
	move.w	starx(a6),d0	;gonna do the projection now
	ext.l	d0		; x first
	asl	#6,d0		; zoom factor
	divs.w	d2,d0		;do the /z part  32/16 --> 16r:16q
	add.w	#160,d0		; add screen x center
	ext.l	d0
	bmi.b	cont		;check X screen boundaries
	cmp.w	#320-1,d0	; if checks fail, random new star
	bhi.b	cont
	
	move.w	stary(a6),d1	;lets do y
	ext.l	d1
	asl	#6,d1		;zoom factor
	divs.w	d2,d1		;do the /z part
	add.w	#128,d1		;add screen y center
	bmi.b	cont		;check Y screen boundaries
	cmp.w	#256-1,d1	; if checks fail, random new star
	bhi.b	cont


	lsl.w	#6,d1		;here is where our 64 byte width screen pays off
				;z is by design in the 0-1023 range, so...
	lsr.w	#6,d2		;bring Z to 0-15 range for color
	move.l	(a4,d2.w*4),a5	;get Plot from table based on color
	jmp	(a5)		;and jump to it
				;all PlotPixels jump back to label cont
cont:	adda	#starsize,a6	;a6+=sizeof(star structure), go to next star
	dbf	d7,lp
	move.w	speedframes,d1
	beq.s   .nospeed
	subq	#1,d1
	move.w	d1,speedframes
	move.w  speed,d0
	bsr	PrintD0
.nospeed
.hold	btst 	#10,$DFF016	;press right mouse button to enable raster
	bne 	.noraster2	; timing
	;move.b	Key,d0
	;move.b  KeyRepeat,d0
	;and.w	#$ff,d0
	;move.l	timer,d0
	;and.w	#$ff,d0
	;move.b  $dff006,d0	;grab raster vertical position
	;and.w   #$ff,d0
	move.w	hslide,d0
	lsr.w	#4,d0
	move.b Key,d0
	bsr	PrintD0		;print it
.noraster2
	move.w	#$000,$dff180	
;-------  end      ----
	lea.l  timer(pc),a0 
	addq.l #1,(a0) 
	 
	move.w #$0020,$dff09c ; Intreq = interrupt processed. 
	move.w #$0020,$dff09c ; twice for compatibility 
novbl 	movem.l (sp)+,d0-d7/a0-a6 
	rte 

timer	dc.l 0


;input handler coder from
;http://eab.abime.net/showthread.php?t=45713&page=2
;http://eab.abime.net/showpost.php?p=647260&postcount=23
;Redblade
setupInputHandler:
	move.l	4.w,a6
	lea	InpEvPort,a1
	jsr	LVOAddPort(a6)

	lea	InputDevName,a0
	moveq	#0,d0
	lea	InpEvIOReq,a1
	moveq	#0,d1
	move.l	#InpEvPort,14(a1)
	jsr	LVOOpenDevice(a6)

	lea	InpEvIOReq,a1
	move	#9,IO_COMMAND(a1)	 ; ind_addhandler
	move.b	#1,IO_FLAGS(a1)
	move.l	#InpEvStuff,IO_DATA(a1)
	jsr	LVOSendIO(a6) 
	rts

removeInputHandler
	move.l	4.w,a6
	lea	InpEvIOReq,a1
	move	#10,IO_COMMAND(a1) ; ind_remhandler
	move.b	#1,IO_FLAGS(a1)
	move.l	#InpEvStuff,IO_DATA(a1)
	jsr	LVOSendIO(a6)

	lea	InpEvIOReq,a1
	jsr	LVOCloseDevice(a6)

	lea	InpEvPort,a1
	jsr	LVORemPort(a6)
	rts
;--------------------------------------
; A0-InputEvent, A1-Data Area
;
InputHandler:	movem.l	d1/a0-a3,-(sp)
		sub.l	a2,a2
		move.l	a0,a1
nextev		move.b	4(a1),d0		; ie_class
		cmp.b	#1,d0  			; rawkey
		beq.s	InpRawkey
		move.l	a1,a2
InpNext		move.l	(a1),a1
		move.l	a1,d0
		bne.s	nextev	;was bne.s	InputHandler
InphEnd		move.l	a0,d0
		movem.l	(sp)+,d1/a0-a3
		rts

InpRawkey	bsr.s	InpUnchain
		move	6(a1),d0
		move.b	d0,Key
		cmp.b	#98,d0
		beq.s	KeyRepOn
		cmp.b	#98+128,d0
		beq.s	KeyRepOff
		bra.s	InpNext

InpUnchain	move.l	a2,d0
		bne.s	InpUnc2
		move.l	(a1),a0
		rts
InpUnc2		move.l	(a1),(a2)
		rts

KeyRepOn	ST	KeyRepeat
		RTS

KeyRepOff	SF	KeyRepeat
		RTS

newstar:;expect tabs at a6, trashes d0/d1 
	bsr	Rand
	move.w	d2,d0
	swap	d2
	move.w	d2,d1
	and.w	#4095,d0
	sub.w	#2048,d0
	and.w	#4095,d1
	sub.w	#2048,d1

	move.w	d0,starx(a6)
	move.w	d1,stary(a6)
	move.w	#1023,d2 	;z is not really random, z at maximum
	move.w	d2,starz(a6)	;d2 is expected to hold z on exit
	
	rts

;Stringray's getvbr
GetVBR:
	move.l	a5,-(a7)	 ; store a5 on stack
	moveq	#0,d0	 ; Put 0, which is the default offset for the vector jumps in d0
	move.l	$4.w,a6	 ; something with exex.lib 
	btst	#0,296+1(a6)	 ; Is it 68010+ machine? 
	beq.b	.is68k	 ; nope, branch to end of this routine
	lea	.getit(pc),a5	 ; If it is 68+, load address of the -getit routine in a5
	jsr	-30(a6)	 ; And enter SuperVisor mode. This does some thingemabobs, including jumping to the .getit code.
	; Once where back here, d0 contains offset for vectors.

.is68k	move.l	(a7)+,a5 ; Restore a5 from stack
	rts

.getit
;dc.w $4e7a,$0801 for movec VBR,d0.
	movec vbr,d0	; Vector Base register to d0
	rte	 	; back to user state code

;keyboard routines lifted from Bullfrog's tutorial
;added vbl interrupt setup as well
setup_VBL_interrupt
	bsr	GetVBR
	move.l	d0,a0
	move.w  #%0000000000001000,$dff09a
	move.l	$6c(a0),old_intvbl 
	lea 	interruptlev3(pc),a1 
	move.l	a1,$6c(a0)
	move.w  #MyInts,$dff09a ;enable two bits for disk 1 & 12
	rts


;http://eab.abime.net/687445-post52.html
;Leffmann picks a rand 
Rand:	;movem.l	.state(pc),d0-d1
	;for some reason asmone chokes on pc relative here
	;I am pretty sure it used to assemble in prior source versions
	movem.l	.state,d0-d1 ;
	move.l	d0,d2
	lsl.l	#2,d0
	eor.l	d2,d2
	move.l	d1,d2
	lsr.l	#3,d2
	eor.l	d1,d2
	eor.l	d0,d2
	lsr.l	#7,d0
	eor.l	d0,d2
	movem.l	d1-d2,.state
	rts
.state	dc.l	1971,3101


;the OwnBlitter/DisownBlitter wrapping is to not have conflict
;with the floppy's mfm decoding...could be useless
;have to check when floppy access doesn't guru on me
clrscr:	lea.l	_custom,a5
	move.l	pGraphicsBase,a6
	jsr	OwnBlitter(a6)
	btst	#14,dmaconr(a5)
.wait1	btst	#14,dmaconr(a5)
	bne.s	.wait1
	move.l  a0,bltdpt(a5)	;destination pointer in D channel
	move.w	#w/8-40,bltdmod(a5)	;modulos are in bytes, not words
	move.w	#0,bltcon1(a5)	;no special modes	
	move.w	#DEST,bltcon0(a5)	;only enable destination
	move.w	#20,bltsize(a5)	;zap it
	jsr	DisownBlitter(a6)
;	btst	#14,dmaconr
;.wait2	btst	#14,dmaconr
;	bne.s	.wait2
	rts



initstars:
	move.w	#NSTARS-1,d7
	lea	tabs,a0
.ls	bsr	Rand
	move.w	d2,d0
	swap	d2
	move.w	d2,d1
	and.w	#4095,d0
	sub.w	#2048,d0
	and.w	#4095,d1
	sub.w	#2048,d1
	move.w	d0,starx(a0)
	move.w	d1,stary(a0)
	bsr	Rand
	and.w	#1023,d2
	move.w	d2,starz(a0)
	adda	#starsize,a0
	dbf	d7,.ls
	rts		

GetFont	
	movem.l	d0-d7/a0-a6,-(sp)
	move.w	ltf_Modulo,d0
	lea	Font,a1			;destination
	move.l	TopazCharData,a0	;source
	lea.l	16(a0),a0
	moveq	#10-1,d6
.line	move.l	a0,a2
	move.l	a1,a3
	moveq	#8-1,d7
.copy	move.b	(a2),(a3)
	add.w	d0,a2
	add.w	#40,a3
	dbf	d7,.copy
	addq.w	#1,a0
	addq.w	#1,a1
	dbf	d6,.line
	movem.l	(sp)+,d0-d7/a0-a6
	rts


PrintD0	;move.w	#$005,$dff180
	movem.l	d0-d2/a0-a3,-(sp)
	;move	#12345,d0
	ext.l	d0
	lea	nbuf,a0
	divu.w	#10000,d0
	move.b	d0,(a0)+
 	move.w	#0,d0
	swap	d0
	divu.w	#1000,d0
	move.b	d0,(a0)+
	move.w	#0,d0
	swap	d0
	divu.w	#100,d0
	move.b	d0,(a0)+
	move.w	#0,d0
	swap	d0
	divu.w	#10,d0
	move.b	d0,(a0)+
	move.w	#0,d0
	swap	d0
	move.b	d0,(a0)+
	

	
	move.l	activeScreen+4,a1
	move.w	hslide,d1
	lea.l	(a1,d1.w),a1
	lea	(256-8)*64(a1),a1
	lea	Font,a2
	lea	nbuf,a0
	moveq	#5-1,d2
.skLead	move.b	(a0)+,d0
	ext.w	d0

	move.l	a1,a3
	addq.l	#1,a1
	
	moveq	#8-1,d1
.lchar	move.b	(a2,d0.w),(a3)
	add.w	#40,d0
	lea	(64,a3),a3
	dbra	d1,.lchar
	dbra	d2,.skLead
	movem.l	(sp)+,d0-d2/a0-a3
	;move.w	#$000,$dff180
	rts
	
	;first will be display, second drawing, third clearing
activeScreen dc.l Screen1,Screen2,Screen3
nbuf	dc.b	0,1,3,4,5,0,0,0,0,0
gfxname:
	dc.b "graphics.library",0
	EVEN
dosname:
	dc.b "dos.library",0
	EVEN
;Here is the star 'structure'
;x,y,z are 'world' coordinates
;starsize is the structure's size
	RSRESET
starx	rs.w	1
stary	rs.w	1
starz	rs.w	1
starsize rs.w	0

lTextAttr:
	dc.l	0
	dc.w	8
	dc.b	0,0
TopazName:
	dc.b	'topaz.font',0
	even
TopazCharData:
	dc.l	0
ltf_Modulo dc.w	0	

InpEvStuff	dc.l	0,0
		dc.b	2,52 ; type, priority
		dc.l	inpevname
		dc.l	0,InputHandler

inpevname	dc.b "just_InputHandler",0
		even
InputDevName	dc.b	'input.device',0
		even

speed   dc.w	7
hslide	dc.w	0
	SECTION	tutdata,DATA_C
Copper:
	dc.w	$1fc,0		;slow fetch mode, AGA compartibility
	dc.w	$100,$0200	; set number of bitplanes to display to zero
	dc.w	$8e,$2c81
	dc.w	$90,$2cc1
	dc.w	$2b07,$fffe	;when I moved the stuff in vbl, had timing
				;issues (bottom half screen not showing right)
				;this fixes it

	dc.w	$92,$30		;DMA start horiz (38 pre-smooth-scroll)
	dc.w	$94,$d0		;DMA stop  horiz
	dc.w	$108,22		;modulo for odd bitplanes (24 pre-smooth-scroll)
	dc.w	$10a,22		;modulo for even bitplanes (24 pre-smooth-scroll)
		
CopBplP:
	dc.w	$e0,0		;bitplane 0
	dc.w	$e2,0		
	dc.w	$e4,0		;bitplane 1
	dc.w	$e6,0
	dc.w	$e8,0		;bitplane 2
	dc.w	$ea,0		;
	dc.w	$ec,0		;bitplane 3
	dc.w	$ee,0
	
	dc.w	$102,$0000
	dc.w	$100,$4200

	dc.w	$ffff,$fffe


	EVEN
	;phx's suggestion to reduce executable's size
	SECTION startable,BSS 


oDMA		ds.w	1
oINTENA		ds.w	1
oldintreq 	ds.w	1
oldcopper 	ds.l	1
old_keyboard_int ds.l 1
old_intvbl:	ds.l	1
pGraphicsBase:	ds.l	1
pDOSBase:	ds.l	1
tabs:		ds.b	NSTARS*starsize
filebuffer:	ds.b	24*1024

speedframes:	ds.w	1

Key		ds.b	1
KeyRepeat	ds.b	1
InpEvPort	ds.b	34
InpEvIOReq	ds.b	48
	SECTION chip,BSS_C
Font:	 ds.b	8*40
Screen1: ds.b	4*bplsize	;4 is the number of bitplanes	
Screen2: ds.b	4*bplsize	;
Screen3: ds.b	4*bplsize	;


SU_tf_CharData = 34
OpenFont = -72
_LVODelay	= -198
tf_Modulo = 38

int_set = $8000
dma_set = $8000
MODE_OLDFILE	EQU	1005
reserve EQU	4
vsize	EQU	6
count	SET	-vsize*(reserve+1)
LIBENT	MACRO
_LVO\1	EQU	count
count	SET	count-vsize
	ENDM

   LIBENT   Open
   LIBENT   Close
   LIBENT   Read

bltdpt	EQU	$054
bltdmod EQU	$066
dmaconr EQU	$002
bltcon0 EQU	$040
bltcon1 EQU	$042
bltsize EQU	$058
DEST      equ   $100
OwnBlitter	EQU	-456
DisownBlitter	EQU	-462

_custom 	equ	$dff000
_ciaa		equ	$bfe001
intenar		= $1c
cop1lc		= $80
intreq	    	EQU   	$09C
intena		equ	$09A
dmacon		equ	$096
bplcon3		= $106
bplcon4		= $10c
vposr	    	EQU   	$004
vhposr	    	EQU   	$006
INTB_PORTS  	EQU   	(3)   ;I/O Ports and timers
INTF_PORTS     	EQU   	(1<<3)
intreqr     	EQU   	$01E
ciasdr	  	EQU	$0C00
ciaicr	  	EQU	$0D00
ciacra	  	EQU	$0E00

CIACRAF_SPMODE	  EQU	(1<<6)
* interrupt control register bit numbers
CIAICRB_TA	  EQU	0
CIAICRB_TB	  EQU	1
CIAICRB_ALRM	  EQU	2
CIAICRB_SP	  EQU	3
CIAICRB_FLG	  EQU	4
CIAICRB_IR	  EQU	7
CIAICRB_SETCLR	  EQU	7
DMAF_SETCLR    EQU   $8000
DMAF_AUDIO     EQU   $000F  * 4 bit mask
DMAF_AUD0      EQU   $0001
DMAF_AUD1      EQU   $0002
DMAF_AUD2      EQU   $0004
DMAF_AUD3      EQU   $0008
DMAF_DISK      EQU   $0010
DMAF_SPRITE    EQU   $0020
DMAF_BLITTER   EQU   $0040
DMAF_COPPER    EQU   $0080
DMAF_RASTER    EQU   $0100
DMAF_MASTER    EQU   $0200
DMAF_BLITHOG   EQU   $0400
DMAF_ALL       EQU   $01FF  * all dma channels

LVOOpenDevice	EQU	-444
LVOCloseDevice	EQU	-450
LVODoIO		EQU	-456
LVOSendIO	EQU	-462
LVOAddPort	EQU	-354
LVORemPort	EQU	-360
LVOWaitPort	EQU	-384

; IO Block Offsets
IO_COMMAND	EQU	$1C
IO_FLAGS	EQU	$1E
IO_ACTUAL	EQU	$20
IO_LENGTH	EQU	$24
IO_DATA		EQU	$28
IO_OFFSET	EQU	$2C
