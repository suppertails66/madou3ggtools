
.include "sys/sms_arch.s"
  ;.include "base/ram.s"
;.include "base/macros.s"
  ;.include "res/defines.s"

.rombankmap
  bankstotal 64
  banksize $4000
  banks 64
.endro

.emptyfill $FF

.background "madou3.gg"

;.unbackground $80000 $FFFFF
; don't free the script banks
.unbackground $80000 $FFFFF

;======================
; free unused space
;======================

; end-of-banks
;.unbackground $7F6B $7FEF
.unbackground $7F60 $7FEF
.unbackground $B110 $BFFF
;.unbackground $BA60 $BB5F
.unbackground $B110 $BFFF

; diacritic handler
;.unbackground $281E $2839
.unbackground $288F $28AA

; dictionary handler
;.unbackground $28C8 $28EB
.unbackground $2938 $295E

.include "vwf_consts.inc"
.include "ram.inc"
.include "util.s"
.include "vwf.s"
.include "vwf_user.s"

;.macro orig_read16BitTable
;  rst $20
;.endm

; B = tile count
; DE = srcptr
; HL = dstcmd
.macro rawTilesToVdp_macro
  ; set vdp dst
  ld c,vdpCtrlPort
  out (c),l
  out (c),h
  ; write data to data port
  ex de,hl
  dec c
  ld a,b
  -:
    .rept bytesPerTile
      push ix
      pop ix
      outi
    .endr
    dec a
    jp nz,-
.endm

; B = tile count
; DE = srcptr
; HL = dstcmd
.macro rawTilesToVdp_macro_safe
  ; set vdp dst
  ld c,vdpCtrlPort
  out (c),l
  nop
  out (c),h
  nop
  ; write data to data port
  ex de,hl
  dec c
  ld a,b
  -:
    .rept bytesPerTile
      push ix
      pop ix
      outi
    .endr
    dec a
    jp nz,-
.endm

; B = tile count
; DE = srcptr
; HL = srccmd
.macro rawTilesFromVdp_macro
  ; set vdp src
  ld c,vdpCtrlPort
  out (c),l
  nop
  out (c),h
  nop
  ; read data from data port
  ex de,hl
  dec c
  ld a,b
  -:
    .rept bytesPerTile
      push ix
      pop ix
      ini
    .endr
    dec a
    jp nz,-
.endm

; BC = tile count
; DE = srcptr
; HL = dstcmd
.macro rawTilesToVdp_big_macro
  push bc
    ; set vdp dst
    ld c,vdpCtrlPort
    out (c),l
    nop
    out (c),h
    nop
  pop bc
  ; write data to data port
  ex de,hl
  -:
    push bc
      ld c,vdpDataPort
      .rept bytesPerTile
        push ix
        pop ix
        outi
      .endr
    pop bc
    
    dec bc
    ld a,b
    or c
    jr nz,-
.endm

;.macro old_read16BitTable
;  rst $28
;.endm

;===============================================
; Update header after building
;===============================================
.smstag

;========================================
; local defines
;========================================

.define numScriptRegions 5

;========================================
; vwf settings
;========================================

;  ld a,vwfTileSize_main
;  ld b,vwfScrollZeroFlag_main
;  ld c,vwfNametableHighMask_main
;  ld hl,vwfTileBase_main
;  doBankedCall setUpVwfTileAlloc

;.bank $01 slot 1
;.section "extra startup code" free
;  newStartup:
;    ; init vwf
;    ld a,vwfTileSize_main
;    ld b,vwfScrollZeroFlag_main
;    ld c,vwfNametableHighMask_main
;    ld hl,vwfTileBase_main
;    doBankedCallSlot1 setUpVwfTileAlloc
;    
;    ret
;.ends

;========================================
; script
;========================================

; each script region is assigned one bank starting from this bank
.define scriptBaseBank $20

;.include "out/script/string_bucket_hashtabledialogue.inc"
;
;.slot 2
;.section "enemy names" superfree
;  enemyNames:
;    .incbin "out/script/enemies.bin"
;  enemyNamesPlural:
;    .incbin "out/script/enemies_plural.bin"
;  
;  
;.ends

.bank scriptBaseBank+0 slot 2
.org $0000
.section "script region 0" force
  scriptRegion0:
    .incbin "out/script/region0.bin"
.ends

.bank scriptBaseBank+1 slot 2
.org $0000
.section "script region 1" force
  scriptRegion1:
    .incbin "out/script/region1.bin"
.ends

/*.bank scriptBaseBank+2 slot 2
.org $0000
.section "script region 2" force
  scriptRegion2:
    .incbin "out/script/region2.bin"
.ends */

.bank scriptBaseBank+2 slot 2
.org $0000
.section "script region 2a" force
  scriptRegion2a:
    .incbin "out/script/region2a.bin"
.ends

.bank scriptBaseBank+2+numScriptRegions slot 2
.org $0000
.section "script region 2b" force
  scriptRegion2b:
    .incbin "out/script/region2b.bin"
.ends

.bank scriptBaseBank+3 slot 2
.org $0000
.section "script region 3" force
  scriptRegion3:
    .incbin "out/script/region3.bin"
.ends

.bank scriptBaseBank+4 slot 2
.org $0000
.section "script region 4" force
  scriptRegion4:
    .incbin "out/script/region4.bin"
.ends

;========================================
; use new script
;========================================

.bank 0 slot 0
;.org $26A4
.org $2715
.section "use new script" overwrite
  ; B = region num
  ; HL = $FFFF
  
  doBankedCallSlot2 useNewScript2
  
  ; load target bank in slot2
;  ld (hl),a
  ld (de),a
  ; base pointer will always be $8000
  ld hl,$8000
  ; jump to script lookup
;  jp $26D5
  jp $273C
.ends

.slot 2
.section "use new script 2" superfree
  useNewScript2:
    ; B = region num
    ; HL = $FFFF
    
    ; if region is 2 and message number >= 0x80, target second half
    ; in extra bank
    ld a,2
    cp b
    jr nz,+
      ld a,$7F
      cp c
      jr nc,+
        ld a,b
        add a,numScriptRegions
        ld b,a
        
        ld a,c
        sub $80
        ld c,a
    +:
    
    ; new target bank = scriptBaseBank+$20
    ld a,scriptBaseBank
    add a,b
    
    ret
.ends

; unbackground disused space
;.unbackground $26B9 $26D4
.unbackground $272A $273B

;========================================
; use vwf printing
;========================================

.define scriptIncrementAndContinue $288C

; reset vwf before starting new string
.bank 0 slot 0
;.org $2714
.org $2780
.section "use new printing 1a" overwrite
  jp newScriptRunLoop
.ends

.bank 0 slot 0
.section "use new printing 1b" free
  newScriptRunLoop:
    ; reset vwf before running script
;    call resetVwf
    doBankedCallSlot2 fullyResetVwf
    -:
      ; handle next command
      call handleNextScriptCmd
      ; continue while termination code not returned
      or a
      jr z,-
    ret
.ends

.bank 0 slot 0
;.org $27D4
.org $2840
;.section "use new printing 2" SIZE $47 overwrite
.section "use new printing 2" SIZE $4A overwrite
  ; save tilemap target address
  ld (vwfTilemapTargetAddr),de
  
  ; fetch from srcptr
  ld a,(hl)
  
  ; check for char-by-char print flag
  cp vwfCbcModeIndex
  jr nz,+
    ; save address of remaining script data for future printing operations
    ld a,(mapperSlot2Ctrl)
    ld (vwfCbcScriptBank),a
    inc hl
    ld (vwfCbcScriptAddr),hl
    
    ; reset vwf in preparation for cbc print
    doBankedJumpSlot2 doCbcSetup
  +:
  
  ; print character
;  push hl
  ; B = src bank
  ld a,(mapperSlot2Ctrl)
  ld b,a
  ; A = character to handle
  ld a,(hl)
  
  ; hl should point to next script byte
  inc hl
    
    doBankedCallSlot2 handleVwfOp
    
    ; get updated tilemap target address
    ld de,(vwfTilemapTargetAddr)
  ; will be incremented back in following code
  dec hl
;  pop hl
  
  @done:
  jp scriptIncrementAndContinue
  
.ends

; reset vwf at start of each box
.bank 0 slot 0
;.org $286A
.org $28DA
.section "use new printing 3a" overwrite
  jp resetVwfBeforeBox
.ends

.bank 1 slot 1
.section "use new printing 3b" free
  resetVwfBeforeBox:
    ; make up work
    ld (pendingExpRamTileCount),a
    
    doBankedCallSlot2 fullyResetVwf
    jp scriptIncrementAndContinue
.ends

;========================================
; 1-line linebreaks
;========================================

.bank 0 slot 0
;.org $28A0
.org $2910
.section "linebreak height" overwrite
  ; bytes in virtual tilemap to skip
  ld de,$0050/2
.ends

;========================================
; reset vwf on linebreak
;========================================

.bank 0 slot 0
;.org $28A9
.org $2919
.section "linebreak 1" overwrite
  ; done
  jp linebreakVwfReset
.ends

.bank 0 slot 0
.section "linebreak 2" free
  linebreakVwfReset:
    doBankedCallSlot2 resetVwf
    jp scriptIncrementAndContinue
.ends

;========================================
; char-by-char printing
;========================================

.bank 0 slot 0
;.org $2B64
.org $29D9
;.section "char-by-char printing 1" SIZE $5A overwrite
;.section "char-by-char printing 1" SIZE $24 overwrite
.section "char-by-char printing 1" SIZE $24 overwrite
  ; if cbc printing off, do nothing
  ld a,(vwfCbcActiveFlag)
  or a
  ret z
  
  ; fetch next script byte
  ld a,(vwfCbcScriptBank)
  ld b,a
  ld hl,(vwfCbcScriptAddr)
  push hl
    call bankedFetch
    
    doBankedCallSlot2 cbcPrint
  @done:
  pop hl

  ; advance scriptptr
  inc hl
  ld (vwfCbcScriptAddr),hl
  
  ret
.ends

;.unbackground $2B88 $2BBD

;.unbackground $29FD $2A1B
; 2A0E+ is individually called to flag the bottom box as dirty
.unbackground $29FD $2A0D

;========================================
; use extra VRAM to allow for more text
; in boxes
;========================================

; if more material is pending than will fit in the original area,
; split up the transfer
.bank 0 slot 0
;.org $2795
.org $2801
.section "extra text space 1" overwrite
  doBankedCallSlot2 extraTextSpace_transferSplit
  
;  jp $27AC
  jp $2818
.ends

.slot 2
.section "extra text space 2" superfree
  extraTextSpace_transferSplit:
    ; HL = srcaddr
    ; DE = vdpdst
    
    ; save current vdpdst
    ld (currentTextTilesVdpTarget),de
    ld (currentTextTilesExpRamTarget),hl
      
    ; get current text box type
    ; 0 = right box
    ; 1 = left box
    ; 2 = bottom box
    ld a,(textWindowType)
    cp 1
    jr z,@leftBox
    cp 2
    jp z,@bottomBox
      
    @rightBox:
      ld hl,rightBoxNewSpaceVdpAddr
      ld (boxNewSpaceVdpAddr),hl
      ld hl,rightBoxOldSpaceEndTileNum
      ld (boxOldSpaceEndTileNum),hl
      ld hl,rightBoxOldSpaceEndVdpAddr
      ld (boxOldSpaceEndVdpAddr),hl
      jr @firstTransferCheck
      
    @leftBox:
      ld hl,leftBoxNewSpaceVdpAddr
      ld (boxNewSpaceVdpAddr),hl
      ld hl,leftBoxOldSpaceEndTileNum
      ld (boxOldSpaceEndTileNum),hl
      ld hl,leftBoxOldSpaceEndVdpAddr
      ld (boxOldSpaceEndVdpAddr),hl
      ; drop through
    
    @firstTransferCheck:
      ;=====
      ; if vdpdst (de) >= boxNewSpaceVdpAddr, we've already moved
      ; to the new area: skip
      ;=====
      
      ld hl,(boxNewSpaceVdpAddr)
      or a
      sbc hl,de
      
;      jr z,+
;      jr c,+
        jp c,@finalTransfer
;      +:
      
      ;=====
      ; if (vdpdst + (tilecount * bytesPerTile)) <= boxOldSpaceEndVdpAddr,
      ; we're transferring entirely to the old area: skip
      ;=====
      
      ; A = count of tiles to be sent
      ld a,(pendingExpRamTileCount)
      
      ; HL = vdpdst
;      ld hl,(currentTextTilesVdpTarget)
      ; divide by bytesPerTile (32)
      .rept 5
        srl d
        rr e
      .endr
      
      ; A = low byte of target tile index (high byte is 1)
      add a,e
      
      ; if target tile index <= boxOldSpaceEndTileNum, no second
      ; transfer needed
      ld hl,boxOldSpaceEndTileNum
;      cp <boxOldSpaceEndTileNum
      cp (hl)
      jp z,@finalTransfer
      jp c,@finalTransfer
      
      ;=====
      ; otherwise,
      ; transfer ((boxOldSpaceEndVdpAddr - vdpdst) / bytesPerTile) tiles
      ; to vdpdst.
      ; then set up for final transfer to boxNewSpaceVdpAddr
      ; with (tilecount - ((boxOldSpaceEndVdpAddr - vdpdst) / bytesPerTile))
      ; tiles.
      ;=====
      
      ld hl,(boxOldSpaceEndVdpAddr)
      ld de,(currentTextTilesVdpTarget)
      or a
      sbc hl,de
      ; divide by bytesPerTile (32)
      .rept 5
        srl h
        rr l
      .endr
      
      ; A = first transfer size
      ld a,l
      
      ; do transfer
      ld hl,(currentTextTilesExpRamTarget)
      ld de,(currentTextTilesVdpTarget)
      push af
        call sendPendingExpRamTiles
      pop af
      
      ; set up new srcaddr for final transfer
      push af
        ld de,(currentTextTilesExpRamTarget)
        
        ; multiply tiles transferred by bytesPerTile (32)
        ld l,$00
        srl a
        rr l
        srl a
        rr l
        srl a
        rr l
        ld h,a
        
        ; add srcaddr
        add hl,de
        ld (currentTextTilesExpRamTarget),hl
      pop af
      
      ; calculate size of final transfer
      ld e,a
      ld a,(pendingExpRamTileCount)
      sub e
      ld (pendingExpRamTileCount),a
      
      ; vdpdst = new space
      ld de,(boxNewSpaceVdpAddr)
      ld (currentTextTilesVdpTarget),de
      
      jp @finalTransfer
      
      
    @bottomBox:
      ; do nothing
    
    
    
    @finalTransfer:
    ld hl,(currentTextTilesExpRamTarget)
    ld de,(currentTextTilesVdpTarget)
    ld a,(pendingExpRamTileCount)
    push af
      call sendPendingExpRamTiles
    pop af

    ; make up work
    ; HL = tile count * 32
    ld l,$00
    srl a
    rr l
    srl a
    rr l
    srl a
    rr l
    ld h,a
    ; update target VDP address
;    ld (currentTextTilesVdpTarget),hl
    
    ; mark transfer as complete
;    res 6,(hl)
    ret
.ends



;========================================
; don't print hardcoded prices in shops
; (they're now handled with script ops)
;========================================

; buy/sell price
.bank 0 slot 0
.org $3889
.section "no shop prices 1" overwrite
  nop
  nop
  nop
.ends

; sell price confirmation
.bank 0 slot 0
.org $3989
.section "no shop prices 2" overwrite
  nop
  nop
  nop
.ends

;========================================
; don't print hardcoded save file
; numbers
;========================================

.bank 0 slot 0
.org $3347
.section "no save file numbers 1" overwrite
  jp $2D1B
.ends

;========================================
; don't print hardcoded key numbers
;========================================

.bank 1 slot 1
.org $2B7A
.section "no key numbers 1" overwrite
  doBankedCallSlot2 newKeyNumHandler
  nop
.ends

.slot 2
.section "no key numbers 2" superfree
  newKeyNumHandler:
    ; C = key number
    
    ; write key number to BCD buffer
    ld a,c
    ld hl,numberBufferStart+3
    ld (hl),a
    
    ; clear remaining bytes in buffer
    xor a
    ld b,3
    @loop:
      dec hl
      ld (hl),a
      djnz @loop
    
    ; run corresponding script
    ld c,$02
    jp runRegion2Script
.ends

;========================================
; don't print hardcoded dungeon floor
; numbers
;========================================

.bank 1 slot 1
.org $158F
.section "no dungeon floor numbers 1" overwrite
  doBankedCallSlot2 newDungeonFloorNumHandler
  jp $55BC
.ends

.slot 2
.section "no dungeon floor numbers 2" superfree
  newDungeonFloorNumHandler:
    ; make up work: show map
;    ld hl,tilemapSettingsFlags
;    set 4,(hl)
;    call $264C
;    
    ; A = internal floor ID
    ld a,($C0CE)
    ld b,$02
    
    ; decide which section of the dungeon we're in and
    ; print the appropriate map label
    ld c,$6E
    cp $02
    jr c,@result1
      ld b,$07
      inc c
      cp $07
      jr c,@result1
        ld b,$0A
        inc c
        cp $0A
        jr c,@result1
          inc c
          ld d,$09
          jr @result2
    @result1:
      ld d,a
      ld a,b
    @result2:
    sub d
    
    ; now:
    ; A = displayed floor number
    ; C = target script number
    
    ; place floor number in BCD buffer
    
    ; write number to BCD buffer
    ld hl,numberBufferStart+3
    ld (hl),a
    
    ; clear remaining bytes in buffer
    xor a
    ld b,3
    @loop:
      dec hl
      ld (hl),a
      djnz @loop
    
    ; run corresponding script
    jp runRegion0Script
.ends

;========================================
; we're storing extra dialogue tiles in
; the cropped-out bottom of the tilemap.
; this is fine most of the time, but
; some screen-shake effects briefly show
; this area and now need to clean out
; any garbage beforehand.
;========================================

.macro clearVdpTopRowTiles
  ; clear garbage
  push hl
  push de
  push bc
    ld b,2
    ld de,@clearVdpTopRowTiles_newTilemapStorageRowClearTiles
    ld hl,$7D40
    di
      rawTilesToVdp_macro_safe
    ei
  pop bc
  pop de
  pop hl
  jp @clearVdpTopRowTiles_done
  
  @clearVdpTopRowTiles_newTilemapStorageRowClearTiles:
    .rept 2
      .rept bytesPerTile
        .db $00
      .endr
    .endr
  
  @clearVdpTopRowTiles_done:
  
.endm

  ;========================================
  ; blowing up mansion event
  ;========================================

  .bank 1 slot 1
  .org $3D1F
  .section "mansion destruction shake fix 1" overwrite
    call mansionShakeFix
  .ends

  .bank 2 slot 2
  .section "mansion destruction shake fix 2" free
    mansionShakeFix:
      doBankedCallSlot2 clearShakeTiles
      jp $8012
  .ends

  .slot 2
  .section "mansion destruction shake fix 3" superfree
    clearShakeTiles:
      clearVdpTopRowTiles
      ret
  .ends

  ;========================================
  ; lipemco earthquake
  ;========================================

  .bank 2 slot 2
  .org $1527
  .section "lipemco earthquake shake fix 1" overwrite
    doBankedCallSlot2 lipemcoEarthquakeShakeFix
    nop
    nop
  .ends

  .slot 2
  .section "lipemco earthquake shake fix 2" superfree
    lipemcoEarthquakeShakeFix:
      ; clear tiles
      clearVdpTopRowTiles
      
      ; make up work
      call $2FE8
      ld a,$02
      call $1F4D
      ld c,$A5
      call runRegion3Script
      ld de,$0028
      ret
  .ends

  ;========================================
  ; lipemco sea lions
  ;========================================

  .bank 2 slot 2
  .org $158E
  .section "lipemco sea lions shake fix 1" overwrite
    doBankedCallSlot2 lipemcoSeaLionsShakeFix
    nop
    nop
  .ends

  .slot 2
  .section "lipemco sea lions shake fix 2" superfree
    lipemcoSeaLionsShakeFix:
      ; clear tiles
      clearVdpTopRowTiles
      
      ; make up work
      call $2FE8
      ld a,$02
      call $1F4D
      ld c,$B1
      call runRegion3Script
      ld de,$003C
      ret
  .ends

  ;========================================
  ; generic vertical shake for e.g. trap treasure
  ; chest on floor 3
  ;========================================

  .bank 0 slot 0
  .org $2E09
  .section "screen shake fix 1" overwrite
    doBankedJumpSlot2 fixScreenShakeGenericVertical
  .ends

  ;========================================
  ; generic horizontal shake for e.g. minotauros
  ; battle intro
  ;========================================

  .bank 0 slot 0
  .org $2E1D
  .section "screen shake fix 3" overwrite
    doBankedJumpSlot2 fixScreenShakeGenericHorizontal
  .ends

  ;========================================
  ; shared
  ;========================================

  .slot 2
  .section "screen shake fix 2" superfree
    fixScreenShakeGenericVertical:
      clearVdpTopRowTiles
      
      ; make up work
      push de
        ld de,$C005
;        ld hl,$2E26
        ld hl,shakeDataVertical
        ld b,$0C
        ; loop
        @loop:
          ld a,(hl)
          inc hl
          ld (de),a
          xor a
          call $08C3
          djnz @loop
      pop de
      ret
    
    fixScreenShakeGenericHorizontal:
;      clearVdpTopRowTiles
      
      ; make up work
      push de
        ld de,$C007
;        ld hl,$2E32
        ld hl,shakeDataHorizontal
        ld b,$0C
        ; loop
        @loop:
          ld a,(hl)
          inc hl
          ld (de),a
          xor a
          call $08C3
          djnz @loop
      pop de
      ret
    
    ; sequence of x/y offsets to cycle through
    shakeDataVertical:
      .db $03,$05,$06,$07,$06,$05,$03,$00,$DE,$DD,$DE,$00
    shakeDataHorizontal:
      .db $03,$05,$06,$07,$06,$05,$03,$00,$FD,$FC,$FD,$00
  .ends

;========================================
; insert item names on correct lines
;========================================

  ;========================================
  ; fucking window opcodes
  ;========================================

/*  .bank 0 slot 0
  .org $2754
  .section "new box fix 1" overwrite
    call newBoxFix
  .ends

  .bank 0 slot 0
  .section "new box fix 2" free
    newBoxFix:
      ld a,(noBoxReset)
      or a
      jr nz,+
        ; right-box reset
        jp $28B2
      +:
;      jp $28F2
      ret
  .ends */

  ;========================================
  ; lost item at bathhouse
  ;========================================

  .bank 1 slot 1
  .org $31A1
  .section "bathhouse lost item 1" overwrite
    doBankedCallSlot2 bathhouseLostItemFix
    nop
    nop
  .ends

  .slot 2
  .section "bathhouse lost item 2" superfree
    bathhouseLostItemFix:
      ; make sure putpos is not reset after script runs
  ;    push hl
  ;      ld hl,$C073
  ;      set 7,(hl)
  ;    pop hl
      
      ; turn off transfer-tiles-when-done while we compose
      ; the individual scripts into the full message
/*      ld hl,$C034
      set 5,(hl)
      
        ; call newly added script 4-0xFF
        ; ("My[br]")
        ld b,4
        ld c,$FF
        call runScript
    
        ld a,$FF
        ld (noBoxReset),a
      
;;      ld hl,$C073
;;      set 7,(hl)
;;      ld hl,$C034
;;      set 5,(hl)
;      ; print item name on own line
;      ld a,$00
;      ld ($C074),a
;      ld hl,$C0F6
;      call $6607
;;      call $660E
        
        ; print item name on own line
        ld a,$00
        ld ($C074),a
        ld hl,$C0F6
        call $6607
;        push bc
;          ld a,(hl)
;          add a,$C7
;          ld c,a
;          ld b,3
;;          call runScript
;          call runScriptNoClear
;        pop bc
        
        xor a
        ld (noBoxReset),a
  
      ; turn on transfer-tiles-when-done
      ld hl,$C034
      res 5,(hl) */
      
      ; jesus christ.
      ; okay, because this game has been very specifically designed in
      ; such a way as to make it almost impossible to concatenate
      ; messages unless it's by the very very specific method used in
      ; the original game, let's compose the whole damn thing in
      ; RAM and run it from there.
      
/*      ld de,ramScriptBuffer
      
      ; part 1
      ld b,4
      ld c,$FF
      call copyScriptToRamBuf
      
      ; item name
      ld a,($C0F6)
      add a,$C7
      ld c,a
      ld b,3
      call copyScriptToRamBuf
      
      ; part 2
      ld b,2
      ld c,$3C
      call copyScriptToRamBuf
      
      ; add terminator
      ld a,$FF
      ld (de),a
      
      ; now print the damn thing
      ld hl,$C073
      set 7,(hl)
      ld hl,ramScriptBuffer
      jp $2775
      
      ; print original script 2-0x3C
      ; ("[br]disappeared!")
;      ld c,$3C
;      jp $26DC
    
    ; b = region
    ; c = scriptnum
    ; de = dst
    copyScriptToRamBuf:
      ; get correct bank for region
      doBankedCallSlot2 useNewScript2
      
      ; load target bank in slot1
      push af
        ; wait for vblank so we can hopefully get the copy done without
        ; stalling the sound driver or something
        xor a
        call waitVblank
      pop af
      di
        ld (mapperSlot1Ctrl),a
        
        ; base pointer will always be $4000
        ld hl,$4000
        
        ; look up offset and add to base
        push hl
          ld b,$00
          sla c
          rl b
          add hl,bc
          ld c,(hl)
          inc hl
          ld b,(hl)
        pop hl
        add hl,bc
        
        ; now, HL = slot1 pointer
        ; copy content
        @copyLoop:
          ld a,(hl)
          inc hl
          ; check for terminator
          ; (inline FF not handled but if we ever need that i will personally
          ; ... do something, i don't know what)
          cp $FF
          jr z,@copyDone
            ld (de),a
            inc de
            jr @copyLoop
        @copyDone:
        
      ; reload normal bank
      ld a,$01
      ld (mapperSlot1Ctrl),a
      ei
      
      ret */
    
    ; AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    
    xor a
    ld ($C074),a
    
    ; print item name
    ld hl,$C0F6
    call $6607
    
    ; move item name down a line
    ld hl,$D3CE
    ld de,$D3CE+(20*2*1)
    call $661D
    
    ; "lost [x]"
    ld c,$3C
    call $26DC
    
    ret
    
  .ends

;========================================
; carbuncle item eating fix
;========================================

.bank 1 slot 1
.org $2135
.section "carbuncle item eat fix 1" overwrite
  doBankedCallSlot2 carbuncleItemEatFix
  jp $6155
.ends

.slot 2
.section "carbuncle item eat fix 2" superfree
  carbuncleItemEatFix:
    push hl
      ; initial message
      ld c,$BA
      call runRegion0Script
      
      ; ?
      xor a
      ld ($C074),a
      
      ; print item name
      call $6607
      
      ; the item name now occupies the top row of
      ; the back tilemap buffer.
      ; move it down two lines.
;      ld hl,$D3A6
;      ld de,$D3F6
;      call $661D
      ld hl,$D3CE
      ld de,$D3CE+(20*2*2)
      call $661D
      
      ; "he eats the [x]"
      ld c,$BB
      call $26E4
      
      ; arle scold
      ld c,$BC
      call runRegion0Script
    pop hl
    
    ; print item name again
    call $6607
    ; move down a line
    ld hl,$D3CE
    ld de,$D3CE+(20*2*1)
    call $661D
    
    ; "the [x] was eaten"
    ld c,$BD
    call $26E4
    
    ret
.ends

;========================================
; use new title screen
;========================================

  ; new stuff
  .slot 1
  .section "new title screen components" superfree
    newTitleLogo_grp: .incbin "out/grp/title_logo.bin" FSIZE newTitleLogo_grp_size
    .define newTitleLogo_grp_numTiles newTitleLogo_grp_size/bytesPerTile
    
;    newTitleComponent_grp: .incbin "out/grp/title_subcomponents.bin" FSIZE newTitleComponent_grp_size
    newTitleComponent_grp: .incbin "rsrc_raw/grp/title_bg2.bin" FSIZE newTitleComponent_grp_size
    .define newTitleComponent_grp_numTiles newTitleComponent_grp_size/bytesPerTile
    
    newTitleLogo_map: .incbin "out/maps/title_logo.bin"
    newTitleLogo_withSubtitle_map: .incbin "out/maps/title_logo_withsubtitle.bin"
  .ends

  ; overwrite original logo tilemap
;  .bank $D slot 1
;  .org $1B70
;  .section "new title screen logo 1" overwrite
;    .incbin "out/maps/title_logo.bin"
;  .ends

  ; use new logo
  .bank 0 slot 0
  .org $07D9
  .section "new title screen logo 2" overwrite
    ; title
    ld a,newTitleLogo_grp_numTiles
    ld b,:newTitleLogo_grp
    ld hl,newTitleLogo_grp
    ; vdp dst
    ld de,$4000
    call sendRawGrpDataToVdp
    
    ; components
    ld a,newTitleComponent_grp_numTiles
    ld hl,newTitleComponent_grp
    ; vdp dst
    ld de,$6000
    call sendRawGrpDataToVdp
    
    ; map
    ld a,b      ; bank
    ld b,$0B+2    ; height in tiles
    ld de,$D228-(20*2*1)
    ld hl,newTitleLogo_map
    call $2DB0
  .ends

  ; use new logo with subtitle
  .bank 0 slot 0
  .org $0805
  .section "new title screen logo 3" overwrite
    ; map
    ld a,:newTitleLogo_withSubtitle_map
    ld b,$0B+2    ; height in tiles
    ld de,$D228-(20*2*1)
    ld hl,newTitleLogo_withSubtitle_map
    call $2DB0
  .ends

;========================================
; use new menu buttons
;========================================

  ;========================================
  ; graphics
  ;========================================
  
  .slot 1
  .section "new button graphics 1" superfree
    newCompassGrp: .incbin "out/grp/compass.bin" FSIZE newCompassGrp_size
    .define newCompassGrp_numTiles newCompassGrp_size/bytesPerTile
    
    newButtonsMapSaveGrp: .incbin "out/grp/buttons_map_save.bin" FSIZE newButtonsMapSaveGrp_size
    .define newButtonsMapSaveGrp_numTiles newButtonsMapSaveGrp_size/bytesPerTile
    
    newButtonsMagicItemGrp: .incbin "out/grp/buttons_magic_item.bin" FSIZE newButtonsMagicItemGrp_size
    .define newButtonsMagicItemGrp_numTiles newButtonsMagicItemGrp_size/bytesPerTile
    
    newButtonsFleeLipemcoGrp: .incbin "out/grp/buttons_flee_lipemco.bin" FSIZE newButtonsFleeLipemcoGrp_size
    .define newButtonsFleeLipemcoGrp_numTiles newButtonsFleeLipemcoGrp_size/bytesPerTile
    
    newButtonsFileGrp: .incbin "out/grp/buttons_file.bin" FSIZE newButtonsFileGrp_size
    .define newButtonsFileGrp_numTiles newButtonsFileGrp_size/bytesPerTile
    
    newButtonsYesNoGrp: .incbin "out/grp/buttons_yes_no.bin" FSIZE newButtonsYesNoGrp_size
    .define newButtonsYesNoGrp_numTiles newButtonsYesNoGrp_size/bytesPerTile
    
    newButtonsSellBuyLeaveGrp: .incbin "out/grp/buttons_sell_buy_leave.bin" FSIZE newButtonsSellBuyLeaveGrp_size
    .define newButtonsSellBuyLeaveGrp_numTiles newButtonsSellBuyLeaveGrp_size/bytesPerTile
    
    newButtonsTitleGrp: .incbin "out/grp/buttons_title.bin" FSIZE newButtonsTitleGrp_size
    .define newButtonsTitleGrp_numTiles newButtonsTitleGrp_size/bytesPerTile
  .ends

  ;========================================
  ; compass
  ;========================================
  
  .bank 1 slot 1
  .org $1714
  .section "new compass 2" overwrite
    ld a,newCompassGrp_numTiles
    ld b,:newCompassGrp
    ld hl,newCompassGrp
    ld de,$6C00
    call sendRawGrpDataToVdp
  .ends

  ;========================================
  ; main menu
  ;========================================
  
  .bank 0 slot 0
  .org $321E
  .section "new main menu 2" overwrite
    doBankedCallSlot2 loadNewMainMenuButtons
    jp $3231
  .ends
  
  .slot 2
  .section "new main menu 3" superfree
    loadNewMainMenuButtons:
      ld a,newButtonsMapSaveGrp_numTiles
      ld b,:newButtonsMapSaveGrp
      ld de,$6D40
      ld hl,newButtonsMapSaveGrp
      call sendRawGrpDataToVdp
      
      ld a,newButtonsMagicItemGrp_numTiles
      ld b,:newButtonsMagicItemGrp
      ld de,$6EC0
      ld hl,newButtonsMagicItemGrp
      jp sendRawGrpDataToVdp
  .ends

  ;========================================
  ; battle menu
  ;========================================
  
  .bank 0 slot 0
  .org $35AC
  .section "new battle menu 1" overwrite
    doBankedCallSlot2 loadNewBattleButtons
    jp $35BF
  .ends
  
  .slot 2
  .section "new battle menu 2" superfree
    loadNewBattleButtons:
      ld a,newButtonsMagicItemGrp_numTiles
      ld b,:newButtonsMagicItemGrp
      ld de,$6D40
      ld hl,newButtonsMagicItemGrp
      call sendRawGrpDataToVdp
      
      ld a,newButtonsFleeLipemcoGrp_numTiles
      ld b,:newButtonsFleeLipemcoGrp
      ld de,$6EC0
      ld hl,newButtonsFleeLipemcoGrp
      jp sendRawGrpDataToVdp
  .ends

  ;========================================
  ; save menu
  ;========================================
  
  .bank 0 slot 0
  .org $32B9
  .section "new save menu 1" overwrite
    doBankedCallSlot2 loadNewSaveButtons
  .ends
  
  .slot 2
  .section "new save menu 2" superfree
    loadNewSaveButtons:
      ld a,newButtonsFileGrp_numTiles
      ld b,:newButtonsFileGrp
      ld de,$6D40
      ld hl,newButtonsFileGrp
      call sendRawGrpDataToVdp
      
      ; make up work
      ld de,$A8F9
      jp $3B7B
  .ends

  ;========================================
  ; load menu
  ;========================================
  
  .bank 0 slot 0
  .org $3426
  .section "new load menu 1" overwrite
    doBankedCallSlot2 loadNewLoadButtons
    jp $3439
  .ends
  
  .slot 2
  .section "new load menu 2" superfree
    loadNewLoadButtons:
      ld a,newButtonsFileGrp_numTiles
      ld b,:newButtonsFileGrp
      ld de,$6D40
      ld hl,newButtonsFileGrp
      call sendRawGrpDataToVdp
      
      ; make up work
      ld b,$04
      ld de,$A935
      ld hl,$D40A
      jp $346A
  .ends

  ;========================================
  ; yes/no prompt
  ;========================================
  
  .bank 0 slot 0
  .org $33A8
  .section "new yes/no prompt 1" overwrite
    doBankedCallSlot2 loadNewYesNoPromptButtons
  .ends
  
  .slot 2
  .section "new yes/no prompt 2" superfree
    loadNewYesNoPromptButtons:
      ld a,newButtonsYesNoGrp_numTiles
      ld b,:newButtonsYesNoGrp
      ld de,$6D40
      ld hl,newButtonsYesNoGrp
      call sendRawGrpDataToVdp
      
      ; make up work
      ld de,$A912
      jp $33C3
  .ends

  ;========================================
  ; shop
  ;========================================
  
  .bank 0 slot 0
  .org $372C
  .section "new shop buttons 1" overwrite
    doBankedCallSlot2 loadNewShopButtons
  .ends
  
  .slot 2
  .section "new shop buttons 2" superfree
    loadNewShopButtons:
      ld a,newButtonsSellBuyLeaveGrp_numTiles
      ld b,:newButtonsSellBuyLeaveGrp
      ld de,$6D40
      ld hl,newButtonsSellBuyLeaveGrp
      call sendRawGrpDataToVdp
      
      ; make up work
      ld de,$A8DE
      jp $3B7B
  .ends
  
  ; de-optimize tilemap for "leave" button
  ; (original recycles the right part of the button, which is
  ; "ru" in both of the old graphics)
  
  .bank 3 slot 2
  .org $28EA
  .section "new shop buttons 3" overwrite
    .db $76,$77,$78     ; top row low nametable bytes
  .ends
  
  .bank 3 slot 2
  .org $28F1
  .section "new shop buttons 4" overwrite
    .db $79,$7A,$7B     ; bottom row low nametable bytes
  .ends

  ;========================================
  ; title
  ;========================================
  
  .bank 0 slot 0
  .org $33E5
  .section "new title buttons 1" overwrite
    doBankedCallSlot2 loadNewTitleButtons
    nop
  .ends
  
  .slot 2
  .section "new title buttons 2" superfree
    loadNewTitleButtons:
      ld a,newButtonsTitleGrp_numTiles
      ld b,:newButtonsTitleGrp
      ld de,$6D40
      ld hl,newButtonsTitleGrp
      call sendRawGrpDataToVdp
      
      ; make up work
      call $340F
      jr z,+
        ld e,$01
      +:
      ret
  .ends

;========================================
; use new kero^2 entrance
;========================================

  ;========================================
  ; graphics
  ;========================================
  
  .slot 1
  .section "new kero2 graphics 1" superfree
    newKero2EntranceGrp: .incbin "out/grp/kero2_entrance.bin" FSIZE newKero2EntranceGrp_size
    .define newKero2EntranceGrp_numTiles newKero2EntranceGrp_size/bytesPerTile
  .ends
  
  .bank 1 slot 1
  .org $38D0
  .section "new kero2 graphics 2" overwrite
;    doBankedCallSlot2 loadNewKero2Entrance
;    nop
    ld a,newKero2EntranceGrp_numTiles
    ld b,:newKero2EntranceGrp
    ld de,$6200
    ld hl,newKero2EntranceGrp
    call sendRawGrpDataToVdp
  .ends
  
/*  .slot 2
  .section "new kero2 graphics 3" superfree
    loadNewKero2Entrance:
      ld a,newKero2EntranceGrp_numTiles
      ld b,:newKero2EntranceGrp
      ld de,$6200
      ld hl,newKero2EntranceGrp
      call sendRawGrpDataToVdp
      
      ; make up work
      ld a,$0C
      ret
  .ends */

;========================================
; cutscene fixes
;========================================

  ;========================================
  ; new code
  ;========================================
  
  .macro retIfExtraTimerTween
    push hl
      ld hl,extraSceneTimer
      bit 0,(hl)
    pop hl
    ret z
  .endm
  
  .macro retIfExtraTimerNotTween
    push hl
      ld hl,extraSceneTimer
      bit 0,(hl)
    pop hl
    ret nz
  .endm
  
  .bank 2 slot 2
  .section "cutscene fixes 1" free
    newMainTimerInc:
      ld hl,extraSceneTimer
      inc (hl)
      bit 0,(hl)
      ld hl,cutsceneMainTimer
      jr z,+
        inc (hl)
      +:
      ld a,(hl)
      ret
    
/*    newSubTimerInit:
      ; original game prints a char every 5 frames (B = 6).
      ; we want half that, so we alternate between
      ; 2 and 3 frame delays each time.
      ld b,$03
      ld hl,extraSceneTimer
      bit 0,(hl)
      jr z,+
        ld b,$04
      +:
      
      ; make up work
      xor a
      ret */
    
    newSubTimerInc:
      ; halve counter parameter
      srl b
      ; if target counter value is odd and interpolation counter is
      ; set, add 1 to get interpolated target value
      jr nc,+
        ld hl,subsceneTimerInterpolationCounter
;        inc (hl)
        ld a,(hl)
        bit 0,a
        jr z,+
          inc b
      +:

      ; reduce counter parameter to 5/8ths
;      srl b
;      ld a,b
;      srl a
;      add a,b
;      srl b
;      srl b
;      sub b
;      ld b,a
      
      ; tick timer
      ld hl,cutsceneSubTimer
      inc (hl)
      ld a,(hl)
      cp b
      ret c
      ld (hl),$00
      
      ; increment interpolation counter after each full count
      push af
      push hl
        ld hl,subsceneTimerInterpolationCounter
        inc (hl)
      pop hl
      pop af
      
      ret
    
    clearCbcFlag:
      push af
        xor a
        ld (vwfCbcActiveFlag),a
      pop af
      ret
  .ends

  ;========================================
  ; double subtimer tick rate
  ;========================================
  
/*  .bank 2 slot 2
  .org $1E7F
  .section "cutscene fixes 2" overwrite
    ; standard subtimer tick rate for text
;    ld b,$06-3
    call newSubTimerInit
  .ends */
  
  .bank 2 slot 2
  .org $1E85
  .section "cutscene fixes 2" overwrite
    ; standard subtimer tick rate for text
    jp newSubTimerInc
  .ends

  ;========================================
  ; halve maintimer tick rate
  ;========================================
  
  .bank 2 slot 2
  .org $1E8F
  .section "cutscene fixes 3" overwrite
    ; increment only every other call
    jp newMainTimerInc
  .ends

  ;=============================================================================
  ; intro
  ;=============================================================================

  ;========================================
  ; move initial print pos up a line
  ;========================================
  
  .bank 2 slot 2
  .org $18FE
  .section "intro initial text pos 1" overwrite
    ; move up a line (wide mode)
    ld hl,$D60E-(32*2)-(2*0)
  .ends

  ;========================================
  ; clear cbc flag after scene finished
  ;========================================
  
  .bank 2 slot 2
  .org $16FA
  .section "intro cbc flag clear 1" overwrite
    call introCbcFlagClear
  .ends
  
  .bank 2 slot 2
  .section "intro cbc flag clear 2" free
    introCbcFlagClear:
      call clearCbcFlag
      ; make up work
      jp $2C92
  .ends

  ;========================================
  ; intro: halve scrolling speed
  ;========================================
  
  .bank 2 slot 2
  .org $1E63
  .section "intro fix 1" overwrite
    .db $D0/2
  .ends

  ;========================================
  ; intro: prevent spurious object
  ; "double-spawns" caused by halving
  ; advance rate of maintimer in cases
  ; where e.g. an object was programmed to
  ; start whenever the timer ticks to a
  ; certain value (which now occurs twice)
  ;
  ; rulue sweatdrop
  ;========================================
  
  .bank 2 slot 2
  .org $19B5
  .section "intro sweatdrop fix 1" overwrite
    jp introSweatdropFix
  .ends
  
  .bank 2 slot 2
  .section "intro sweatdrop fix 2" free
    introSweatdropFix:
/*      push hl
        ld hl,cutsceneMainTimer
        inc (hl)
      pop hl
      jp $1910 */
      
      ; do nothing if this was the second tick
      ld hl,extraSceneTimer
      bit 0,(hl)
      ret z
      
      ; make up work
      ld hl,$C036
      jp $99B8
  .ends

  ;========================================
  ; carbuncle zoomout
  ;========================================
  
  .bank 2 slot 2
  .org $1B21
  .section "intro carbuncle zoomout fix 1" overwrite
    jp z,introCarbuncleZoomoutFix
  .ends
  
  .bank 2 slot 2
  .section "intro carbuncle zoomout fix 2" free
    introCarbuncleZoomoutFix:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      jp $9BBC
  .ends

  ;========================================
  ; minotauros popup
  ;========================================
  
  .bank 2 slot 2
  .org $1C9B
  .section "intro minotauros popup fix 1" overwrite
    jp introMinotaurosPopupFix
  .ends
  
  .bank 2 slot 2
  .section "intro minotauros popup fix 2" free
    introMinotaurosPopupFix:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      ld b,$18
      ld hl,$C007
      jp $9CA0
  .ends

  ;========================================
  ; minotauros blink transition
  ;========================================
  
  .bank 2 slot 2
  .org $1CC9
  .section "intro minotauros blink transition fix 1" overwrite
    jp introMinotaurosBlinkTransitionFix
  .ends
  
  .bank 2 slot 2
  .section "intro minotauros blink transition 2" free
    introMinotaurosBlinkTransitionFix:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      call $2C92
      jp $9CCC
  .ends

  ;========================================
  ; minotauros blink transition 2
  ;========================================
  
  .bank 2 slot 2
  .org $1DA9
  .section "intro minotauros blink transition2 fix 1" overwrite
    jp introMinotaurosBlinkTransition2Fix
  .ends
  
  .bank 2 slot 2
  .section "intro minotauros blink transition2 fix 2" free
    introMinotaurosBlinkTransition2Fix:
      ; make up work
;      cp $28
;      jp nz,$9D9F
      
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      push hl
      call $2C92
      jp $9DAD
  .ends

  ;========================================
  ; fadeout
  ;========================================
  
  .bank 2 slot 2
  .org $1DE1
  .section "intro fadeout fix 1" overwrite
    jp introFadeoutFix
  .ends
  
  .bank 2 slot 2
  .section "intro fadeout fix 2" free
    introFadeoutFix:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      call $0B51
      jp $9DE4
  .ends

  ;========================================
  ; lip sync
  ;========================================
  
  ; they really contrived to make these as hard to edit as possible, huh?
  ; mouth movements are based on a bunch of precomposed sequences of
  ; "scripting" calls.
  ; the game passes HL as a pointer to the start of this deta to
  ; routine $973F, which looks up the appropriate call from the data
  ; for the current maintimer tick.
  ; the sequences are only as long as necessary for the original
  ; data, so if you try to extend them, you'll go past the end of the
  ; array and spill into unrelelated data (usually for some other
  ; object in the scene).
  ; to change the running time of an action, we have to:
  ; * define a new, extended sequence somewhere
  ; * extend the timer (param B)
  ; * make sure our extended action won't get interrupted
  ;   by the next action in the sequence.
  ; got all that?
 
  ; please reference these against the region 4 scripts, because
  ; i absolutely am not going to try to name and comment them
  ; individually
  
  .bank 2 slot 2
  .section "intro lip sync data 1" free
    
    lipSync9:
;      .db $01,$00,$00,$00,$01,$01,$00,$01,$00,$01,$00,$01,$00,$01,$01,$01,$00,$00
      .db $01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$00,$01,$00,$01,$01,$01,$00,$00
    
    lipSyncA:
      .rept 8+1
        .db $0A,$0B
      .endr
      .db $0A,$0A
    
    lipSyncC:
;      .db $02,$03,$03,$04,$04,$03,$02,$03,$04,$03,$03,$04,$02,$02
      .db $02,$03,$03,$04,$04,$03,$02,$03,$04,$03,$03,$04,$03,$04,$03,$04,$02,$02
    
    lipSyncD:
;      .db $05,$06,$07,$07,$05,$06,$07,$06,$06,$05,$05
      .db $05,$06,$07,$07,$05,$06,$07,$06,$05,$07,$06,$05,$06,$07,$06,$05,$05
    
    lipSyncE:
;      .db $02,$03,$04,$03,$02,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$02,$02
      .db $02,$03,$04,$03,$02,$03,$04,$03,$04,$03,$04,$03,$04,$02,$02,$03,$04,$03,$04,$02,$02
      
    lipSyncF:
;      .db $05,$06,$07,$06,$07,$06,$07,$05,$06,$07,$06,$07,$06,$07,$05,$05
      .db $05,$06,$07,$06,$07,$06,$07,$05,$06,$07,$06,$07,$05,$05
    
    lipSync10:
;      .db $02,$03,$04,$03,$04,$02,$03,$03,$04,$04,$02,$03,$04,$03,$04,$03,$04,$03,$02
      ; accurate for length of line, but it's not a whole sentence,
      ; so let's keep going until the next one starts
;      .db $02,$03,$04,$03,$04,$02,$03,$03
;      .db $04,$04,$02,$03,$04,$03,$04,$03
;      .db $04,$03,$02
;      .db $04,$03,$04,$03,$04,$03,$02,$02
      .db $02,$03,$04,$03,$04,$02,$03,$03
      .db $04,$04,$02,$03,$04,$03,$04,$03
      .db $04,$03,$02,$04,$03,$04,$03,$04
      .db $03,$02,$04,$03,$02,$02,$02
    
    lipSync11:
;      .db $02,$03,$04,$03,$04,$02,$03,$04,$03,$04,$03,$04,$03,$04,$04,$04,$02,$02,$02
      .db $02,$04,$03,$04,$03,$04,$03,$04,$04,$04,$02,$02,$02
    
    lipSync13:
;      .db $02,$03,$04,$04,$02,$03,$04,$03,$04,$03,$04,$03,$04,$02,$02
      .db $02,$03,$04,$02,$03,$04,$03,$04,$03,$04,$02,$02
    
    lipSync17:
;      .db $05,$06,$07,$06,$07,$06,$07,$06,$07,$06,$05
      .db $05,$06,$07,$06,$07,$06,$07,$06,$07,$06,$06,$05,$05
    
    lipSync18:
;      .db $05,$06,$07,$06,$07,$05,$06,$07,$06,$07,$06,$07,$06,$07,$06,$07,$05,$05
      .db $05,$06,$07,$06,$07,$05,$06,$07,$06,$07,$06,$07,$06,$07,$05,$05
    
    lipSync1A:
;      .db $05,$06,$07,$06,$07,$06,$07,$07,$05,$05
      .db $05,$06,$07,$06,$07,$06,$07,$05,$06,$07,$06,$07,$05,$05
    
    lipSync21:
;      .db $08,$09,$08,$09,$08,$09,$08,$09,$08,$09,$08,$09,$09,$09
      .db $08,$09,$08,$09,$08,$09,$08,$09,$08,$09,$08,$08,$08,$08,
    
    lipSync22:
;      .db $09,$08,$09,$08,$09,$09,$08,$09,$08,$09,$08,$09,$08,$08
      .db $09,$08,$09,$08,$09,$09,$08,$09,$08,$09,$08,$09,$08,$09,$08,$08,$09,$08,$09,$08,$08
  .ends
 
  .bank 2 slot 2
  .org $1980
  .section "intro lip sync 8" overwrite
    ld b,$14-4
  .ends
 
  .bank 2 slot 2
  .org $196F
  .section "intro lip sync 9" overwrite
    ld hl,lipSync9
    ld d,$4A
    ld b,$11
  .ends
 
  .bank 2 slot 2
  .org $1963
  .section "intro lip sync a" overwrite
    ld hl,lipSyncA
    ld d,$7E
    ld b,$12+2
  .ends
 
  .bank 2 slot 2
  .org $1A97
  .section "intro lip sync c" overwrite
    ld hl,lipSyncC
    ld d,$01
    ld b,$0D+4
  .ends
 
  .bank 2 slot 2
  .org $1A81
  .section "intro lip sync d" overwrite
    ld hl,lipSyncD
    ld d,$22
    ld b,$0A+6
  .ends
 
  .bank 2 slot 2
  .org $1A61
  .section "intro lip sync e" overwrite
    ld hl,lipSyncE
    ld d,$40
    ld b,$10+4
  .ends
 
  .bank 2 slot 2
  .org $1A54
  .section "intro lip sync f" overwrite
    ld hl,lipSyncF
    ld d,$64
    ld b,$0F-2
  .ends
 
  .bank 2 slot 2
  .org $1A48
  .section "intro lip sync 10" overwrite
    ld hl,lipSync10
    ld d,$87
;    ld b,$12+8
    ld b,$1E
  .ends
 
  .bank 2 slot 2
  .org $1A3C
  .section "intro lip sync 11" overwrite
    ld hl,lipSync11
    ld d,$AD
    ld b,$12-6
  .ends
 
  .bank 2 slot 2
  .org $1B3E
  .section "intro lip sync 13" overwrite
    ld hl,lipSync13
    ld d,$01
    ld b,$0E-3
  .ends
 
  .bank 2 slot 2
  .org $1B13
  .section "intro lip sync 17" overwrite
    ld hl,lipSync17
    ld d,$72
;    ld b,$0A
    ld b,$0A+2
  .ends
 
  .bank 2 slot 2
  .org $1B07
  .section "intro lip sync 18" overwrite
    ld hl,lipSync18
    ld d,$86
;    ld b,$11-6
    ld b,$11-0
  .ends
 
  .bank 2 slot 2
  .org $1AEE
  .section "intro lip sync 1A" overwrite
    ld hl,lipSync1A
    ld d,$B6
    ld b,$09+4
  .ends
 
  .bank 2 slot 2
  .org $1C4A
  .section "intro lip sync 21" overwrite
    ld hl,lipSync21
    ld d,$BC
    ld b,$0D
  .ends
 
  .bank 2 slot 2
  .org $1C3D
  .section "intro lip sync 22" overwrite
    ld hl,lipSync22
    ld d,$DD
    ld b,$0D+9
  .ends

  ;========================================
  ; increase time for final line so
  ; zenki can make a joke
  ;========================================
 
  .bank 2 slot 2
  .org $1D58
  .section "intro extend final line 1" overwrite
    ; maintimer tick at which to transition to next scene
    ; (note: don't set this too early or it will interrupt the fade
    ; effect, resulting in the text being too dark)
;    cp $AA-2
    cp $AA+10
  .ends
 
  .bank 2 slot 2
  .org $1D5D
  .section "intro extend final line 2" overwrite
    ; maintimer tick at which to fade out
    cp $91+10
  .ends

  ;========================================
  ; increase time for final line of
  ; blackout screen
  ;========================================
 
  .bank 2 slot 2
  .org $1E42
  .section "intro extend final final line 1" overwrite
    ; maintimer tick at which to fade out
    cp $45+10
  .ends
 
  .bank 2 slot 2
  .org $1E4B
  .section "intro extend final final line 2" overwrite
    ; maintimer tick at which to start second line
    cp $25-5
  .ends
  

  ;=============================================================================
  ; game start cutscene
  ;=============================================================================

  ;========================================
  ; move initial print pos up a line
  ;========================================
  
  .bank 2 slot 2
  .org $22DB
  .section "game start initial text pos 1" overwrite
    ld hl,$D482-(20*2)
  .ends
  
  .bank 2 slot 2
  .org $22E8
  .section "game start initial text pos 2" overwrite
    ld hl,$D60E-(32*2)
  .ends

  ;========================================
  ; clear cbc flag after scene finished
  ;========================================
  
  .bank 2 slot 2
  .org $2096
  .section "game start cbc flag clear 1" overwrite
    call gameStartCbcFlagClear
  .ends
  
  .bank 2 slot 2
  .section "game start cbc flag clear 2" free
    gameStartCbcFlagClear:
      call clearCbcFlag
      ; make up work
      jp $0B51
  .ends

  ;========================================
  ; scrolling speed
  ;========================================
  
  .bank 2 slot 2
  .org $2219
  .section "game start scroll speed 1" overwrite
    .dw $100/2
  .ends
  
  .bank 2 slot 2
  .org $2227
  .section "game start scroll speed 2" overwrite
    .dw -$100/2
  .ends

  ;========================================
  ; sprite scrolling speed
  ;========================================
  
  .bank 2 slot 2
  .org $21F6
  .section "game start sprite scroll speed 1" overwrite
    call gameStartSpriteScrollLeftFix
    nop
    nop
  .ends
  
  .bank 2 slot 2
  .org $220D
  .section "game start sprite scroll speed 2" overwrite
    call gameStartSpriteScrollRightFix
    nop
    nop
  .ends
  
  .bank 2 slot 2
  .section "game start sprite scroll speed 3" free
    gameStartSpriteScrollLeftFix:
      retIfExtraTimerNotTween
      
      -:
        inc (hl)
        inc hl
        inc hl
        djnz -
      
      ret
    
    gameStartSpriteScrollRightFix:
      retIfExtraTimerTween
      
      -:
        dec (hl)
        inc hl
        inc hl
        djnz -
      
      ret
  .ends

  ;=============================================================================
  ; treasure house/dark zone/rulue's mansion intros
  ;=============================================================================

  ;========================================
  ; move initial print pos up a line
  ;========================================
  
  .bank 2 slot 2
  .org $27D9
  .section "area intro initial text pos 1" overwrite
    ld hl,$D482-(20*2)
  .ends

  ;========================================
  ; clear cbc flag after scene finished
  ;========================================
  
  ; NOTE: no modifications necessary. these scenes are not skippable,
  ; so the scripts themselves will shut the cbc flag off automatically
  ; when they reach their terminators.

  ;=============================================================================
  ; ending
  ;=============================================================================

  ;========================================
  ; move initial print pos up a line
  ;========================================
  
  .bank 2 slot 2
  .org $2EC6
  .section "ending initial text pos 1" overwrite
    ld hl,$D482-(20*2)
  .ends

  ;========================================
  ; no double-transitions
  ;========================================
  
  .bank 2 slot 2
  .org $2D53
  .section "ending fix double transitions 1" overwrite
    jp endingFixDoubleTransition1
  .ends
  
  .bank 2 slot 2
  .section "ending fix double transitions 2" free
    endingFixDoubleTransition1:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      call $AD58
      jp $AD56
  .ends
  
  .bank 2 slot 2
  .org $2D8F
  .section "ending fix double transitions 3" overwrite
    jp endingFixDoubleTransition2
  .ends
  
  .bank 2 slot 2
  .section "ending fix double transitions 4" free
    endingFixDoubleTransition2:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      call $AD94
      jp $AD92
  .ends
  
  .bank 2 slot 2
  .org $2D87
  .section "ending fix double transitions 5" overwrite
    jp endingFixDoubleTransition3
  .ends
  
  .bank 2 slot 2
  .section "ending fix double transitions 6" free
    endingFixDoubleTransition3:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      call $AD58
      jp $AD8A
  .ends
  
  .bank 2 slot 2
  .org $2DCB
  .section "ending fix double transitions 7" overwrite
    jp endingFixDoubleTransition4
  .ends
  
  .bank 2 slot 2
  .section "ending fix double transitions 8" free
    endingFixDoubleTransition4:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      call $AD94
      jp $ADCE
  .ends
  
  .bank 2 slot 2
  .org $2DD6
  .section "ending fix double transitions 9" overwrite
    jp endingFixDoubleTransition5
  .ends
  
  .bank 2 slot 2
  .section "ending fix double transitions 10" free
    endingFixDoubleTransition5:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      call $2C92
      jp $ADD9
  .ends
  
  .bank 2 slot 2
  .org $2DC3
  .section "ending fix double transitions 11" overwrite
    jp endingFixDoubleTransition6
  .ends
  
  .bank 2 slot 2
  .section "ending fix double transitions 12" free
    endingFixDoubleTransition6:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      call $AD94
      jp $ADC6
  .ends
  
  .bank 2 slot 2
  .org $2E97
  .section "ending fix double transitions 13" overwrite
    jp endingFixDoubleTransition7
  .ends
  
  .bank 2 slot 2
  .section "ending fix double transitions 14" free
    endingFixDoubleTransition7:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      call $2C92
      jp $AE9A
  .ends

  ;========================================
  ; no double-satan destruction
  ;========================================
  
  .bank 2 slot 2
  .org $2E07
  .section "ending fix double satan destruction 1" overwrite
    jp endingFixDoubleSatanDestruction
  .ends
  
  .bank 2 slot 2
  .section "ending fix double satan destruction 2" free
    endingFixDoubleSatanDestruction:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      call $0B24
      jp $AE0A
  .ends

  ;========================================
  ; no double-satan popup
  ;========================================
  
  .bank 2 slot 2
  .org $2C1D
  .section "ending fix double satan popup 1" overwrite
    jp endingFixDoubleSatanPopup
  .ends
  
  .bank 2 slot 2
  .section "ending fix double satan popup 2" free
    endingFixDoubleSatanPopup:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      call $2CE2
      jp $AC20
  .ends

  ;========================================
  ; no double-satan flight
  ;========================================
  
  .bank 2 slot 2
  .org $2C7D
  .section "ending fix double satan flight 1" overwrite
    jp endingFixDoubleSatanFlight
  .ends
  
  .bank 2 slot 2
  .section "ending fix double satan flight 2" free
    endingFixDoubleSatanFlight:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      ld hl,$95DC
      jp $AC80
  .ends

  ;========================================
  ; no double-satan something
  ;========================================
  
  .bank 2 slot 2
  .org $2C86
  .section "ending fix double satan something 1" overwrite
    jp endingFixDoubleSatanSomething
  .ends
  
  .bank 2 slot 2
  .section "ending fix double satan something 2" free
    endingFixDoubleSatanSomething:
      ; do nothing if this was the second tick
      retIfExtraTimerTween
      
      ; make up work
      call $1961
      jp $AC89
  .ends

  ;========================================
  ; lip sync
  ;========================================
  
  .bank 2 slot 2
  .section "ending lip sync data 5D" free
    lipSync5D:
;      .db $00,$00,$00,$00,$02,$01,$02,$01,$00,$00
      .db $00,$00,$00,$00,$02,$01,$00,$00,$00,$00
  .ends
 
  .bank 2 slot 2
  .org $291F
  .section "ending lip sync 5D" overwrite
    ld hl,lipSync5D
    ld d,$24
    ld b,$09
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 60" free
    lipSync60:
;      .db $09,$07,$07,$0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0A
      .db $09,$07,$07,$0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0A
  .ends
 
  .bank 2 slot 2
  .org $28FA
  .section "ending lip sync 60" overwrite
    ld hl,lipSync60
    ld d,$87
    ld b,$0F+8
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 61" free
    lipSync61:
;      .db $0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0A
      .db $0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0A
  .ends
 
  .bank 2 slot 2
  .org $28EE
  .section "ending lip sync 61" overwrite
    ld hl,lipSync61
    ld d,$A9
    ld b,$12+2
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 62" free
    lipSync62:
;      .db $0A,$0C,$0B,$0C,$0B,$0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0A,$0A,$0A,$0A
      .db $0A,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0C,$0B,$0C,$0B,$0A,$0A,$0A,$0A,$0A
  .ends
 
  .bank 2 slot 2
  .org $28E1
  .section "ending lip sync 62" overwrite
    ld hl,lipSync62
    ld d,$CF
    ld b,$12-2
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 63" free
    lipSync63:
;      .db $00,$00,$00,$00,$00,$00,$00,$02,$01,$02,$01,$02,$01,$02,$01,$02,$01,$00,$00
      .db $00,$00,$00,$00,$00,$00,$01,$02,$01,$02,$01,$02,$01,$02,$01,$02,$01,$02,$01,$02,$01,$00,$00
  .ends
 
  .bank 2 slot 2
  .org $2991
  .section "ending lip sync 63" overwrite
    ld hl,lipSync63
    ld d,$02
    ld b,$12+4
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 65" free
    lipSync65:
;      .db $00,$00,$00,$00,$02,$01,$02,$01,$01,$00,$00,$00,$00,$00
      .db $00,$00,$00,$00,$02,$01,$02,$01,$02,$01,$02,$01,$02,$01,$02,$01,$02,$01,$00,$00,$00,$00,$00
  .ends
 
  .bank 2 slot 2
  .org $296E
  .section "ending lip sync 65" overwrite
    ld hl,lipSync65
    ld d,$4D
    ld b,$0D+9
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 67" free
    lipSync67:
;      .db $06,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$03,$03,$03
      .db $06,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$03,$03,$03
  .ends
 
  .bank 2 slot 2
  .org $2959
  .section "ending lip sync 67" overwrite
    ld hl,lipSync67
    ld d,$92
    ld b,$13+4
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 68" free
    lipSync68:
;      .db $08,$08,$08,$0D,$0F,$0E,$0D,$0F,$0E,$0F,$0E,$0D,$0D
      .db $08,$08,$08,$0D,$0F,$0E,$0D,$0F,$0E,$0F,$0E,$0F,$0E,$0F,$0E,$0D,$0D
  .ends
 
  .bank 2 slot 2
  .org $29FF
  .section "ending lip sync 68" overwrite
    ld hl,lipSync68
    ld d,$02
    ld b,$0C+4
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 69" free
    lipSync69:
;      .db $0D,$0F,$0E,$0F,$0E,$0F,$0E,$0F,$0E,$0F,$0E,$0D,$0F,$0E,$0F,$0E,$0F,$0E,$0F,$0D,$0D
      .db $0D,$0F,$0E,$0F,$0E,$0F,$0E,$0F,$0E,$0F,$0E,$0D,$0F,$0E,$0F,$0E,$0F,$0E,$0F,$0E,$0F,$0D,$0D
  .ends
 
  .bank 2 slot 2
  .org $29F2
  .section "ending lip sync 69" overwrite
    ld hl,lipSync69
    ld d,$21
    ld b,$14+2
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 6A" free
    lipSync6A:
;      .db $0D,$0F,$0E,$0F,$0E,$0F,$0E,$0F,$0E,$0F,$0E,$0D,$0D
      .db $0D,$0F,$0E,$0F,$0E,$0F,$0E,$0D,$0D
  .ends
 
  .bank 2 slot 2
  .org $29E6
  .section "ending lip sync 6A" overwrite
    ld hl,lipSync6A
    ld d,$48
    ld b,$0C-4
  .ends
  
  ; increase length of line 6D to accommodate longer text
 
  .bank 2 slot 2
  .org $29BF
  .section "ending 6D length" overwrite
    ; target maintimer tick value
    cp $D3+14
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 6F" free
    lipSync6F:
;      .db $0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0A
      .db $0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0A
  .ends
 
  .bank 2 slot 2
  .org $2A6F
  .section "ending lip sync 6F" overwrite
    ld hl,lipSync6F
    ld d,$1E
    ld b,$0E+2
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 70" free
    lipSync70:
;      .db $0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0B,$0A,$0A
      .db $0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0B,$0A,$0A
  .ends
 
  .bank 2 slot 2
  .org $2A63
  .section "ending lip sync 70" overwrite
    ld hl,lipSync70
    ld d,$3F
    ld b,$0D-2
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 71" free
    lipSync71:
;      .db $0A,$0C,$0B,$0C,$0B,$0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0A
      .db $0A,$0B,$0C,$0B,$0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0A,$0A
  .ends
 
  .bank 2 slot 2
  .org $2A57
  .section "ending lip sync 71" overwrite
    ld hl,lipSync71
    ld d,$5F
    ld b,$11-2
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 72" free
    lipSync72:
;      .db $0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0A,$0C,$0B,$0C,$0B,$0A,$0A,$0A
      .db $0A,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0A,$0A,$0A,$0A
  .ends
 
  .bank 2 slot 2
  .org $2A4B
  .section "ending lip sync 72" overwrite
    ld hl,lipSync72
    ld d,$83
    ld b,$11-5
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 73" free
    lipSync73:
;      .db $0A,$0C,$0B,$0C,$0B,$0C,$0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0A
      .db $0A,$0C,$0B,$0C,$0B,$0C,$0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0A,$0A,$0A
  .ends
 
  .bank 2 slot 2
  .org $2A3F
  .section "ending lip sync 73" overwrite
    ld hl,lipSync73
    ld d,$A7
    ld b,$12-2
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 74" free
    lipSync74:
;      .db $0A,$0C,$0B,$0C,$0A,$0C,$0B,$0C,$0B,$0A,$0A
      .db $0A,$0C,$0B,$0C,$0A,$0B,$0C,$0B,$0C,$0B,$0A,$0A
  .ends
 
  .bank 2 slot 2
  .org $2A32
  .section "ending lip sync 74" overwrite
    ld hl,lipSync74
    ld d,$CD
    ld b,$0A+1
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 75" free
    lipSync75:
;      .db $0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0A,$0A
      .db $0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0A
  .ends
 
  .bank 2 slot 2
  .org $2AEC
  .section "ending lip sync 75" overwrite
    ld hl,lipSync75
    ld d,$01
    ld b,$0D+3
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 76" free
    lipSync76:
;      .db $0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0A,$0A,$0A,$07
      .db $0A,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0A,$0A,$0A,$07
  .ends
 
  .bank 2 slot 2
  .org $2AE0
  .section "ending lip sync 76" overwrite
    ld hl,lipSync76
    ld d,$21
    ld b,$0F+5
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 79" free
    lipSync79:
;      .db $07,$07,$07,$0A,$0C,$0B,$0C,$0B,$0A,$0C,$0B,$0C,$0B,$0C,$0A,$0C,$0B,$0C,$0A,$0A
      .db $07,$07,$07,$0A,$0C,$0B,$0C,$0B,$0A,$0C,$0B,$0C,$0B,$0C,$0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0A,$0A
  .ends
 
  .bank 2 slot 2
  .org $2ABA
  .section "ending lip sync 79" overwrite
    ld hl,lipSync79
    ld d,$93
    ld b,$13+4
  .ends
  
  .bank 2 slot 2
  .section "ending lip sync data 7A" free
    lipSync7A:
;      .db $0A,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0B,$0A,$0A,$0A
      .db $0A,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0C,$0B,$0A,$0A,$0A
  .ends
 
  .bank 2 slot 2
  .org $2AAD
  .section "ending lip sync 7A" overwrite
    ld hl,lipSync7A
    ld d,$B9
    ld b,$12+4
  .ends

;========================================
; credits
;========================================
  
  ;=====
  ; use standard char-by-char printing
  ;=====

  .bank 0 slot 0
  .org $2A58
  .section "credits cbc 1" overwrite
    @loop:
      ; print until CBC mode shut off
      ld a,(vwfCbcActiveFlag)
      or a
      jr z,@done
      
      ; print character
      call copyCharToFrontBuffer
      
      ; wait 5 frames
      ld a,$0A/2
      call waitVblank
      jr @loop
      
    @done:
    jp $2AA9
  .ends

  ;=====
  ; extend credits
  ;=====

  .bank 2 slot 2
  .org $2C9E
  .section "credits 1" overwrite
    jp newCredits
  .ends

  .bank 2 slot 2
  .section "credits 2" free
    newCredits:
      ; the original game doesn't reprint repeated job titles,
      ; which is why it has calls to various routines other than
      ; the one used here.
      ; not worth the effort to reproduce with the new system.
      .rept 41
        call $2A1C
      .endr

      ; new translation credits, starting from script 4-C0
      ld c,$C0
      .rept 7
        call $2A1C
      .endr
/*      call $2A1C
      call $2A1C
      call $2A1C
      call $2A1C
      call $2A1C
      call $2A1C
      call $2A1C */
      
      
      
      call $2CBD
      
      
      
      jp $AD1C
  .ends
  
  ;=====
  ; fix final copyright notice
  ;=====

  .bank 2 slot 2
  .org $2D24
  .section "credits final 1" overwrite
    ; number of characters in final copyright string
    ld b,20
  .ends

/*  .bank 2 slot 2
  .org $3989
  .section "credits end 1" overwrite
    ; script ID
    ld c,$7E
    ; target address within buffer
    ld hl,$D480+(2*6)
    ; run script
    call $267D
    
    ; count of characters
    ld b,$0F*2
  .ends */

;========================================
; there are two revisions of this game.
; they are nearly identical and differ
; by exactly one byte:
; byte 0x547 is $1B in rev0 but $3B
; in rev1.
; this changes the instruction
; 000545  01 FF 1B    ld bc,$1BFF
; to
; 000545  01 FF 3B    ld bc,$3BFF
;
; this increases the amount of the
; expansion RAM that's cleared out
; during game startup. rev0 only
; clears 0400-1FFF, while rev1 clears
; 0400-3FFF.
;
; while i have no way of checking,
; this seems to imply that the later
; revision used a larger 16kb RAM chip,
; since according to SMS Power docs,
; an 8kb chip would mirror
; 0000-1FFF at 2000-3FFF and this clear
; operation would wipe the save data.
; 
; we use the larger value from rev1.
;========================================

.bank 0 slot 0
.org $0545
.section "rev1 expram clear alteration" overwrite
;  ld bc,$1BFF
  ld bc,$3BFF
.ends

/*;========================================
; don't print hardcoded dungeon floor
; numbers
;========================================

.bank 0 slot 0
.org $1EB7
.section "no dungeon floor numbers 1" overwrite
  ; overwrite original call to print routine
  
  ; save scriptnum
  ld a,c
  ld (paramWord1),a
  nop
.ends

.bank 0 slot 0
.org $1ED5
.section "no dungeon floor numbers 2" SIZE 22 overwrite
  ; C = tens digit of floor number
  ; B = ones digit
  
  ; update number buffer
  
  ld hl,numberBufferStart
  
  ; top 2 digits zero
  xor a
  ld (hl),a
  inc hl
  ld (hl),a
  inc hl
  
  ld a,c
  ld (hl),a
  inc hl
  
  ld a,b
  ld (hl),a
;  inc hl
  
  ; make up the print call we previously omitted
  ld a,(paramWord1)
  ld c,a
  call runRegion0Script
  nop
  nop
  
.ends

;========================================
; fix "leave" button in shops.
; original game optimizes うる and でる
; to recycle the right part of the る
; for both options, which obviously
; isn't going to fly in English.
;========================================

  .bank 0 slot 0
  .org $3AC9
  .section "fix shop buttons 1" overwrite
    doBankedCallSlot2NoParams loadNewShopButtons
  .ends
  
  .bank 2 slot 2
  .section "fix shop buttons 2" free
    loadNewShopButtons:
      doBankedCallSlot2NoParams loadNewShopButtons_content
    
      ; make up work
      ld de,$B5E5
      ; load tilemap
      call $3F1C
      ld c,$64
      ret
  .ends
  
  ; tile $176
  .define newLeaveButtonVramTile $176
  .define newLeaveButtonVramTarget (newLeaveButtonVramTile*bytesPerTile)|$4000
  
  .slot 2
  .section "fix shop buttons 3" superfree
    newLeaveButton:
      .incbin "out/button_leave_new.bin" FSIZE newLeaveButtonSize
    .define newLeaveButtonNumTiles newLeaveButtonSize/bytesPerTile
    
    loadNewShopButtons_content:
      ; copy in new tiles
      ld b,newLeaveButtonNumTiles
      ld de,newLeaveButton
      ld hl,newLeaveButtonVramTarget
      di
        rawTilesToVdp_macro_safe
      ei
      
      ret
  .ends

  ; update tilemap
  
  .bank 6 slot 2
  .org $35F1
  .section "fix shop buttons tilemap 1" overwrite
    .db <newLeaveButtonVramTile+0,<newLeaveButtonVramTile+1,<newLeaveButtonVramTile+2
  .ends
  
  .bank 6 slot 2
  .org $35F8
  .section "fix shop buttons tilemap 2" overwrite
    .db <newLeaveButtonVramTile+3,<newLeaveButtonVramTile+4,<newLeaveButtonVramTile+5
  .ends


;========================================
; bugfix: reinitialize reserve encounter
; table at CC00 during the lyra ruins
; cutscene.
; otherwise, garbage data will be left
; behind that will cause glitched
; encounters on dungeon floor 4 if the
; player manages to make it there
; without resetting.
;========================================

  ;=====
  ; during normal scene progression
  ;=====

;  .bank 2 slot 2
;  .org $2EB2
;  .section "reserve encounter table fix 1" overwrite
;    ; initialize the table instead of simply shutting off
;    ; the effect
;    call $0BCD
;  .ends

  ;=====
  ; if scene skipped with button press
  ; (actually, turns out this runs at the end of the scene anyway)
  ;=====

  .bank 2 slot 2
  .org $28D1
  .section "reserve encounter table fix 2a" overwrite
    doBankedCallSlot2NoParams reserveTableFix_sceneEnd
    nop
    nop
  .ends

  .slot 2
  .section "reserve encounter table fix 2b" superfree
    reserveTableFix_sceneEnd:
      ; make up work
;      call $0ED0
;      call $2DC9
      call $3001
      xor a
      ld ($C016),a
      ld ($C0F0),a
      
      ; shut off cbc mode
      xor a
      ld (vwfCbcActiveFlag),a
      
      ; initialize the table instead of simply shutting off
      ; the effect.
      
      ; THIS WILL FORCE SLOT 2 TO BANK 2, MAPPING THIS CODE OUT!
      ; IT MUST BE THE LAST THING THAT HAPPENS IN THIS ROUTINE!
      jp $0BCD
      
      ; make up work
;      jp $0B5F
  .ends

  ;=====
  ; for opening scene, make sure cbc mode gets shut off
  ;=====

  .bank 2 slot 2
  .org $1A95
  .section "reserve encounter table fix 3a" overwrite
    doBankedCallSlot2NoParams intro_sceneEnd_ext
    nop
    nop
  .ends

  .slot 2
  .section "reserve encounter table fix 3b" superfree
    intro_sceneEnd_ext:
      ; make up work
;      call $0ED0
;      call $2DC9
      call $3001
      xor a
      ld ($C016),a
      ld ($C0F0),a
      
      ; shut off cbc mode
      xor a
      ld (vwfCbcActiveFlag),a
      
      ret
  .ends

;========================================
; we're storing extra dialogue tiles in
; the cropped-out bottom of the tilemap.
; this is fine most of the time, but
; some screen-shake effects briefly show
; this area and now need to clean out
; any garbage beforehand.
;========================================

  ;=====
  ; free up space by moving frog hop animation out of bank
  ;=====
  
  .bank 1 slot 1
  .org $2318
  .section "clear tilemap garbage on screen shake 1" overwrite
    doBankedJumpSlot2 newFrogShopAnimation
  .ends
  
  .slot 2
  .section "clear tilemap garbage on screen shake 2" superfree
    frogAnimClearTiles:
      .rept 2
        .rept bytesPerTile
          .db $00
        .endr
      .endr
    
    newFrogShopAnimation:
      ; clear garbage
;      push hl
;      push de
;      push bc
;        ld b,2
;        ld de,frogAnimClearTiles
;        ld hl,$7D40
;        di
;          rawTilesToVdp_macro_safe
;        ei
;      pop bc
;      pop de
;      pop hl
      
      ; do normal work
      
      ld b,$04
      -:
        push bc
        ld hl,$9CF7
        ld ($C0A9),hl
        call $331F
        ; do a screen-shake
        call $5CE3
        call $347F
        ; hop animation
        call $331F
        ld a,$5B
        call $0EB4
        ; do a screen-shake
        call $5CE3
        call $347F
        pop bc
        djnz -
      ret 
  .ends
  
  .unbackground $6318+14 $633B

  ;=====
  ; clear screen when shake effect triggered
  ;=====
  
  .bank 1 slot 1
  .org $1CE3
  .section "clear tilemap garbage on screen shake 3" overwrite
    call clearTilemapStorageTopRow_hop
  .ends
  
  .bank 1 slot 1
  .section "clear tilemap garbage on screen shake 4" free
    clearTilemapStorageTopRow_hop:
      doBankedJumpSlot2NoParams clearTilemapStorageTopRow_hop_ext
  
  .ends
  
  .slot 2
  .section "clear tilemap garbage on screen shake 5" superfree
    newTilemapStorageRowClearTiles:
      .rept 2
        .rept bytesPerTile
          .db $00
        .endr
      .endr
    
    clearTilemapStorageTopRow_ext:
      ; clear garbage
      push hl
      push de
      push bc
        ld b,2
        ld de,newTilemapStorageRowClearTiles
        ld hl,$7D40
        di
          rawTilesToVdp_macro_safe
        ei
      pop bc
      pop de
      pop hl
      
      ret
    
    clearTilemapStorageTopRow_hop_ext:
      ; clear garbage
      call clearTilemapStorageTopRow_ext
      
      ; make up work
      ld hl,$5CF4
      ret
    
    clearTilemapStorageTopRow_landmine_ext:
      ; make up work (run script)
;      call $2688
      
      ; clear garbage
      jp clearTilemapStorageTopRow_ext
      
      ; make up work
;      ld b,$03
;      call $1C26
;      ret
  
  .ends

  ;=====
  ; clear screen for landmine effect
  ;=====
  
  .bank 1 slot 1
  .org $30C6
  .section "clear tilemap garbage on screen shake landmine 1" overwrite
    call clearTilemapStorageTopRow_landmine
  .ends
  
  .bank 0 slot 0
  .section "clear tilemap garbage on screen shake landmine 2" free
    clearTilemapStorageTopRow_landmine:
      call $1C26
      doBankedJumpSlot2NoParams clearTilemapStorageTopRow_landmine_ext
  .ends

  ;=====
  ; clear screen for gems placed effect
  ;=====
  
  .bank 2 slot 2
  .org $050F
  .section "clear tilemap garbage on screen shake gems 1" overwrite
    call clearTilemapStorageTopRow_landmine
  .ends

  ;=====
  ; clear screen for sea lion army
  ;=====
  
  .bank 2 slot 2
  .org $096E
  .section "clear tilemap garbage on screen shake sea lions 1" overwrite
    call clearTilemapStorageTopRow_landmine
  .ends

  ;=====
  ; clear screen for world destruction
  ;=====
  
  .bank 0 slot 0
  .org $2A25
  .section "clear tilemap garbage on screen shake world destruction 1" overwrite
    call clearTilemapStorageTopRow_landmine
  .ends

  ;=====
  ; clear screen for earthquake
  ;=====
  
  .bank 2 slot 2
  .org $084C
  .section "clear tilemap garbage on screen shake earthquake 1" overwrite
    call clearTilemapStorageTopRow_landmine
  .ends

;========================================
; use new title screen
;========================================

  ; new stuff
  .slot 1
  .section "new title screen components" superfree
    newTitleLogo_grp: .incbin "out/grp/title_logo.bin" FSIZE newTitleLogo_grp_size
    .define newTitleLogo_grp_numTiles newTitleLogo_grp_size/bytesPerTile
    
    newTitleComponent_grp: .incbin "out/grp/title_subcomponents.bin" FSIZE newTitleComponent_grp_size
    .define newTitleComponent_grp_numTiles newTitleComponent_grp_size/bytesPerTile
  .ends

  ; overwrite original logo tilemap
  .bank 13 slot 2
  .org $3AED
  .section "new title screen logo 1" overwrite
    .incbin "out/maps/title_logo.bin"
  .ends

  ; use new logo
  .bank 0 slot 0
  .org $0BE5
  .section "new title screen logo 2" overwrite
    ; title
    ld a,newTitleLogo_grp_numTiles
    ld b,:newTitleLogo_grp
    ld hl,newTitleLogo_grp
    ; vdp dst
    ld de,$4000
    call $3227
    
    ; components
    ld a,newTitleComponent_grp_numTiles
    ld hl,newTitleComponent_grp
    ; vdp dst
    ld de,$6000
    call $3227
  .ends

;========================================
; adjust cutscene timing
;========================================

  ;========================================
  ; halve the rate at which the cutscene text timer advances.
  ; this compensates for the decrease in the delay per cycle
  ; of the cutscene logic.
  ;========================================

  .bank 2 slot 2
  .org $2122
  .section "halve global cutscene timer 1" overwrite
    jp newTickSceneTimer
  .ends

  .bank 0 slot 0
  .section "halve global cutscene timer 2" free
    newTickSceneTimer:
      doBankedJumpNoParamsSlot2 newTickSceneTimer_ext
  .ends

  .slot 2
  .section "halve global cutscene timer 3" superfree
    newTickSceneTimer_ext:
      ; only increment the timer every other call
      ld hl,extraSceneTimer
      inc (hl)
      bit 0,(hl)
      jr nz,+
        ld a,($C0D7)
        ret
      +:
      
      ld hl,$C0D7
      inc (hl)
      ld a,(hl)
      ret
  .ends
  
  ;========================================
  ; intro
  ;========================================
  
  .bank 2 slot 2
  .org $2114
  .section "cutscenes 1" overwrite
    ; wait for vblank
    xor a
    call $0D3D
    doBankedJumpNoParamsSlot2 newTickSceneSubTimer
  .ends
  
  .slot 2
  .section "cutscenes 2" superfree
    newTickSceneSubTimer:
      ; halve counter parameter
      srl b
      ; if target counter value is odd and interpolation counter is
      ; set, add 1 to get interpolated target value
      jr nc,+
        ld hl,subsceneTimerInterpolationCounter
;        inc (hl)
        ld a,(hl)
        bit 0,a
        jr z,+
          inc b
      +:

      ; reduce counter parameter to 5/8ths
;      srl b
;      ld a,b
;      srl a
;      add a,b
;      srl b
;      srl b
;      sub b
;      ld b,a
      
      ; tick timer
      ld hl,$C0D6
      inc (hl)
      ld a,(hl)
      cp b
      ret c
      ld (hl),$00
      
      ; increment interpolation counter after each full count
      push hl
        ld hl,subsceneTimerInterpolationCounter
        inc (hl)
      pop hl
      
      ret
  .ends
  
  ;=====
  ; extend intro running section 1
  ; ("believe it or not"...)
  ;=====

  .bank 2 slot 2
  .org $1D98
  .section "intro running final length" overwrite
    ; NOTE: arle's animation phase for the second part of the running
    ; scene is not correctly reset after this part finishes.
    ; if her running animation finishes in the wrong phase, then it
    ; will be displayed incorrectly during the second part of the scene.
    ; this bug happens not to menifest with the numbers used in the original
    ; game and apparently went unnoticed for that reason.
    ; basically, if the animation is broken, change this number until it
    ; works.
    cp $5A+5
  .ends
  
  ;=====
  ; extend intro schezo appearance
  ; ("heheheh"...)
  ;=====

  .bank 2 slot 2
  .org $2100
  .section "intro schezo appearance" overwrite
    ld a,$14+22
  .ends
  
  ;=====
  ; intro 5 ("who are you")
  ;=====

;  .bank 2 slot 2
;  .org $2140
;  .section "intro 5a" overwrite
;    ld b,$0A/2
;  .ends
  
  ; delay to end of scene
  .bank 2 slot 2
  .org $2149
  .section "intro 5b" overwrite
    cp $1E+4
  .ends

  ; delay to end of text printing
  .bank 2 slot 2
  .org $214D
  .section "intro 5c" overwrite
    cp $14+4
  .ends
  
  ;=====
  ; adjust intro text positioning for lines that are supposed
  ; to be centered
  ;=====
  
  ; "whoa"
  .bank 2 slot 2
  .org $1FE7
  .section "intro text pos 9" overwrite
    ld hl,$D61A+(2*1)
  .ends
  
  ; "heheheh"
  .bank 2 slot 2
  .org $20D6
  .section "intro text pos 10" overwrite
    ld hl,$D48C+(2*0)
  .ends
  
  ; "who are you"
  .bank 2 slot 2
  .org $2134
  .section "intro text pos 8" overwrite
    ld hl,$D484+(2*(-2))
  .ends
  
  ; "i want you"
  .bank 2 slot 2
  .org $21D1
  .section "intro text pos 1" overwrite
    ld hl,$D482+(2*2)
  .ends
  
  ; "eek"
  .bank 2 slot 2
  .org $229C
  .section "intro text pos 2" overwrite
    ld hl,$D48A+(2*3)
  .ends
  
  ; "it's a pervert"
  .bank 2 slot 2
  .org $22C1
  .section "intro text pos 3" overwrite
    ld hl,$D486+(2*2)
  .ends
  
  ; "very much"
  .bank 2 slot 2
  .org $230B
  .section "intro text pos 4" overwrite
    ld hl,$D482+(2*4)
  .ends
  
  ; "!!!"
  .bank 2 slot 2
  .org $2374
  .section "intro text pos 5" overwrite
    ld hl,$D48A+(2*5)
  .ends
  
  ; "sleep"
  .bank 2 slot 2
  .org $240D
  .section "intro text pos 6" overwrite
    ld hl,$D486+(2*3)
  .ends
  
  ; "aaah"
  .bank 2 slot 2
  .org $24E5
  .section "intro text pos 7" overwrite
    ld hl,$D482+(2*4)
  .ends
  
  ;=====
  ; centering for lyra cutscene
  ;=====
  
  ; "lyra's ruins"
  .bank 2 slot 2
  .org $2951
  .section "lyra text pos 1" overwrite
    ld hl,$D610+(2*4)
  .ends
  
  ; "what's left of"
  .bank 2 slot 2
  .org $2993
  .section "lyra text pos 2" overwrite
    ld hl,$D610+(2*0)
  .ends
  
  ;=====
  ; intro dungeon scenes
  ;=====

  .bank 2 slot 2
  .org $2713
  .section "intro dungeon scene timers" overwrite
    ; byte 1 = timer
    ; byte 2 = number of characters (ignored in hack)
    
    ; "ow"
    .db $20,$16
    ; "what was up with that mage"
    .db $17+4,$0D
    ; "he can't get away with it"
    .db $1C+16,$12
    ; "ugh"
    .db $1F,$15
    ; "i'll use"
    .db $1F,$15
    ; "uhmm"
    .db $1A,$10
    ; "gasp"
    .db $20,$16
    ; "oh come on"
    .db $1A,$10
    ; "i lost"
    .db $28,$12
  .ends
  
  ;=====
  ; intro: fix dungeon scene scrolling
  ;=====

  ; subtimer
  .bank 2 slot 2
  .org $274F
  .section "intro dungeon scroll 1" overwrite
    ld b,$07*2
  .ends

  ; timer
  .bank 2 slot 2
  .org $2758
  .section "intro dungeon scroll 2" overwrite
    cp $38/2
  .ends
  
  ;=====
  ; intro: longer display of "that monster's standing watch"
  ;=====

  .bank 2 slot 2
  .org $279A
  .section "intro dungeon 'standing watch'" overwrite
    cp $58+8
  .ends
  
  ;=====
  ; ruins entrance cutscene: fix double-rope glitch
  ;=====

  .bank 2 slot 2
  .org $2B66
  .section "ruins cutscene fix 1" overwrite
    doBankedCallSlot2NoParams ruinsRope_ext
    nop
  .ends

  .slot 2
  .section "ruins cutscene fix 2" superfree
    ruinsRope_ext:
      ; increment scene timer.
      ; the game already did this, expecting it to tick from 0x36
      ; to 0x37, but due to the half-rate hack it remained at 0x36.
      ; if not corrected, this will cause arle's second rappeling
      ; animation to play again after the rope frays.
      ld hl,$C0D7
      inc (hl)
      
      ; make up work
      ld a,$40
      ld ($C0D8),a
      xor a
      ld ($C0D9),a
      ret
  .ends
  
  ;=====
  ; arle walking animation speed is directly tied to scene timer.
  ; halve to match
  ;=====

  .bank 2 slot 2
  .org $36CF
  .section "ending walk fix 1" overwrite
    call creditsWalkFix
  .ends

  .bank 2 slot 2
  .section "ending walk fix 2" free
    creditsWalkFix:
      ; only update walk animation every other sceneTimer tick
      bit 0,a
      jr z,+
        call $B793
      +:
      ret
  .ends
  
  ;=====
  ; extend "it's probably nothing important" line
  ;=====
  
  ; extra ticks to display the line
  .define endingExtendedLineExtraTime $0C

  .bank 2 slot 2
  .org $38BA
  .section "ending extend line 1" overwrite
    cp $46+endingExtendedLineExtraTime
  .ends

  .bank 2 slot 2
  .org $38C2
  .section "ending extend line 2" overwrite
    cp $28+endingExtendedLineExtraTime
  .ends

  .bank 2 slot 2
  .org $38CB
  .section "ending extend line 3" overwrite
    cp $11+endingExtendedLineExtraTime
  .ends */
  
  
  